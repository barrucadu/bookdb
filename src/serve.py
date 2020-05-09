#!/usr/bin/env python3

from common import fixup_book_for_index
from datetime import datetime
from elasticsearch import Elasticsearch
from elasticsearch.exceptions import ConflictError, NotFoundError
from flask import Flask, abort, jsonify, redirect, render_template, request, send_from_directory

import os
import re

app = Flask(__name__)

es = Elasticsearch([os.getenv("ES_HOST", "http://localhost:9200")])

BASE_URI = os.getenv("BASE_URI", "http://bookdb.nyarlathotep")

COVER_DIR = os.getenv("COVER_DIR", "covers")

ALLOW_WRITES = os.getenv("ALLOW_WRITES", "0") == "1"

ORDERED_CATEGORIES = [
    ("a3bbb1c6-5ff8-4ddf-81f4-820593a2a5ff", ("Light Novels + Manga", None)),

    ("58c63aea-72b6-4988-96a0-aca572661303", ("Nonfiction", None)),
    ("afc7135b-bf67-4284-bcc1-2bbd3386aea3", ("Computer Science, Software Engineering, + Digital Culture", "58c63aea-72b6-4988-96a0-aca572661303")),
    ("b5451ba6-a3ef-4c27-99fc-a07d8cd54161", ("Kitchen, Food, + Recipe", "58c63aea-72b6-4988-96a0-aca572661303")),
    ("ac4706f3-54c3-4e6d-a72d-85321d9dcd72", ("Politics, Philosophy, Economics, + History", "58c63aea-72b6-4988-96a0-aca572661303")),
    ("219ae9f4-84f1-4ea2-a9cd-fbc16698a669", ("RPG Rulebooks", "58c63aea-72b6-4988-96a0-aca572661303")),
    ("fb2dd601-883a-4254-a294-fcf0a0f98d2f", ("Miscellaneous", "58c63aea-72b6-4988-96a0-aca572661303")),

    ("590ac55d-0644-4a71-b902-587faa5b03d9", ("Prose Fiction + Visual Novels", None)),
    ("c2bfb2d8-ce64-4480-aaa6-c2d2dcb34ace", ("Gothic, Horror, + Weird", "590ac55d-0644-4a71-b902-587faa5b03d9")),
    ("bcb54905-2e45-4a8f-bda0-e1d12e4114d2", ("High Fantasy", "590ac55d-0644-4a71-b902-587faa5b03d9")),
    ("ea4e9b04-3ebb-4ff3-b387-d8bfc603f58a", ("Miscellaneous", "590ac55d-0644-4a71-b902-587faa5b03d9")),

    ("a7d83bc9-0352-4fb5-a9dc-68428562a17f", ("Religion, Mythology, + Folklore", None)),
    ("384664d5-e256-4787-a729-1eb5e7a7de6f", ("Abrahamic", "a7d83bc9-0352-4fb5-a9dc-68428562a17f")),
    ("0435b803-d2e9-49b9-a1c7-bc7fc1b30cdd", ("Miscellaneous", "a7d83bc9-0352-4fb5-a9dc-68428562a17f")),

    ("217f8eaa-b54a-466c-83f8-3f569f46732e", ("Verse", None)),
]

CATEGORIES = {k: v for k, v in ORDERED_CATEGORIES}

ORDERED_LOCATIONS = [
    ("be60be7b-a10f-42e1-8769-d43f12cad02d", "London"),
    ("f256ed66-4c09-4207-86de-adc8e9fb86ec", "Hull"),
    ("6a233e5e-3b64-4169-ac67-cf46113afd97", "Missing"),
]

LOCATIONS = {k: v for k, v in ORDERED_LOCATIONS}


def __expand_category(uuid):
    out = []
    while uuid is not None:
        name, uuid = CATEGORIES[uuid]
        out.append(name)

    return out


def __children_categories(uuid):
    out = [uuid]
    for k, v in CATEGORIES.items():
        if v[1] == uuid:
            out.extend(__children_categories(k))
    return out


def __full_category_name(uuid):
    return " / ".join(reversed(__expand_category(uuid)))


MEMOISED_EXPAND_CATEGORY = {}
MEMOISED_CHILDREN_CATEGORIES = {}
MEMOISED_FULL_CATEGORY_NAME = {}

for uuid in CATEGORIES.keys():
    MEMOISED_EXPAND_CATEGORY[uuid] = __expand_category(uuid)
    MEMOISED_CHILDREN_CATEGORIES[uuid] = __children_categories(uuid)
    MEMOISED_FULL_CATEGORY_NAME[uuid] = __full_category_name(uuid)


def expand_category(uuid):
    return MEMOISED_EXPAND_CATEGORY[uuid]


def children_categories(uuid):
    return MEMOISED_CHILDREN_CATEGORIES[uuid]


def full_category_name(uuid):
    return MEMOISED_FULL_CATEGORY_NAME[uuid]


def expand_location(uuid):
    return LOCATIONS[uuid]


def transform_book(bId, book):
    def convert_bit(bit):
        try:
            return int(bit)
        except ValueError:
            return bit

    book = {k: v for k, v in book.items() if v}
    book["id"] = bId
    book["has_cover_image"] = "cover_image_mimetype" in book

    if "volume_number" in book:
        book["volume_number"]["bits"] = [convert_bit(bit) for bit in book["volume_number"]["bits"]]
    if "fascicle_number" in book:
        book["fascicle_number"]["bits"] = [convert_bit(bit) for bit in book["fascicle_number"]["bits"]]
    if "holdings" in book:
        for holding in book["holdings"]:
            holding["location"] = expand_location(holding["location_uuid"])
    if "category_uuid" in book:
        book["category"] = expand_category(book["category_uuid"])
        book["category_name"] = full_category_name(book["category_uuid"])

    return book


def sort_key_for_book(book):
    return (
        book["bucket"],
        book["title"].lower(),
        book.get("volume_number", {}).get("bits", []),
        book.get("fascicle_number", {}).get("bits", []),
        book.get("subtitle", "").lower(),
        book.get("volume_title", "").lower(),
    )


def get_book(bId):
    try:
        book = es.get(index="bookdb", id=bId)
        return transform_book(bId, book["_source"])
    except NotFoundError:
        return None


def do_search(request_args):
    search_params = {}
    for k in request_args:
        if k in ["keywords", "match", "location", "category"]:
            v = request_args.get(k)
            if v:
                search_params[k] = v
        elif k in ["author[]", "editor[]", "translator[]"]:
            vs = [v for v in request_args.getlist(k) if v]
            if vs:
                search_params[k] = vs

    filters = {
        "people.authors": search_params.get("author[]"),
        "people.editors": search_params.get("editor[]"),
        "people.translators": search_params.get("translator[]"),
        "has_been_read": {"only-read": [True], "only-unread": [False]}.get(search_params.get("match"), None),
    }

    queries = []
    if "keywords" in search_params:
        queries.append({"query_string": {"query": search_params["keywords"], "default_field": "display_title"}})
    else:
        queries.append({"match_all": {}})
    for f, vs in filters.items():
        for v in vs or []:
            queries.append({"term": {f: v}})
    if "location" in search_params:
        queries.append({"nested": {"path": "holdings", "query": {"bool": {"must": {"term": {"holdings.location_uuid": search_params["location"]}}}}}})
    if "category" in search_params:
        queries.append({"terms": {"category_uuid": children_categories(search_params["category"])}})

    body = {
        "query": {"bool": {"must": queries}},
        "aggs": {
            **{
                k: {"terms": {"field": v, "size": 500}}
                for k, v in [("author", "people.authors"), ("editor", "people.editors"), ("translator", "people.translators"), ("match", "has_been_read"), ("category", "category_uuid")]
            },
            **{"location": {"nested": {"path": "holdings"}, "aggs": {"location": {"terms": {"field": "holdings.location_uuid", "size": 500}}}}},
        },
        "size": 5000,
    }

    results = es.search(index="bookdb", body=body)
    hits = results["hits"]["hits"]

    aggregations = {k: results["aggregations"][k]["buckets"] for k in ["author", "editor", "translator"]}
    aggregations["match"] = []
    read = list(d["doc_count"] for d in results["aggregations"]["match"]["buckets"] if d["key_as_string"] == "true")
    unread = list(d["doc_count"] for d in results["aggregations"]["match"]["buckets"] if d["key_as_string"] == "false")
    if read:
        aggregations["match"].append({"key": "only-read", "doc_count": min(read)})
    if unread:
        aggregations["match"].append({"key": "only-unread", "doc_count": min(unread)})
    aggregations["category"] = [{"key": expand_category(d["key"]), "key_uuid": d["key"], "doc_count": d["doc_count"]} for d in results["aggregations"]["category"]["buckets"]]
    aggregations["location"] = [{"key": expand_location(d["key"]), "key_uuid": d["key"], "doc_count": d["doc_count"]} for d in results["aggregations"]["location"]["location"]["buckets"]]

    return {
        "aggregations": {k: sorted(v, key=lambda d: d["key"]) for k, v in aggregations.items()},
        "books": sorted([transform_book(hit["_id"], hit["_source"]) for hit in hits], key=sort_key_for_book),
        "count": len(hits),
        "search_params": search_params,
    }


def form_to_book(form, files, this_is_an_insert=True):
    bId = None
    book = {}
    cover = None
    errors = []

    now = datetime.now().strftime("%Y-%m-%dT%H:%M:%S")
    if this_is_an_insert:
        book["created_at"] = now
    book["updated_at"] = now

    if this_is_an_insert:
        if form.get("code"):
            bId = form.get("code").strip()
        if not bId:
            errors.append("The code cannot be blank.")
        if not re.match("^[a-zA-Z0-9-]+$", bId):
            errors.append("The code can only contain letters, numbers, and dashes.")

    if form.get("category"):
        uuid = form.get("category")
        if uuid in CATEGORIES:
            book["category_uuid"] = uuid
        else:
            errors.append(f"There is no such category: {uuid}.")

    book["title"] = form.get("title", "").strip()
    if not book["title"]:
        errors.append("The title cannot be blank.")

    book["subtitle"] = form.get("subtitle", "").strip() or None
    book["volume_title"] = form.get("volume_title", "").strip() or None
    if form.get("volume_number", "").strip():
        book["volume_number"] = {"raw": form.get("volume_number").strip()}
    else:
        book["volume_number"] = None
    if form.get("fascicle_number", "").strip():
        book["fascicle_number"] = {"raw": form.get("fascicle_number").strip()}
    else:
        book["fascicle_number"] = None

    book["has_been_read"] = "has_been_read" in form
    book["last_read_date"] = form.get("last_read_date", "").strip() or None

    book["people"] = {
        "authors": [n.strip() for n in form.getlist("author[]") if n.strip()],
        "editors": [n.strip() for n in form.getlist("editor[]") if n.strip()],
        "translators": [n.strip() for n in form.getlist("translator[]") if n.strip()],
    }
    if not book["people"]["authors"]:
        errors.append("There must be at least one author.")

    book["holdings"] = []
    for uuid, notes in zip(form.getlist("location[]"), form.getlist("notes[]")):
        if not uuid:
            continue
        if uuid in LOCATIONS:
            book["holdings"].append({"location_uuid": uuid, "notes": notes})
        else:
            errors.append(f"There is no such location: {uuid}.")
    if not book["holdings"]:
        errors.append("There must be at least one holding.")

    book["bucket"] = form.get("bucket", "").strip() or None

    if "cover" in files and files["cover"].filename:
        if "." not in files["cover"].filename:
            errors.append("Cover filename must be of the form [a-zA-Z0-9-]+.{gif,jpg,jpeg,png}")
        else:
            ext = files["cover"].filename.rsplit(".", 1)[1].lower()
            if ext == "jpg":
                ext = "jpeg"
            if ext in ["gif", "jpeg", "png"]:
                book["cover_image_mimetype"] = f"image/{ext}"
                cover = files["cover"]
            else:
                errors.append("Cover filename must be of the form [a-zA-Z0-9-]+.{gif,jpg,jpeg,png}")

    return bId, fixup_book_for_index(book), cover, errors


def standard_template_args():
    all_books = do_search({})

    return {
        "allow_writes": ALLOW_WRITES,
        "base_uri": BASE_URI,
        "locations": [{"key": k, "label": v} for k, v in ORDERED_LOCATIONS],
        "categories": [{"key": k, "label": full_category_name(k)} for k, _ in ORDERED_CATEGORIES],
        "authors": [d["key"] for d in all_books["aggregations"]["author"]],
        "editors": [d["key"] for d in all_books["aggregations"]["editor"]],
        "translators": [d["key"] for d in all_books["aggregations"]["translator"]],
    }


@app.route("/")
def redirect_index():
    return redirect(f"{BASE_URI}/search", code=301)


@app.route("/list")
def redirect_list():
    return redirect(f"{BASE_URI}/search", code=301)


@app.route("/search")
def search():
    results = do_search(request.args)

    num_read = list(d["doc_count"] for d in results["aggregations"]["match"] if d["key"] == "only-read")
    if num_read:
        num_read = min(num_read)
    else:
        num_read = 0

    return render_template(
        "search.html",
        books=results["books"],
        num_authors=len(results["aggregations"]["author"]),
        num_read=num_read,
        percent_read=int((num_read / results["count"]) * 100) if results["count"] > 0 else 100,
        search_params=results["search_params"],
        **standard_template_args(),
    )


@app.route("/add", methods=["GET", "HEAD", "POST"])
def add():
    if not ALLOW_WRITES:
        abort(403)

    if request.method == "POST":
        bId, candidate, cover, errors = form_to_book(request.form, request.files)
        if errors:
            return render_template("edit.html", book=candidate, errors=errors, **standard_template_args())
        try:
            es.create(index="bookdb", id=bId, body=candidate)
        except ConflictError:
            return render_template("edit.html", book=candidate, errors=["Code already in use"], **standard_template_args())
        if cover:
            cover.save(os.path.join(COVER_DIR, bId))
        return render_template("info.html", message="The book has been created.", base_uri=BASE_URI)

    return render_template("edit.html", book={}, **standard_template_args())


@app.route("/book/<bId>/edit", methods=["GET", "HEAD", "POST"])
def edit(bId):
    if not ALLOW_WRITES:
        abort(403)

    book = get_book(bId)
    if not book:
        abort(404)

    if request.method == "POST":
        _, candidate, cover, errors = form_to_book(request.form, request.files, this_is_an_insert=False)
        if errors:
            return render_template("edit.html", book={"id": bId, **candidate}, errors=errors, **standard_template_args())
        es.update(index="bookdb", id=bId, body={"doc": candidate})
        if cover:
            cover.save(os.path.join(COVER_DIR, bId))
        return render_template("info.html", message="The book has been updated.", base_uri=BASE_URI)

    return render_template("edit.html", book=book, **standard_template_args())


@app.route("/book/<bId>/delete", methods=["GET", "HEAD", "POST"])
def delete(bId):
    if not ALLOW_WRITES:
        abort(403)

    book = get_book(bId)
    if not book:
        abort(404)

    if request.method == "POST":
        es.delete(index="bookdb", id=bId)
        return render_template("info.html", message="The book has been deleted.", base_uri=BASE_URI)

    return render_template("delete.html", book=book, base_uri=BASE_URI)


@app.route("/search.json")
def search_json():
    return jsonify(do_search(request.args))


@app.route("/book/<bId>.json")
def book_json(bId):
    book = get_book(bId)
    if not book:
        abort(404)
    return jsonify(book)


@app.route("/book/<bId>/cover")
def book_cover(bId):
    book = get_book(bId)
    if not book:
        abort(404)
    if not book["cover_image_mimetype"]:
        abort(404)
    return send_from_directory(COVER_DIR, bId, mimetype=book["cover_image_mimetype"])


@app.route("/static/<path>")
def static_files(path):
    return send_from_directory("static", path)


app.run(host="0.0.0.0", port=8888)
