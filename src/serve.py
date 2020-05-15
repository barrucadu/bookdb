#!/usr/bin/env python3

from common import fixup_book_for_index
from datetime import datetime
from elasticsearch import Elasticsearch
from elasticsearch.exceptions import ConflictError, NotFoundError
from flask import Flask, abort, jsonify, redirect, render_template, request, send_from_directory

import os
import re
import sys
import yaml


def flatten_tree_config(cfg, out, parent=None):
    for o in cfg:
        uuid = o["uuid"]
        out.append((uuid, o["name"], parent))
        flatten_tree_config(o.get("ordered_children", []), out, parent=uuid)
    return out


app = Flask(__name__)

es = Elasticsearch([os.getenv("ES_HOST", "http://localhost:9200")])

BASE_URI = os.getenv("BASE_URI", "http://bookdb.nyarlathotep")

COVER_DIR = os.getenv("COVER_DIR", "covers")

ALLOW_WRITES = os.getenv("ALLOW_WRITES", "0") == "1"

try:
    with open("config/uuids.yaml") as f:
        tree_config = yaml.safe_load(f)
except FileNotFoundError:
    print("Could not read config/uuids.yaml")
    sys.exit(1)
if "ordered_categories" not in tree_config:
    print("Missing ordered_categories")
    sys.exit(1)
if "ordered_locations" not in tree_config:
    print("Missing ordered_locations")
    sys.exit(1)

ORDERED_CATEGORIES = [(k, (v, p)) for k, v, p in flatten_tree_config(tree_config["ordered_categories"], [])]
ORDERED_LOCATIONS = [(k, v) for k, v, _ in flatten_tree_config(tree_config["ordered_locations"], [])]
CATEGORIES = {k: v for k, v in ORDERED_CATEGORIES}
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
    return MEMOISED_EXPAND_CATEGORY.get(uuid, [])


def children_categories(uuid):
    return MEMOISED_CHILDREN_CATEGORIES.get(uuid, [])


def full_category_name(uuid):
    return MEMOISED_FULL_CATEGORY_NAME.get(uuid, uuid)


def expand_location(uuid):
    return LOCATIONS.get(uuid, uuid)


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
    queries = [{"match_all": {}}]
    for k in request_args:
        vs = [v for v in request_args.getlist(k) if v]
        v = request_args.get(k)
        if k == "keywords" and v:
            search_params[k] = v
            queries.append({"query_string": {"query": v, "default_field": "display_title"}})
        elif k == "match" and v:
            search_params[k] = v
            queries.append({"term": {"has_been_read": v == "only-read"}})
        elif k == "location" and v:
            search_params[k] = v
            queries.append({"nested": {"path": "holdings", "query": {"bool": {"must": {"term": {"holdings.location_uuid": v}}}}}})
        elif k == "category" and v:
            search_params[k] = v
            queries.append({"terms": {"category_uuid": children_categories(v)}})
        elif k == "author[]" and vs:
            search_params[k] = vs
            queries.append({"terms": {"people.authors": vs}})
        elif k == "editor[]" and vs:
            search_params[k] = vs
            queries.append({"terms": {"people.editors": vs}})
        elif k == "translator[]" and vs:
            search_params[k] = vs
            queries.append({"terms": {"people.translators": vs}})

    results = es.search(
        index="bookdb",
        body={
            "query": {"bool": {"must": queries}},
            "aggs": {
                "author": {"terms": {"field": "people.authors", "size": 500}},
                "editor": {"terms": {"field": "people.editors", "size": 500}},
                "translator": {"terms": {"field": "people.translators", "size": 500}},
                "has_been_read": {"terms": {"field": "has_been_read", "size": 500}},
                "category_uuid": {"terms": {"field": "category_uuid", "size": 500}},
                "holdings": {"nested": {"path": "holdings"}, "aggs": {"location_uuid": {"terms": {"field": "holdings.location_uuid", "size": 500}}}},
            },
            "size": 5000,
        },
    )

    agg_match = {}
    read = list(d["doc_count"] for d in results["aggregations"]["has_been_read"]["buckets"] if d["key_as_string"] == "true")
    unread = list(d["doc_count"] for d in results["aggregations"]["has_been_read"]["buckets"] if d["key_as_string"] == "false")
    if read:
        agg_match["only-read"] = min(read)
    if unread:
        agg_match["only-unread"] = min(unread)

    hits = results["hits"]["hits"]
    return {
        "aggregations": {
            "author": {d["key"]: d["doc_count"] for d in results["aggregations"]["author"]["buckets"]},
            "editor": {d["key"]: d["doc_count"] for d in results["aggregations"]["editor"]["buckets"]},
            "translator": {d["key"]: d["doc_count"] for d in results["aggregations"]["translator"]["buckets"]},
            "match": agg_match,
            "category": {d["key"]: {"expanded": expand_category(d["key"]), "count": d["doc_count"]} for d in results["aggregations"]["category_uuid"]["buckets"]},
            "location": {d["key"]: {"expanded": expand_location(d["key"]), "count": d["doc_count"]} for d in results["aggregations"]["holdings"]["location_uuid"]["buckets"]},
        },
        "books": sorted([transform_book(hit["_id"], hit["_source"]) for hit in hits], key=sort_key_for_book),
        "count": len(hits),
        "search_params": search_params,
    }


def is_invalid_isbn(code):
    # this function doesn't care about codes which aren't ISBNs at
    # all.
    if len(code) == 0 or not code[0].isdigit():
        return False

    try:
        ds = [10 if x == "X" else int(x) for x in code]
    except ValueError:
        return True

    if len(ds) == 10:
        check = ds[0] * 10 + ds[1] * 9 + ds[2] * 8 + ds[3] * 7 + ds[4] * 6 + ds[5] * 5 + ds[6] * 4 + ds[7] * 3 + ds[8] * 2 + ds[9]
        mod = 11
    elif len(ds) == 13:
        check = ds[0] + ds[1] * 3 + ds[2] + ds[3] * 3 + ds[4] + ds[5] * 3 + ds[6] + ds[7] * 3 + ds[8] + ds[9] * 3 + ds[10] + ds[11] * 3 + ds[12]
        mod = 10
    else:
        return True
    return check % mod != 0


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
        elif not re.match("^[a-zA-Z0-9-]+$", bId):
            errors.append("The code can only contain letters, numbers, and dashes.")
        elif is_invalid_isbn(bId):
            errors.append(f"The ISBN '{bId}' is invalid.")

    if form.get("category"):
        uuid = form.get("category")
        if uuid in CATEGORIES:
            book["category_uuid"] = uuid
        else:
            errors.append(f"There is no such category: {uuid}.")

    book["title"] = form.get("title", "").strip()
    if not book["title"]:
        errors.append("The title cannot be blank.")

    book["subtitle"] = form.get("subtitle", "").strip()
    book["volume_title"] = form.get("volume_title", "").strip()
    if form.get("volume_number", "").strip():
        book["volume_number"] = {"raw": form.get("volume_number").strip()}
    if form.get("fascicle_number", "").strip():
        book["fascicle_number"] = {"raw": form.get("fascicle_number").strip()}

    book["has_been_read"] = "has_been_read" in form
    book["last_read_date"] = form.get("last_read_date", "").strip()

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

    book["bucket"] = form.get("bucket", "").strip()

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
        "authors": list(all_books["aggregations"]["author"].keys()),
        "editors": list(all_books["aggregations"]["editor"].keys()),
        "translators": list(all_books["aggregations"]["translator"].keys()),
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

    num_read = results["aggregations"]["match"].get("only-read", 0)
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
