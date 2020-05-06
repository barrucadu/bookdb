#!/usr/bin/env python3

from flask import Flask, abort, jsonify, request, send_from_directory

from elasticsearch import Elasticsearch
from elasticsearch.exceptions import NotFoundError

import os

app = Flask(__name__)

es = Elasticsearch([os.getenv("ES_HOST", "http://localhost:9200")])

COVER_DIR = os.getenv("COVER_DIR", "covers")

CATEGORIES = {
    "590ac55d-0644-4a71-b902-587faa5b03d9": ("Prose Fiction + Visual Novels", None),
    "c2bfb2d8-ce64-4480-aaa6-c2d2dcb34ace": ("Gothic, Horror, + Weird", "590ac55d-0644-4a71-b902-587faa5b03d9",),
    "bcb54905-2e45-4a8f-bda0-e1d12e4114d2": ("High Fantasy", "590ac55d-0644-4a71-b902-587faa5b03d9",),
    "ea4e9b04-3ebb-4ff3-b387-d8bfc603f58a": ("Miscellaneous", "590ac55d-0644-4a71-b902-587faa5b03d9",),
    "a3bbb1c6-5ff8-4ddf-81f4-820593a2a5ff": ("Manga", None),
    "217f8eaa-b54a-466c-83f8-3f569f46732e": ("Verse", None),
    "a7d83bc9-0352-4fb5-a9dc-68428562a17f": ("Religion, Mythology, + Folklore", None),
    "384664d5-e256-4787-a729-1eb5e7a7de6f": ("Abrahamic", "a7d83bc9-0352-4fb5-a9dc-68428562a17f",),
    "0435b803-d2e9-49b9-a1c7-bc7fc1b30cdd": ("Miscellaneous", "a7d83bc9-0352-4fb5-a9dc-68428562a17f",),
    "58c63aea-72b6-4988-96a0-aca572661303": ("Nonfiction", None),
    "219ae9f4-84f1-4ea2-a9cd-fbc16698a669": ("RPG Rulebooks", "58c63aea-72b6-4988-96a0-aca572661303",),
    "afc7135b-bf67-4284-bcc1-2bbd3386aea3": ("Computer Science, Software Engineering, + Digital Culture", "58c63aea-72b6-4988-96a0-aca572661303",),
    "ac4706f3-54c3-4e6d-a72d-85321d9dcd72": ("Politics, Philosophy, Economics, + History", "58c63aea-72b6-4988-96a0-aca572661303",),
    "b5451ba6-a3ef-4c27-99fc-a07d8cd54161": ("Kitchen, Food, + Recipe", "58c63aea-72b6-4988-96a0-aca572661303",),
    "fb2dd601-883a-4254-a294-fcf0a0f98d2f": ("Miscellaneous", "58c63aea-72b6-4988-96a0-aca572661303",),
}

LOCATIONS = {
    "be60be7b-a10f-42e1-8769-d43f12cad02d": "London",
    "f256ed66-4c09-4207-86de-adc8e9fb86ec": "Hull",
    "6a233e5e-3b64-4169-ac67-cf46113afd97": "Missing",
}


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


MEMOISED_EXPAND_CATEGORY = {}
MEMOISED_CHILDREN_CATEGORIES = {}

for uuid in CATEGORIES.keys():
    MEMOISED_EXPAND_CATEGORY[uuid] = __expand_category(uuid)
    MEMOISED_CHILDREN_CATEGORIES[uuid] = __children_categories(uuid)


def expand_category(uuid):
    return MEMOISED_EXPAND_CATEGORY[uuid]


def children_categories(uuid):
    return MEMOISED_CHILDREN_CATEGORIES[uuid]


def expand_location(uuid):
    return LOCATIONS[uuid]


def transform_book(bId, book):
    def convert_bit(bit):
        try:
            return int(bit)
        except ValueError:
            return bit

    book["id"] = bId

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
    for k in request_args:
        if k in ["keywords", "match", "location", "category"]:
            search_params[k] = request_args.get(k)
        elif k in ["author[]", "editor[]", "translator[]"]:
            search_params[k] = request_args.getlist(k)

    filters = {
        "people.authors": search_params.get("author[]"),
        "people.editors": search_params.get("editor[]"),
        "people.translators": search_params.get("translator[]"),
        "has_been_read": {"only-read": True, "only-unread": False}.get(search_params.get("match"), None),
        "holdings.location_uuid": search_params.get("location"),
    }

    queries = []
    if "keywords" in search_params:
        queries.append({"query_string": {"query": search_params["keywords"], "default_field": "display_title"}})
    else:
        queries.append({"match_all": {}})
    for f, vs in filters.items():
        for v in vs or []:
            queries.append({"term": {f: v}})
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
