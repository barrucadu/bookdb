from bookdb.common import COVER_DIR, THUMB_DIR, cover_file_for, thumb_file_for, fixup_book_for_index
import bookdb.codes

from datetime import datetime
from elasticsearch import Elasticsearch
from elasticsearch.exceptions import ConflictError, ConnectionError, NotFoundError
from flask import Flask, abort, jsonify, make_response, redirect, render_template, request, send_from_directory
from werkzeug.exceptions import HTTPException

import os
import re
import subprocess
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

ALLOW_WRITES = os.getenv("ALLOW_WRITES", "0") == "1"

UUIDS_FILE = os.getenv("UUIDS_FILE", "config/uuids.yaml")

try:
    with open(UUIDS_FILE) as f:
        tree_config = yaml.safe_load(f)
except FileNotFoundError:
    print(f"Could not read {UUIDS_FILE}")
    sys.exit(1)
if "ordered_categories" not in tree_config:
    print("Missing ordered_categories")
    sys.exit(1)
if "ordered_locations" not in tree_config:
    print("Missing ordered_locations")
    sys.exit(1)

if not os.path.isdir(THUMB_DIR):
    os.makedirs(THUMB_DIR)

COVER_MAX_AGE = 60 * 60 * 24 * 7 * 4 * 3
THUMB_MAX_AGE = COVER_MAX_AGE
STATIC_MAX_AGE = 60 * 60 * 24 * 7

DATE_FORMAT = "%Y-%m-%dT%H:%M:%S"

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


###############################################################################
## Search helpers


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
                "author": {"terms": {"field": "people.authors", "size": 1000}},
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


###############################################################################
## Form helpers


def form_to_book(form, files, this_is_an_insert=True):
    bId = None
    book = {}
    cover = None
    errors = []

    now = datetime.now().strftime(DATE_FORMAT)
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
        elif not bookdb.codes.validate(bId):
            errors.append(f"The code '{bId}' is invalid.")

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


###############################################################################
## Response helpers


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


def accepts_html(request):
    return (request.view_args or {}).get("fmt") in [None, "html"] and request.accept_mimetypes.accept_html


def accepts_json(request):
    return (request.view_args or {}).get("fmt") in [None, "json"] and request.accept_mimetypes.accept_json


def unacceptable(request):
    return not accepts_html(request) and not accepts_json(request)


def fmt_errors(request, book, errors):
    if accepts_html(request):
        return render_template("edit.html", book=book, errors=errors, **standard_template_args())
    elif accepts_json(request):
        return jsonify(errors)
    else:
        abort(500)  # unreachable if 'unacceptable' is checked


def fmt_message(request, message):
    if accepts_html(request):
        return render_template("info.html", message=message, base_uri=BASE_URI)
    elif accepts_json(request):
        return jsonify({"message": message})
    else:
        abort(500)  # unreachable if 'unacceptable' is checked


def fmt_http_error(request, code, message):
    if accepts_html(request):
        return render_template("error.html", code=code, message=message, base_uri=BASE_URI), code
    else:
        return jsonify({"code": code, "message": message}), code


###############################################################################
## Controllers


def do_create_book(request):
    bId, candidate, cover, errors = form_to_book(request.form, request.files)
    if errors:
        return fmt_errors(request, candidate, errors), 422

    try:
        es.create(index="bookdb", id=bId, document=candidate)
    except ConflictError:
        return fmt_errors(request, candidate, ["Code already in use"]), 409

    if cover:
        cover.save(cover_file_for(bId))

    thumb_file = thumb_file_for(bId)
    if os.path.isfile(thumb_file):
        os.remove(thumb_file)

    resp = make_response(fmt_message(request, "The book has been created."), 201)
    resp.headers["Location"] = f"{BASE_URI}/book/{bId}"
    return resp


def do_update_book(bId, book, request):
    _, candidate, cover, errors = form_to_book(request.form, request.files, this_is_an_insert=False)
    if errors:
        return fmt_errors(request, {"id": bId, **candidate}, errors), 422

    es.update(index="bookdb", id=bId, doc=candidate)

    if cover:
        cover.save(cover_file_for(bId))

        thumb_file = thumb_file_for(bId)
        if os.path.isfile(thumb_file):
            os.remove(thumb_file)

    return fmt_message(request, "The book has been updated.")


def do_delete_book(bId, request):
    es.delete(index="bookdb", id=bId)

    cover_file = cover_file_for(bId)
    thumb_file = thumb_file_for(bId)
    if os.path.isfile(cover_file):
        os.remove(cover_file)
    if os.path.isfile(thumb_file):
        os.remove(thumb_file)

    return fmt_message(request, "The book has been deleted.")


###############################################################################
## Routes


@app.route("/")
@app.route("/list")
def redirect_to_search():
    return redirect(f"{BASE_URI}/search", code=301)


@app.route("/search")
@app.route("/search.<fmt>")
def search(**kwargs):
    if unacceptable(request):
        abort(406)

    results = do_search(request.args)

    if accepts_html(request):
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
    else:
        return jsonify(results)


@app.route("/book/<bId>", methods=["GET", "PUT", "DELETE", "HEAD"])
@app.route("/book/<bId>.<fmt>", methods=["GET", "PUT", "DELETE", "HEAD"])
def book_controller(bId, **kwargs):
    if request.method in ["PUT", "DELETE"] and not ALLOW_WRITES:
        abort(403)

    book = get_book(bId)
    if not book:
        abort(404)

    if unacceptable(request):
        abort(406)

    if request.method == "PUT":
        return do_update_book(bId, book, request)
    elif request.method == "DELETE":
        return do_delete_book(bId, request)
    else:
        if accepts_json(request):
            return jsonify(book)
        else:
            abort(406)  # reachable


@app.route("/add", methods=["GET", "HEAD", "POST"])
@app.route("/add.<fmt>", methods=["GET", "HEAD", "POST"])
def add(**kwargs):
    if not ALLOW_WRITES:
        abort(403)

    if unacceptable(request):
        abort(406)

    if request.method == "POST":
        return do_create_book(request)

    if accepts_html(request):
        return render_template("edit.html", book={}, **standard_template_args())
    else:
        abort(406)  # reachable


@app.route("/book/<bId>/edit", methods=["GET", "HEAD", "POST"])
@app.route("/book/<bId>/edit.<fmt>", methods=["GET", "HEAD", "POST"])
def edit(bId, **kwargs):
    if not ALLOW_WRITES:
        abort(403)

    book = get_book(bId)
    if not book:
        abort(404)

    if unacceptable(request):
        abort(406)

    if request.method == "POST":
        return do_update_book(bId, book, request)

    if accepts_html(request):
        return render_template("edit.html", book=book, **standard_template_args())
    else:
        abort(406)  # reachable


@app.route("/book/<bId>/delete", methods=["GET", "HEAD", "POST"])
@app.route("/book/<bId>/delete.<fmt>", methods=["GET", "HEAD", "POST"])
def delete(bId, **kwargs):
    if not ALLOW_WRITES:
        abort(403)

    book = get_book(bId)
    if not book:
        abort(404)

    if unacceptable(request):
        abort(406)

    if request.method == "POST":
        return do_delete_book(bId, request)

    if accepts_html(request):
        return render_template("delete.html", book=book, base_uri=BASE_URI)
    else:
        abort(406)  # reachable


@app.route("/book/<bId>/cover")
def book_cover(bId):
    book = get_book(bId)
    if not book:
        abort(404)
    if not book["cover_image_mimetype"]:
        abort(404)
    return send_from_directory(
        COVER_DIR,
        os.path.basename(cover_file_for(bId)),
        max_age=COVER_MAX_AGE,
        last_modified=datetime.strptime(book["updated_at"], DATE_FORMAT),
        mimetype=book["cover_image_mimetype"],
    )


@app.route("/book/<bId>/thumb")
def book_thumb(bId):
    book = get_book(bId)
    if not book:
        abort(404)
    if not book["cover_image_mimetype"]:
        abort(404)

    thumb_file = thumb_file_for(bId)
    if not os.path.isfile(thumb_file):
        subprocess.run(["convert", cover_file_for(bId), "-resize", "16x24", thumb_file])

    return send_from_directory(
        THUMB_DIR,
        os.path.basename(thumb_file),
        max_age=THUMB_MAX_AGE,
        last_modified=datetime.strptime(book["updated_at"], DATE_FORMAT),
        mimetype="image/jpeg",
    )


@app.route("/static/<path>")
def static_files(path):
    return send_from_directory("static", path, max_age=STATIC_MAX_AGE)


@app.errorhandler(ConnectionError)
def handle_connection_error(*args):
    return fmt_http_error(request, 503, "The search server is unavailable.  Try again in a minute or two.")


@app.errorhandler(HTTPException)
def handle_http_exception(e):
    return fmt_http_error(request, e.code, e.description)


def run():
    app.run(host="0.0.0.0", port=8888)


if __name__ == "__main__":
    run()
