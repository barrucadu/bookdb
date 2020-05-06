#!/usr/bin/env python3

from flask import Flask, abort, jsonify, send_from_directory

from elasticsearch import Elasticsearch
from elasticsearch.exceptions import NotFoundError

import os

app = Flask(__name__)

es = Elasticsearch([os.getenv("ES_HOST", "http://localhost:9200")])

COVER_DIR = os.getenv("COVER_DIR", "covers")

CATEGORIES = {}

LOCATIONS = {}


def transform_book(book):
    def convert_bit(bit):
        try:
            return int(bit)
        except ValueError:
            return bit

    if "volume_number" in book:
        book["volume_number"]["bits"] = [convert_bit(bit) for bit in book["volume_number"]["bits"]]
    if "fascicle_number" in book:
        book["fascicle_number"]["bits"] = [convert_bit(bit) for bit in book["fascicle_number"]["bits"]]
    if "holdings" in book:
        for holding in book["holdings"]:
            holding["location"] = LOCATIONS[holding["location_uuid"]]
    if "category_uuid" in book:
        book["category"] = CATEGORIES[book["category_uuid"]]

    return book


def get_book(bId):
    try:
        book = es.get(index="bookdb", id=bId)
        return transform_book(book["_source"])
    except NotFoundError:
        return None


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
