from bookdb.common import fixup_book_for_index

from elasticsearch import Elasticsearch
from elasticsearch.exceptions import RequestError
from elasticsearch.helpers import bulk

import os
import sys
import yaml

SCHEMA = {
    "title": {
        "type": "text",
        "analyzer": "english",
    },
    "subtitle": {
        "type": "text",
        "analyzer": "english",
    },
    "volume_title": {
        "type": "text",
        "analyzer": "english",
    },
    "display_title": {
        "type": "text",
        "analyzer": "english",
    },
    "volume_number": {
        "type": "object",
        "properties": {
            "raw": {
                "type": "keyword",
            },
            # 'raw' split into numeric and non-numeric chunks
            "bits": {
                "type": "keyword",
            },
        },
    },
    "fascicle_number": {
        "type": "object",
        "properties": {
            "raw": {
                "type": "keyword",
            },
            # 'raw' split into numeric and non-numeric chunks
            "bits": {
                "type": "keyword",
            },
        },
    },
    "people": {
        "type": "object",
        "dynamic": "strict",
        "properties": {
            "authors": {
                "type": "keyword",
            },
            "translators": {
                "type": "keyword",
            },
            "editors": {
                "type": "keyword",
            },
        },
    },
    "has_been_read": {
        "type": "boolean",
    },
    "last_read_date": {
        "type": "date",
        "format": "yyyy-MM-dd",
    },
    # all entries with a cover image have this field set
    "cover_image_mimetype": {
        "type": "keyword",
    },
    # a holding is a single copy of a book
    "holdings": {
        "type": "nested",
        "dynamic": "strict",
        "properties": {
            "location_uuid": {
                "type": "keyword",
            },
            "notes": {
                "type": "text",
            },
        },
    },
    # case-insensitive bucket (usually first author surname) to use for sorting
    "bucket": {
        "type": "keyword",
    },
    "category_uuid": {
        "type": "keyword",
    },
    # metadata
    "created_at": {
        "type": "date",
    },
    "updated_at": {
        "type": "date",
    },
}


def run():
    es = Elasticsearch([os.getenv("ES_HOST", "http://localhost:9200")])
    try:
        es.indices.create(index="bookdb", mappings={"properties": SCHEMA})
    except RequestError:
        if os.getenv("DELETE_EXISTING_INDEX", "0") == "1":
            print("Index already exists - recreating it...")
            es.indices.delete(index="bookdb")
            es.indices.create(index="bookdb", mappings={"properties": SCHEMA})
        else:
            print("Index already exists - set DELETE_EXISTING_INDEX=1 to recreate it")
            sys.exit(2)

    if len(sys.argv) == 2:
        try:
            if sys.argv[1] == "-":
                dump = yaml.safe_load(sys.stdin)
            else:
                with open(sys.argv[1]) as f:
                    dump = yaml.safe_load(f)
        except FileNotFoundError:
            print(f"Could not open data file {sys.argv[1]}")
            sys.exit(1)

        try:
            ok, errors = bulk(es, [{"_index": "bookdb", "_id": doc_id, "_source": fixup_book_for_index(doc)} for doc_id, doc in dump.items()])
            print(f"Indexed {ok} records")
            if errors:
                print("")
            for error in errors:
                print(error)
        except AttributeError:
            print(f"Expected {sys.argv[1]} to be an object")
            sys.exit(3)


if __name__ == "__main__":
    run()