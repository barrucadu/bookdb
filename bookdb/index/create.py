import bookdb
import bookdb.codes
import bookdb.index

from elasticsearch.exceptions import RequestError
from elasticsearch.helpers import bulk

import os
import sys
import yaml


def fixup_legacy_codes(dump):
    fixed_dump = {}
    for doc_id, doc in dump.items():
        if bookdb.codes.validate(doc_id):
            fixed_dump[doc_id] = doc
            continue

        for prefix in bookdb.codes.CODES:
            candidate = prefix + "-" + doc_id
            if bookdb.codes.validate(candidate):
                fixed_doc_id = candidate
                bookdb.rename_cover_and_thumb(doc_id, fixed_doc_id)
                print(f"Renamed {doc_id} to {fixed_doc_id}")
                break
        fixed_dump[fixed_doc_id] = doc

    return fixed_dump


def run():
    es = bookdb.elasticsearch()
    try:
        es.indices.create(index=bookdb.index.NAME, mappings={"properties": bookdb.index.SCHEMA})
    except RequestError:
        if os.getenv("DELETE_EXISTING_INDEX", "0") == "1":
            print("Index already exists - recreating it...")
            es.indices.delete(index=bookdb.index.NAME)
            es.indices.create(index=bookdb.index.NAME, mappings={"properties": bookdb.index.SCHEMA})
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
            if os.getenv("FIXUP_LEGACY_CODES", "0") == "1":
                dump = fixup_legacy_codes(dump)
            ok, errors = bulk(es, [{"_index": bookdb.index.NAME, "_id": doc_id, "_source": bookdb.index.fixup_book(doc)} for doc_id, doc in dump.items()])
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
