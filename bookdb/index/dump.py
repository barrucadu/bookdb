import bookdb
import bookdb.index

from elasticsearch.helpers import scan

import json


def run():
    out = {}

    for doc in scan(bookdb.elasticsearch(), index=bookdb.index.NAME):
        out[doc["_id"]] = doc["_source"]

    print(json.dumps(out))


if __name__ == "__main__":
    run()
