from elasticsearch import Elasticsearch
from elasticsearch.helpers import scan

import json
import os


def run():
    es = Elasticsearch([os.getenv("ES_HOST", "http://localhost:9200")])
    out = {}

    for doc in scan(es, index="bookdb"):
        out[doc["_id"]] = doc["_source"]

    print(json.dumps(out))


if __name__ == "__main__":
    run()
