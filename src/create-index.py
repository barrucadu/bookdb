#!/usr/bin/env python3

from elasticsearch import Elasticsearch
from elasticsearch.exceptions import RequestError

import os
import sys
import yaml

try:
    with open("config/elasticsearch-schema.yaml") as f:
        es_config = yaml.safe_load(f)
except FileNotFoundError:
    print("Could not read config/elasticsearch-schema.yaml")
    sys.exit(1)

es = Elasticsearch([os.getenv("ES_HOST", "http://localhost:9200")])
try:
    es.indices.create(index="bookdb", body=es_config)
except RequestError:
    if os.getenv("DELETE_EXISTING_INDEX", "0") == "1":
        print("Index already exists - recreating it...")
        es.indices.delete(index="bookdb")
        es.indices.create(index="bookdb", body=es_config)
    else:
        print("Index already exists - set DELETE_EXISTING_INDEX=1 to recreate it")
        sys.exit(2)

if len(sys.argv) == 2:
    try:
        with open(sys.argv[1]) as f:
            dump = yaml.safe_load(f)
    except FileNotFoundError:
        print(f"Could not open data file {sys.argv[1]}")
        sys.exit(1)

    try:
        for doc_id, doc in dump.items():
            es.index(index="bookdb", id=doc_id, body=doc)
        print(f"Indexed {len(dump)} records")
    except AttributeError:
        print(f"Expected {sys.argv[1]} to be an object")
        sys.exit(3)
