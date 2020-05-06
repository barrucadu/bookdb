#!/usr/bin/env python3

from elasticsearch import Elasticsearch

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
es.indices.create(index="bookdb", body=es_config)
