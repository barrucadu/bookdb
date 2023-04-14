import os

from elasticsearch import Elasticsearch

COVER_DIR = os.getenv("COVER_DIR", "covers")
THUMB_DIR = os.path.join(COVER_DIR, "thumbs")
ES_HOST = os.getenv("ES_HOST", "http://localhost:9200")


def elasticsearch():
    return Elasticsearch([ES_HOST])


def cover_file_for(bId):
    return os.path.join(COVER_DIR, bId)


def thumb_file_for(bId):
    return os.path.join(THUMB_DIR, bId + ".jpg")


def rename_cover_and_thumb(old, new):
    cover_file = cover_file_for(old)
    thumb_file = thumb_file_for(old)
    if os.path.isfile(cover_file):
        os.rename(cover_file, cover_file_for(new))
    if os.path.isfile(thumb_file):
        os.rename(thumb_file, thumb_file_for(new))


def delete_cover_and_thumb(bId):
    cover_file = cover_file_for(bId)
    thumb_file = thumb_file_for(bId)
    if os.path.isfile(cover_file):
        os.remove(cover_file)
    if os.path.isfile(thumb_file):
        os.remove(thumb_file)
