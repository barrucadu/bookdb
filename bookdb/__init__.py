import os

from elasticsearch import Elasticsearch
from itertools import cycle

CODE_PREFIXES = ["isbn", "ean", "x"]

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


def validate_code(code):
    bits = code.split("-")
    if len(bits) <= 1:
        return False

    try:
        if bits[0] == "isbn":
            return __validate_isbn(bits[1]) and len(bits) == 2
        elif bits[0] == "ean":
            return __validate_ean(bits[1]) and len(bits) == 2
        elif bits[0] == "x":
            # Fallback for books without standard codes
            return True
    except ValueError:
        return False


def __validate_isbn(code):
    if len(code) == 10:
        return __weight_sum(code, [10, 9, 8, 7, 6, 5, 4, 3, 2, 1], x_is_ten=True) % 11 == 0
    elif len(code) == 13:
        return __weight_sum(code, [1, 3]) % 10 == 0
    else:
        return False


def __validate_ean(code):
    if len(code) == 8:
        return __weight_sum(code, [3, 1]) % 10 == 0
    elif len(code) == 13:
        return __weight_sum(code, [1, 3]) % 10 == 0
    else:
        return False


def __weight_sum(code, weights, x_is_ten=False):
    digits = [10 if x_is_ten and x == "X" else int(x) for x in code]

    return sum(d * w for d, w in list(zip(digits, cycle(weights))))
