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


def display_title(book):
    """Compute the display title of a book."""

    out = book["title"]
    if book.get("subtitle"):
        out += f": {book['subtitle']}"
    if book.get("volume_number") and book.get("fascicle_number"):
        out += f" (vol. {book['volume_number']['raw']}; fas. {book['fascicle_number']['raw']})"
    elif book.get("volume_number"):
        out += f" (vol. {book['volume_number']['raw']})"
    elif book.get("fascicle_number"):
        out += f" (fas. {book['fascicle_number']['raw']})"
    if book.get("volume_title"):
        out += f" / {book['volume_title']}"
    return out


def to_bits(cs):
    """Split a string into a list of numeric and non-numeric substrings."""

    if not cs:
        return []

    out = []
    tmp = ""
    is_number = True
    first = True
    for c in cs:
        if c in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]:
            if is_number:
                tmp += c
            else:
                if not first:
                    out.append(tmp)
                tmp = c
                is_number = True
        else:
            if is_number:
                if not first:
                    out.append(tmp)
                tmp = c
                is_number = False
            else:
                tmp += c
        first = False
    if not first:
        out.append(tmp)
    return out


def fixup_book_for_index(book):
    """Get a book ready to be indexed."""

    book = {k: v for k, v in book.items() if v}

    if book.get("title"):
        book["display_title"] = display_title(book)
    if book.get("volume_number"):
        book["volume_number"]["bits"] = to_bits(book["volume_number"]["raw"])
    if book.get("fascicle_number"):
        book["fascicle_number"]["bits"] = to_bits(book["fascicle_number"]["raw"])

    if not book.get("bucket") and book["people"].get("authors"):
        book["bucket"] = min(n.split()[-1].lower() for n in book["people"]["authors"])

    book["has_been_read"] = book.get("has_been_read") is True

    return book
