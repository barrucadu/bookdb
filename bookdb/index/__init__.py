INDEX = "bookdb"

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


def fixup_book(book):
    """Get a book ready to be indexed."""

    book = {k: v for k, v in book.items() if v}

    if book.get("title"):
        book["display_title"] = __display_title(book)
    if book.get("volume_number"):
        book["volume_number"]["bits"] = __to_bits(book["volume_number"]["raw"])
    if book.get("fascicle_number"):
        book["fascicle_number"]["bits"] = __to_bits(book["fascicle_number"]["raw"])

    if not book.get("bucket") and book["people"].get("authors"):
        book["bucket"] = min(n.split()[-1].lower() for n in book["people"]["authors"])

    book["has_been_read"] = book.get("has_been_read") is True

    return book


def __display_title(book):
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


def __to_bits(cs):
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
