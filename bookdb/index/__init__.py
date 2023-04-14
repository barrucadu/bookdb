NAME = "bookdb"

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


def get(es, bId):
    return __fixup_get(bId, es.get(index=NAME, id=bId)["_source"])


def insert(es, bId, book):
    es.create(index=NAME, id=bId, document=book)


def update(es, bId, book):
    es.update(index=NAME, id=bId, doc=book)


def delete(es, bId):
    es.delete(index=NAME, id=bId)


def search(es, query_string=None, has_been_read=None, location_uuid=None, category_uuid=None, authors=None, editors=None, translators=None):
    queries = [{"match_all": {}}]
    if query_string is not None:
        queries.append({"query_string": {"query": query_string, "default_field": "display_title"}})
    if has_been_read is not None:
        queries.append({"term": {"has_been_read": has_been_read}})
    if location_uuid is not None:
        queries.append({"nested": {"path": "holdings", "query": {"bool": {"must": {"term": {"holdings.location_uuid": location_uuid}}}}}})
    if category_uuid is not None:
        queries.append({"terms": {"category_uuid": category_uuid}})
    if authors is not None:
        queries.append({"terms": {"people.authors": authors}})
    if editors is not None:
        queries.append({"terms": {"people.editors": editors}})
    if translators is not None:
        queries.append({"terms": {"people.translators": translators}})

    results = es.search(
        index=NAME,
        body={
            "query": {"bool": {"must": queries}},
            "aggs": {
                "author": {"terms": {"field": "people.authors", "size": 1000}},
                "editor": {"terms": {"field": "people.editors", "size": 500}},
                "translator": {"terms": {"field": "people.translators", "size": 500}},
                "has_been_read": {"terms": {"field": "has_been_read", "size": 500}},
                "category_uuid": {"terms": {"field": "category_uuid", "size": 500}},
                "holdings": {"nested": {"path": "holdings"}, "aggs": {"location_uuid": {"terms": {"field": "holdings.location_uuid", "size": 500}}}},
            },
            "size": 5000,
        },
    )

    agg_match = {}
    read = list(d["doc_count"] for d in results["aggregations"]["has_been_read"]["buckets"] if d["key_as_string"] == "true")
    unread = list(d["doc_count"] for d in results["aggregations"]["has_been_read"]["buckets"] if d["key_as_string"] == "false")
    if read:
        agg_match["only-read"] = min(read)
    if unread:
        agg_match["only-unread"] = min(unread)

    hits = results["hits"]["hits"]
    return {
        "aggregations": {
            "author": {d["key"]: d["doc_count"] for d in results["aggregations"]["author"]["buckets"]},
            "editor": {d["key"]: d["doc_count"] for d in results["aggregations"]["editor"]["buckets"]},
            "translator": {d["key"]: d["doc_count"] for d in results["aggregations"]["translator"]["buckets"]},
            "match": agg_match,
            "category": {d["key"]: d["doc_count"] for d in results["aggregations"]["category_uuid"]["buckets"]},
            "location": {d["key"]: d["doc_count"] for d in results["aggregations"]["holdings"]["location_uuid"]["buckets"]},
        },
        "books": sorted([__fixup_get(hit["_id"], hit["_source"]) for hit in hits], key=lambda book: book["sort_key"]),
        "count": len(hits),
    }


def __fixup_get(bId, book):
    def convert_bit(bit):
        try:
            return int(bit)
        except ValueError:
            return bit

    book = {k: v for k, v in book.items() if v}
    book["id"] = bId
    book["has_cover_image"] = "cover_image_mimetype" in book

    if "volume_number" in book:
        book["volume_number"]["bits"] = [convert_bit(bit) for bit in book["volume_number"]["bits"]]
    if "fascicle_number" in book:
        book["fascicle_number"]["bits"] = [convert_bit(bit) for bit in book["fascicle_number"]["bits"]]

    book["sort_key"] = __sort_key_for_book(book)

    return book


def __sort_key_for_book(book):
    return (
        book["bucket"],
        book["title"].lower(),
        book.get("volume_number", {}).get("bits", []),
        book.get("fascicle_number", {}).get("bits", []),
        book.get("subtitle", "").lower(),
        book.get("volume_title", "").lower(),
    )


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
