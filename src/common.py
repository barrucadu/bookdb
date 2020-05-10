def display_title(book):
    """Computer the display title of a book.
    """

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
    """Split a string into a list of numeric and non-numeric substrings.
    """

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
    """Get a book ready to be indexed.
    """

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
