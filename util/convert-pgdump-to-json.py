from datetime import datetime

import csv
import json

LOCATIONS = {
    "hull": "f256ed66-4c09-4207-86de-adc8e9fb86ec",
    "london": "be60be7b-a10f-42e1-8769-d43f12cad02d",
    "missing": "6a233e5e-3b64-4169-ac67-cf46113afd97",
}

CATEGORIES = {
    "-": None,
    "cs": "afc7135b-bf67-4284-bcc1-2bbd3386aea3",
    "f": "ea4e9b04-3ebb-4ff3-b387-d8bfc603f58a",
    "hi": "ac4706f3-54c3-4e6d-a72d-85321d9dcd72",
    "kf": "b5451ba6-a3ef-4c27-99fc-a07d8cd54161",
    "m": "a3bbb1c6-5ff8-4ddf-81f4-820593a2a5ff",
    "nf": "fb2dd601-883a-4254-a294-fcf0a0f98d2f",
    "p": "217f8eaa-b54a-466c-83f8-3f569f46732e",
    "ph": "ac4706f3-54c3-4e6d-a72d-85321d9dcd72",
    "po": "ac4706f3-54c3-4e6d-a72d-85321d9dcd72",
    "pr": "afc7135b-bf67-4284-bcc1-2bbd3386aea3",
    "rm": "a7d83bc9-0352-4fb5-a9dc-68428562a17f",
    "rpg": "219ae9f4-84f1-4ea2-a9cd-fbc16698a669",
}

MIMETYPES = {
    "gif": "image/gif",
    "jpeg": "image/jpeg",
    "jpg": "image/jpeg",
    "png": "image/png",
}

PERSON_FIXUPS = {
    "Moore, Alan and Lloyd, David": ["Lloyd, David", "Moore, Alan"],
    "Rensin, David K. * Kawahara, Kent": ["Kawahara, Kent", "Rensin, David K."],
}

BUCKET_FIXUPS = {
    "Lackey, Chris": "Lackey",
    "Lovecraft, H. P.": "Lovecraft",
    "Pratchett, Terry": "Pratchett",
    "Wilde, Oscar": "Wilde",
}

ISBN_FIXUPS = {
    "0552134627:0": "0552134627",
}

BOOKS = {}

# COPY public.books ("bookIsbn", "bookTitle", "bookSubtitle", "bookCover", "bookVolume", "bookFascicle", "bookVoltitle", "bookAuthor", "bookTranslator", "bookEditor", "bookSorting", "bookRead", "bookLastRead", "bookLocation", "bookBorrower", "bookCategoryCode") FROM stdin;


def to_people(people):
    if not people:
        return []
    out = []
    for p in [p.strip() for p in people.split("&")]:
        for person in PERSON_FIXUPS.get(p, [p]):
            lst = person
            if "," in person:
                lst, fst = person.split(",")
                person = f"{fst.strip()} {lst.strip()}"
            out.append(person)
    return out


now = datetime.now().strftime("%Y-%m-%dT%H:%M:%S")
with open("/home/barrucadu/bookdb-data/restore.sql") as f:
    rows = csv.reader(f, delimiter="\t")
    for row in rows:
        rs = [None if r == "NULL" else r for r in row]

        book = {
            "title": rs[1],
            "subtitle": rs[2],
            "volume_title": rs[6],
            "people": {"authors": to_people(rs[7]), "translators": to_people(rs[8]), "editors": to_people(rs[9])},
            "has_been_read": rs[11] == "t",
            "last_read_date": rs[12].split()[0] if rs[12] else None,
            "cover_image_mimetype": MIMETYPES[rs[3].split(".")[1].lower()] if rs[3] else None,
            "holdings": [{"location_uuid": LOCATIONS[rs[13].lower()]}],
            "bucket": (BUCKET_FIXUPS.get(rs[10], rs[10]) or "").lower() or None,
            "category_uuid": CATEGORIES[rs[15].lower()],
            "created_at": now,
            "updated_at": now,
        }
        book = {k: v for k, v in book.items() if v}
        book["people"] = {k: v for k, v in book["people"].items() if v}
        if rs[4]:
            book["volume_number"] = {"raw": rs[4]}
        if rs[5]:
            book["fascicle_number"] = {"raw": rs[5]}
        BOOKS[ISBN_FIXUPS.get(rs[0], rs[0])] = book
print(json.dumps(BOOKS))
