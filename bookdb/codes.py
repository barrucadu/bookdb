from itertools import cycle

CODES = ["isbn", "ean", "x"]


def validate(code):
    bits = code.split("-")
    if len(bits) <= 1:
        return False

    try:
        if bits[0] == "isbn":
            return validate_isbn(bits[1]) and len(bits) == 2
        elif bits[0] == "ean":
            return validate_ean(bits[1]) and len(bits) == 2
        elif bits[0] == "x":
            # Fallback for books without standard codes
            return True
    except ValueError:
        return False


def validate_isbn(code):
    if len(code) == 10:
        return weight_sum(code, [10, 9, 8, 7, 6, 5, 4, 3, 2, 1], x_is_ten=True) % 11 == 0
    elif len(code) == 13:
        return weight_sum(code, [1, 3]) % 10 == 0
    else:
        return False


def validate_ean(code):
    if len(code) == 8:
        return weight_sum(code, [3, 1]) % 10 == 0
    elif len(code) == 13:
        return weight_sum(code, [1, 3]) % 10 == 0
    else:
        return False


def weight_sum(code, weights, x_is_ten=False):
    digits = [10 if x_is_ten and x == "X" else int(x) for x in code]

    return sum(d * w for d, w in list(zip(digits, cycle(weights))))
