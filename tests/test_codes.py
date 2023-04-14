import bookdb

from hypothesis import assume, given
import hypothesis.strategies as st


def a_digit():
    return st.integers(min_value=0, max_value=9)


def a_list_of_digits_of_length(length):
    return st.lists(a_digit(), min_size=length, max_size=length)


def to_code(prefix, digits, check):
    return f"{prefix}-{''.join(map(str, digits))}{check}"


def isbn13_check_digit(digits):
    wsum = 3 * (digits[1] + digits[3] + digits[5] + digits[7] + digits[9] + digits[11]) + digits[0] + digits[2] + digits[4] + digits[6] + digits[8] + digits[10]
    return (10 - (wsum % 10)) % 10


def isbn10_check_digit(digits):
    wsum = 10 * digits[0] + 9 * digits[1] + 8 * digits[2] + 7 * digits[3] + 6 * digits[4] + 5 * digits[5] + 4 * digits[6] + 3 * digits[7] + 2 * digits[8]
    mod = (11 - (wsum % 11)) % 11
    return "X" if mod == 10 else mod


def ean13_check_digit(digits):
    # same validation
    return isbn13_check_digit(digits)


def ean8_check_digit(digits):
    wsum = 3 * (digits[0] + digits[2] + digits[4] + digits[6]) + digits[1] + digits[3] + digits[5]
    return (10 - (wsum % 10)) % 10


def test_isbn13_check_digit():
    assert isbn13_check_digit([9, 7, 8, 1, 8, 5, 7, 2, 3, 1, 3, 8]) == 0
    assert isbn13_check_digit([9, 7, 8, 1, 8, 4, 0, 2, 2, 6, 4, 4]) == 7


def test_isbn10_check_digit():
    assert isbn10_check_digit([0, 0, 0, 6, 7, 5, 5, 1, 8]) == 6
    assert isbn10_check_digit([0, 0, 0, 6, 7, 5, 5, 1, 6]) == "X"


def test_ean13_check_digit():
    assert ean13_check_digit([2, 3, 7, 0, 0, 0, 9, 1, 1, 3, 1, 9]) == 2
    assert ean13_check_digit([2, 3, 7, 0, 0, 1, 1, 4, 9, 0, 5, 9]) == 5


def test_ean8_check_digit():
    assert ean8_check_digit([6, 2, 9, 2, 5, 2, 4]) == 2
    assert ean8_check_digit([9, 7, 4, 4, 2, 9, 2]) == 9


@given(a_list_of_digits_of_length(12))
def test_valid_isbn13(digits):
    check = isbn13_check_digit(digits)
    assert bookdb.validate_code(to_code("isbn", digits, check))


@given(a_list_of_digits_of_length(12), a_digit())
def test_invalid_isbn13(digits, check):
    assume(check != isbn13_check_digit(digits))
    assert not bookdb.validate_code(to_code("isbn", digits, check))


@given(a_list_of_digits_of_length(9))
def test_valid_isbn10(digits):
    check = isbn10_check_digit(digits)
    assert bookdb.validate_code(to_code("isbn", digits, check))


@given(a_list_of_digits_of_length(9), a_digit())
def test_invalid_isbn10(digits, check):
    assume(check != isbn10_check_digit(digits))
    assert not bookdb.validate_code(to_code("isbn", digits, check))


@given(a_list_of_digits_of_length(12))
def test_valid_ean13(digits):
    check = ean13_check_digit(digits)
    assert bookdb.validate_code(to_code("ean", digits, check))


@given(a_list_of_digits_of_length(12), a_digit())
def test_invalid_ean13(digits, check):
    assume(check != ean13_check_digit(digits))
    assert not bookdb.validate_code(to_code("ean", digits, check))


@given(a_list_of_digits_of_length(7))
def test_valid_ean8(digits):
    check = ean8_check_digit(digits)
    assert bookdb.validate_code(to_code("ean", digits, check))


@given(a_list_of_digits_of_length(7), a_digit())
def test_invalid_ean8(digits, check):
    assume(check != ean8_check_digit(digits))
    assert not bookdb.validate_code(to_code("ean", digits, check))
