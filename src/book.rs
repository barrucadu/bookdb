use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::fmt;
use std::str;
use time::Date;

use crate::config::Slug;

static ISBN10_WEIGHTS: [u32; 10] = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1];
static ISBN13_WEIGHTS: [u32; 13] = [1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1];
static EAN8_WEIGHTS: [u32; 8] = [3, 1, 3, 1, 3, 1, 3, 1];
static EAN13_WEIGHTS: [u32; 13] = [1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1, 3, 1];

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Book {
    pub inner: BookV1,
}

impl<'de> Deserialize<'de> for Book {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        match BookV1::deserialize(deserializer) {
            Ok(inner) => Ok(Book { inner }),
            Err(err) => Err(err),
        }
    }
}

impl Serialize for Book {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        BookV1::serialize(&self.inner, serializer)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Deserialize, Serialize)]
pub struct BookV1 {
    pub code: Code,
    pub title: String,
    pub subtitle: Option<String>,
    pub volume_title: Option<String>,
    pub volume_number: Option<String>,
    pub fascicle_number: Option<String>,
    pub authors: Vec<String>,
    pub translators: Option<Vec<String>>,
    pub editors: Option<Vec<String>>,
    pub has_been_read: bool,
    pub last_read_date: Option<Date>,
    pub cover_image_mimetype: Option<String>,
    pub holdings: Vec<Holding>,
    pub bucket: String,
    pub category: Slug,
}

impl Book {
    pub fn display_title(&self) -> String {
        BookDisplayTitle::from(self).to_string()
    }
}

impl PartialOrd for Book {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Book {
    fn cmp(&self, other: &Self) -> Ordering {
        BookSortKey::from(self).cmp(&BookSortKey::from(other))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Code {
    ISBN10(String),
    ISBN13(String),
    EAN8(String),
    EAN13(String),
    Nonstandard(String),
}

impl Code {
    pub fn is_valid(&self) -> bool {
        match self {
            Code::ISBN10(code) => validate_weight_sum(code, &ISBN10_WEIGHTS, true, 11),
            Code::ISBN13(code) => validate_weight_sum(code, &ISBN13_WEIGHTS, false, 10),
            Code::EAN8(code) => validate_weight_sum(code, &EAN8_WEIGHTS, false, 10),
            Code::EAN13(code) => validate_weight_sum(code, &EAN13_WEIGHTS, false, 10),
            Code::Nonstandard(code) => code.chars().all(|c| c.is_alphanumeric() || c == '-'),
        }
    }
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Code::ISBN10(code) | Code::ISBN13(code) => write!(f, "isbn-{code}"),
            Code::EAN8(code) | Code::EAN13(code) => write!(f, "ean-{code}"),
            Code::Nonstandard(code) => write!(f, "x-{code}"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ParseCodeError {
    Invalid,
    IncorrectLength,
    UnknownPrefix,
}

impl str::FromStr for Code {
    type Err = ParseCodeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parsed = if let Some(code) = s.strip_prefix("isbn-") {
            if code.len() == 10 {
                Ok(Code::ISBN10(code.to_string()))
            } else if code.len() == 13 {
                Ok(Code::ISBN13(code.to_string()))
            } else {
                Err(ParseCodeError::IncorrectLength)
            }
        } else if let Some(code) = s.strip_prefix("ean-") {
            if code.len() == 8 {
                Ok(Code::EAN8(code.to_string()))
            } else if code.len() == 13 {
                Ok(Code::EAN13(code.to_string()))
            } else {
                Err(ParseCodeError::IncorrectLength)
            }
        } else if let Some(code) = s.strip_prefix("x-") {
            Ok(Code::Nonstandard(code.to_string()))
        } else {
            Err(ParseCodeError::UnknownPrefix)
        };

        match parsed {
            Ok(candidate) if !candidate.is_valid() => Err(ParseCodeError::Invalid),
            _ => parsed,
        }
    }
}

impl fmt::Display for ParseCodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseCodeError::Invalid => write!(f, "code does not pass validation"),
            ParseCodeError::IncorrectLength => write!(f, "code is not the expected length"),
            ParseCodeError::UnknownPrefix => write!(f, "code prefix is unknown"),
        }
    }
}

impl Serialize for Code {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'a> Deserialize<'a> for Code {
    fn deserialize<D: serde::Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        let s = <&str>::deserialize(deserializer)?;
        s.parse::<Code>().map_err(serde::de::Error::custom)
    }
}

fn validate_weight_sum(code: &str, weights: &[u32], x_is_ten: bool, modulus: u32) -> bool {
    if code.len() != weights.len() {
        return false;
    }

    let mut weight_sum = 0;
    for (c, w) in std::iter::zip(code.chars(), weights) {
        if c == 'X' && x_is_ten {
            weight_sum += 10 * w;
        } else if let Some(d) = c.to_digit(10) {
            weight_sum += d * w;
        } else {
            return false;
        }
    }

    weight_sum % modulus == 0
}

#[derive(PartialEq, Eq, Clone, Debug, Deserialize, Serialize)]
pub struct Holding {
    pub location: Slug,
    pub note: Option<String>,
}

pub struct BookDisplayTitle {
    pub title: String,
    pub subtitle: Option<String>,
    pub volume_title: Option<String>,
    pub volume_number: Option<String>,
    pub fascicle_number: Option<String>,
}

impl From<&Book> for BookDisplayTitle {
    fn from(book: &Book) -> Self {
        Self {
            title: book.inner.title.clone(),
            subtitle: book.inner.subtitle.clone(),
            volume_title: book.inner.volume_title.clone(),
            volume_number: book.inner.volume_number.clone(),
            fascicle_number: book.inner.fascicle_number.clone(),
        }
    }
}

impl std::fmt::Display for BookDisplayTitle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut display_title = self.title.clone();

        if let Some(subtitle) = &self.subtitle {
            display_title = format!("{display_title}: {subtitle}");
        }
        match (&self.volume_number, &self.fascicle_number) {
            (Some(volume_number), Some(fascicle_number)) => {
                display_title =
                    format!("{display_title} (vol. {volume_number}; fas. {fascicle_number})");
            }
            (Some(volume_number), None) => {
                display_title = format!("{display_title} (vol. {volume_number})");
            }
            (None, Some(fascicle_number)) => {
                display_title = format!("{display_title} (fas. {fascicle_number})");
            }
            (None, None) => {}
        }
        if let Some(volume_title) = &self.volume_title {
            display_title = format!("{display_title} / {volume_title}");
        }

        write!(f, "{display_title}")
    }
}

/// Sorting books is a complex affair, especially when volume and fascicle
/// numbers get involved, it's not just lexicographic order.
///
/// For example, if we have two books by the same author with the same title,
/// and one is volume number "1A" and the other is volume number "11", then
/// lexicographic ordering would put volume number "11" first.  But those volume
/// numbers are read as "one A" and "eleven", so "1A" should come first.
///
/// Make this work by splitting up volume and fascicle numbers into subsequences
/// of just letters and just numbers, and compare them that way.
#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug)]
struct BookSortKey {
    bucket: String,
    title: String,
    volume_number_bits: Option<Vec<NumberPart>>,
    fascicle_number_bits: Option<Vec<NumberPart>>,
    subtitle: Option<String>,
    volume_title: Option<String>,
}

impl From<&Book> for BookSortKey {
    fn from(book: &Book) -> Self {
        Self {
            bucket: book.inner.bucket.to_lowercase(),
            title: book.inner.title.to_lowercase(),
            volume_number_bits: book.inner.volume_number.as_deref().map(alphanum_to_bits),
            fascicle_number_bits: book.inner.fascicle_number.as_deref().map(alphanum_to_bits),
            subtitle: book.inner.subtitle.clone(),
            volume_title: book.inner.volume_title.clone(),
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Clone, Debug)]
enum NumberPart {
    Str(String),
    Num(i32),
}

#[allow(clippy::cast_possible_wrap)]
fn alphanum_to_bits(alphanum: &str) -> Vec<NumberPart> {
    let mut bits = Vec::new();

    let mut subint = -1;
    let mut substr = String::new();
    let mut mode_digit = true;
    for c in alphanum.chars() {
        match (mode_digit, c.to_digit(10)) {
            (true, Some(d)) => {
                if subint == -1 {
                    subint = 0;
                }
                subint = subint * 10 + (d as i32);
            }
            (true, None) => {
                if subint != -1 {
                    bits.push(NumberPart::Num(subint));
                }
                substr.push(c);
                subint = -1;
                mode_digit = false;
            }
            (false, Some(d)) => {
                if !substr.is_empty() {
                    bits.push(NumberPart::Str(substr));
                }
                substr = String::new();
                subint = d as i32;
                mode_digit = true;
            }
            (false, None) => {
                substr.push(c);
            }
        }
    }
    if subint != -1 {
        bits.push(NumberPart::Num(subint));
    }
    if !substr.is_empty() {
        bits.push(NumberPart::Str(substr));
    }

    bits
}

#[cfg(test)]
mod tests {
    use super::test_helpers::*;
    use super::*;

    use proptest::prelude::*;

    #[test]
    fn sorts_title() {
        let book1 = fixture_taocp_1();

        let mut book2 = fixture_taocp_1();
        book2.inner.title = "The AAAArt of Computer Programming".to_string();

        assert!(book2 < book1);
    }

    #[test]
    fn sorts_volumes_fascicles() {
        assert!(fixture_taocp_1() < fixture_taocp_1_1());
        assert!(fixture_taocp_1_1() < fixture_taocp_2());
        assert!(fixture_taocp_2() < fixture_taocp_3());
        assert!(fixture_taocp_3() < fixture_taocp_4a());
    }

    proptest! {
        #[test]
        fn valid_isbn10(s in "[0-9X]{9}") {
            let check = isbn10_check_char(&s);
            let code = Code::ISBN10(format!("{s}{check}"));
            assert!(code.is_valid());

            let serialised = serde_json::to_string(&code).unwrap();
            assert_eq!(code, serde_json::from_str(&serialised).unwrap());

            let display = format!("isbn-{s}{check}");
            assert_eq!(Ok(code.clone()), display.parse());
            assert_eq!(code.to_string(), display);
        }

        #[test]
        fn invalid_isbn10(s in "[0-9X]{9}", c in "[0-9X]") {
            let check = c.chars().next().unwrap();
            prop_assume!(check != isbn10_check_char(&s));

            let code = Code::ISBN10(format!("{s}{check}"));
            assert!(!code.is_valid());

            let display = format!("isbn-{s}{check}");
            assert_eq!(Err(ParseCodeError::Invalid), display.parse::<Code>());
        }

        #[test]
        fn valid_isbn13(s in "[0-9]{12}") {
            let check = isbn13_check_char(&s);
            let code = Code::ISBN13(format!("{s}{check}"));
            assert!(code.is_valid());

            let serialised = serde_json::to_string(&code).unwrap();
            assert_eq!(code, serde_json::from_str(&serialised).unwrap());

            let display = format!("isbn-{s}{check}");
            assert_eq!(Ok(code.clone()), display.parse());
            assert_eq!(code.to_string(), display);
        }

        #[test]
        fn invalid_isbn13(s in "[0-9]{12}", c in "[0-9]") {
            let check = c.chars().next().unwrap();
            prop_assume!(check != isbn13_check_char(&s));

            let code = Code::ISBN13(format!("{s}{check}"));
            assert!(!code.is_valid());

            let display = format!("isbn-{s}{check}");
            assert_eq!(Err(ParseCodeError::Invalid), display.parse::<Code>());
        }

        #[test]
        fn malformed_isbn(s in ".+") {
            prop_assume!(s.len() != 10);
            prop_assume!(s.len() != 13);

            assert!(!Code::ISBN10(s.clone()).is_valid());
            assert!(!Code::ISBN13(s.clone()).is_valid());

            let display = format!("isbn-{s}");
            assert_eq!(Err(ParseCodeError::IncorrectLength), display.parse::<Code>());
        }

        #[test]
        fn valid_ean8(s in "[0-9]{7}") {
            let check = ean8_check_char(&s);
            let code = Code::EAN8(format!("{s}{check}"));
            assert!(code.is_valid());

            let serialised = serde_json::to_string(&code).unwrap();
            assert_eq!(code, serde_json::from_str(&serialised).unwrap());

            let display = format!("ean-{s}{check}");
            assert_eq!(Ok(code.clone()), display.parse());
            assert_eq!(code.to_string(), display);
        }

        #[test]
        fn invalid_ean8(s in "[0-9]{7}", c in "[0-9]") {
            let check = c.chars().next().unwrap();
            prop_assume!(check != ean8_check_char(&s));

            let code = Code::EAN8(format!("{s}{check}"));
            assert!(!code.is_valid());

            let display = format!("ean-{s}{check}");
            assert_eq!(Err(ParseCodeError::Invalid), display.parse::<Code>());
        }

        #[test]
        fn valid_ean13(s in "[0-9]{12}") {
            let check = ean13_check_char(&s);
            let code = Code::EAN13(format!("{s}{check}"));
            assert!(code.is_valid());

            let serialised = serde_json::to_string(&code).unwrap();
            assert_eq!(code, serde_json::from_str(&serialised).unwrap());

            let display = format!("ean-{s}{check}");
            assert_eq!(Ok(code.clone()), display.parse());
            assert_eq!(code.to_string(), display);
        }

        #[test]
        fn invalid_ean13(s in "[0-9]{12}", c in "[0-9]") {
            let check = c.chars().next().unwrap();
            prop_assume!(check != ean13_check_char(&s));

            let code = Code::EAN13(format!("{s}{check}"));
            assert!(!code.is_valid());

            let display = format!("ean-{s}{check}");
            assert_eq!(Err(ParseCodeError::Invalid), display.parse::<Code>());
        }

        #[test]
        fn malformed_ean(s in ".+") {
            prop_assume!(s.len() != 8);
            prop_assume!(s.len() != 13);

            assert!(!Code::EAN8(s.clone()).is_valid());
            assert!(!Code::EAN13(s.clone()).is_valid());

            let display = format!("ean-{s}");
            assert_eq!(Err(ParseCodeError::IncorrectLength), display.parse::<Code>());
        }

        #[test]
        fn valid_nonstandard(s in "[a-zA-Z0-9-]+") {
            let code = Code::Nonstandard(s.to_string());
            assert!(code.is_valid());

            let serialised = serde_json::to_string(&code).unwrap();
            assert_eq!(code, serde_json::from_str(&serialised).unwrap());

            let display = format!("x-{s}");
            assert_eq!(Ok(code.clone()), display.parse());
            assert_eq!(code.to_string(), display);
        }

        #[test]
        fn invalid_nonstandard(s in ".*[^a-zA-Z0-9-].+") {
            let code = Code::Nonstandard(s.to_string());
            assert!(!code.is_valid());

            let display = format!("x-{s}");
            assert_eq!(Err(ParseCodeError::Invalid), display.parse::<Code>());
        }

        #[test]
        fn num_to_bits(s in "[0-9]{1,3}") {
            let as_num = s.parse().unwrap();
            assert_eq!(
                alphanum_to_bits(&s),
                vec![NumberPart::Num(as_num)]
            );
        }

        #[test]
        fn alpha_to_bits(s in "[^0-9]+") {
            assert_eq!(
                alphanum_to_bits(&s),
                vec![NumberPart::Str(s)]
            );
        }

        #[test]
        fn mixed_to_bits(
            num_parts in prop::collection::vec("[0-9]{1,3}", 1..10),
            str_parts in prop::collection::vec("[^0-9]+", 1..10),
            num_first: bool,
        ) {
            let mut s = String::new();
            let mut expected = Vec::new();

            for (num_part, str_part) in std::iter::zip(&num_parts, &str_parts) {
                if num_first {
                    s = format!("{s}{num_part}{str_part}");
                    expected.push(NumberPart::Num(num_part.parse().unwrap()));
                    expected.push(NumberPart::Str(str_part.clone()));
                } else {
                    s = format!("{s}{str_part}{num_part}");
                    expected.push(NumberPart::Str(str_part.clone()));
                    expected.push(NumberPart::Num(num_part.parse().unwrap()));
                }
            }

            assert_eq!(
                alphanum_to_bits(&s),
                expected
            );
        }
    }

    // test helpers

    #[test]
    fn test_isbn10_check_char() {
        assert_eq!('6', isbn10_check_char("000675518"));
        assert_eq!('X', isbn10_check_char("000675516"));
    }

    #[test]
    fn test_isbn13_check_char() {
        assert_eq!('0', isbn13_check_char("978185723138"));
        assert_eq!('7', isbn13_check_char("978184022644"));
    }

    #[test]
    fn test_ean8_check_char() {
        assert_eq!('2', ean8_check_char("6292524"));
        assert_eq!('9', ean8_check_char("9744292"));
    }

    #[test]
    fn test_ean13_check_char() {
        assert_eq!('2', ean13_check_char("237000911319"));
        assert_eq!('5', ean13_check_char("237001149059"));
    }

    // fixtures

    fn fixture_taocp_1() -> Book {
        fixture_taocp("Fundamental Algorithms", "1")
    }

    fn fixture_taocp_1_1() -> Book {
        let mut book = fixture_taocp("MMIX A RISC Computer for the New Millenium", "1");
        book.inner.fascicle_number = Some("1".to_string());
        book
    }

    fn fixture_taocp_2() -> Book {
        fixture_taocp("Seminumerical Algorithms", "2")
    }

    fn fixture_taocp_3() -> Book {
        fixture_taocp("Sorting and Searching", "3")
    }

    fn fixture_taocp_4a() -> Book {
        fixture_taocp("Combinatorial Algorithms, Part 1", "4A")
    }

    fn fixture_taocp(volume_title: &str, volume_number: &str) -> Book {
        Book {
            inner: BookV1 {
                code: Code::Nonstandard("fixture".to_string()),
                title: "The Art of Computer Programming".to_string(),
                subtitle: None,
                volume_title: Some(volume_title.to_string()),
                volume_number: Some(volume_number.to_string()),
                fascicle_number: None,
                authors: vec!["Donald E. Knuth".to_string()],
                translators: None,
                editors: None,
                has_been_read: false,
                last_read_date: None,
                cover_image_mimetype: None,
                holdings: Vec::new(),
                bucket: "Knuth".to_string(),
                category: Slug("computer-science".to_string()),
            },
        }
    }
}

#[cfg(test)]
pub mod test_helpers {
    use super::*;

    use proptest::prelude::*;

    prop_compose! {
        pub fn arbitrary_book() (
            code in arbitrary_code(),
            title in "[\\d\\s\\w]+",
            subtitle in proptest::option::of("[\\d\\s\\w]+"),
            volume_title in proptest::option::of("[\\d\\s\\w]+"),
            volume_number in proptest::option::of("[\\d\\w]+"),
            fascicle_number in proptest::option::of("[\\d\\w]+"),
            authors in proptest::collection::vec("[\\d\\s\\w]+", 1..3),
            translators in proptest::option::of(proptest::collection::vec("[\\d\\s\\w]+", 1..3)),
            editors in proptest::option::of(proptest::collection::vec("[\\d\\s\\w]+", 1..3)),
            has_been_read in proptest::bool::ANY,
            last_read_date in proptest::option::of(arbitrary_date()),
            cover_image_mimetype in proptest::option::of("[a-z]+/[a-z]+"),
            holdings in proptest::collection::vec(arbitrary_holding(), 1..3),
            bucket in "[a-z]+",
            category in arbitrary_slug()
        ) -> Book {
            Book {
                inner: BookV1 {
                    code,
                    title,
                    subtitle,
                    volume_title,
                    volume_number,
                    fascicle_number,
                    authors,
                    translators,
                    editors,
                    has_been_read,
                    last_read_date,
                    cover_image_mimetype,
                    holdings,
                    bucket,
                    category
                }
            }
        }
    }

    fn arbitrary_code() -> BoxedStrategy<Code> {
        prop_oneof![
            arbitrary_isbn10_code(),
            arbitrary_isbn13_code(),
            arbitrary_ean8_code(),
            arbitrary_ean13_code(),
            arbitrary_nonstandard_code(),
        ]
        .boxed()
    }

    prop_compose! {
        fn arbitrary_isbn10_code()(s in "[0-9X]{9}") -> Code {
            let check = isbn10_check_char(&s);
            Code::ISBN10(format!("{s}{check}"))
        }
    }

    prop_compose! {
        fn arbitrary_isbn13_code()(s in "[0-9]{12}") -> Code {
            let check = isbn13_check_char(&s);
            Code::ISBN13(format!("{s}{check}"))
        }
    }

    prop_compose! {
        fn arbitrary_ean8_code()(s in "[0-9]{7}") -> Code {
            let check = ean8_check_char(&s);
            Code::EAN8(format!("{s}{check}"))
        }
    }

    prop_compose! {
        fn arbitrary_ean13_code()(s in "[0-9]{12}") -> Code {
            let check = ean13_check_char(&s);
            Code::EAN13(format!("{s}{check}"))
        }
    }

    prop_compose! {
        fn arbitrary_nonstandard_code()(s in "[a-zA-Z0-9-]+") -> Code {
            Code::Nonstandard(s)
        }
    }

    prop_compose! {
        fn arbitrary_slug()(s in "[a-z0-9-]+") -> Slug {
            Slug(s)
        }
    }

    prop_compose! {
        fn arbitrary_date()(y in 2000i32..2100, m in 1u8..12, d in 1u8..28) -> Date {
            Date::from_calendar_date(y, time::Month::January.nth_next(m), d).unwrap()
        }
    }

    prop_compose! {
        fn arbitrary_holding()(
            location in arbitrary_slug(),
            note in proptest::option::of("[\\d\\s\\w]+")
        ) -> Holding {
            Holding { location, note }
        }
    }

    pub fn isbn10_check_char(digits: &str) -> char {
        let digits = digits.chars().collect::<Vec<char>>();
        let wsum = unwrap_digit(digits[0], true) * 10
            + unwrap_digit(digits[1], true) * 9
            + unwrap_digit(digits[2], true) * 8
            + unwrap_digit(digits[3], true) * 7
            + unwrap_digit(digits[4], true) * 6
            + unwrap_digit(digits[5], true) * 5
            + unwrap_digit(digits[6], true) * 4
            + unwrap_digit(digits[7], true) * 3
            + unwrap_digit(digits[8], true) * 2;
        let modulus = (11 - (wsum % 11)) % 11;
        if modulus == 10 {
            'X'
        } else {
            char::from_digit(modulus, 10).unwrap()
        }
    }

    pub fn isbn13_check_char(digits: &str) -> char {
        let digits = digits.chars().collect::<Vec<char>>();
        let wsum = unwrap_digit(digits[0], false)
            + unwrap_digit(digits[1], false) * 3
            + unwrap_digit(digits[2], false)
            + unwrap_digit(digits[3], false) * 3
            + unwrap_digit(digits[4], false)
            + unwrap_digit(digits[5], false) * 3
            + unwrap_digit(digits[6], false)
            + unwrap_digit(digits[7], false) * 3
            + unwrap_digit(digits[8], false)
            + unwrap_digit(digits[9], false) * 3
            + unwrap_digit(digits[10], false)
            + unwrap_digit(digits[11], false) * 3;
        char::from_digit((10 - (wsum % 10)) % 10, 10).unwrap()
    }

    pub fn ean8_check_char(digits: &str) -> char {
        let digits = digits.chars().collect::<Vec<char>>();
        let wsum = unwrap_digit(digits[0], false) * 3
            + unwrap_digit(digits[1], false)
            + unwrap_digit(digits[2], false) * 3
            + unwrap_digit(digits[3], false)
            + unwrap_digit(digits[4], false) * 3
            + unwrap_digit(digits[5], false)
            + unwrap_digit(digits[6], false) * 3;
        char::from_digit((10 - (wsum % 10)) % 10, 10).unwrap()
    }

    pub fn ean13_check_char(digits: &str) -> char {
        // it's the same checksum algorithm
        isbn13_check_char(digits)
    }

    fn unwrap_digit(c: char, x_is_ten: bool) -> u32 {
        if c == 'X' && x_is_ten {
            10
        } else if let Some(d) = c.to_digit(10) {
            d
        } else {
            panic!("could not unwrap {c}");
        }
    }
}
