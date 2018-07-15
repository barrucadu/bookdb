BEGIN TRANSACTION;

ALTER TABLE `book` RENAME TO `_book_old`;

CREATE TABLE `book` (
       `bookIsbn` VARCHAR NOT NULL PRIMARY KEY,
       `bookTitle` VARCHAR NOT NULL,
       `bookSubtitle` VARCHAR NOT NULL,
       `bookCover` VARCHAR NULL,
       `bookVolume` VARCHAR NOT NULL,
       `bookFascicle` VARCHAR NOT NULL,
       `bookVoltitle` VARCHAR NOT NULL,
       `bookAuthor` VARCHAR NOT NULL,
       `bookTranslator` VARCHAR NULL,
       `bookEditor` VARCHAR NULL,
       `bookSorting` VARCHAR NULL,
       `bookRead` BOOLEAN NOT NULL,
       `bookLastRead` TIMESTAMP NULL,
       `bookNowReading` BOOLEAN NOT NULL,
       `bookLocation` VARCHAR NOT NULL,
       `bookBorrower` VARCHAR NOT NULL,
       `bookCategory` VARCHAR NOT NULL
);

INSERT INTO `book` (`bookIsbn`, `bookTitle`, `bookSubtitle`, `bookCover`, `bookVolume`, `bookFascicle`, `bookVoltitle`, `bookAuthor`, `bookTranslator`, `bookEditor`, `bookSorting`, `bookRead`, `bookLastRead`, `bookNowReading`, `bookLocation`, `bookBorrower`, `bookCategory`)
  SELECT `isbn`, `title`, `subtitle`, `cover`, `volume`, `fascicle`, `voltitle`, `author`, `translator`, `editor`, `sorting`, `read`, REPLACE(REPLACE(`lastread`, " UTC", ""), "T", " "), `nowreading`, `location`, `borrower`, `category` FROM `_book_old`;

DROP TABLE `_book_old`;

COMMIT;
