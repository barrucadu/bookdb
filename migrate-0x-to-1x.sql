BEGIN TRANSACTION;

CREATE TABLE `book_categories` (
       `categoryCode` VARCHAR NOT NULL PRIMARY KEY,
       `categoryName` VARCHAR NOT NULL
);

INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("CS", "Computer Science");
INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("PR", "Programming");
INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("PH", "Philosophy");
INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("PO", "Politics");
INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("HI", "History");
INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("RM", "Religion, Mythology, & Folklore");
INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("P",  "Poetry");
INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("M",  "Manga");
INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("F",  "Miscellaneous Fiction");
INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("NF", "Miscellaneous Nonfiction");
INSERT INTO `book_categories` (`categoryCode`, `categoryName`) VALUES ("-",  "Uncategorised");

ALTER TABLE `book` RENAME TO `_book_old`;

CREATE TABLE `books` (
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
       `bookCategoryCode` VARCHAR NOT NULL,
       FOREIGN KEY(`bookCategoryCode`) REFERENCES `book_categories`(`categoryCode`)
);

INSERT INTO `books` (`bookIsbn`, `bookTitle`, `bookSubtitle`, `bookCover`, `bookVolume`, `bookFascicle`, `bookVoltitle`, `bookAuthor`, `bookTranslator`, `bookEditor`, `bookSorting`, `bookRead`, `bookLastRead`, `bookNowReading`, `bookLocation`, `bookBorrower`, `bookCategoryCode`)
  SELECT `isbn`,
         `title`,
         `subtitle`,
         `cover`,
         `volume`,
         `fascicle`,
         `voltitle`,
         `author`,
         `translator`,
         `editor`,
         `sorting`,
         `read`,
         REPLACE(REPLACE(`lastread`, " UTC", ""), "T", " "),
         `nowreading`,
         `location`,
         `borrower`,
         REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(`category`, "ComputerScience", "CS"), "Programming", "PR"), "Philosophy", "PH"), "Politics", "PO"), "History", "HI"), "ReligionAndMythology", "RM"), "Poetry", "P"), "Manga", "M"), "MiscFiction", "F"), "MiscNonFiction", "NF"), "Uncategorised", "-")
  FROM `_book_old`;

DROP TABLE `_book_old`;

COMMIT;
