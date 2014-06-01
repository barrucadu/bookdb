{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Data.Text (Text)
import Web.Routes (PathInfo(..), patternParse)

-- |The possible routes in bookdb
data Sitemap = Booklist
             -- ^ The root book list, the site index
             | Search
             -- ^ The search form and results

             | Author Text
             -- ^ Filter by author
             | Translator Text
             -- ^ Filter by translator
             | Editor Text
             -- ^ Filter by editor
             | Read
             -- ^ Filter by read
             | Unread
             -- ^ Filter by unread
             | Location Text
             -- ^ Filter by location
             | Borrower Text
             -- ^ Filter by borrower

             | Image Text
             -- ^ A book cover image
             | Stylesheet
             -- ^ The stylesheet
             | Javascript
             -- ^ The javascript

             | Add
             -- ^ Add a new book
             | Edit Text
             -- ^ Edit a book by ISBN
             | Delete Text
             -- ^ Delete a book by ISBN

             | Error404
             -- ^ Catch-all route

               deriving (Read, Show)

instance PathInfo Sitemap where
    toPathSegments Booklist = []
    toPathSegments Search   = ["search"]

    toPathSegments (Author a)     = ["author", a]
    toPathSegments (Translator t) = ["translator", t]
    toPathSegments (Editor e)     = ["editor", e]
    toPathSegments Read           = ["read"]
    toPathSegments Unread         = ["unread"]
    toPathSegments (Location l)   = ["location", l]
    toPathSegments (Borrower b)   = ["borrower", b]

    toPathSegments (Image i)  = ["covers", i]
    toPathSegments Stylesheet = ["style.css"]
    toPathSegments Javascript = ["script.js"]

    toPathSegments Add        = ["add"]
    toPathSegments (Edit e)   = ["edit", e]
    toPathSegments (Delete d) = ["delete", d]

    fromPathSegments = patternParse parse
      where parse = Right . parse'

            parse' []         = Booklist
            parse' ["search"] = Search

            parse' ["author", a]     = Author a
            parse' ["translator", t] = Translator t
            parse' ["editor", e]     = Editor e
            parse' ["read"]          = Read
            parse' ["unread"]        = Unread
            parse' ["location", l]   = Location l
            parse' ["borrower", b]   = Borrower b

            parse' ["covers", i] = Image i
            parse' ["style.css"] = Stylesheet
            parse' ["script.js"] = Javascript

            parse' ["add"]       = Add
            parse' ["edit", e]   = Edit e
            parse' ["delete", d] = Delete d

            parse' _ = Error404
