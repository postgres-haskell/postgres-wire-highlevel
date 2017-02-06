module Database.PostgreSQL.Other where

data Encode = Encode String
    deriving (Show)

data Decode a = Decode
    deriving (Show)

data Oid = Oid Int
    deriving (Show)

data Query = Query [(Oid, Encode)]
    deriving (Show)

