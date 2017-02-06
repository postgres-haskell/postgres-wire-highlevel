{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language KindSignatures #-}
module Database.PostgreSQL.Query where

import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Word
import Data.Proxy
import Data.Monoid
import Data.Coerce

-- import Database.PostgreSQL.Session
import Database.PostgreSQL.Other


---------------------
-- Row parser
---------------------
data RowParserError = RowParserError
data RowParser s a = RowParser (s -> Either RowParserError (s, a))

--------------------
-- Encoders
--------------------
data EncodeResult
    = PrimEncode (Oid, Maybe B.ByteString)
    | ArrayEncode EncodeResult
    | CompositeEncode [EncodeResult]

runEncodeResult :: EncodeResult -> (Oid, Maybe B.ByteString)
runEncodeResult = undefined

type Name = B.ByteString
type NameSet = HS.HashSet Name
type NameMap = HM.HashMap Name Oid

data QueryM a = QueryM NameSet (NameMap -> a)

instance Functor QueryM where
    fmap f (QueryM s g) = QueryM s (g . f)

instance Applicative QueryM where
    pure  = QueryM HS.empty . const
    (QueryM s1 f) <*> (QueryM s2 x) = QueryM (s1 `HS.union` s2) (f <*> g)

type Encoder a = a -> QueryM EncodeResult

builtinEncoder :: (a -> (Oid, Maybe B.ByteString)) -> Encoder a
builtinEncoder = undefined

enumEncoder :: Name -> (a -> String) -> Encoder a
enumEncoder name f = undefined

arrayEncoder :: Encoder a -> Encoder [a]
arrayEncoder _ = undefined

composite :: Name -> (a -> [QueryM EncodeResult]) -> Encoder a
composite name xs = undefined

class ToPostgres a where
    toPostgres :: Encoder a

class IsParams a where
    params :: a -> [QueryM EncodeResult]

instance (ToPostgres a, ToPostgres b) => IsParams (a, b) where
    params (a, b) = [toPostgres a, toPostgres b]

----------------------
-- Results
-------------------

data ResultParser a
    = OneRowParser
    | MaybeRowParser
    | ManyRowsParser

data ResultParserError
    = ResultNoRows
    | ResultToManyRows

data SingleRow a
data MaybeRow a
data ManyRows a

class FromResult a where
    type Result a :: *

    resultParser :: ResultParser (Result a)

instance FromPostgres a => FromResult (SingleRow a) where
    type Result (SingleRow a) = a

    resultParser = OneRowParser

instance FromPostgres a => FromResult (MaybeRow a) where
    type Result (MaybeRow a) = Maybe a

    resultParser = MaybeRowParser

instance FromPostgres a => FromResult (ManyRows a) where
    type Result (ManyRows a) = V.Vector a

    resultParser = ManyRowsParser

------------------------
-- Session
-----------------------

data SessionQuery a b = SessionQuery { sqStatement :: B.ByteString }
    deriving (Show)

type Context = [QueryM EncodeResult]

class ToSession a where
    type SessionType a :: *

    derive :: a -> Context -> SessionType a

instance (IsParams a, FromResult b) => ToSession (SessionQuery a b) where
    type SessionType a = a -> Session (Result b)

    derive q ctx = buildSession s $ ctx <> params

instance (ToPostgres x, ToSession xs, FromResult b)
      => ToSession (SessionQuery (x ': xs) b) where

    type SessionType (x ': xs) = x -> SessionType xs

    derive q ctx v = derive (coerce q :: SessionQuery xs b) (toPostgres v : ctx)

instance FromResult b => ToSession (SessionQuery '[] b) where
    type SessionType a = Session (Result b)

    derive = buildSession

query :: ToSession q => q -> SessionType q
query q = derive q []

buildSession
    :: FromResult b
    => SessionQuery a b -> [QueryM EncoderResult] -> Session (Result b)
buildSession = undefined
 -- makeQuery . runEncodeResult <$> sequence params
 --   where
 --     makeQuery values = Query


tq :: SessionQuery '[Int, Char, Word] b
tq = undefined

