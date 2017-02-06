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

-- import Database.PostgreSQL.Session
import Database.PostgreSQL.Other

---------------
-- Result Parser
---------------

data ResultParser
    = OneResurtParser
    | MaybeResultParser
    | ManyParser

data ResultParserError
    = ResultNoRows
    | ResultToManyRows

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

-- Params

class IsParams a where
    params :: a -> [QueryM EncodeResult]

instance (ToPostgres a, ToPostgres b) => IsParams (a, b) where
    params (a, b) = [toPostgres a, toPostgres b]

type Context = [QueryM EncodeResult]

class ToParams a where
    type ParamType a :: *

    derive :: Proxy a -> Context -> ParamType a

instance IsParams a => ToParams a where
    type ParamType a = a -> [QueryM EncodeResult]

    derive p ctx = ctx <> params

instance (ToPostgres x, ToParams xs) => ToParams (x ': xs) where
    type ParamType (x ': xs) = x -> ParamType xs

    derive p ctx v = derive (Proxy :: Proxy xs) (toPostgres v : ctx)

instance ToParams '[] where
    type ParamType a = [QueryM EncodeResult]

    derive p ctx = ctx

getParams :: ToParams a => SessionQuery a b -> ParamType a
getParams _ = derive (Proxy :: Proxy a) []


buildSession :: SessionQuery a b -> [QueryM EncoderResult] -> Session b
buildSession = undefined
 -- makeQuery . runEncodeResult <$> sequence params
 --   where
 --     makeQuery values = Query

----------------------
-- Results
-------------------
data ResultType a
    = SingleRow a
    | MaybeRow a
    | ManyRows a

data SessionQuery a (b :: ResultType *) = SessionQuery { sqStatement :: B.ByteString }
    deriving (Show)

type family Result a where
    Result (SingleRow a) = a
    Result (MaybeRow a) = Maybe a
    Result (ManyRows a) = V.Vector a

query :: (ToParams a, FromRows b) => SessionQuery a b -> a -> Session (Result b)
query = undefined


tq :: SessionQuery '[Int, Char, Word] b
tq = undefined

