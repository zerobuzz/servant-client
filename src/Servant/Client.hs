{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client
  ( client
  , HasClient(..)
  , module Servant.Common.BaseUrl
  ) where

import Control.Monad
import Control.Monad.Trans.Either
import Data.ByteString.Lazy (ByteString)
import Data.List
import Data.Proxy
import Data.String.Conversions
import Data.Text (unpack)
import GHC.TypeLits
import qualified Network.HTTP.Types as H
import Servant.API
import Servant.Common.BaseUrl
import Servant.Common.Req
import Servant.Common.Text

import Servant.Server.ContentTypes

class TypeListElem x (xs :: k)
instance TypeListElem x '[x]
instance TypeListElem x xs => TypeListElem x (y ': xs)


-- * Accessing APIs as a Client

-- | 'client' allows you to produce operations to query an API from a client.
--
-- > type MyApi = "books" :> Get [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: BaseUrl -> EitherT String IO [Book]
-- > postNewBook :: Book -> BaseUrl -> EitherT String IO Book
-- > (getAllBooks :<|> postNewBook) = client myApi
client :: forall ctyp layout . HasClient ctyp layout
      => Proxy (ctyp, layout) -> Client ctyp layout
client p = clientWithRoute p defReq

-- | This class lets us define how each API combinator
-- influences the creation of an HTTP request. It's mostly
-- an internal class, you can just use 'client'.
class HasClient ctyp layout where
  type Client ctyp layout :: *
  clientWithRoute :: Proxy (ctyp, layout) -> Req -> Client ctyp layout

-- | A client querying function for @a ':<|>' b@ will actually hand you
--   one function for querying @a@ and another one for querying @b@,
--   stitching them together with ':<|>', which really is just like a pair.
--
-- > type MyApi = "books" :> Get [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getAllBooks :: BaseUrl -> EitherT String IO [Book]
-- > postNewBook :: Book -> BaseUrl -> EitherT String IO Book
-- > (getAllBooks :<|> postNewBook) = client myApi
instance (HasClient ctyp a, HasClient ctyp b) => HasClient ctyp (a :<|> b) where
  type Client ctyp (a :<|> b) = Client ctyp a :<|> Client ctyp b
  clientWithRoute Proxy req =
    clientWithRoute (Proxy :: Proxy (ctyp, a)) req :<|>
    clientWithRoute (Proxy :: Proxy (ctyp, b)) req

-- | If you use a 'Capture' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'Capture'.
-- That function will take care of inserting a textual representation
-- of this value at the right place in the request path.
--
-- You can control how values for this type are turned into
-- text by specifying a 'ToText' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBook :: Text -> BaseUrl -> EitherT String IO Book
-- > getBook = client myApi
-- > -- then you can just use "getBook" to query that endpoint
instance (KnownSymbol capture, ToText a, HasClient ctyp sublayout)
      => HasClient ctyp (Capture capture a :> sublayout) where

  type Client ctyp (Capture capture a :> sublayout) =
    a -> Client ctyp sublayout

  clientWithRoute Proxy req val =
    clientWithRoute (Proxy :: Proxy (ctyp, sublayout)) $
      appendToPath p req

    where p = unpack (toText val)

-- | If you have a 'Delete' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance HasClient ctyp Delete where
  type Client ctyp Delete = BaseUrl -> EitherT String IO ()

  clientWithRoute Proxy req host =
    void $ performRequest H.methodDelete req (== 204) host

-- | If you have a 'Get' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance (MimeUnrender ctyp result, TypeListElem ctyp ctyps)
      => HasClient ctyp (Get ctyps result) where
  type Client ctyp (Get ctyps result) = BaseUrl -> EitherT String IO result
  clientWithRoute Proxy req host =
    performRequestCT (Proxy :: Proxy ctyp) H.methodGet req 200 host

-- | If you use a 'Header' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'Header',
-- wrapped in Maybe.
--
-- That function will take care of encoding this argument as Text
-- in the request headers.
--
-- All you need is for your type to have a 'ToText' instance.
--
-- Example:
--
-- > newtype Referer = Referer Text
-- >   deriving (Eq, Show, FromText, ToText)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get Referer
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > viewReferer :: Maybe Referer -> BaseUrl -> EitherT String IO Book
-- > viewReferer = client myApi
-- > -- then you can just use "viewRefer" to query that endpoint
-- > -- specifying Nothing or Just "http://haskell.org/" as arguments
instance (KnownSymbol sym, ToText a, HasClient ctyp sublayout)
      => HasClient ctyp (Header sym a :> sublayout) where

  type Client ctyp (Header sym a :> sublayout) =
    Maybe a -> Client ctyp sublayout

  clientWithRoute Proxy req mval =
    clientWithRoute (Proxy :: Proxy (ctyp, sublayout)) $
      maybe req (\value -> addHeader hname value req) mval

    where hname = symbolVal (Proxy :: Proxy sym)

-- | If you have a 'Post' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance (MimeUnrender ctyp a, TypeListElem ctyp ctyps)
      => HasClient ctyp (Post ctyps a) where
  type Client ctyp (Post ctyps a) = BaseUrl -> EitherT String IO a

  clientWithRoute Proxy req uri =
    performRequestCT (Proxy :: Proxy ctyp) H.methodPost req 201 uri

-- | If you have a 'Put' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
instance (MimeUnrender ctyp a, TypeListElem ctyp ctyps)
      => HasClient ctyp (Put ctyps a) where
  type Client ctyp (Put ctyps a) = BaseUrl -> EitherT String IO a

  clientWithRoute Proxy req host =
    performRequestCT (Proxy :: Proxy ctyp) H.methodPut req 200 host

-- | If you use a 'QueryParam' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'QueryParam',
-- enclosed in Maybe.
--
-- If you give Nothing, nothing will be added to the query string.
--
-- If you give a non-'Nothing' value, this function will take care
-- of inserting a textual representation of this value in the query string.
--
-- You can control how values for your type are turned into
-- text by specifying a 'ToText' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: Maybe Text -> BaseUrl -> EitherT String IO [Book]
-- > getBooksBy = client myApi
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy Nothing' for all books
-- > -- 'getBooksBy (Just "Isaac Asimov")' to get all books by Isaac Asimov
instance (KnownSymbol sym, ToText a, HasClient ctyp sublayout)
      => HasClient ctyp (QueryParam sym a :> sublayout) where

  type Client ctyp (QueryParam sym a :> sublayout) =
    Maybe a -> Client ctyp sublayout

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute Proxy req mparam =
    clientWithRoute (Proxy :: Proxy (ctyp, sublayout)) $
      maybe req (flip (appendToQueryString pname) req . Just) mparamText

    where pname  = cs pname'
          pname' = symbolVal (Proxy :: Proxy sym)
          mparamText = fmap toText mparam

-- | If you use a 'QueryParams' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument, a list of values of the type specified
-- by your 'QueryParams'.
--
-- If you give an empty list, nothing will be added to the query string.
--
-- Otherwise, this function will take care
-- of inserting a textual representation of your values in the query string,
-- under the same query string parameter name.
--
-- You can control how values for your type are turned into
-- text by specifying a 'ToText' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: [Text] -> BaseUrl -> EitherT String IO [Book]
-- > getBooksBy = client myApi
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy []' for all books
-- > -- 'getBooksBy ["Isaac Asimov", "Robert A. Heinlein"]'
-- > --   to get all books by Asimov and Heinlein
instance (KnownSymbol sym, ToText a, HasClient ctyp sublayout)
      => HasClient ctyp (QueryParams sym a :> sublayout) where

  type Client ctyp (QueryParams sym a :> sublayout) =
    [a] -> Client ctyp sublayout

  clientWithRoute Proxy req paramlist =
    clientWithRoute (Proxy :: Proxy (ctyp, sublayout)) $
      foldl' (\ req' -> maybe req' (flip (appendToQueryString pname) req' . Just)) req paramlist'

    where pname  = cs pname'
          pname' = symbolVal (Proxy :: Proxy sym)
          paramlist' = map (Just . toText) paramlist

-- | If you use a 'QueryFlag' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional 'Bool' argument.
--
-- If you give 'False', nothing will be added to the query string.
--
-- Otherwise, this function will insert a value-less query string
-- parameter under the name associated to your 'QueryFlag'.
--
-- Example:
--
-- > type MyApi = "books" :> QueryFlag "published" :> Get [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooks :: Bool -> BaseUrl -> EitherT String IO [Book]
-- > getBooks = client myApi
-- > -- then you can just use "getBooks" to query that endpoint.
-- > -- 'getBooksBy False' for all books
-- > -- 'getBooksBy True' to only get _already published_ books
instance (KnownSymbol sym, HasClient ctyp sublayout)
      => HasClient ctyp (QueryFlag sym :> sublayout) where

  type Client ctyp (QueryFlag sym :> sublayout) =
    Bool -> Client ctyp sublayout

  clientWithRoute Proxy req flag =
    clientWithRoute (Proxy :: Proxy (ctyp, sublayout)) $
      if flag
        then appendToQueryString paramname Nothing req
        else req

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)

-- | If you use a 'MatrixParam' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'MatrixParam',
-- enclosed in Maybe.
--
-- If you give Nothing, nothing will be added to the query string.
--
-- If you give a non-'Nothing' value, this function will take care
-- of inserting a textual representation of this value in the query string.
--
-- You can control how values for your type are turned into
-- text by specifying a 'ToText' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> MatrixParam "author" Text :> Get [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: Maybe Text -> BaseUrl -> EitherT String IO [Book]
-- > getBooksBy = client myApi
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy Nothing' for all books
-- > -- 'getBooksBy (Just "Isaac Asimov")' to get all books by Isaac Asimov
instance (KnownSymbol sym, ToText a, HasClient ctyp sublayout)
      => HasClient ctyp (MatrixParam sym a :> sublayout) where

  type Client ctyp (MatrixParam sym a :> sublayout) =
    Maybe a -> Client ctyp sublayout

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute Proxy req mparam =
    clientWithRoute (Proxy :: Proxy (ctyp, sublayout)) $
      maybe req (flip (appendToMatrixParams pname . Just) req) mparamText

    where pname = symbolVal (Proxy :: Proxy sym)
          mparamText = fmap (cs . toText) mparam

-- | If you use a 'MatrixParams' in one of your endpoints in your API,
-- the corresponding querying function will automatically take an
-- additional argument, a list of values of the type specified by your
-- 'MatrixParams'.
--
-- If you give an empty list, nothing will be added to the query string.
--
-- Otherwise, this function will take care of inserting a textual
-- representation of your values in the path segment string, under the
-- same matrix string parameter name.
--
-- You can control how values for your type are turned into text by
-- specifying a 'ToText' instance for your type.
--
-- Example:
--
-- > type MyApi = "books" :> MatrixParams "authors" Text :> Get [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooksBy :: [Text] -> BaseUrl -> EitherT String IO [Book]
-- > getBooksBy = client myApi
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy []' for all books
-- > -- 'getBooksBy ["Isaac Asimov", "Robert A. Heinlein"]'
-- > --   to get all books by Asimov and Heinlein
instance (KnownSymbol sym, ToText a, HasClient ctyp sublayout)
      => HasClient ctyp (MatrixParams sym a :> sublayout) where

  type Client ctyp (MatrixParams sym a :> sublayout) =
    [a] -> Client ctyp sublayout

  clientWithRoute Proxy req paramlist =
    clientWithRoute (Proxy :: Proxy (ctyp, sublayout)) $
      foldl' (\ req' value -> maybe req' (flip (appendToMatrixParams pname) req' . Just . cs) value) req paramlist'

    where pname  = cs pname'
          pname' = symbolVal (Proxy :: Proxy sym)
          paramlist' = map (Just . toText) paramlist

-- | If you use a 'MatrixFlag' in one of your endpoints in your API,
-- the corresponding querying function will automatically take an
-- additional 'Bool' argument.
--
-- If you give 'False', nothing will be added to the path segment.
--
-- Otherwise, this function will insert a value-less matrix parameter
-- under the name associated to your 'MatrixFlag'.
--
-- Example:
--
-- > type MyApi = "books" :> MatrixFlag "published" :> Get [Book]
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > getBooks :: Bool -> BaseUrl -> EitherT String IO [Book]
-- > getBooks = client myApi
-- > -- then you can just use "getBooks" to query that endpoint.
-- > -- 'getBooksBy False' for all books
-- > -- 'getBooksBy True' to only get _already published_ books
instance (KnownSymbol sym, HasClient ctyp sublayout)
      => HasClient ctyp (MatrixFlag sym :> sublayout) where

  type Client ctyp (MatrixFlag sym :> sublayout) =
    Bool -> Client ctyp sublayout

  clientWithRoute Proxy req flag =
    clientWithRoute (Proxy :: Proxy (ctyp, sublayout)) $
      if flag
        then appendToMatrixParams paramname Nothing req
        else req

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)

-- | Pick a 'Method' and specify where the server you want to query is. You get
-- back the status code and the response body as a 'ByteString'.
instance HasClient ctyp Raw where
  type Client ctyp Raw = H.Method -> BaseUrl -> EitherT String IO (Int, ByteString)

  clientWithRoute :: Proxy (ctyp, Raw) -> Req -> Client ctyp Raw
  clientWithRoute Proxy req httpMethod host =
    performRequest httpMethod req (const True) host

-- | If you use a 'ReqBody' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'ReqBody'.
-- That function will take care of encoding this argument as JSON and
-- of using it as the request body.
--
-- All you need is for your type to have a 'ToJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody Book :> Post Book
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- >
-- > addBook :: Book -> BaseUrl -> EitherT String IO Book
-- > addBook = client myApi
-- > -- then you can just use "addBook" to query that endpoint
instance (MimeRender ctyp a, TypeListElem ctyp ctyps, HasClient ctyp sublayout)
      => HasClient ctyp (ReqBody ctyps a :> sublayout) where

  type Client ctyp (ReqBody ctyps a :> sublayout) =
    a -> Client ctyp sublayout

  clientWithRoute Proxy req body =
    clientWithRoute (Proxy :: Proxy (ctyp, sublayout)) $
      setRQBody (toByteString (Proxy :: Proxy ctyp) body) req

-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path, HasClient ctyp sublayout)
      => HasClient ctyp (path :> sublayout) where
  type Client ctyp (path :> sublayout) = Client ctyp sublayout

  clientWithRoute Proxy req =
     clientWithRoute (Proxy :: Proxy (ctyp, sublayout)) $
       appendToPath p req

    where p = symbolVal (Proxy :: Proxy path)
