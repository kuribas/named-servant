{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.Record (RecordParam, UnRecordParam) where
import Servant.API
import Data.Proxy
import GHC.TypeLits
import GHC.Generics

data RecordParam (a :: *)

type family ServantAppend x y where
  ServantAppend (a :> b) c = a :> ServantAppend b c
  ServantAppend a c = a :> c

-- | Type family to rewrite a RecordParam Api to a regular servant API.
-- Useful to define instances for classes that extract information from
-- the API type, such as Servant.Swagger, or servant-foreign.
--
-- Typical use:
-- 
-- > instance SomeClass (UnRecordParam (RecordParam a :> api))) =>
-- >          SomeClass (RecordParam a :> api) where
-- >    someMethod _ =
-- >      someMethod (Proxy :: Proxy (UnRecordParam (RecordParam a :> api))

type family UnRecordParam (x :: *) :: * where
  UnRecordParam (a :> b) = ServantAppend (UnRecordParam a) b
  UnRecordParam (RecordParam a) = UnRecordParam (Rep a ())
  UnRecordParam (D1 m c d) = UnRecordParam (c d)
  UnRecordParam ((a :*: b) d) = ServantAppend (UnRecordParam (a d))
                                (UnRecordParam (b d))
  UnRecordParam (C1 m a d) = UnRecordParam (a d)
  UnRecordParam (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool) d) =
    QueryFlag sym
  UnRecordParam (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a]) d) =
    QueryParams sym a
  UnRecordParam (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)) d) =
    QueryParam' [Optional, Strict] sym a
  UnRecordParam (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a) d) =
    QueryParam' [Required, Strict] sym a
  
instance (Generic a, GHasLink (Rep a) sub) => HasLink (RecordParam a :> sub)
  where
    type MkLink (RecordParam a :> sub) b = a -> MkLink sub b
    toLink toA _ l record =
      gToLink toA (Proxy :: Proxy sub) l (from record :: Rep a ())

data GParam a

instance GHasLink a sub => HasLink (GParam (a ()) :> sub) where
  type MkLink (GParam (a ()) :> sub) b = a () -> MkLink sub b
  toLink toA _ = gToLink toA (Proxy :: Proxy sub)
  {-# INLINE toLink #-}
  
class HasLink sub => GHasLink (a :: * -> *) sub where
  gToLink :: (Link -> b) -> Proxy sub -> Link -> a () -> MkLink sub b

instance GHasLink c sub => GHasLink (D1 m c) sub where
  gToLink toA _ l (M1 x) = gToLink toA (Proxy :: Proxy sub) l x
  {-# INLINE gToLink #-}

instance ( HasLink sub
         , GHasLink a (GParam (b ()) :> sub)
         )
         => GHasLink (a :*: b) sub where
  gToLink toA _ l (a :*: b) =
    gToLink toA (Proxy :: Proxy (GParam (b ()) :> sub)) l a b
  {-# INLINE gToLink #-}

instance (GHasLink a sub, HasLink sub) =>
         GHasLink (C1 m a) sub where
  gToLink toA _ l (M1 x) = gToLink toA (Proxy :: Proxy sub) l x
  {-# INLINE gToLink #-}

instance {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , HasLink sub
  ) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool)) sub where
  gToLink toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryFlag sym :> sub)) l x
  {-# INLINE gToLink #-}

instance {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a])) sub where
  gToLink toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParams sym a :> sub)) l x
  {-# INLINE gToLink #-}

instance {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a))) sub where
  gToLink toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParam' '[Optional, Strict] sym a :> sub))
    l x
  {-# INLINE gToLink #-}

instance {-# OVERLAPPABLE #-}
  ( KnownSymbol sym
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a)) sub where
  gToLink toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParam' '[Required, Strict] sym a :> sub))
    l x
  {-# INLINE gToLink #-}
