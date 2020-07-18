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
module Servant.Record (RecordParam) where
import Servant.API
import Data.Proxy
import GHC.TypeLits
import GHC.Generics

data RecordParam (a :: *)

instance (Generic a, GHasLink (Rep a ()) sub) =>
         HasLink (RecordParam a :> sub)
  where
    type MkLink (RecordParam a :> sub) b = a -> MkLink sub b
    toLink toA _ l record =
      gToLink toA (Proxy :: Proxy sub) l (from record :: Rep a ())

data GParam a

instance GHasLink a sub => HasLink (GParam a :> sub) where
  type MkLink (GParam a :> sub) b = a -> MkLink sub b
  toLink toA _ = gToLink toA (Proxy :: Proxy sub)
  {-# INLINE toLink #-}
  
class HasLink sub => GHasLink a sub where
  gToLink :: (Link -> b) -> Proxy sub -> Link -> a -> MkLink sub b

instance GHasLink (c m2 ()) sub => GHasLink (D1 m (c m2) ()) sub where
  gToLink toA _ l (M1 x) = gToLink toA (Proxy :: Proxy sub) l x
  {-# INLINE gToLink #-}

instance ( HasLink sub
         , GHasLink (a ()) (GParam (b ()) :> sub)
         )
         => GHasLink ((a :*: b) ()) sub where
  gToLink toA _ l (a :*: b) =
    gToLink toA (Proxy :: Proxy (GParam (b ()) :> sub)) l a b
  {-# INLINE gToLink #-}

instance (GHasLink (a ()) sub, HasLink sub) =>
         GHasLink (C1 m a ()) sub where
  gToLink toA _ l (M1 x) = gToLink toA (Proxy :: Proxy sub) l x
  {-# INLINE gToLink #-}

instance {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , HasLink sub
  ) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 Bool) ()) sub where
  gToLink toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryFlag sym :> sub)) l x
  {-# INLINE gToLink #-}

instance {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 [a]) ()) sub where
  gToLink toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParams sym a :> sub)) l x
  {-# INLINE gToLink #-}

instance {-# OVERLAPPING #-}
  ( KnownSymbol sym
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 (Maybe a)) ()) sub where
  gToLink toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParam' '[Optional, Strict] sym a :> sub))
    l x
  {-# INLINE gToLink #-}

instance {-# OVERLAPPABLE #-}
  ( KnownSymbol sym
  , ToHttpApiData a
  , HasLink (a :> sub)
  , HasLink sub) =>
  GHasLink (S1 ('MetaSel ('Just sym) d1 d2 d3) (Rec0 a) ()) sub where
  gToLink toA _ l (M1 (K1 x)) =
    toLink toA (Proxy :: Proxy (QueryParam' '[Required, Strict] sym a :> sub))
    l x
  {-# INLINE gToLink #-}

