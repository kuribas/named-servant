{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
module Servant.Named (NamedQueryParam, OptionalQueryParam, NamedQueryParams,
                      NamedQueryParam') where
import Servant.API
import Servant.API.Modifiers
import Data.Proxy
import GHC.TypeLits
import Data.Functor.Identity
import Named

-- | Like `QueryParam'`, but instead of extracting a type @a@, it
-- extracts a named type @`NamedF` f a sym@, where the name
-- corresponds to the query parameter string.
data NamedQueryParam' (mods :: [*]) (sym :: Symbol) (a :: *)

unarg :: NamedF f a name -> f a
unarg (ArgF a) = a

instance (KnownSymbol sym, ToHttpApiData v, HasLink sub,
          SBoolI (FoldRequired mods))
    => HasLink (NamedQueryParam' mods sym v :> sub)
  where
    type MkLink (NamedQueryParam' mods sym v :> sub) a =
      If (FoldRequired mods) (sym :! v) (sym :? v) -> MkLink sub a
    toLink toA _ l qparam =
      toLink toA (Proxy :: Proxy (QueryParam' mods sym v :> sub)) l $
      case sbool :: SBool (FoldRequired mods) of
        STrue  -> runIdentity (unarg qparam)
        SFalse -> unarg qparam

-- | Lookup the value associated to the sym query string parameter and
-- try to extract it as an optional named argument of type @sym `:?`
-- a@.
type OptionalQueryParam = NamedQueryParam' [Optional, Strict]

-- | Like `QueryParam`, but instead of extracting a type @a@, it
-- extracts a named type @named `:!` a@, where named corresponds to
-- the query parameter string.
type NamedQueryParam = NamedQueryParam' [Required, Strict]

-- | Like `QueryParams`, but extracts a named type @named `:!` [a]@
-- instead, where named corresponds to the query parameter string.
data NamedQueryParams (sym :: Symbol) (a :: *)

instance (KnownSymbol sym, ToHttpApiData v, HasLink sub)
         => HasLink (NamedQueryParams sym v :> sub)
  where
    type MkLink (NamedQueryParams sym v :> sub) a = sym :! [v] -> MkLink sub a
    toLink toA _ l (Arg params) =
      toLink toA (Proxy :: Proxy (QueryParams sym v :> sub)) l params

