{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module PolyRPC.Handle where

import GHC.TypeLits
import Data.Kind (Constraint)
import Data.Proxy (Proxy(Proxy))

-- | Locations in the system (kind-level)
data Loc = Client | Server | Worker Symbol

-- | Simple Serializable class for demo purposes.
class Serializable a
instance Serializable Int
instance Serializable ()

-- | RPC monad indexed by current location.
newtype RPC (l :: Loc) a = RPC { runRPC :: IO a }

instance Functor (RPC l) where
  fmap f (RPC io) = RPC (fmap f io)
instance Applicative (RPC l) where
  pure = RPC . pure
  RPC f <*> RPC x = RPC (f <*> x)
instance Monad (RPC l) where
  RPC x >>= k = RPC (x >>= runRPC . k)

-- | Static permission: which locations may call which.
type family CanCall (from :: Loc) (to :: Loc) :: Constraint where
  CanCall Client Server      = ()
  CanCall Client (Worker w)  = ()
  CanCall x x                = ()   -- local allowed
  CanCall from to            =
    TypeError (Text "Disallowed RPC: " :<>: ShowType from
               :<>: Text " → " :<>: ShowType to)

-- | Singleton for Loc (minimal demo version).
data SLoc (l :: Loc) where
  SClient :: SLoc 'Client
  SServer :: SLoc 'Server
  SWorker :: KnownSymbol w => SLoc ('Worker w)

-- | Handle to a remote execution context living at l.
data Handle (l :: Loc) = Handle
  { endpointId :: String
  }

-- | Existential wrapper to mix handles of different locations.
data SomeHandle where
  SomeHandle :: SLoc l -> Handle l -> SomeHandle

-- | Spawn a new context at location l' and get its handle.
-- The SLoc l' parameter ensures type-level consistency between 
-- the singleton location and the returned handle's type parameter.
spawn
  :: forall l l'. SLoc l'   -- where to spawn (singleton witness)
  -> String                 -- image/binary/options (demo only)  
  -> RPC l (Handle l')     -- handle typed with same location l'
spawn sloc opts = RPC (pure (Handle (endpointForLocation sloc opts)))

-- | Generate endpoint ID based on location type (demo implementation)
endpointForLocation :: SLoc l -> String -> String
endpointForLocation SClient opts = "client-" ++ opts
endpointForLocation SServer opts = "server-" ++ opts  
endpointForLocation (SWorker @w) opts = "worker-" ++ symbolVal (Proxy @w) ++ "-" ++ opts

-- | Call using a handle; the handle's location fixes the destination.
callH
  :: forall l l' req res.
     (CanCall l l', Serializable req, Serializable res)
  => Handle l'               -- destination, at type level
  -> (req -> RPC l' res)     -- function to run there
  -> req
  -> RPC l res
callH _h f x = RPC (runRPC (f x))

-- | Dynamic call variant for mixed handle collections.
callDyn
  :: forall l req res.
     (Serializable req, Serializable res)
  => SomeHandle
  -> (forall l'. SLoc l' -> req -> RPC l' res)
  -> req
  -> RPC l res
callDyn (SomeHandle sl _) f x = RPC (runRPC (f sl x))
