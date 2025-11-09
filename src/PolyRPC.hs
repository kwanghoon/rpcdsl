{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module PolyRPC
  ( Loc(..), RPC(..)
  , Handle(..), SomeHandle(..)
  , SLoc(..)
  , Serializable
  , CanCall
  , spawn, callH, callDyn
  ) where

import PolyRPC.Handle
