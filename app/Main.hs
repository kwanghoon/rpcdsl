{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import PolyRPC
import PolyRPC.QQ (rpc)

-- A server-side function
incS :: Int -> RPC 'Server Int
incS n = pure (n+1)

main :: IO ()
main = do
  -- Run the client program
  r <- runRPC client
  putStrLn ("Result = " ++ show r)

-- Client program: spawn a server handle, then call via handle.
client :: RPC 'Client Int
client = do
  hS <- spawn SServer "srv-img"
  -- Library style
  x  <- callH hS incS 41
  -- QuasiQuoter style:  [$rpc| at hS { incS 41 } |]  ≡  callH hS incS 41
  y  <- [rpc| at hS { incS 41 } |]
  pure y
