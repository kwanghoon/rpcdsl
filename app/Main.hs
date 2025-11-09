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

{-

For a client incS, the Haskell typechecker
will produce type errors!

incS :: Int -> RPC 'Client Int
incS n = pure (n+1)

app\Main.hs:23:18: error:
    * Couldn't match type 'Client with 'Server
      Expected: Int -> RPC 'Server Int
        Actual: Int -> RPC 'Client Int
    * In the second argument of `callH', namely `incS'       
      In a stmt of a 'do' block: x <- callH hS incS 41       
      In the expression:
        do hS <- spawn SServer "srv-img"
           x <- callH hS incS 41
           y <- (((callH hS) incS) 41)
           pure y
   |
23 |   x  <- callH hS incS 41
   |                  ^^^^

app\Main.hs:25:14: error:
    * Couldn't match type 'Client with 'Server
      Expected: Int -> RPC 'Server Int
        Actual: Int -> RPC 'Client Int
    * In the second argument of `callH', namely `incS'       
      In a stmt of a 'do' block: y <- (((callH hS) incS) 41) 
      In the expression:
        do hS <- spawn SServer "srv-img"
           x <- callH hS incS 41
           y <- (((callH hS) incS) 41)
           pure y
   |
25 |   y  <- [rpc| at hS { incS 41 } |]

-}