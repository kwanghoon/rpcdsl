# PolyRPC Demo (Handle-as-Location)

This repo demonstrates a minimal PolyRPC-style DSL implementation in Haskell using GHC's type system:

- **Library EDSL** (`PolyRPC.Handle`) with kind-indexed locations (`Loc`), a location-indexed monad `RPC l a`, a handle type `Handle l`, and a typed `callH`.
- **"Handle-as-Location"**: The handle's type parameter fixes the destination location at compile time, enabling static verification of RPC calls.
- **QuasiQuoter** (`PolyRPC.QQ`) providing `[rpc| at h { f arg } |]` syntax sugar for `callH h f arg`.

## Build & Run

```bash
cabal build
cabal run polyrpc-demo-app
```

Expected output:
```
Result = 42
```

## Example Code

The demo shows both library-style and QuasiQuoter-style RPC calls:

```haskell
client :: RPC 'Client Int  
client = do
  hS <- spawn SServer "srv-img"
  
  -- Library style
  x <- callH hS incS 41
  
  -- QuasiQuoter style (equivalent)
  y <- [rpc| at hS { incS 41 } |]
  
  pure y
```

The `CanCall` type family enforces valid RPC patterns:
- ✅ `Client` → `Server`
- ✅ `Client` → `Worker w` 
- ✅ `l` → `l` (local calls)
- ❌ Other combinations (compile-time error)

## Key Features

- **Type-safe RPC calls**: The type system prevents invalid location-to-location calls via the `CanCall` type family.
- **Location-indexed monad**: `RPC l a` tracks the current execution location at the type level.
- **Handle-based routing**: Handles `Handle l` encode the destination location in their type.
- **QuasiQuoter syntax**: `[rpc| at h { f arg } |]` provides convenient syntax sugar for `callH h f arg`.

## Language Extensions Required

The following GHC extensions are used:
- `DataKinds` - for kind-level locations
- `TypeFamilies` - for the `CanCall` permission system  
- `UndecidableInstances` - for `TypeError` in `CanCall`
- `GADTs` - for singleton types
- `QuasiQuotes` - for the `[rpc| ... |]` syntax

## Dependencies

- `base` - standard Haskell library
- `template-haskell` - for QuasiQuoter implementation

## Notes

- The quasiquoter is intentionally minimal and supports: `at <handle> { <fun> <expr> }`.
- Currently supports variable names and integer literals as arguments.
- Extend the parser or add a source plugin for more complex syntax.
