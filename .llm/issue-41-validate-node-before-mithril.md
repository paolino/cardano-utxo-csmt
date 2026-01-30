# Issue #41: Validate Node Connection Before Mithril Bootstrap

**Base branch:** `fix/remove-partial-history` (rebased from `dd0378d`)

## Problem

When `--mithril-bootstrap` is enabled, the application spends ~1 hour importing UTxOs before attempting to connect to cardano-node. If the node connection is misconfigured, this time is wasted.

## Solution

Add early validation that connects to the node before starting Mithril import. Fail fast with a clear error if unreachable.

## Implementation Plan

### 1. Add validation function in `lib/Cardano/UTxOCSMT/Ouroboros/Connection.hs`

Export new function and types:

```haskell
module Cardano.UTxOCSMT.Ouroboros.Connection
    ( runNodeApplication
    , validateNodeConnection  -- NEW
    , NodeConnectionError (..) -- NEW
    , ChainSyncApplication
    , BlockFetchApplication
    )
```

Add error type:

```haskell
data NodeConnectionError
    = NodeResolutionFailed String
    | NodeHandshakeFailed String
    | NodeConnectionTimeout
    deriving (Show, Eq)
```

Add validation function using existing `resolve` and handshake logic:

```haskell
validateNodeConnection
    :: NetworkMagic
    -> String        -- ^ hostname
    -> PortNumber    -- ^ port
    -> Int           -- ^ timeout in microseconds
    -> IO (Either NodeConnectionError ())
```

Implementation:
- Use `System.Timeout.timeout` to wrap the connection attempt
- Reuse existing `resolve` function
- Perform a minimal handshake (connect, version negotiation, disconnect)
- Catch exceptions and map to `NodeConnectionError`

### 2. Add trace types in `Application/Run/Traces.hs`

Add constructor to `MainTraces`:

```haskell
data MainTraces
    = ...
    | NodeValidation NodeValidationTrace  -- NEW
```

```haskell
data NodeValidationTrace
    = ValidatingNodeConnection String PortNumber
    | NodeValidationSuccess
    | NodeValidationFailed NodeConnectionError
    deriving (Show)
```

Add render function:

```haskell
renderNodeValidationTrace :: NodeValidationTrace -> String
```

### 3. Add CLI options in `lib/Cardano/UTxOCSMT/Application/Options.hs`

Add to `Options` record:

```haskell
data Options = Options
    { ...
    , skipNodeValidation :: Bool  -- default False
    }
```

Add option parser:

```haskell
skipNodeValidationSwitch :: Parser Bool
skipNodeValidationSwitch =
    setting
        [ long "skip-node-validation"
        , help "Skip node connection validation before Mithril bootstrap"
        , reader auto
        , value False
        , switch True
        ]
```

### 4. Modify `setupDB` in `Application/Run/Setup.hs`

Change signature to accept validation parameters:

```haskell
setupDB
    :: Tracer IO MainTraces
    -> Point
    -> MithrilOptions
    -> NetworkMagic         -- NEW
    -> String               -- NEW: nodeName
    -> PortNumber           -- NEW: nodePort
    -> Bool                 -- NEW: skipNodeValidation
    -> ArmageddonParams hash
    -> RunCSMTTransaction ...
    -> IO SetupResult
```

Add validation before `bootstrapFromMithril` (~line 117):

```haskell
if new
    then do
        if mithrilEnabled mithrilOpts || Mithril.mithrilBootstrapOnly mithrilOpts
            then do
                -- Validate node connection before expensive Mithril import
                unless skipValidation $ do
                    trace $ NodeValidation $ ValidatingNodeConnection nodeName nodePort
                    result <- validateNodeConnection magic nodeName nodePort 30_000_000
                    case result of
                        Left err -> do
                            trace $ NodeValidation $ NodeValidationFailed err
                            error $ "Node connection failed: " ++ show err
                                ++ "\nCheck --node-name and --node-port settings."
                        Right () ->
                            trace $ NodeValidation NodeValidationSuccess
                bootstrapFromMithril
            else regularSetup
```

### 5. Update `Main.hs` to pass new parameters

Pass validation parameters to `setupDB`:

```haskell
SetupResult{setupStartingPoint, setupMithrilSlot} <-
    setupDB
        tracer
        startingPoint
        mithrilOptions
        (networkMagic options)      -- NEW
        (nodeName options)          -- NEW
        (portNumber options)        -- NEW
        (skipNodeValidation options) -- NEW
        armageddonParams
        runner
```

## Files to Modify

| File | Change |
|------|--------|
| `lib/Cardano/UTxOCSMT/Ouroboros/Connection.hs` | Add `validateNodeConnection`, `NodeConnectionError` |
| `lib/Cardano/UTxOCSMT/Application/Options.hs` | Add `skipNodeValidation` field and parser |
| `application/Cardano/UTxOCSMT/Application/Run/Traces.hs` | Add `NodeValidation`, `NodeValidationTrace` |
| `application/Cardano/UTxOCSMT/Application/Run/Setup.hs` | Add validation call, new parameters |
| `application/Cardano/UTxOCSMT/Application/Run/Main.hs` | Pass new options to `setupDB` |

## Acceptance Criteria (from issue)

- [x] Connection validation runs before Mithril import when `--mithril-bootstrap` is used
- [x] Clear error message if connection fails (including configuration hints)
- [x] Validation has reasonable timeout (default 30 seconds)
- [x] Optional flag to skip validation if needed (`--skip-node-validation`)
- [x] Logs indicate validation is happening

## Verification

1. `just build` - ensure it compiles
2. `just unit` - ensure tests pass
3. Manual test with valid node: validation passes, Mithril proceeds
4. Manual test with invalid hostname: fails fast with clear error
5. Manual test with `--skip-node-validation`: bypasses check
