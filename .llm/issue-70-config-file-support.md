# Plan: Config File Support for cardano-utxo-csmt (Issue #70)

## Summary

Remove hardcoded Mithril aggregator URLs and add YAML config file support using opt-env-conf. Create network-specific config files that can be loaded via `--config-file`.

## Critical Files

- `lib/Cardano/UTxOCSMT/Application/Options.hs` - Main options parser
- `lib/Cardano/UTxOCSMT/Mithril/Options.hs` - Mithril options parser
- `lib/Cardano/UTxOCSMT/Mithril/Client.hs` - Contains hardcoded URLs (lines 93-99)
- `application/Cardano/UTxOCSMT/Application/Run/Setup.hs` - Uses defaultMithrilConfig
- `run/cardano-utxo.sh` - Run script to update
- `cardano-utxo-csmt.cabal` - Dependencies

## Implementation Steps

### 1. Add Dependencies

In `cardano-utxo-csmt.cabal`, add to library dependencies:
- `autodocodec` - For HasCodec instances required by opt-env-conf `conf` combinator
- `path` - For `Path Abs File` type used by config file parsers

### 2. Update Mithril/Options.hs

Add `conf` combinator to each setting for config file support:

```haskell
-- Add imports
import OptEnvConf (conf)

-- Add conf to mithrilAggregatorOption
mithrilAggregatorOption :: Parser (Maybe String)
mithrilAggregatorOption =
    optional $ setting
        [ long "aggregator-endpoint"
        , env "AGGREGATOR_ENDPOINT"
        , conf "aggregator-endpoint"  -- NEW
        , help "..."
        , ...
        ]

-- Similarly for: genesis-verification-key, client-path, download-dir,
-- ancillary-verification-key
```

### 3. Update Application/Options.hs

Add config file loading wrapper and `conf` to settings:

```haskell
-- Add imports
import OptEnvConf (conf, withYamlConfig, subConfig, filePathSetting)
import Path (Path, Abs, File)

-- Add config file option
configFileOption :: Parser (Maybe (Path Abs File))
configFileOption = optional $ filePathSetting
    [ long "config-file"
    , short 'c'
    , help "Path to YAML configuration file"
    , metavar "FILE"
    ]

-- Wrap parser with config file support
optionsParser :: Parser Options
optionsParser = withYamlConfig configFileOption optionsParserCore

-- Rename existing parser and nest mithril options
optionsParserCore :: Parser Options
optionsParserCore =
    mkOptions
        <$> networkOption       -- add: conf "network"
        <*> nodeNameOption      -- add: conf "node-name"
        <*> portNumberOption    -- add: conf "port"
        ...
        <*> subConfig "mithril" mithrilOptionsParser'
        ...
```

### 4. Remove Hardcoded URLs from Client.hs

Delete `defaultAggregatorUrl` function (lines 93-99).

Update `defaultMithrilConfig` signature to require aggregator URL:

```haskell
defaultMithrilConfig
    :: Manager
    -> MithrilNetwork
    -> String          -- aggregatorUrl (now required)
    -> FilePath
    -> MithrilConfig
```

### 5. Update Setup.hs

Update `runMithrilBootstrap` to require aggregator URL and fail gracefully if not provided:

```haskell
runMithrilBootstrap manager downloadDir =
    case Mithril.mithrilAggregatorUrl mithrilOpts of
        Nothing -> do
            trace $ Mithril $ ImportError MithrilMissingAggregatorUrl
            regularSetup
        Just url -> do
            let mithrilConfig = defaultMithrilConfig manager network url downloadDir
            -- ... rest unchanged
```

Add new error type `MithrilMissingAggregatorUrl` to Client.hs.

### 6. Create Config Files

Create `config/` directory with network-specific YAML files:

**config/mainnet.yaml:**
```yaml
network: mainnet
mithril:
  aggregator-endpoint: "https://aggregator.release-mainnet.api.mithril.network/aggregator"
```

**config/preprod.yaml:**
```yaml
network: preprod
mithril:
  aggregator-endpoint: "https://aggregator.release-preprod.api.mithril.network/aggregator"
```

**config/preview.yaml:**
```yaml
network: preview
mithril:
  aggregator-endpoint: "https://aggregator.pre-release-preview.api.mithril.network/aggregator"
```

### 7. Update Run Script

Update `run/cardano-utxo.sh` to use config files:

```bash
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CONFIG_FILE="${CONFIG_FILE:-$SCRIPT_DIR/config/$NETWORK.yaml}"

args=(
    --config-file "$CONFIG_FILE"
    --network "$NETWORK"
    ...
)
```

## Priority Order

opt-env-conf default: CLI > Environment Variable > Config File > Default Value

This means existing scripts using env vars or CLI args continue to work unchanged.

## Testing

1. Update `test/Cardano/UTxOCSMT/Application/OptionsSpec.hs`:
   - Add tests for config file loading
   - Test priority order (CLI overrides config)
   - Test nested mithril options under `mithril:` key

2. Manual testing:
   - `just build` compiles successfully
   - `just unit` tests pass
   - Run with `--config-file config/preview.yaml` works
   - Run without config file but with `AGGREGATOR_ENDPOINT` env var works
   - Run with both (CLI should override config file)

## Documentation Updates

### docs/getting-started.md

Add config file section before "Setup Environment Variables":

```markdown
## Configuration

The service supports three configuration methods (priority: CLI > env > config file):

1. **Config file** (recommended for deployment):
   ```bash
   cardano-utxo --config-file config/preprod.yaml --csmt-db-path /data/csmt
   ```

2. **Environment variables** (good for containers):
   ```bash
   export AGGREGATOR_ENDPOINT=https://...
   cardano-utxo --mithril-bootstrap ...
   ```

3. **CLI arguments**:
   ```bash
   cardano-utxo --aggregator-endpoint https://... ...
   ```

Pre-configured files are provided in `config/` for each network.
```

Update Configuration Options table to include `--config-file`.

### docs/mithril-bootstrap.md

Update "CLI Options" section to mention config file alternative:

```markdown
## Configuration

Configuration can be provided via config file, environment variables, or CLI:

**Config file (recommended):**
```bash
cardano-utxo --config-file config/preview.yaml --csmt-db-path /data/csmt
```

The config files include network-specific Mithril aggregator URLs.

**Environment variables:** (existing content)
```

Update the Networks table to note URLs are now in config files.

## Backwards Compatibility

- All existing CLI options work unchanged
- All existing environment variables work unchanged
- New: `--config-file` option available
- Breaking: Running `--mithril-bootstrap` without providing aggregator URL (via any method) will fail with clear error message

---

## Progress Log

### Commit 1: `feat: add YAML config file support` (9b71f9c)

Initial implementation of config file support:
- Added `--config-file` option using `withYamlConfig`
- Added `conf` combinators to options for config file parsing
- Created `config/` directory with network-specific YAML files (mainnet, preprod, preview)
- Mithril options nested under `mithril:` key in config
- Added tests for config file loading and priority order

### Pending: `refactor: require node-name and port from config`

Remove hardcoded network defaults for peer node settings. Previously `node-name` and `port` had per-network defaults (e.g., mainnet used `backbone.cardano.iog.io:3001`). Now these must be provided via config file, environment, or CLI.

Changes:
- `nodeName` and `portNumber` are now required fields in `Options` (not `Maybe`)
- Removed `defaultNodeFor` and `defaultPortFor` functions
- Removed `nodeNameOverride` and `portNumberOverride` - simplified to direct fields
- Added `HasCodec PortNumber` orphan instance for config file parsing
- Updated config files to include `node-name` and `port` for each network
- Updated tests to provide required fields in test configs

Rationale: Config files should be the source of truth for network-specific settings, making the codebase simpler and configuration more explicit.
