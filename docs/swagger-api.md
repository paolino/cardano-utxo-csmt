# Swagger API Documentation

The Cardano UTxO CSMT HTTP service includes comprehensive Swagger/OpenAPI documentation for its REST API.

## Overview

The `Cardano.UTxOCSMT.HTTP.Swagger` module provides:

- `swaggerDoc` - A Swagger specification that documents the API
- `swaggerSchemaUIServer` - A Servant server that hosts an interactive Swagger UI

## API Endpoints

The API currently exposes:

- `GET /metrics` - Returns metrics about CSMT operations

### Metrics Response

The metrics endpoint returns a JSON object with the following fields:

```json
{
  "averageQueueLength": 0.0,
  "maxQueueLength": null,
  "utxoChangesCount": 0,
  "lastBlockPoint": null,
  "utxoSpeed": 0.0,
  "blockSpeed": 0.0,
  "currentEra": null,
  "currentMerkleRoot": null
}
```

#### Field Descriptions

- `averageQueueLength` (Double) - Average length of the block fetch queue
- `maxQueueLength` (Maybe Int) - Maximum queue length observed
- `utxoChangesCount` (Int) - Total number of UTxO changes processed
- `lastBlockPoint` (Maybe String) - Timestamp of the last processed block
- `utxoSpeed` (Double) - Speed of UTxO change processing (changes per second)
- `blockSpeed` (Double) - Speed of block processing (blocks per second)
- `currentEra` (Maybe String) - Current Cardano era (e.g., "byron", "shelley", "alonzo", etc.)
- `currentMerkleRoot` (Maybe String) - Current Merkle root hash of the CSMT

## Usage Example

To integrate the Swagger UI into your HTTP server:

```haskell
import Cardano.UTxOCSMT.HTTP.API (api)
import Cardano.UTxOCSMT.HTTP.Swagger (swaggerSchemaUIServer)
import Servant ((:<|>)(..), serve)

-- Combine your API with the Swagger UI
type FullAPI = API :<|> SwaggerAPI

-- Create a server that serves both the API and Swagger UI
fullServer :: Server FullAPI
fullServer = apiServer :<|> swaggerSchemaUIServer

-- Run with Warp
main :: IO ()
main = run 8080 $ serve (Proxy @FullAPI) fullServer
```

After starting the server, you can access:
- The API at `http://localhost:8080/metrics`
- The Swagger UI at `http://localhost:8080/swagger-ui`
- The OpenAPI JSON spec at `http://localhost:8080/swagger.json`

## Future Enhancements

As the API grows to include UTxO inclusion/exclusion proofs and address queries, the Swagger documentation will automatically be updated to reflect these new endpoints.
