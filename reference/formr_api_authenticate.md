# Authenticate with formr

Connects to the API. If no credentials are provided, the auto-pickup
chain is: the package's hidden `.formr` env (set automatically when the
code runs inside an OpenCPU session on formr.org), then the
calling-frame chain (for legacy injectors that wrote bare locals into
the wrapper scope), then the keyring.

## Usage

``` r
formr_api_authenticate(
  host = "https://formr.org",
  client_id = NULL,
  client_secret = NULL,
  access_token = NULL,
  account = NULL
)
```

## Arguments

- host:

  API Base URL. Defaults to `.formr$host` when running on formr.org,
  otherwise `"https://formr.org"`.

- client_id:

  OAuth Client ID.

- client_secret:

  OAuth Client Secret.

- access_token:

  Direct Access Token.

- account:

  Optional string identifier for multiple accounts on the same host.
