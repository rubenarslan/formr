# Store Credentials in Keyring

Securely stores formr credentials in the system keyring. This function
supports two modes:

1.  **Classic Mode:** Stores email/password (and optional 2FA) for a
    specific account name.

2.  **API Mode:** Stores OAuth credentials or Access Tokens for a
    specific host.

## Usage

``` r
formr_store_keys(
  account_name = NULL,
  email = NULL,
  password = NULL,
  secret_2fa = NULL,
  host = "https://rforms.org",
  client_id = NULL,
  client_secret = NULL,
  access_token = NULL,
  account = NULL
)
```

## Arguments

- account_name:

  (Classic) A shorthand name for the account. If provided, Classic mode
  is triggered.

- email:

  (Classic) Email address for the account. Will be prompted if omitted.

- password:

  (Classic) Optional. Provide to skip interactive prompt (useful for
  scripts/tests).

- secret_2fa:

  (Classic) A 2FA secret. Set to NULL to be prompted, or "" if not used.

- host:

  (API) The API URL (e.g., https://rforms.org). Defaults to rforms.org.

- client_id:

  (API) OAuth Client ID.

- client_secret:

  (API) OAuth Client Secret.

- access_token:

  (API) Direct Personal Access Token (alternative to OAuth).

- account:

  (API) Optional string identifier for multiple accounts on the same
  host.

## Examples

``` r
if (FALSE) { # \dontrun{
# --- Classic EXAMPLES ---
# Prompts for password interactively
formr_store_keys("formr_diary_study_account")

# --- NEW API EXAMPLES ---

# Store OAuth Credentials for a custom host
formr_store_keys(host = "http://localhost",
                 client_id = "my-id",
                 client_secret = "my-secret")
                 
# Store token for a specific secondary account
formr_store_keys(host = "http://localhost",
                 client_id = "my-id",
                 client_secret = "my-secret",
                 account = "project_b")
} # }
```
