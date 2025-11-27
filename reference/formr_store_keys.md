# Store keys in keyring

Store keys in the system keyring/keychain instead of plaintext.

## Usage

``` r
formr_store_keys(account_name, email = NULL, secret_2fa = NULL)
```

## Arguments

- account_name:

  a shorthand for the account you're using

- email:

  email address for the account, will be prompted if omitted

- secret_2fa:

  a 2FA secret, optional, set to NULL if you want to be prompted for it
  when logging in, set to "" if you don't have 2FA

## Examples

``` r
if (FALSE) { # \dontrun{
formr_store_keys("formr_diary_study_account")
} # }
```
