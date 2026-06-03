# Send text message via Clickatell

Connects to Clickatell using your token and sends a text message.

## Usage

``` r
text_message_clickatell(To, Body, Token, return_result = FALSE)
```

## Arguments

- To:

  the number you're texting to (usually without zeroes at the beginning)

- Body:

  the text message body/text

- Token:

  your Clickatell token

- return_result:

  whether to return simply TRUE/FALSE on success/failure or the whole
  result

## Value

Logical `TRUE`/`FALSE` indicating send success, or (when
`return_result = TRUE`) the raw API response list.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: sends a real SMS via the Clickatell gateway (needs an account).
text_message_clickatell(
  To = '492222', 
  Body = 'Hello friend', 
  Token = 'Tokentokentoken')
  } # }
```
