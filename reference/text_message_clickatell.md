# Send text message via Clickatell

Connects to Clickatell using your token and sends a text message.

## Usage

``` r
text_message_clickatell(To, Body, Token, return_result = F)
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

## Examples

``` r
if (FALSE) { # \dontrun{
text_message_clickatell(
  To = '492222', 
  Body = 'Hello friend', 
  Token = 'Tokentokentoken')
  } # }
```
