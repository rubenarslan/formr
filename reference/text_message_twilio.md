# Send text message via Twilio

Connects to Twilio using your token and sends a text message.

## Usage

``` r
text_message_twilio(To, From, Body, Account, Token, return_result = FALSE)
```

## Arguments

- To:

  the number you're texting to (usually without zeroes at the beginning)

- From:

  the number you're texting from

- Body:

  the text message body/text

- Account:

  your Twilio account ID

- Token:

  your Twilio token

- return_result:

  whether to return simply TRUE/FALSE on success/failure or the whole
  result

## Examples

``` r
if (FALSE) { # \dontrun{
text_message_twilio(
  To = '492222',
  From = '15005000',
  Body = 'Hello friend',
  Account = 'ID', Token = 'Tokentokentoken')
} # }
```
