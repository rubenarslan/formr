# Send text message via Massenversand.de

Connects to Massenversand.de using your token and sends a text message.

## Usage

``` r
text_message_massenversand(
  To,
  From,
  Body,
  id,
  pw,
  time = "0",
  msgtype = "t",
  tarif = "OA",
  test = "0",
  return_result = F
)
```

## Arguments

- To:

  the number you're texting to (usually without zeroes at the beginning)

- From:

  the number you're texting from

- Body:

  the text message body/text

- id:

  your Massenversand ID

- pw:

  your Massenversand password

- time:

  see provider API (defaults to immediate sending)

- msgtype:

  see provider API

- tarif:

  see provider API

- test:

  see provider API

- return_result:

  whether to return simply TRUE/FALSE on success/failure or the whole
  result

## Examples

``` r
if (FALSE) { # \dontrun{
text_message_massenversand(
  To = '492222', 
  From = '15005000', 
  Body = 'Hello friend',
  id = 'ID', 
  pw = 'Tokentokentoken')
  } # }
```
