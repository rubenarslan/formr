#' Send text message via Twilio
#'
#' Connects to Twilio using your token and sends a text message.
#'
#' @param To the number you're texting to (usually without zeroes at the beginning)
#' @param From the number you're texting from
#' @param Body the text message body/text
#' @param Account your Twilio account ID
#' @param Token your Twili token
#' @param return_result whether to return simply TRUE/FALSE on success/failure or the whole result
#' @export
#' @examples
#' text_message_twilio(
#' 	To = '492222', 
#' 	From = '15005000', 
#' 	Body = 'Hello friend',
#' 	Account = 'ID', Token = 'Tokentokentoken')

text_message_twilio = function(To, From, Body, Account, Token, 
  return_result = F) {
  result = httr::POST(paste0("https://api.twilio.com/2010-04-01/Accounts/", 
    Account, "/Messages.json"), httr::authenticate(Account, 
    Token), body = list(From = From, To = To, Body = Body))
  result2 = httr::content(result)
  if (return_result) {
    result2
  } else {
    if (is.null(result2$error_code) & result2$status < 300) {
      TRUE
    } else {
      FALSE
    }
  }
}

#' Send text message via Clickatell
#'
#' Connects to Clickatell using your token and sends a text message.
#'
#' @param To the number you're texting to (usually without zeroes at the beginning)
#' @param Body the text message body/text
#' @param Token your Clickatell token
#' @param return_result whether to return simply TRUE/FALSE on success/failure or the whole result
#' @export
#' @examples
#' text_message_clickatell(
#' 	To = '492222', 
#' 	Body = 'Hello friend', 
#' 	Token = 'Tokentokentoken')

text_message_clickatell = function(To, Body, Token, return_result = F) {
  result = httr::POST("https://api.clickatell.com/rest/message", 
    httr::add_headers(`X-Version` = 1, `Content-Type` = "application/json", 
      Authorization = paste("Bearer", Token), accept = "application/json"), 
    body = paste0("{\"to\":[\"", To, "\"],\"text\":\"", Body, 
      "\"}"))
  
  result2 = httr::content(result)
  if (return_result) {
    result2
  } else {
    if (is.null(result2$error)) {
      TRUE
    } else {
      FALSE
    }
  }
}


#' Send text message via Massenversand.de
#'
#' Connects to Massenversand.de using your token and sends a text message.
#'
#' @param To the number you're texting to (usually without zeroes at the beginning)
#' @param From the number you're texting from
#' @param Body the text message body/text
#' @param id your Massenversand ID
#' @param pw your Massenversand password
#' @param time see provider API (defaults to immediate sending)
#' @param msgtype see provider API
#' @param tarif see provider API
#' @param test see provider API
#' @param return_result whether to return simply TRUE/FALSE on success/failure or the whole result
#' 
#' @export
#' @examples
#' text_message_massenversand(
#' 	To = '492222', 
#' 	From = '15005000', 
#' 	Body = 'Hello friend',
#' 	id = 'ID', 
#' 	pw = 'Tokentokentoken')

text_message_massenversand = function(To, From, Body, id, pw, 
  time = "0", msgtype = "t", tarif = "OA", test = "0", return_result = F) {
  # don't forget to use html character encoding (e.g. for
  # emptyspaces)
  params = paste0("test=", test, "&", "receiver=", To, "&", 
    "sender=", From, "&", "msg=", Body, "&", "id=", id, "&", 
    "pw=", pw, "&", "time=", time, "&", "msgtype=", msgtype, 
    "&", "tarif=", tarif)
  
  # there are two servers, maybe one should try the other one
  # as well in case of failure ...
  
  answ = httr::GET(paste0("https://gate1.goyyamobile.com/sms/sendsms.asp?", 
    params, sep = ""))
  
  
  # returns false if message was not sent correctly (see
  # provider API for error codes)
  if (return_result) {
    result2 = rawToChar(answ$content)
    result2
  } else {
    return(rawToChar(answ$content) == "OK")
  }
}