#' Connect to formr API
#'
#' Connects to formr using your client_id and client_secret (OAuth 2.0 grant type: client_credentials).
#'
#' @param client_id your client_id
#' @param client_secret your client_secret
#' @param host defaults to https://formr.org
#' @export
#' @examples
#' \dontrun{
#' formr_api_access_token(client_id = 'your_id', client_secret = 'your_secret' )
#' }

formr_api_access_token = function(client_id, client_secret, host = "https://api.formr.org/") {
  base_url = httr::parse_url(host)
  
  .formr_current_session$set(base_url)
  token_url = base_url
  token_url$path = paste0(token_url$path, "oauth/access_token")
  
  result = httr::POST(url = token_url, body = list(client_id = client_id, 
    client_secret = client_secret, grant_type = "client_credentials"))
  
  token = httr::content(result)
  
  if (result$status_code != 200 & is.null(token$error)) {
    stop("Connection error using formr API: ", token)
  } else if (!is.null(token$error)) {
    stop("Error using formr API: ", token$error_code, " ", 
      token$error, " ", token$description)
  }
  base_url$query = list(access_token = token$access_token)
  .formr_current_session$set(base_url)
  
  invisible(result)
}

.store_formr_current_session <- function() {
  .formr_store_current_session <- NULL
  
  list(get = function() .formr_store_current_session, set = function(value) .formr_store_current_session <<- value)
}
.formr_current_session <- .store_formr_current_session()


#' Get current API session
#' Return or set URL in list form for formr API (if available)
#' @export
#'
formr_api_session = function() {
  .formr_current_session$get()
}

#' Get result from formr
#'
#' After obtaining a token from formr, use this request
#'
#' @param request parameter (see example, API docs)
#' @param token defaults to last used token
#' 
#' @export
#' @examples
#' \dontrun{
#' request <- 
#' 	list(
#' 		"run[name]" = 'widgets',
#' 		"run[sessions]" = 
#' 		  'PJ_nACjFQDEBhx7pMUfZQz3mV-OtetnpEdqT88aiY8eXE4-HegFI7Sri4yifxPXO',
#' 		"surveys[all_widgets]" = "abode, yourstory, mc_god"
#' )
#' formr_api_results(request)
#' }

formr_api_results = function(request = NULL, token = NULL) {
  stopifnot(!is.null(request))
  get_url = formr_api_session()
  if (!is.null(token)) {
    get_url = token
  }
  
  get_url$path = paste0(get_url$path, "get/results")
  result = httr::GET(get_url, query = request)
  res = httr::content(result)
  res
}

