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
#' formr_api_access_token(client_id = "your_id", client_secret = "your_secret" )
#' }

formr_api_access_token = function(client_id, client_secret, host = "https://formr.org/") {
	url = httr::parse_url(host)

	token_url =	formr_api_session(url)
	token_url$path = paste0(token_url$path, "api/oauth/token")
	
	result = httr::POST(url = token_url, 
								config = httr::authenticate(user = client_id, password = client_secret),
								body = list(grant_type = "client_credentials")
	)
	
	token = httr::content(result)
	
	if(result$status_code != 200 & is.null(token$error)) {
		stop("Connection error using formr API: ", token)
	}	else if(!is.null(token$error)) {
		stop("Error using formr API: ",token$error_code," ", token$error, " ",token$description)
	}
	base_url = formr_api_session()
	base_url$query = list(access_token = token$access_token)
	formr_api_session(base_url)
	
	invisible(result)
}

.formr_current_session <- NULL

#' Get current API session
#' @param set leave out to return current value
#' Return or set URL in list form for formr API (if available)
#'
formr_api_session = function(set = NULL) {
	if(is.null(set)) {
		return(.formr_current_session)
	} else {
		.formr_current_session <<- set
	}
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
#' formr_api_results(list(
#'   run = list(
#'      name = "rotate_me",  # for which run do you want results
#'      session = "some_session_code" # and for which user
#'    )
#'  ))
#' }
formr_api_results = function(request = NULL, token = NULL) {
	stopifnot(!is.null(request))
	get_url = formr_api_session()
	if(!is.null(token)) get_url = token
	
	get_url$path = paste0(get_url$path, "api/get/results")
	get_url$query$request = jsonlite::toJSON(
		request
		,auto_unbox = T)
	result = httr::GET(get_url)
	res = httr::content(result)
	res
}
