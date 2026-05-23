#' Internal environment to store session state
#' @noRd
.formr_state <- new.env(parent = emptyenv())

#' Get Current API session
#'
#' Returns the current session object or NULL if not authenticated.
#'
#' @return A list, or NULL if not authenticated:
#'   - `base_url`: parsed URL (httr style).
#'   - `token`: the bearer access token.
#'   - `scope`: space-delimited string of scopes granted to this
#'     token. `NA_character_` when the auth path couldn't introspect
#'     (direct access-token authentication, or older server). `""`
#'     means the credential was issued with no scopes — every API call
#'     will 403 until the user picks scopes at admin/account#api.
#'   - `expires_at`: POSIXct of token expiry (or NULL).
#' @export
formr_api_session <- function() {
	if (exists("session", envir = .formr_state)) {
		session <- get("session", envir = .formr_state)
		if (.validate_session(session, silent = TRUE)) {
			return(session)
		} else {
			return(NULL)
		}
	}
	return(NULL)
}

#' Validate session object
#'
#' Internal function to validate session structure and expiry.
#'
#' @param session The session object to validate.
#' @param silent If TRUE, returns FALSE on failure. If FALSE, throws error.
#' @return TRUE if valid, FALSE or throws error if invalid.
#' @noRd
.validate_session <- function(session, silent = FALSE) {
	if (!is.list(session)) {
		if (silent) return(FALSE)
		stop("Session corrupted: not a list")
	}

	if (is.null(session$token)) {
		if (silent) return(FALSE)
		stop("Session corrupted: missing token")
	}

	if (!is.character(session$token) || nchar(session$token) == 0) {
		if (silent) return(FALSE)
		stop("Session corrupted: invalid token")
	}

	if (is.null(session$base_url)) {
		if (silent) return(FALSE)
		stop("Session corrupted: missing base_url")
	}

	if (!is.list(session$base_url) || is.null(session$base_url$hostname)) {
		if (silent) return(FALSE)
		stop("Session corrupted: invalid base_url")
	}

	if (!is.null(session$expires_at)) {
		if (!inherits(session$expires_at, "POSIXct")) {
			if (silent) return(FALSE)
			stop("Session corrupted: invalid expires_at")
		}
		if (session$expires_at <= Sys.time()) {
			if (silent) return(FALSE)
			stop("Session expired. Please run formr_api_authenticate() again.")
		}
	}

	TRUE
}

#' Authenticate with formr
#'
#' Connects to the API. If no credentials are provided, the auto-pickup
#' chain is: the package's hidden `.formr` env (set automatically when
#' the code runs inside an OpenCPU session on rforms.org), then the
#' calling-frame chain (for legacy injectors that wrote bare locals into
#' the wrapper scope), then the keyring.
#'
#' @param host API Base URL. Defaults to `.formr$host` when running on
#'   rforms.org, otherwise `"https://rforms.org"`.
#' @param client_id OAuth Client ID.
#' @param client_secret OAuth Client Secret.
#' @param access_token Direct Access Token.
#' @param account Optional string identifier for multiple accounts on the same host.
#' @export
formr_api_authenticate <- function(host = "https://rforms.org",
																	 client_id = NULL,
																	 client_secret = NULL,
																	 access_token = NULL,
																	 account = NULL) {

	# 0a. Hidden `.formr` env — preferred, set by rforms.org without
	# polluting any visible scope.
	if ((missing(access_token) || is.null(access_token)) && !is.null(.formr$access_token)) {
		access_token <- .formr$access_token
	}
	if ((missing(host) || is.null(host)) && !is.null(.formr$host)) {
		host <- .formr$host
	}

	# 0b. Calling-frame chain (legacy OpenCPU injection that wrote
	# bare `access_token` / `host` locals into the wrapper scope).
	if ((missing(access_token) || is.null(access_token)) && exists("access_token", envir = parent.frame(), inherits = TRUE)) {
		access_token <- get("access_token", envir = parent.frame(), inherits = TRUE)
	}
	if ((missing(host) || is.null(host)) && exists("host", envir = parent.frame(), inherits = TRUE)) {
		host <- get("host", envir = parent.frame(), inherits = TRUE)
	}
	
	# 1. Try to load from Keyring if missing
	if (is.null(client_id) && is.null(access_token) &&
			requireNamespace("keyring", quietly = TRUE)) {
		
		# Construct service name with optional account identifier
		service_name <- paste0("formr_", host)
		if (!is.null(account)) {
			service_name <- paste0(service_name, "_", account)
		}
		
		try({
			keys <- keyring::key_list(service = service_name)
			if ("access_token" %in% keys$username) {
				access_token <- keyring::key_get(
					service = service_name, username = "access_token")
			} else if (all(c("client_id", "client_secret") %in% keys$username)) {
				client_id <- keyring::key_get(
					service = service_name, username = "client_id")
				client_secret <- keyring::key_get(
					service = service_name, username = "client_secret")
			}
		}, silent = TRUE)
	}
	
	# 2. Authenticate
	if (!is.null(access_token)) {
		session_data <- list(
			base_url = httr::parse_url(host),
			token = access_token,
			# Direct-token path can't introspect the token's grants
			# without an extra round-trip — leave NA so callers can
			# distinguish "unknown" from "no scopes."
			scope = NA_character_,
			expires_at = NULL
		)

		assign("auth_params", list(host = host, account = account), envir = .formr_state)
		assign("session", session_data, envir = .formr_state)

		message("[SUCCESS] Authenticated via Access Token.")

	} else if (!is.null(client_id) && !is.null(client_secret)) {
		token_url <- httr::parse_url(host)
		token_url$path <- paste0(token_url$path, "/oauth/access_token")
		token_url$path <- gsub("//", "/", token_url$path)

		res <- httr::POST(
			token_url,
			httr::authenticate(client_id, client_secret, type = "basic"),
			body = list(grant_type = "client_credentials"),
			encode = "form"
		)

		if (httr::status_code(res) >= 400)
			stop("OAuth Error: ", httr::content(res, "text"))

		token_content <- httr::content(res)

		if (is.null(token_content$access_token))
			stop("OAuth Error: No access_token in response")

		token <- token_content$access_token

		expires_at <- NULL
		if (!is.null(token_content$expires_in)) {
			expires_at <- Sys.time() + token_content$expires_in
		} else {
			expires_at <- Sys.time() + 3600
		}

		# Capture the granted scopes from the OAuth response so callers
		# can introspect via formr_api_session()$scope. The server's
		# bshaffer flow stamps the client's stored scope string onto
		# the token; what comes back is exactly what was granted. NA
		# (not "") signals "older formr server that didn't return a
		# scope field" so we don't lie about granting nothing.
		granted_scope <- if (is.null(token_content$scope)) NA_character_
		else as.character(token_content$scope)

		session_data <- list(
			base_url = httr::parse_url(host),
			token = token,
			scope = granted_scope,
			expires_at = expires_at
		)

		assign("auth_params", list(host = host, account = account), envir = .formr_state)
		assign("session", session_data, envir = .formr_state)

		if (!is.na(granted_scope) && nzchar(granted_scope)) {
			message("[SUCCESS] Authenticated via OAuth. Granted scopes: ", granted_scope)
		} else if (!is.na(granted_scope) && !nzchar(granted_scope)) {
			# Token issued but with no scopes — every API call will 403.
			# Surface this loudly so users hit the fix path (pick scopes
			# at admin/account#api) instead of debugging blind 403s.
			warning("OAuth token has NO scopes. The credential at admin/account#api needs scopes selected. Every API call will return 403 until that's fixed.")
		} else {
			message("[SUCCESS] Authenticated via OAuth.")
		}
		
	} else {
		stop("No credentials found. Use formr_store_keys() or provide arguments.")
	}
}

#' Revoke Access Token (Logout)
#'
#' Invalidates the current access token on the server and 
#' clears the local session state.
#'
#' @export
formr_api_logout <- function() {
	# 1. Get current session
	session <- formr_api_session()
	
	if (is.null(session)) {
		message("No active session found.")
		return(invisible(FALSE))
	}
	
	# 2. Construct the URL (Manually, as this is an OAuth endpoint, not V1)
	# Matches PHP: public function oauthAction... elseif ($action === 'delete_token')
	url <- session$base_url
	url$path <- paste0(url$path, "/oauth/delete_token")
	url$path <- gsub("//", "/", url$path)
	
	# 3. Send Request
	# Matches PHP: $this->post->access_token inside delete_token()
	tryCatch({
		res <- httr::POST(
			url,
			body = list(access_token = session$token),
			encode = "form"
		)
		
		# Check for success (200 OK)
		if (httr::status_code(res) == 200) {
			message("[SUCCESS] Token revoked on server.")
		} else {
			warning("Server could not revoke token (it may already be expired): ", 
							httr::content(res, "text"))
		}
		
	}, error = function(e) {
		warning("Network error during logout: ", e$message)
	})
	
	# 4. Clear Local Session (Always do this, even if server request fails)
	if (exists("session", envir = .formr_state)) {
		rm("session", envir = .formr_state)
	}
	if (exists("auth_params", envir = .formr_state)) {
		rm("auth_params", envir = .formr_state)
	}
	
	message("[SUCCESS] Local session cleared.")
	return(invisible(TRUE))
}

#' Check if currently authenticated
#'
#' Checks if there is a valid, non-expired session. Does NOT verify
#' token validity with the server (use formr_api_session() for that).
#'
#' @return TRUE if authenticated and token not expired, FALSE otherwise.
#' @export
formr_api_is_authenticated <- function() {
	session <- formr_api_session()
	if (is.null(session)) return(FALSE)

	if (!is.null(session$expires_at)) {
		if (session$expires_at <= Sys.time()) {
			return(FALSE)
		}
	}

	TRUE
}

#' Get token expiry information
#'
#' Returns information about when the current token expires.
#'
#' @return A list with:
#'   - `expires_at`: POSIXct of expiry time (or NULL if unknown)
#'   - `seconds_left`: Seconds until expiry (or NA if unknown)
#'   - `is_expired`: TRUE if token has expired
#' @export
formr_api_token_expiry <- function() {
	session <- formr_api_session()

	if (is.null(session)) {
		return(list(
			expires_at = NULL,
			seconds_left = NA,
			is_expired = TRUE
		))
	}

	if (is.null(session$expires_at)) {
		return(list(
			expires_at = NULL,
			seconds_left = NA,
			is_expired = FALSE
		))
	}

	seconds_left <- as.numeric(difftime(session$expires_at, Sys.time(), units = "secs"))

	list(
		expires_at = session$expires_at,
		seconds_left = seconds_left,
		is_expired = seconds_left <= 0
	)
}

#' Internal: API Request Handler
#' @noRd
formr_api_request <- function(endpoint,
															method = "GET",
															body = NULL,
															query = NULL,
															api_version = "v1",
															encode = NULL,
															retry = TRUE) {
	
	session <- formr_api_session()
	
	if (is.null(session) && exists("auth_params", envir = .formr_state) && retry) {
		auth_params <- get("auth_params", envir = .formr_state)
		try({
			suppressMessages(formr_api_authenticate(host = auth_params$host, account = auth_params$account))
			session <- formr_api_session()
		}, silent = TRUE)
	}
	
	if (is.null(session))
		stop("Not authenticated. Run formr_api_authenticate().")
	
	url <- session$base_url
	url$path <- paste0(url$path, "/", api_version, "/", endpoint)
	url$path <- gsub("//", "/", url$path)
	
	auth <- httr::add_headers(Authorization = paste("Bearer", session$token))
	
	if (is.null(encode)) {
		encode <- "json"
		if (any(sapply(body, inherits, "form_file")))
			encode <- "multipart"
	}
	
	if (method == "GET")
		req <- httr::GET(url, query = query, auth)
	else if (method == "POST")
		req <- httr::POST(url, body = body, encode = encode, auth)
	else if (method == "PUT")
		req <- httr::PUT(url, body = body, encode = encode, auth)
	else if (method == "PATCH")
		req <- httr::PATCH(url, body = body, encode = encode, auth)
	else if (method == "DELETE")
		req <- httr::DELETE(url, auth)
	
	if (httr::http_type(req) != "application/json") {
		stop(
			sprintf(
				"API Error: Expected JSON, got %s.\nPreview: %s",
				httr::http_type(req),
				substr(httr::content(req, "text"), 1, 200)
			)
		)
	}

	status <- httr::status_code(req)

	if (status == 401) {
		# Clear the bad session
		if (exists("session", envir = .formr_state)) {
			rm("session", envir = .formr_state)
		}
		
		# If we haven't retried yet, and we have the credentials to try again
		if (retry && exists("auth_params", envir = .formr_state)) {
			auth_params <- get("auth_params", envir = .formr_state)
			try({
				suppressMessages(formr_api_authenticate(host = auth_params$host, account = auth_params$account))
			}, silent = TRUE)
			
			# Recursively call the request again, but disable retry so it fails properly if it 401s again
			return(formr_api_request(endpoint = endpoint, method = method, body = body, 
															 query = query, api_version = api_version, encode = encode, 
															 retry = FALSE))
		} else {
			stop("Authentication failed (401). Your session may have expired. Please run formr_api_authenticate() to reconnect.")
		}
	}

	if (status >= 400) {
		body_text <- httr::content(req, "text")
		# Add a hint when the failure is one of the scoping-aware 403s
		# emitted by the v1 API. Three flavours, all surface from the
		# same admin/account#api page so the fix path is identical.
		hint <- ""
		if (status == 403) {
			if (grepl("Insufficient permissions:", body_text, fixed = TRUE)) {
				hint <- paste0(
					"\nHint: this credential is missing the OAuth scope this endpoint needs. ",
					"Open admin/account#api on your formr instance, tick the matching read/write scope, ",
					"and rotate. Granted scopes were: ",
					.formatted_session_scope()
				)
			} else if (grepl("not authorized for run", body_text)) {
				hint <- paste0(
					"\nHint: this credential is restricted to a subset of your runs and the run you asked for is outside that subset. ",
					"Open admin/account#api on your formr instance, adjust the run allowlist, and rotate."
				)
			} else if (grepl("not authorized for survey", body_text)) {
				hint <- paste0(
					"\nHint: this credential's run allowlist doesn't include any run that uses this survey. ",
					"Either add the survey to one of the allowed runs, or widen the allowlist at admin/account#api."
				)
			} else if (grepl("Cannot create surveys with a run-restricted API client", body_text)) {
				hint <- paste0(
					"\nHint: run-restricted credentials cannot create new surveys (the new survey would be unreachable through the allowlist until you linked it into a run). ",
					"Create the survey via the admin UI first, then update it via the API."
				)
			}
		}
		stop(sprintf("API Error (%s): %s%s", status, body_text, hint))
	}

	httr::content(req, "parsed")
}

#' Format the current session's granted scopes for an error hint.
#' Returns the scope string, "(unknown)" when the auth flow couldn't
#' capture it (direct access_token path), or "(none)" when the server
#' explicitly granted an empty scope set.
#' @noRd
.formatted_session_scope <- function() {
	session <- formr_api_session()
	if (is.null(session) || is.null(session$scope) || is.na(session$scope)) return("(unknown)")
	if (!nzchar(session$scope)) return("(none)")
	session$scope
}