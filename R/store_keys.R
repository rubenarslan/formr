#' Store Credentials in Keyring
#'
#' Securely stores formr credentials in the system keyring.
#' This function supports two modes:
#' 1. **Classic Mode:** Stores email/password (and optional 2FA) for a specific account name.
#' 2. **API Mode:** Stores OAuth credentials or Access Tokens for a specific host.
#'
#' @param account_name (Classic) A shorthand name for the account. If provided, Classic mode is triggered.
#' @param email (Classic) Email address for the account. Will be prompted if omitted.
#' @param password (Classic) Optional. Provide to skip interactive prompt (useful for scripts/tests).
#' @param secret_2fa (Classic) A 2FA secret. Set to NULL to be prompted, or "" if not used.
#' @param host (API) The API URL (e.g., https://formr.org). Defaults to formr.org.
#' @param client_id (API) OAuth Client ID.
#' @param client_secret (API) OAuth Client Secret.
#' @param access_token (API) Direct Personal Access Token (alternative to OAuth).
#' @param account (API) Optional string identifier for multiple accounts on the same host.
#'
#' @export
#' @examples
#' \dontrun{
#' # --- Classic EXAMPLES ---
#' # Prompts for password interactively
#' formr_store_keys("formr_diary_study_account")
#'
#' # --- NEW API EXAMPLES ---
#'
#' # Store OAuth Credentials for a custom host
#' formr_store_keys(host = "http://localhost",
#'                  client_id = "my-id",
#'                  client_secret = "my-secret")
#'                  
#' # Store token for a specific secondary account
#' formr_store_keys(host = "http://localhost",
#'                  client_id = "my-id",
#'                  client_secret = "my-secret",
#'                  account = "project_b")
#' }
formr_store_keys <- function(account_name = NULL,
														 email = NULL,
														 password = NULL,
														 secret_2fa = NULL,
														 host = "https://formr.org",
														 client_id = NULL,
														 client_secret = NULL,
														 access_token = NULL,
														 account = NULL) {
	
	if (!requireNamespace("keyring", quietly = TRUE)) {
		stop("Package 'keyring' is required.")
	}
	
	# --- LOGIC BRANCH 1: Classic MODE ---
	# Triggered if the user provides a positional argument or explicitly sets account_name
	if (!is.null(account_name)) {
		
		if (is.null(email)) {
			email <- readline("Enter your email: ")
		}
		
		# Store main password
		if (!is.null(password)) {
			# Non-interactive mode (for tests/scripts)
			keyring::key_set_with_value(
				service = account_name, 
				username = email, 
				password = password
			)
		} else {
			# Interactive mode
			keyring::key_set(service = account_name, username = email)
		}
		
		# Store 2FA Secret
		if (!is.null(secret_2fa)) {
			keyring::key_set_with_value(
				service = account_name, 
				username = paste(email, "2FA"), 
				password = secret_2fa
			)
		} else {
			# Interactive prompt for 2FA
			keyring::key_set(
				service = account_name, 
				username = paste(email, "2FA"),
				prompt = "2FA secret if applicable"
			)
		}
		
		message("[SUCCESS] Classic credentials stored for account: ", account_name)
		return(invisible(NULL))
	}
	
	# --- LOGIC BRANCH 2: NEW API MODE ---
	# Triggered if account_name is NULL.
	
	# Validation: Ensure the user actually provided new credentials
	if (is.null(access_token) && (is.null(client_id) || is.null(client_secret))) {
		stop("Invalid usage. Please provide either a Classic 'account_name' OR API credentials (access_token OR client_id + client_secret).")
	}
	
	# Construct service name with optional account identifier
	service_name <- paste0("formr_", host)
	if (!is.null(account)) {
		service_name <- paste0(service_name, "_", account)
	}
	
	if (!is.null(access_token)) {
		keyring::key_set_with_value(
			service = service_name,
			username = "access_token",
			password = access_token
		)
		message("[SUCCESS] Access Token stored for ", service_name)
		
	} else if (!is.null(client_id) && !is.null(client_secret)) {
		keyring::key_set_with_value(
			service = service_name,
			username = "client_id",
			password = client_id
		)
		keyring::key_set_with_value(
			service = service_name,
			username = "client_secret",
			password = client_secret
		)
		message("[SUCCESS] OAuth Credentials stored for ", service_name)
	}
}