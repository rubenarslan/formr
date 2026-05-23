test_that("formr_store_keys throws error if arguments are missing", {
	# Should fail if neither legacy account_name nor API args are provided
	expect_error(
		formr_store_keys(),
		"Invalid usage"
	)
})

test_that("formr_store_keys (API Mode) stores Access Token correctly", {
	skip_if_not_installed("keyring")
	
	# Setup: Use the 'env' backend to avoid OS popups/interaction during tests
	# This stores keys in a temporary environment variable backend
	old_backend <- getOption("keyring_backend")
	options(keyring_backend = "env")
	on.exit(options(keyring_backend = old_backend))
	
	# Define test data
	test_host <- "https://test-api.formr.org"
	test_token <- "secret_access_token_123"
	service_name <- paste0("formr_", test_host)
	
	# Run the function
	expect_message(
		formr_store_keys(host = test_host, access_token = test_token),
		"\\[SUCCESS\\] Access Token stored"
	)
	
	# Verify: Retrieve the key back from the keyring to ensure it was saved
	stored_val <- keyring::key_get(service = service_name, username = "access_token")
	expect_equal(stored_val, test_token)
	
	# Cleanup (good practice, though env backend is temporary)
	keyring::key_delete(service = service_name, username = "access_token")
})

test_that("formr_store_keys (API Mode) stores OAuth Client Credentials correctly", {
	skip_if_not_installed("keyring")
	
	# Setup: Use 'env' backend
	old_backend <- getOption("keyring_backend")
	options(keyring_backend = "env")
	on.exit(options(keyring_backend = old_backend))
	
	test_host <- "https://oauth.formr.org"
	test_id <- "my_client_id"
	test_secret <- "my_client_secret"
	service_name <- paste0("formr_", test_host)
	
	# Run function
	expect_message(
		formr_store_keys(host = test_host, client_id = test_id, client_secret = test_secret),
		"\\[SUCCESS\\] OAuth Credentials stored"
	)
	
	# Verify both keys were stored
	stored_id <- keyring::key_get(service = service_name, username = "client_id")
	stored_secret <- keyring::key_get(service = service_name, username = "client_secret")
	
	expect_equal(stored_id, test_id)
	expect_equal(stored_secret, test_secret)
})


test_that("formr_store_keys (Classic Mode) stores credentials correctly", {
	skip_if_not_installed("keyring")
	
	# 1. Setup: Use 'env' backend to avoid OS popups and persistence
	old_backend <- getOption("keyring_backend")
	options(keyring_backend = "env")
	on.exit(options(keyring_backend = old_backend))
	
	# 2. Define test data
	test_account <- "test_diary_study"
	test_email   <- "test@example.com"
	test_pass    <- "super_secret_123"
	test_2fa     <- "A1B2C3D4"
	
	# 3. Run function with ALL arguments (to ensure non-interactive path)
	expect_message(
		formr_store_keys(
			account_name = test_account,
			email = test_email,
			password = test_pass,
			secret_2fa = test_2fa
		),
		"\\[SUCCESS\\] Classic credentials stored" # <-- Updated this line
	)
	
	# 4. Verify the keys were actually stored in the keyring
	stored_pass <- keyring::key_get(service = test_account, username = test_email)
	stored_2fa  <- keyring::key_get(service = test_account, username = paste(test_email, "2FA"))
	
	expect_equal(stored_pass, test_pass)
	expect_equal(stored_2fa, test_2fa)
})

test_that("formr_store_keys throws informative error if keyring is missing", {
	# This test requires the 'mockery' package to stub the environment
	skip_if_not_installed("mockery")
	
	# 1. Setup the stub
	# We tell R: "When 'formr_store_keys' calls 'requireNamespace', 
	# make it return FALSE instead of actually checking the package."
	mockery::stub(formr_store_keys, "requireNamespace", FALSE)
	
	# 2. Expect the specific error message
	expect_error(
		formr_store_keys(account_name = "dummy_account"),
		"Package 'keyring' is required"
	)
})