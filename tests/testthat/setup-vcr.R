# Tests in test-api_integration.R rely on vcr-recorded cassettes.
# vcr is in Suggests, so skip configuration when it's not installed
# (e.g. CRAN's `_R_CHECK_DEPENDS_ONLY_` strict run). Tests that need
# vcr already gate on skip_if_not_installed("vcr").
if (!requireNamespace("vcr", quietly = TRUE)) {
	return(invisible(NULL))
}

# 1. Set Dummy Defaults for CI
# If these variables are missing (like on CI), set them to the values
# you used when you recorded the cassettes (likely localhost).
if (Sys.getenv("FORMR_HOST") == "") {
	Sys.setenv(FORMR_HOST = "http://api.localhost")
}
if (Sys.getenv("FORMR_CLIENT_ID") == "") {
	Sys.setenv(FORMR_CLIENT_ID = "dummy_client_id")
}
if (Sys.getenv("FORMR_CLIENT_SECRET") == "") {
	Sys.setenv(FORMR_CLIENT_SECRET = "dummy_client_secret")
}

# 2. Configure VCR Unconditionally
# Clean the host to get ONLY the domain for filtering
host_domain <- gsub("^https?://", "", Sys.getenv("FORMR_HOST"))

vcr::vcr_configure(
	dir = "../fixtures/vcr_cassettes",
	filter_sensitive_data = list(
		"formr-client-id-redacted" = Sys.getenv("FORMR_CLIENT_ID"),
		"formr-client-secret-redacted" = Sys.getenv("FORMR_CLIENT_SECRET"),
		"formr-host-redacted" = host_domain
	)
)
