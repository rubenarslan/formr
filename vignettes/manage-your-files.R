## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval = FALSE------------------------------------------------------
# library(formr)
# # Automatically finds your stored keys
# formr_api_authenticate(host = "https://api.rforms.org", account = "dashboard") # or your custom URL and account name!

## ----eval = FALSE-------------------------------------------------------------
# # List all files attached to the study
# files <- formr_api_files("my-study-name")
# 
# # View the first few files
# head(files)

## ----eval = FALSE-------------------------------------------------------------
# # Upload a single logo
# formr_api_upload_file("my-study-name", path = "assets/logo.png")

## ----eval = FALSE-------------------------------------------------------------
# # Upload multiple specific files
# formr_api_upload_file("my-study-name", path = c("assets/img1.jpg", "assets/img2.jpg"))
# 
# # Upload an entire folder of stimuli
# formr_api_upload_file("my-study-name", path = "assets/stimuli/")

## ----eval = FALSE-------------------------------------------------------------
# # Delete a specific file
# formr_api_delete_file("my-study-name", file_name = "old_logo.png")
# 
# # Delete a list of files
# formr_api_delete_file("my-study-name", file_name = c("test1.jpg", "test2.jpg"))

## ----eval = FALSE-------------------------------------------------------------
# # Delete ALL files (prompts for confirmation)
# formr_api_delete_all_files("my-study-name")

