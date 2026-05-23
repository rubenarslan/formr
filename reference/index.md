# Package index

## Setup & Authentication

Connect to formr using the API or classic authentication

- [`formr_store_keys()`](https://rubenarslan.github.io/formr/reference/formr_store_keys.md)
  : Store Credentials in Keyring
- [`formr_api_authenticate()`](https://rubenarslan.github.io/formr/reference/formr_api_authenticate.md)
  : Authenticate with formr
- [`formr_api_logout()`](https://rubenarslan.github.io/formr/reference/formr_api_logout.md)
  : Revoke Access Token (Logout)
- [`formr_api_session()`](https://rubenarslan.github.io/formr/reference/formr_api_session.md)
  : Get Current API session
- [`formr_api_is_authenticated()`](https://rubenarslan.github.io/formr/reference/formr_api_is_authenticated.md)
  : Check if currently authenticated
- [`formr_api_token_expiry()`](https://rubenarslan.github.io/formr/reference/formr_api_token_expiry.md)
  : Get token expiry information
- [`formr_connect()`](https://rubenarslan.github.io/formr/reference/formr_connect.md)
  : Connect to formr
- [`formr_disconnect()`](https://rubenarslan.github.io/formr/reference/formr_disconnect.md)
  : Disconnect from formr
- [`formr_last_host()`](https://rubenarslan.github.io/formr/reference/formr_last_host.md)
  : Get the last specified host
- [`.formr`](https://rubenarslan.github.io/formr/reference/dot-formr.md)
  : Per-request environment populated by formr.org

## API - Project Management

Tools for managing runs and syncing local files with the server

- [`formr_api_pull_project()`](https://rubenarslan.github.io/formr/reference/formr_api_pull_project.md)
  : Pull Project from Server Scaffolds folder structure if missing, then
  overwrites local files with Server state.
- [`formr_api_push_project()`](https://rubenarslan.github.io/formr/reference/formr_api_push_project.md)
  : Push Project to Server
- [`formr_api_backup_run()`](https://rubenarslan.github.io/formr/reference/formr_api_backup_run.md)
  : Backup a study

## API - Runs & Sessions

Manage runs, create sessions, and perform actions

- [`formr_api_runs()`](https://rubenarslan.github.io/formr/reference/formr_api_runs.md)
  : List all runs
- [`formr_api_create_run()`](https://rubenarslan.github.io/formr/reference/formr_api_create_run.md)
  : Create a new run
- [`formr_api_run_settings()`](https://rubenarslan.github.io/formr/reference/formr_api_run_settings.md)
  : Get or Update Run Settings
- [`formr_api_run_structure()`](https://rubenarslan.github.io/formr/reference/formr_api_run_structure.md)
  : Get or Update Run Structure (Run Units)
- [`formr_api_delete_run()`](https://rubenarslan.github.io/formr/reference/formr_api_delete_run.md)
  : Delete a Run
- [`formr_api_sessions()`](https://rubenarslan.github.io/formr/reference/formr_api_sessions.md)
  : List Sessions in a Run
- [`formr_api_create_session()`](https://rubenarslan.github.io/formr/reference/formr_api_create_session.md)
  : Create Session(s)
- [`formr_api_session_action()`](https://rubenarslan.github.io/formr/reference/formr_api_session_action.md)
  : Perform Action on Session(s)
- [`formr_api_unit_sessions()`](https://rubenarslan.github.io/formr/reference/formr_api_unit_sessions.md)
  : List Per-Unit Sessions in a Run
- [`formr_overview_sankey()`](https://rubenarslan.github.io/formr/reference/formr_overview_sankey.md)
  : Render a participant-flow Sankey for an overview script

## API - Surveys & Files

Upload and manage survey structures and attached files

- [`formr_api_surveys()`](https://rubenarslan.github.io/formr/reference/formr_api_surveys.md)
  : List Surveys
- [`formr_api_survey_structure()`](https://rubenarslan.github.io/formr/reference/formr_api_survey_structure.md)
  : Get Survey Structure (Items)
- [`formr_api_upload_survey()`](https://rubenarslan.github.io/formr/reference/formr_api_upload_survey.md)
  : Upload/Update Survey
- [`formr_api_delete_survey()`](https://rubenarslan.github.io/formr/reference/formr_api_delete_survey.md)
  : Delete a Survey
- [`formr_api_files()`](https://rubenarslan.github.io/formr/reference/formr_api_files.md)
  : List files attached to a run
- [`formr_api_upload_file()`](https://rubenarslan.github.io/formr/reference/formr_api_upload_file.md)
  : Upload File(s) to Run
- [`formr_api_delete_file()`](https://rubenarslan.github.io/formr/reference/formr_api_delete_file.md)
  : Delete file(s) from a run
- [`formr_api_delete_all_files()`](https://rubenarslan.github.io/formr/reference/formr_api_delete_all_files.md)
  : Delete ALL files attached to a run

## API - Results

Fetch, recognize, and process results using the new pipeline

- [`formr_api_results()`](https://rubenarslan.github.io/formr/reference/formr_api_results.md)
  : Get and Process Run Results
- [`formr_api_fetch_results()`](https://rubenarslan.github.io/formr/reference/formr_api_fetch_results.md)
  : Lower-level API Result Fetcher
- [`formr_api_recognise()`](https://rubenarslan.github.io/formr/reference/formr_api_recognise.md)
  : Apply Type Definitions and Labels
- [`formr_api_reverse()`](https://rubenarslan.github.io/formr/reference/formr_api_reverse.md)
  : Reverse Items and Update Labels
- [`formr_api_aggregate()`](https://rubenarslan.github.io/formr/reference/formr_api_aggregate.md)
  : Aggregate Scales
- [`summary(`*`<formr_results>`*`)`](https://rubenarslan.github.io/formr/reference/summary.formr_results.md)
  : Summarize Processing History

## Classic formr Functions

Classic functions for retrieving data

- [`formr_results()`](https://rubenarslan.github.io/formr/reference/formr_results.md)
  : Download processed, aggregated results from formr
- [`formr_raw_results()`](https://rubenarslan.github.io/formr/reference/formr_raw_results.md)
  : Download data from formr
- [`formr_post_process_results()`](https://rubenarslan.github.io/formr/reference/formr_post_process_results.md)
  : Processed, aggregated results
- [`formr_recognise()`](https://rubenarslan.github.io/formr/reference/formr_recognise.md)
  : Recognise data types based on item table
- [`formr_aggregate()`](https://rubenarslan.github.io/formr/reference/formr_aggregate.md)
  : Aggregate data based on item table
- [`formr_reverse()`](https://rubenarslan.github.io/formr/reference/formr_reverse.md)
  : Reverse items based on item table or a fallback_max
- [`formr_items()`](https://rubenarslan.github.io/formr/reference/formr_items.md)
  : Download items from formr
- [`formr_item_displays()`](https://rubenarslan.github.io/formr/reference/formr_item_displays.md)
  : Download detailed result timings and display counts from formr
- [`formr_uploaded_files()`](https://rubenarslan.github.io/formr/reference/formr_uploaded_files.md)
  : Download uploaded files from formr
- [`formr_shuffled()`](https://rubenarslan.github.io/formr/reference/formr_shuffled.md)
  : Download random groups
- [`formr_user_detail()`](https://rubenarslan.github.io/formr/reference/formr_user_detail.md)
  : Download random groups
- [`formr_user_overview()`](https://rubenarslan.github.io/formr/reference/formr_user_overview.md)
  : Download random groups
- [`formr_backup_study()`](https://rubenarslan.github.io/formr/reference/formr_backup_study.md)
  : Backup a study
- [`formr_backup_surveys()`](https://rubenarslan.github.io/formr/reference/formr_backup_surveys.md)
  : Backup surveys
- [`formr_backup_files()`](https://rubenarslan.github.io/formr/reference/formr_backup_files.md)
  : Backup uploaded files from formr
- [`formr_upload_items()`](https://rubenarslan.github.io/formr/reference/formr_upload_items.md)
  : Upload new item table
- [`formr_run_structure()`](https://rubenarslan.github.io/formr/reference/formr_run_structure.md)
  : Download run structure from formr
- [`formr_simulate_from_items()`](https://rubenarslan.github.io/formr/reference/formr_simulate_from_items.md)
  : Simulate data based on item table

## Feedback & Plotting

Generate feedback plots and text for participants

- [`qplot_on_normal()`](https://rubenarslan.github.io/formr/reference/qplot_on_normal.md)
  : Plot a normed value on the standard normal
- [`qplot_on_bar()`](https://rubenarslan.github.io/formr/reference/qplot_on_bar.md)
  : Plot normed values as a barchart
- [`qplot_on_polar()`](https://rubenarslan.github.io/formr/reference/qplot_on_polar.md)
  : Time-polar plot
- [`feedback_chunk()`](https://rubenarslan.github.io/formr/reference/feedback_chunk.md)
  : Text feedback based on groups
- [`email_image()`](https://rubenarslan.github.io/formr/reference/email_image.md)
  : generates valid email cids

## RMarkdown & Rendering

Helpers for rendering RMarkdown within formr

- [`formr_render()`](https://rubenarslan.github.io/formr/reference/formr_render.md)
  : render text for formr
- [`formr_render_commonmark()`](https://rubenarslan.github.io/formr/reference/formr_render_commonmark.md)
  : render inline text for formr
- [`formr_inline_render()`](https://rubenarslan.github.io/formr/reference/formr_inline_render.md)
  : render inline text for formr
- [`formr_knit()`](https://rubenarslan.github.io/formr/reference/formr_knit.md)
  : knit rmarkdown to markdown for formr
- [`asis_knit_child()`](https://rubenarslan.github.io/formr/reference/asis_knit_child.md)
  : knit_child as is
- [`knit_prefixed()`](https://rubenarslan.github.io/formr/reference/knit_prefixed.md)
  : knit prefixed
- [`markdown_custom_options()`](https://rubenarslan.github.io/formr/reference/markdown_custom_options.md)
  : custom markdown options for rmarkdown's pandoc
- [`markdown_github()`](https://rubenarslan.github.io/formr/reference/markdown_github.md)
  : github_markdown for rmarkdown
- [`markdown_hard_line_breaks()`](https://rubenarslan.github.io/formr/reference/markdown_hard_line_breaks.md)
  : hard line breaks
- [`render_text()`](https://rubenarslan.github.io/formr/reference/render_text.md)
  : render text
- [`word_document()`](https://rubenarslan.github.io/formr/reference/word_document.md)
  : word_document from rmarkdown, but has an added option not to break
  on error
- [`paste.knit_asis()`](https://rubenarslan.github.io/formr/reference/paste.knit_asis.md)
  : paste.knit_asis
- [`print(`*`<knit_asis>`*`)`](https://rubenarslan.github.io/formr/reference/print.knit_asis.md)
  : Print new lines in knit_asis outputs

## Helpers & Shorthands

Utility functions for writing survey logic

- [`first()`](https://rubenarslan.github.io/formr/reference/first.md) :
  Gives the first non-missing element

- [`last()`](https://rubenarslan.github.io/formr/reference/last.md) :
  Gives the last non-missing element

- [`current()`](https://rubenarslan.github.io/formr/reference/current.md)
  : Gives the last element, doesn't omit missings

- [`finished()`](https://rubenarslan.github.io/formr/reference/finished.md)
  : How many surveys were finished?

- [`expired()`](https://rubenarslan.github.io/formr/reference/expired.md)
  : How many surveys were expired?

- [`if_na()`](https://rubenarslan.github.io/formr/reference/if_na.md) :
  Replace NA values with something else

- [`if_na_null()`](https://rubenarslan.github.io/formr/reference/if_na_null.md)
  : This function makes sure you know what to expect when evaluating
  uncertain results in an if-clause. In most cases, you should not use
  this function, because it can lump a lot of very different cases
  together, but it may have some use for fool-proofing certain
  if-clauses on formr.org, where a field in a survey may either not
  exist, be missing or have a value to check.

- [`ifelsena()`](https://rubenarslan.github.io/formr/reference/ifelsena.md)
  :

  Like [`ifelse()`](https://rdrr.io/r/base/ifelse.html), but allows you
  to assign a third value to missings.

- [`time_passed()`](https://rubenarslan.github.io/formr/reference/time_passed.md)
  : checks how much time has passed relative to the user's last action

- [`next_day()`](https://rubenarslan.github.io/formr/reference/next_day.md)
  : checks whether a new day has broken (date has increased by at least
  one day)

- [`in_time_window()`](https://rubenarslan.github.io/formr/reference/in_time_window.md)
  : checks whether the current time is in a certain time window

- [`` `%contains%` ``](https://rubenarslan.github.io/formr/reference/grapes-contains-grapes.md)
  : check whether a character string contains another

- [`` `%contains_word%` ``](https://rubenarslan.github.io/formr/reference/grapes-contains_word-grapes.md)
  : check whether a character string contains another as a word

- [`` `%begins_with%` ``](https://rubenarslan.github.io/formr/reference/grapes-begins_with-grapes.md)
  : check whether a character string begins with a string

- [`` `%ends_with%` ``](https://rubenarslan.github.io/formr/reference/grapes-ends_with-grapes.md)
  : check whether a character string ends with a string

- [`random_date_in_range()`](https://rubenarslan.github.io/formr/reference/random_date_in_range.md)
  : Random date in range

## Data Wrangling

Internal utilities for item and scale manipulation

- [`aggregate_and_document_scale()`](https://rubenarslan.github.io/formr/reference/aggregate_and_document_scale.md)
  : Aggregate variables and remember which variables this were
- [`choice_labels_for_values()`](https://rubenarslan.github.io/formr/reference/choice_labels_for_values.md)
  : switch choice values with labels
- [`reverse_labelled_values()`](https://rubenarslan.github.io/formr/reference/reverse_labelled_values.md)
  : Reverse labelled values
- [`rescue_attributes()`](https://rubenarslan.github.io/formr/reference/rescue_attributes.md)
  : Rescue lost attributes
- [`item()`](https://rubenarslan.github.io/formr/reference/item.md) :
  get item from survey attribute
- [`items()`](https://rubenarslan.github.io/formr/reference/items.md) :
  get item list from survey attributes
- [`as.data.frame(`*`<formr_item_list>`*`)`](https://rubenarslan.github.io/formr/reference/as.data.frame.formr_item_list.md)
  : Transform formr_item_list into a data.frame for ease of use
- [`as.data.frame(`*`<formr_api_run_structure>`*`)`](https://rubenarslan.github.io/formr/reference/as.data.frame.formr_api_run_structure.md)
  : Convert formr run structure to data.frame
- [`print(`*`<formr_api_run_structure>`*`)`](https://rubenarslan.github.io/formr/reference/print.formr_api_run_structure.md)
  : Print method for formr run structure
- [`get_opencpu_rds()`](https://rubenarslan.github.io/formr/reference/get_opencpu_rds.md)
  : pass in the url to the RDS representation of a openCPU session
  object, get the object

## Text Messaging

Integrations with SMS providers

- [`text_message_twilio()`](https://rubenarslan.github.io/formr/reference/text_message_twilio.md)
  : Send text message via Twilio
- [`text_message_clickatell()`](https://rubenarslan.github.io/formr/reference/text_message_clickatell.md)
  : Send text message via Clickatell
- [`text_message_massenversand()`](https://rubenarslan.github.io/formr/reference/text_message_massenversand.md)
  : Send text message via Massenversand.de
