# Perform Action on Session(s)

Controls the flow of one or more sessions.

## Usage

``` r
formr_api_session_action(run_name, session_codes, action, position = NULL)
```

## Arguments

- run_name:

  Name of the run.

- session_codes:

  A single code or vector of session codes.

- action:

  One of: "end_external", "toggle_testing", "move_to_position",
  "execute", "advance".

- position:

  Required only if action is "move_to_position".

## Value

A logical vector indicating success for each session.
