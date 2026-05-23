# Get or Update Run Structure (Run Units)

Export the current run structure as a list (GET) or replace it by
importing a JSON file (PUT).

## Usage

``` r
formr_api_run_structure(run_name, structure_json_path = NULL, file = NULL)
```

## Arguments

- run_name:

  Name of the run.

- structure_json_path:

  Optional path to a JSON file to IMPORT (PUT) structure. If provided,
  the function uploads this file to the server.

- file:

  Optional path to save the DOWNLOADED (GET) structure as a .json file.
  This ensures a perfect 1:1 backup of the server configuration.

## Value

- GET (default): A `formr_run_structure` object (list) for inspection.

- GET (file provided): Invisibly returns the file path.

- PUT: Invisibly returns TRUE on success.
