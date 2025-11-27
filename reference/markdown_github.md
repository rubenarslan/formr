# github_markdown for rmarkdown

Custom template with github-flavoured markdown based on the standard
[`rmarkdown::html_document()`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html).
Adds +pipe_tables, +raw_html, +tex_math_single_backslash,
+fenced_code_blocks, +auto_identifiers, +ascii_identifiers,
+backtick_code_blocks, +autolink_bare_uris, +intraword_underscores,
+strikeout, +hard_line_breaks over markdown_strict. A number of pandoc
features are disabled (see
[`markdown_custom_options()`](http://rubenarslan.github.io/formr/reference/markdown_custom_options.md)),
but +yaml_metadata_block is re-enabled, so that it is possible to
specify this output function using YAML.

## Usage

``` r
markdown_github(fragment.only = FALSE, break_on_error = FALSE, ...)
```

## Arguments

- fragment.only:

  whether to get only a html fragment

- break_on_error:

  should an error in the R code execution interrupt the rendering or
  should rendering continue, defaults to FALSE

- ...:

  all other arguments passed to
  [`rmarkdown::html_document()`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)
