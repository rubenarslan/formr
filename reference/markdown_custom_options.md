# custom markdown options for rmarkdown's pandoc

custom markdown options for rmarkdown's pandoc

## Usage

``` r
markdown_custom_options(
  add_to_format = c("+autolink_bare_uris", "+ascii_identifiers",
    "+tex_math_single_backslash", "-implicit_figures"),
  fragment.only = FALSE,
  section_divs = TRUE,
  break_on_error = FALSE,
  ...
)
```

## Arguments

- add_to_format:

  add these arguments to the default specification

- fragment.only:

  whether to get only a html fragment

- section_divs:

  whether to disable –section-divs (headings generate section including
  everything up to the next same-or-higher-level heading)

- break_on_error:

  should an error in the R code execution interrupt the rendering or
  should rendering continue, defaults to FALSE

- ...:

  all other arguments passed to
  [`rmarkdown::html_document()`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html)

  Custom rmarkdown input format options based on the standard
  [`rmarkdown::html_document()`](https://pkgs.rstudio.com/rmarkdown/reference/html_document.html),
  but with options that you can specify. Find the format options here in
  the pandoc documentation:
  http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html
  Pandoc's enhanced version of markdown includes syntax for footnotes,
  tables, flexible ordered lists, definition lists, fenced code blocks,
  superscript, subscript, strikeout, title blocks, automatic tables of
  contents, embedded LaTeX math, citations, and markdown inside HTML
  block elements or spoken in options: +escaped_line_breaks,
  +header_attributes, +yaml_metadata_block, +auto_identifiers,
  +implicit_header_references, +blank_before_blockquote,
  +fenced_code_blocks, +fenced_code_attributes, +line_blocks,
  +definition_lists, +startnum, +fancy_lists, +pipe_tables,
  +pandoc_title_block, +intraword_underscores, +strikeout, +superscript,
  +subscript, +tex_math_dollars, +raw_html, +markdown_in_html_blocks,
  +implicit_figures, +footnotes, +inline_notes, +citations. The current
  default rmarkdown additions to Pandoc's enhanced markdown are:
  +autolink_bare_uris, +ascii_identifiers, +tex_math_single_backslash,
  -implicit_figures.
