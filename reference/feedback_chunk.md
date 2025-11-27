# Text feedback based on groups

If you pass in a z-standardised value (x - Mean)/SD, and a vector of
feedback text chunks, that has either three or five elements, the text
chunks will be used in this order \[very low\], low, average, high,
\[very high\] corresponding to these intervals \[low, -2\], \[-2, -1\],
\[-1, 1\], \[1, 2\], \[2, high\]

## Usage

``` r
feedback_chunk(normed_value, chunks)
```

## Arguments

- normed_value:

  a z-standardised value

- chunks:

  a three or five element long character vector containing the text
  chunks for feedback

## Examples

``` r
feedback_chunk(normed_value = 0.7, chunks = c("You are rather introverted.",
"You're approximately as extraverted as most people.","You are rather extraverted."))
#> [1] "You're approximately as extraverted as most people."
```
