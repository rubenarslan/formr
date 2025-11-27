# Replace NA values with something else

Often, you want to substitute missing values with some implicit known
value (e.g. if the question on number of sexual partners was skipped for
sexually inactive people, you know the missing should turn into zero)

## Usage

``` r
if_na(x, missing)
```

## Arguments

- x:

  the variable

- missing:

  What to replace missing values with

## Examples

``` r
number_of_sex_partners <- c(1, 3, 5, 10, NA, 29)
if_na(number_of_sex_partners, 0)
#> [1]  1  3  5 10  0 29
```
