# Estimate the share of AI-assisted responses from a randomised-response question

For a question of the form "answer Yes if at least one of these is true:
you have an identical twin; you are an AI agent; you used a bot, a
script or an AI for some of your answers". Nobody can tell from a single
Yes which statement applies, which makes it safe to answer honestly;
across a sample, the known rate of the harmless statement gives the
share of the others.

## Usage

``` r
dq_rrt(yes, p = 0.008)
```

## Arguments

- yes:

  a logical or 0/1 vector, `TRUE` for "Yes". Missing values are dropped.

- p:

  the probability of the harmless statement. The default is the share of
  people who are identical twins (about four monozygotic twin pairs per
  1,000 births).

## Value

A named numeric vector: `n`, `yes_rate`, the estimated share `ai_share`,
its standard error `se`, and the bounds `lower` and `upper` of a 95%
confidence interval.

## Details

Everyone the sensitive statements apply to (share `pi`) answers Yes
whether or not they are a twin, everyone else only if they are one (rate
`p`), so `P(Yes) = pi + (1 - pi) * p` and `pi = (P(Yes) - p) / (1 - p)`.
The only assumption is that the harmless statement applies to the others
at rate `p`. With a small sample the estimate can be below zero.

## Examples

``` r
answers <- c(rep(TRUE, 12), rep(FALSE, 288))
dq_rrt(answers)
#>            n     yes_rate     ai_share           se        lower        upper 
#> 3.000000e+02 4.000000e-02 3.225806e-02 1.140495e-02 9.904366e-03 5.461176e-02 
```
