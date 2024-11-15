bootstrap
================
Yuying Lu
2024-11-07

``` r
library(tidyverse)
library(p8105.datasets)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

do some bootstrapping!!

make up some data

``` r
n_samp = 250

sim_df_constant = 
  tibble(
    x = rnorm(n_samp, 1, 1),
    error = rnorm(n_samp, 0, 1),
    y = 2 + 3 * x + error
  )

sim_df_nonconstant = 
  sim_df_constant |> 
  mutate(
    error = error * .75 * x,
    y = 2 + 3 * x + error
  )
```

Let’s look at these.

``` r
sim_df_nonconstant |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() + 
  stat_smooth(method = "lm")
```

<img src="bootstrap_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

``` r
sim_df_constant |> 
 lm( y ~ x, data = _) |> 
  broom::tidy() |> 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    1.915 |     0.094 |    20.344 |       0 |
| x           |    3.119 |     0.065 |    47.921 |       0 |

``` r
sim_df_nonconstant |> 
 lm( y ~ x, data = _) |> 
  broom::tidy() |> 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    2.033 |     0.106 |    19.224 |       0 |
| x           |    3.078 |     0.073 |    42.096 |       0 |

Never trust p-value is the bottom line here

## Draw a bootstrap sample

``` r
boot_sample = function(df){
  boot_df = 
    sample_frac(df, replace = TRUE)
  return(boot_df)
}
```

Let’s try running this!

``` r
sim_df_nonconstant |> 
  boot_sample()|> 
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = .5) + 
  stat_smooth(method = "lm")
```

<img src="bootstrap_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

can we do this as part of an analysis?

``` r
sim_df_nonconstant |> 
  boot_sample() |> 
  lm(y ~ x, data = _) |> 
  broom::tidy() |> 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    2.008 |     0.096 |    20.917 |       0 |
| x           |    3.093 |     0.068 |    45.187 |       0 |

## bootstrap A LOT

``` r
boot_straps = 
  tibble(
    strap_number = 1:10
  ) |> 
  mutate(
    strap_sample = map(strap_number, \(i) boot_sample(df = sim_df_nonconstant)),
    models = map(strap_sample, \(df) lm(y ~ x, data = df)),
    results = map(models, broom::tidy)
  )

boot_straps |> unnest(strap_sample)
```

    ## # A tibble: 2,500 × 6
    ##    strap_number      x   error     y models results         
    ##           <int>  <dbl>   <dbl> <dbl> <list> <list>          
    ##  1            1  1.41   1.02   7.25  <lm>   <tibble [2 × 5]>
    ##  2            1  1.76  -0.642  6.65  <lm>   <tibble [2 × 5]>
    ##  3            1  0.910  0.169  4.90  <lm>   <tibble [2 × 5]>
    ##  4            1  2.34   0.313  9.34  <lm>   <tibble [2 × 5]>
    ##  5            1  2.49   0.0279 9.50  <lm>   <tibble [2 × 5]>
    ##  6            1  0.309 -0.0513 2.87  <lm>   <tibble [2 × 5]>
    ##  7            1  1.41   1.02   7.25  <lm>   <tibble [2 × 5]>
    ##  8            1  0.699 -0.348  3.75  <lm>   <tibble [2 × 5]>
    ##  9            1  1.19  -0.607  4.97  <lm>   <tibble [2 × 5]>
    ## 10            1 -0.614  0.574  0.731 <lm>   <tibble [2 × 5]>
    ## # ℹ 2,490 more rows

``` r
boot_strap_results = 
  boot_straps |> 
  select(strap_number, results) |> 
  unnest(results) |> 
  group_by(term) |> 
  summarise(boot_se = sd(estimate)) |> 
  knitr::kable(digits = 3)

boot_strap_results
```

| term        | boot_se |
|:------------|--------:|
| (Intercept) |   0.072 |
| x           |   0.131 |

compare ti with our initial result:

``` r
sim_df_nonconstant |> 
 lm( y ~ x, data = _) |> 
  broom::tidy() |> 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    2.033 |     0.106 |    19.224 |       0 |
| x           |    3.078 |     0.073 |    42.096 |       0 |

``` r
boot_straps = 
  tibble(
    strap_number = 1:10
  ) |> 
  mutate(
    strap_sample = map(strap_number, \(i) boot_sample(df = sim_df_constant)),
    models = map(strap_sample, \(df) lm(y ~ x, data = df)),
    results = map(models, broom::tidy)
  )

boot_strap_results = 
  boot_straps |> 
  select(strap_number, results) |> 
  unnest(results) |> 
  group_by(term) |> 
  summarise(boot_se = sd(estimate)) |> 
  knitr::kable(digits = 3)

sim_df_constant |> 
 lm( y ~ x, data = _) |> 
  broom::tidy() |> 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |    1.915 |     0.094 |    20.344 |       0 |
| x           |    3.119 |     0.065 |    47.921 |       0 |

## do this all using modelr

``` r
boot_straps = 
  sim_df_nonconstant |> 
  modelr::bootstrap(1000) |> 
  mutate(
    strap = map(strap, as.tibble),
    models = map(strap, \(df) lm(y ~ x, data = df)),
    results = map(models, broom::tidy)
  ) |> 
  select(.id, results) |> 
  unnest(results)
```

## what do you want to repeat

``` r
boot_straps |> 
  group_by(term) |> 
  summarise(
    boot_est = mean(estimate),
    boot_se = sd(estimate),
    boot_ci_ll = quantile(estimate, .025),
    boot_ci_ul = quantile(estimate, .975),
  )
```

    ## # A tibble: 2 × 5
    ##   term        boot_est boot_se boot_ci_ll boot_ci_ul
    ##   <chr>          <dbl>   <dbl>      <dbl>      <dbl>
    ## 1 (Intercept)     2.03  0.0648       1.89       2.15
    ## 2 x               3.08  0.0947       2.90       3.27

## Air BNB

``` r
data("nyc_aribnb")

manhattan_df = 
  nyc_airbnb |>
  mutate(stars = review_scores_location / 2) |> 
  rename(borough = neighbourhood_group, 
         neighbourhood = neighbourhood) |> 
  filter(borough == "Manhattan")

manhattan_df |> 
  ggplot(aes(x = stars, price)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)
```

<img src="bootstrap_files/figure-gfm/unnamed-chunk-15-1.png" width="90%" />

``` r
manhattan_df |> 
  lm(price ~ stars + room_type, data = _) |> 
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)               95.7     22.2       4.31 1.62e-  5
    ## 2 stars                     27.1      4.59      5.91 3.45e-  9
    ## 3 room_typePrivate room   -124.       3.46    -35.8  9.40e-270
    ## 4 room_typeShared room    -154.      10.1     -15.3  2.47e- 52

bootstrap for better inferece

``` r
manhattan_df |> 
  modelr::bootstrap(1000) |> 
  mutate(
    strap = map(strap, as.tibble),
    models = map(strap, \(df) lm(price ~ stars + room_type, data = df)),
    results = map(models, broom::tidy)
  ) |> 
  select(.id, results) |> 
  unnest(results) |> 
  filter(term == "stars") |> 
  ggplot(aes(estimate)) +
  geom_density()
```

<img src="bootstrap_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" />

``` r
boot_straps |> 
  group_by(term) |> 
  summarise(
    boot_est = mean(estimate),
    boot_se = sd(estimate),
    boot_ci_ll = quantile(estimate, .025),
    boot_ci_ul = quantile(estimate, .975),
  )
```

    ## # A tibble: 2 × 5
    ##   term        boot_est boot_se boot_ci_ll boot_ci_ul
    ##   <chr>          <dbl>   <dbl>      <dbl>      <dbl>
    ## 1 (Intercept)     2.03  0.0648       1.89       2.15
    ## 2 x               3.08  0.0947       2.90       3.27
