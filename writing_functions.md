writing_functions
================
Kimia Faroughi
2025-10-23

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
#global ggplot settings
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

## Smart small!

Everyone loves z scores.

``` r
x_vec = rnorm(20, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.01451083  0.33553283 -1.33236517 -1.00518989 -0.05127182 -0.22712853
    ##  [7] -1.20599697  1.27544657 -0.05908098  1.07628745 -1.53256754  0.30446904
    ## [13] -1.22432722  2.18444643 -0.81061752  1.01074764 -0.21002915  0.53806418
    ## [19] -0.11377995  1.03284977

Write a function to compute z scores.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("The input x should be numeric")
  }
  
  if (length(x) < 5) {
    stop("Only compute z scores when the input has 5 or more numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
  
}
```

Let’s try our function…

``` r
z_scores(x = x_vec)
```

    ##  [1]  0.01451083  0.33553283 -1.33236517 -1.00518989 -0.05127182 -0.22712853
    ##  [7] -1.20599697  1.27544657 -0.05908098  1.07628745 -1.53256754  0.30446904
    ## [13] -1.22432722  2.18444643 -0.81061752  1.01074764 -0.21002915  0.53806418
    ## [19] -0.11377995  1.03284977

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1] -0.562174235  0.588136657 -0.478720172  0.143773771 -0.201753445
    ##   [6] -1.505053814  0.664782013  0.473305730 -0.576372198  0.081909276
    ##  [11] -1.019183512 -0.830423563  0.887933013 -2.404265664  0.620936477
    ##  [16] -1.017867233 -0.003505491 -1.062948858 -2.059468881  0.479718454
    ##  [21] -0.715801182 -0.300006989  0.066964894 -0.426657830 -1.374047268
    ##  [26]  0.037101918  0.445603596 -0.900164905  1.236770062  1.912777752
    ##  [31] -1.569544153  1.197657563  0.079233965 -0.257815525  0.564991405
    ##  [36]  1.747161595  0.095450360  0.855529383  1.131510648  1.021098649
    ##  [41]  0.799235838 -0.301726388 -1.064349671  0.805472688 -2.221543264
    ##  [46]  0.934052336 -1.281198957 -0.518374178 -1.550070609 -0.463459044
    ##  [51] -1.385713920  0.312774475 -0.956681715  2.039050181  0.066594109
    ##  [56]  0.047417341 -1.383713333 -0.827541551 -1.092349071  1.469267832
    ##  [61]  2.769164495  0.172034325  0.665593716 -1.250365463 -0.280492744
    ##  [66]  0.243685955  0.243307494  0.990053500 -0.564633704  1.192902934
    ##  [71] -1.880807344  0.012032697 -1.294661117 -0.247246065 -0.430523875
    ##  [76]  0.067322459  0.661589040  0.239928716  1.825094484  0.152233120
    ##  [81] -0.166073054  0.391850580  0.311030333  1.401134140  0.313439012
    ##  [86]  1.344014085  0.949030033 -0.362378591  0.986427272  0.944823062
    ##  [91]  0.803866806 -0.709284688  1.541537176  0.150798344 -0.169204659
    ##  [96] -0.596539487 -0.231053601  0.590575775 -0.275578599  1.718355786
    ## [101] -0.350126302  0.662825344  0.412484590 -1.157845422  0.244923827
    ## [106] -0.750995224  0.587199944 -0.318008878 -0.576270961 -1.514684607
    ## [111] -0.856761965  1.176433601 -0.391170861 -0.059100039 -1.341496526
    ## [116] -0.602540172  2.513586959  0.157718819  0.328030979  0.692336003
    ## [121] -0.156380626  0.468480798 -1.885336993

Let’s break our function…

``` r
z_scores(3)
```

    ## Error in z_scores(3): Only compute z scores when the input has 5 or more numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): The input x should be numeric

## Let’s compute some stuff

Let’s compute and return the mean and sd of a numeric vector.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("The input x should be numeric")
  }
  
  if (length(x) < 5) {
    stop("Only compute mean and sd when the input has 5 or more numbers")
  }
  
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}

mean_and_sd(x = x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.73  3.88

This will import the function for use here.

``` r
source("source/mean_and_sd.R")
```

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.73  3.88

## Make up data…

Let’s *simulate* some data

``` r
sim_df = 
  tibble(
    x = rnorm(n = 300, mean = 5, sd = 2)
  )

sim_df |> 
  summarize(
    mu_hat = mean(x), 
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   5.04      2.06

Write a function to do simulations.

The inputs are

- `n_subj` is number of subjects
- `mu` is the true mean
- `sigma` is the true sd

Function simulates data from a normal and computes sample mean and sd.

(We wrote this in a code chunk last time but now using source from R
script)

``` r
source("source/sim_mean_sd.R")
```

Let’s run this function.

``` r
sim_mean_sd(n_subj = 50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.03      2.00

Import the LoTR data

``` r
fellowship_ring =
  read_excel("LotR_Words.xlsx", range = "B3:D6") |> 
  mutate(movie = "Fellowship of the Ring")

two_towers =
  read_excel("LotR_Words.xlsx", range = "F3:H6") |> 
  mutate(movie = "Two Towers")

return_king =
  read_excel("LotR_Words.xlsx", range = "J3:L6") |> 
  mutate(movie = "Return of the King")

lotr_df =
  bind_rows(fellowship_ring, two_towers, return_king)
```

Turn this into a function

``` r
lotr_import = function(cell_range, movie_title) {
  
  df = 
    read_excel("LotR_Words.xlsx", range = cell_range) |> 
    mutate(movie = movie_title)
  
  df
  
}

fellowship = lotr_import(cell_range = "B3:D6", movie_title = "Fellowship")
two_towers = lotr_import(cell_range = "F3:H6", movie_title = "Two Towers")
return = lotr_import(cell_range = "J3:L6", movie_title = "Return")

bind_rows(fellowship, two_towers, return)
```

    ## # A tibble: 9 × 4
    ##   Race   Female  Male movie     
    ##   <chr>   <dbl> <dbl> <chr>     
    ## 1 Elf      1229   971 Fellowship
    ## 2 Hobbit     14  3644 Fellowship
    ## 3 Man         0  1995 Fellowship
    ## 4 Elf       331   513 Two Towers
    ## 5 Hobbit      0  2463 Two Towers
    ## 6 Man       401  3589 Two Towers
    ## 7 Elf       183   510 Return    
    ## 8 Hobbit      2  2673 Return    
    ## 9 Man       268  2459 Return

Look at one more example.

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

Write an import function

``` r
nsduh_import = function(html, table_num) {
  
  data = 
  html |> 
  html_table() |> 
  nth(table_num) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))

  data
  
}

nsduh_import(nsduh_html, table_num = 1)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    9.98
    ##  2 Alabama 12+   2014-2015    9.6 
    ##  3 Alabama 12-17 2013-2014    9.9 
    ##  4 Alabama 12-17 2014-2015    9.71
    ##  5 Alabama 18-25 2013-2014   27.0 
    ##  6 Alabama 18-25 2014-2015   26.1 
    ##  7 Alabama 26+   2013-2014    7.1 
    ##  8 Alabama 26+   2014-2015    6.81
    ##  9 Alabama 18+   2013-2014    9.99
    ## 10 Alabama 18+   2014-2015    9.59
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html, table_num = 2)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    5.57
    ##  2 Alabama 12+   2014-2015    5.35
    ##  3 Alabama 12-17 2013-2014    4.98
    ##  4 Alabama 12-17 2014-2015    5.16
    ##  5 Alabama 18-25 2013-2014   15.0 
    ##  6 Alabama 18-25 2014-2015   14.3 
    ##  7 Alabama 26+   2013-2014    4.03
    ##  8 Alabama 26+   2014-2015    3.86
    ##  9 Alabama 18+   2013-2014    5.63
    ## 10 Alabama 18+   2014-2015    5.37
    ## # ℹ 500 more rows
