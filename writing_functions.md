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

    ##  [1]  1.236149678  0.234335982  1.010248422  1.626232957 -0.571105312
    ##  [6] -1.821258669 -0.726946258 -0.034440735  0.425245363  1.224215954
    ## [11]  1.032721338 -2.075142906 -0.410401743 -0.001336214 -0.979336719
    ## [16] -0.616726959  0.870081010  0.200990068 -0.195529797 -0.427995458

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

    ##  [1]  1.236149678  0.234335982  1.010248422  1.626232957 -0.571105312
    ##  [6] -1.821258669 -0.726946258 -0.034440735  0.425245363  1.224215954
    ## [11]  1.032721338 -2.075142906 -0.410401743 -0.001336214 -0.979336719
    ## [16] -0.616726959  0.870081010  0.200990068 -0.195529797 -0.427995458

``` r
num_vec = rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1]  0.024771266 -0.792384145 -0.504790442  2.110180206 -0.478153576
    ##   [6]  1.601802528 -0.302538820  1.215667288 -0.731372393 -0.523617855
    ##  [11] -0.055519415 -0.710183554  1.773325218 -0.023408387  0.172028218
    ##  [16] -0.484199521 -1.343165716  0.797617530  0.795959326  0.783891277
    ##  [21] -0.197759520 -0.308353220 -0.376738176 -0.743002992  1.645748481
    ##  [26] -2.719172376  0.658501616 -0.406318517  0.508309933  1.622667341
    ##  [31]  1.566160405  0.328216408  0.095073747  1.545489075 -1.547796693
    ##  [36] -0.821493180 -1.268244851 -0.554534583 -0.902570911 -0.165932620
    ##  [41]  0.111262797  0.264808983 -0.543303760  0.435644087 -0.598563460
    ##  [46] -0.836629661  0.691275981 -0.052965898 -0.668718265 -0.805537952
    ##  [51] -1.287215117 -2.001399459 -1.271478322 -0.689483304 -1.495021500
    ##  [56] -0.595921716  0.426349959  0.577407799  1.633422025 -0.959079613
    ##  [61]  0.623850119  1.078069630 -0.027628641 -1.872018726  1.348602766
    ##  [66]  1.053740327 -1.601899321 -0.558079434  0.083150906  0.460050669
    ##  [71] -0.027805237  1.337009562 -1.156044924 -0.950062971  0.554056039
    ##  [76]  1.136243113 -0.626338811  0.621876980 -0.031263501  1.294489819
    ##  [81] -0.251801826 -0.591777848 -0.024412015  0.027988560  2.014021859
    ##  [86] -1.315911017  0.731142719 -0.453648599  1.189996127 -1.392069103
    ##  [91] -0.172815943  0.015978580  2.136247419 -0.195675366 -0.789532642
    ##  [96] -0.679128141 -1.452696218  0.925210291 -0.056566204 -0.278257378
    ## [101] -1.323372124 -0.624749231 -2.019811859 -0.607045370  0.273130816
    ## [106]  0.383323252  1.271183334  0.582889594 -0.030263638 -0.981284815
    ## [111]  0.676430771 -0.008407471  2.311667725  0.744768396  2.147405070
    ## [116]  0.384743025  0.467869090 -0.537921710  0.826176418 -0.369413281
    ## [121]  0.414725218  0.595485953 -0.322832786

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
z_scores = function(x) {
  
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

z_scores(x = x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.82  3.86
