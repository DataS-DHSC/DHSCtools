
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dhsctools

<!-- badges: start -->

<!-- badges: end -->

The goal of dhsctools is to provide a collection of helper functions to
try and solve common problems experienced by analysts when using the
Department’s IT infrastructure and working on the estate.

For more general guidance on using R in the Department please see the
dhsc-r-guide repository.

This code has been developed in-house and not extensively tested, if you
find any bugs or have suggestions of other functions to add please raise
an issue in this repository.

## Installation

You can install the development version of dhsctools using:

``` r
devtools::install_github("DataS-DHSC/dhsctools")
```

## Examples

Configure http and https proxy settings for our corporate firewall when
using git on the DHSC estate through a wired connection:

``` r
dhsctools::configure_git_proxy()
```

Configure proxy settings for functions based on curl using Windows
settings (needed when on the DHSC estate using a wired connection due to
corporate firewall):

``` r
dhsctools::configure_curl_proxy()

httr::content(httr::GET("https://api.ipify.org"), encoding = "UTF-8")
#> [1] "136.228.234.8"
```
