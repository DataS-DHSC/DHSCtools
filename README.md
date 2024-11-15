
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *\*\* Package in development \*\**

# DHSCtools

The goal of DHSCtools is to provide a collection of tools to try and
embed best practices for coding in the analytical community and to solve
common problems experienced by analysts when using the Department’s IT
infrastructure.

The main component of this package is a DHSC project template that
attempts to simplify the process of writing clean and maintainable code
that conforms to best practice as set out in [UK Government Analytical
Community - Quality assurance of code for analysis and
research](https://best-practice-and-impact.github.io/qa-of-code-guidance/).

This code has been developed in-house and not extensively tested, if you
find any bugs or have suggestions of other functions to add please raise
an issue in this repository.

## Installation

You can install the development version of DHSCtools using:

``` r
if (!requireNamespace("librarian")) install.packages("librarian", quiet = TRUE)
librarian::stock(DataS-DHSC/DHSCtools)
```

**Note:** You will need to restart your R session (either by restarting
RStudio or using *Session* \> *Restart R*) on installing to see the
*DHSC Project Template* option.

To UPDATE DHSCtools, run the following:

``` r
librarian::stock(DataS-DHSC/DHSCtools, update_all = TRUE)
```

Again, you will need to restart your R session after installation.

## DHSC project template

After installing the package, to use the DHSC project template go to
*File \> New Project…* then select *New Directory* followed by *DHSC
Project Template*.

## Examples

Configure http and https proxy settings for our corporate firewall when
using git on the DHSC estate through a wired connection (only works if
repo was cloned via https rather than SSH):

``` r
DHSCtools::configure_git_proxy()
```

Configure proxy settings for functions based on curl using Windows
settings (needed when on the DHSC estate using a wired connection due to
corporate firewall):

``` r
DHSCtools::configure_curl_proxy()

httr::content(httr::GET("https://api.ipify.org"), encoding = "UTF-8")
```
