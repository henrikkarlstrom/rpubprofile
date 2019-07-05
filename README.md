
<!-- README.md is generated from README.Rmd. Please edit that file -->
rpubprofile
===========

<!-- badges: start -->
<!-- badges: end -->
The goal of rpubprofile is to use the Scopus API to fetch publication and citation data based on a researcher's Scopus ID and create two plots that summarise a researcher's publishing history in terms of number of publications over time, citations received and most common subject areas. It requires a subscription to Scopus (authentication is IP-based) and the passing of valid Scopus API key to the data fetching function.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("henrikkarlstrom/rpubprofile")
```

Example
-------

Use of rpubprofile functions:

``` r
library(rpubprofile)
## get researcher data on Rosalind Franklin
data <- get_researcher_data("23056907500", apiKey = Sys.getenv("SCOPUS_API_KEY"))
```
