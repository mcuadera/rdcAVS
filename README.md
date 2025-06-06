rdcAVS
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rdcAVS is to streamline the process of creating SIA campaign
folders in the DRC. The package is designed to be portable and secure.
Data for the geographies and permissions are stored locally and are
uploaded by the user directly. Currently, the package comes with the
campaign template, but future versions will allow users to upload their
own modified campaign template file. The package is still under
development, but a beta version is available.

## Installation

You can install the development version of rdcAVS from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mcuadera/rdcAVS")
```

## Example

There is only one function in this package. To deploy the app locally,
simply run the following code:

``` r
rdcAVS::campagneApp()
```

## Demonstration

An example of the app deployed can be viewed
[here](https://mcuadera.shinyapps.io/rdcAVS/).
