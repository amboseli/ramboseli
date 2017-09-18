An R package for the ARBP
================

The `ramboseli` package aims to provide a set of small functions and utilities for R that can be shared among members of the Amboseli Baboon Research Project. Currently it's just a barebones proof-of-concept, but this documentation will grow as features are added to the package.

Preparation
-----------

To use this package, you first need to install the `devtools` package:

``` r
    install.packages("devtools")
```

Then, you can install the latest development version of `ramboseli` from github:

``` r
  devtools::install_github("amboseli/ramboseli")
```

After you have installed the package once, you can simply load it in the future using:

``` r
  library(ramboseli)
```

Current functionality provided by the package:
----------------------------------------------

-   [Plotting helpers and functions](documentation/plotting.md)
-   [Group membership](documentation/group-membership.md)

<br>

### Guidelines for using `ramboseli` functions

TO DO: Expand this section

Instructions to be added:

-   Detailed instructions for linking babase to R via ssh tunnel
-   Managing passwords and keeping them of your scripts: ssh key or rstudioapi::askForPassword()
-   Creating connection to database using DBI::dbConnect()

<br>

### Best practices for supplying babase tables as arguments to functions

1.  Supply a database connection and no tables to obtain data directly through queries

    -   Uses most up-to-date data
    -   Use when carrying out exploratory analysis
    -   Not reproducible

2.  Supply tables from the R environment and no database connection

    -   Uses static input files (a "snapshot" of specific babase tables)
    -   Reqires that user queries database first to produce static files and loads them into R environment
    -   Use for final analysis if you desire reproducibility
