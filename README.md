An R package for the ABRP
================

![](img/amboseli_small.jpg)

The `ramboseli` package provides a set of small functions and utilities for R that can be shared among members of the Amboseli Baboon Research Project. There are instructions for using the package below.

<br> <br>

Installation
============

To install this package, you first need to install the `devtools` package:

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

<br>
<hr>
<br>

Current Functionality
=====================

-   [Sociality Indices](documentation/sociality-indices.md)
-   [Plotting helpers and functions](documentation/plotting.md)

<br>
<hr>
<br>

Instructions for creating an SSH tunnel
=======================================

Some (but not all!) functions in this package query the babase database directly. This allows you to pull "live" data from babase directly into R without going through the intermediate steps of writing queries in SQL, saving the results, and reading that data into R from static CSV files.

To use these functions, you must create an SSH tunnel to the database server. The instructions below explain how to do this.

<br>

### Getting a login

First, you must have a username and password for the babase database. If you're here and you're affiliated with the ABRP, you probably have this already. To create the tunnel, you must ALSO request a login to SSH into the server, papio.biology.duke.edu. **Most people do NOT have this—it must be requested from Jake.**

<br>

### Creating a Tunnel

These instructions are known to work in macOS and Linux.  They have not been tested but should also work in Windows, provided that you are using one of:
   1) Windows 11 or newer
   2) Windows 10 from 2018 or later
   3) Windows 10 from before 2018 but with the 2018 update for native SSH

(If you are a Windows user and these instructions work for you, please let Jake know.)

The simplest way to create the tunnel is at the command line.  In macOS/Linux, open a Terminal window.  In Windows, open a Powershell or Command Prompt window.  Type the following, substituting your actual Duke NetID for "YourNetID":

    ssh -f YourNetID@papio.biology.duke.edu -L 2222:localhost:5432 -N

    (If you're a Windows user, don't include the "-f")

Your Terminal window should then prompt you for your password for papio.biology.duke.edu.  Provide the password for your _NetID_, not your Babase password.  You will then have to provide another form of authentication.  If you haven't already setup MFA with Duke, talk to Jake. After that's done, a message appears indicating that the connection has been made:

    ###############################################################################
    # You are about to access a Duke University computer network that is intended #
    # for authorized users only. You should have no expectation of privacy in     #
    # your use of this network. Use of this network constitutes consent to        #
    # monitoring, retrieval, and disclosure of any information stored within the  #
    # network for any purpose including criminal prosecution.                     #
    ###############################################################################

Once this is done, the tunnel has been created. Keep the Terminal/Powershell/Command Prompt window open and return to R for your analysis.

<br>

### Additional Notes

There are shortcuts that can make this process more streamlined, including:

-   Using an SSH key and a .pgpass file rather than a password so that you don't have to type the password each time.
-   Setting up an alias in your .bash\_profile for the tunnel commands.

Fernando can help with this if you're interested.

Every so often, when the connection is left idle, the tunnel seems to get corrupted. Often, you can simply reconnect using the same steps as above. Sometimes reconnecting doesn't work. In those cases, you can "kill" the corrupted tunnel by typing the following into Terminal:

    ps aux | grep ssh

A list of processes should appear in the window, similar to this

    fac13            23128   0.0  0.0  2445836    260   ??  S    25Jun18   0:00.02 /usr/bin/ssh-agent -l
    fac13            14419   0.0  0.0  2432804    776 s000  S+   12:30PM   0:00.00 grep ssh
    fac13            14205   0.0  0.0  2462536    904   ??  Ss   12:27PM   0:00.00 ssh -f fac13@papio.biology.duke.edu -L 2222:localhost:5432 -N

The last process is the corrupted tunnel. Kill it by typing `kill` followed by the process ID number:

    kill 14205

<br>
<hr>
<br>

Reading data from babase
========================

With the tunnel created, we can now connect to babase.

``` r
  library(ramboseli)
  library(tidyverse)
  library(dbplyr)

  # You will need to change user to your personal babase login AND get your password
  # One approach to doing that is through Rstudio
  # You could also just type it in your script, but that's not recommended for security.
  babase <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                           host = "localhost",
                           port = 2222,
                           user = "fac13",
                           dbname = "babase",
                           password = rstudioapi::askForPassword("Database password"))
```

Now we can create a connection to any table or view in babase. Here's an example for the biograph table.

``` r
biograph <- tbl(babase, "biograph")
```

You now have a live connection to the biograph table, stored as an object in R. It's important to note that this hasn't pulled in all the data, only the first few rows. Let's see what this objects looks like:

``` r
biograph
#> # Source:   table<biograph> [?? x 17]
#> # Database: postgres 10.4.0 [fac13@localhost:2222/babase]
#>    bioid sname name    pid    birth      bstatus sex   matgrp statdate  
#>    <int> <chr> <chr>   <chr>  <date>       <dbl> <chr>  <dbl> <date>    
#>  1  2227 <NA>  <NA>    TIB1   2017-03-24     0   U       1.22 2017-03-24
#>  2  2229 <NA>  <NA>    RAN9   2017-04-12     0   U       1.22 2017-04-12
#>  3   566 CLE   CLEM    <NA>   1975-03-01     1   M       5    1988-12-16
#>  4  2027 QN6   QN6     LAN106 2013-09-15     0.5 U       2.12 2013-11-16
#>  5    80 CRA   CRANE   CHE2   1991-09-05     0.5 F       1.1  1993-12-20
#>  6   602 LIP   LIP     <NA>   1966-07-02     9   M       9    1978-12-12
#>  7  2146 QA6   QA6     LAO106 2016-03-22     1   U       2.12 2016-09-12
#>  8    34 <NA>  <NA>    JUH8   1990-05-01     0   U       3    1990-05-01
#>  9    35 <NA>  <NA>    JUH9   1990-12-06     0   U       3    1990-12-06
#> 10  1471 EZE   EZEKIEL ELD1   2008-10-23     0   M       1.22 2009-04-05
#> # ... with more rows, and 8 more variables: status <int>, dcause <int>,
#> #   matgrpconfidence <int>, alt_snames <lgl>, entrydate <date>,
#> #   entrytype <chr>, dcausenatureconfidence <int>,
#> #   dcauseagentconfidence <int>
```

We can perform basic queries and joins on the the table using the `dplyr` and `dbplyr` packages. There's a lot more information [here](https://cran.r-project.org/web/packages/dbplyr/vignettes/dbplyr.html).

If we want to pull in all the data into our current R environment as a normal data frame, we can use the function `collect`.

``` r
biograph_l <- collect(biograph)
biograph_l
#> # A tibble: 2,183 x 17
#>    bioid sname name    pid    birth      bstatus sex   matgrp statdate  
#>  * <int> <chr> <chr>   <chr>  <date>       <dbl> <chr>  <dbl> <date>    
#>  1  2227 <NA>  <NA>    TIB1   2017-03-24     0   U       1.22 2017-03-24
#>  2  2229 <NA>  <NA>    RAN9   2017-04-12     0   U       1.22 2017-04-12
#>  3   566 CLE   CLEM    <NA>   1975-03-01     1   M       5    1988-12-16
#>  4  2027 QN6   QN6     LAN106 2013-09-15     0.5 U       2.12 2013-11-16
#>  5    80 CRA   CRANE   CHE2   1991-09-05     0.5 F       1.1  1993-12-20
#>  6   602 LIP   LIP     <NA>   1966-07-02     9   M       9    1978-12-12
#>  7  2146 QA6   QA6     LAO106 2016-03-22     1   U       2.12 2016-09-12
#>  8    34 <NA>  <NA>    JUH8   1990-05-01     0   U       3    1990-05-01
#>  9    35 <NA>  <NA>    JUH9   1990-12-06     0   U       3    1990-12-06
#> 10  1471 EZE   EZEKIEL ELD1   2008-10-23     0   M       1.22 2009-04-05
#> # ... with 2,173 more rows, and 8 more variables: status <int>,
#> #   dcause <int>, matgrpconfidence <int>, alt_snames <lgl>,
#> #   entrydate <date>, entrytype <chr>, dcausenatureconfidence <int>,
#> #   dcauseagentconfidence <int>
```

Need a table that in a different schema? Use the `in_schema` function:

``` r
all_ranks <- tbl(babase, in_schema("babase_pending", "ALL_RANKS"))
collect(all_ranks)
#> # A tibble: 109,801 x 10
#>    rnkid sname rnkdate      grp rnktype  rank proprank elo_score
#>  * <int> <chr> <date>     <dbl> <chr>   <int>    <dbl>     <int>
#>  1 58094 SKI   1971-01-01     1 ADF         1     1           NA
#>  2 58098 RIN   1971-01-01     1 ADF         5     0           NA
#>  3 58097 FLU   1971-01-01     1 ADF         4     0.25        NA
#>  4 58096 MOM   1971-01-01     1 ADF         3     0.5         NA
#>  5 58095 ALT   1971-01-01     1 ADF         2     0.75        NA
#>  6 58100 ALT   1971-02-01     1 ADF         2     0.8         NA
#>  7 58099 SKI   1971-02-01     1 ADF         1     1           NA
#>  8 58103 PRE   1971-02-01     1 ADF         5     0.2         NA
#>  9 58102 FLU   1971-02-01     1 ADF         4     0.4         NA
#> 10 58101 MOM   1971-02-01     1 ADF         3     0.6         NA
#> # ... with 109,791 more rows, and 2 more variables: elo_score_std <dbl>,
#> #   elo_ordinal <int>
```

<br>
<hr>
<br>

Best practices for using `ramboseli` functions
==============================================

1.  Supply a database connection and no tables to obtain data directly through queries

    -   Uses most up-to-date data
    -   Use when carrying out exploratory analysis
    -   Not reproducible

2.  Supply tables from the R environment and no database connection

    -   Uses static input files (a "snapshot" of specific babase tables)
    -   Reqires that user queries database first to produce static files and loads them into R environment
    -   Use for final analysis if you need reproducibility
