Group Membership
================

The function `get_n_members` calculates the number of individuals present, given a particular group and a date.

*This will not work with "live" babase data until you have set up a connection between R and babase!*

This entails getting a papio login, and creating an ssh tunnel from your computer to papio in putty (Windows) or terminal (Mac/Unix).

Creating the connections
------------------------

On my machine, I type something like this into terminal to make the ssh tunnel:

`ssh -f fac13@papio.biology.duke.edu -L 2222:localhost:5432 -N`

If you get that sorted out, you can create a connection to babase:

``` r
  library(ramboseli)
  library(tidyverse)

  # You will need to change user to your personal babase login AND get your password
  # One approach to doing that is through Rstudio
  # You could also just type it in your script, but that's not recommended for security.
  babase <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                           host = "localhost",
                           port = 2222,
                           user = "fac13",
                           dbname = "babase",
                           password = rstudioapi::askForPassword("Database password"))

  # Create connections to tables
  biograph <- tbl(babase, "biograph")
  members <- tbl(babase, "members")
```

Not bothering with connections?
-------------------------------

An alternative approach is to feed the function unaltered `biograph` and `members` tables that you have loaded into your R environment from CSV files dumped directly from a babase query.

Examples
--------

#### Obtain group size for a particular date/group

``` r
# Get number of females in group 1.220 on Aug. 25, 2012
get_n_members(biograph, members, 1.220, "2012-08-25", "F")
#> [1] 48
```

#### Add group size to existing data frame

This function might be especially useful if you have an existing dataframe and you want to add group size to it. For example, you might decide that this would me an important predictor to include in some analysis you plan to run.

In the example below, we'll add group size to a dataframe that contains three columns from biograph: sname, matgrp, and birthdate. The function will be used to calculate group size on each individual's date of birth.

I'll be using `dplyr` syntax, which might appear weird if you haven't encountered it before. The main thing to know is that the `>%>` operator that appears after each line can just be read as "and then perform...". I think `dplyr` is the best tool for both interacting with databases and data wrangling in R. A nice introduction to `dplyr` can be found here: [dplyr](https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html)

Here's an example of how this function can be applied to an existing dataframe. To illustrate, we'll generate a small data set for testing the function. To keep it short, I'll select just 10 lines at random from biograph.

``` r

# Randomly select 10 rows from biograph (where matgrp is < 4 and sname is not missing)
set.seed(1)
test_data <- biograph %>% 
  filter(!is.na(matgrp) & matgrp < 4 & !is.na(sname)) %>% 
  select(sname, matgrp, birth) %>% 
  collect() %>% 
  sample_n(10)
```

What does this data set look like?

``` r
test_data
#> # A tibble: 10 x 3
#>    sname matgrp      birth
#>    <chr>  <dbl>     <date>
#>  1   PUN   2.00 1992-05-19
#>  2   WAO   2.20 2003-03-12
#>  3   DEO   1.10 2008-09-12
#>  4   CRU   1.10 2000-07-09
#>  5   NAZ   1.00 1973-10-15
#>  6   HAP   1.10 1992-12-17
#>  7   FER   1.10 2008-09-03
#>  8   WOK   2.20 2004-08-27
#>  9   YEM   3.10 1997-02-13
#> 10   NIS   1.11 2016-06-16
```

Now we're ready to apply the function to each row to obtain a group size on the date of birth.

``` r
now_with_group_size <- test_data %>% 
  rowwise() %>% 
  mutate(grp_size_at_birth = get_n_members(biograph, members, matgrp, birth))

now_with_group_size
#> Source: local data frame [10 x 4]
#> Groups: <by row>
#> 
#> # A tibble: 10 x 4
#>    sname matgrp      birth grp_size_at_birth
#>    <chr>  <dbl>     <date>             <int>
#>  1   PUN   2.00 1992-05-19                59
#>  2   WAO   2.20 2003-03-12                66
#>  3   DEO   1.10 2008-09-12                88
#>  4   CRU   1.10 2000-07-09                58
#>  5   NAZ   1.00 1973-10-15                44
#>  6   HAP   1.10 1992-12-17                40
#>  7   FER   1.10 2008-09-03                85
#>  8   WOK   2.20 2004-08-27                69
#>  9   YEM   3.10 1997-02-13                71
#> 10   NIS   1.11 2016-06-16                70
```
