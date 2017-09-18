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

    #> Loading tidyverse: ggplot2
    #> Loading tidyverse: tibble
    #> Loading tidyverse: tidyr
    #> Loading tidyverse: readr
    #> Loading tidyverse: purrr
    #> Loading tidyverse: dplyr
    #> Conflicts with tidy packages ----------------------------------------------
    #> filter(): dplyr, stats
    #> lag():    dplyr, stats

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

Here's an example of how this function can be applied to an existing dataframe. To illustrate, we'll generate a small data set for testing the function. To keep it short, I'll select just 30 lines at random from biograph.

``` r

# Randomly select 30 rows from biograph (where matgrp is < 4 and sname is not missing)
set.seed(1)
test_data <- biograph %>% 
  filter(!is.na(matgrp) & matgrp < 4 & !is.na(sname)) %>% 
  select(sname, matgrp, birth) %>% 
  collect() %>% 
  sample_n(30)
```

What does this data set look like?

``` r
# Use print because by default dplyr only prints 10 rows to the console
print(test_data, n = 30)
#> # A tibble: 30 x 3
#>    sname matgrp      birth
#>    <chr>  <dbl>     <date>
#>  1   PUN  2.000 1992-05-19
#>  2   WAO  2.200 2003-03-12
#>  3   DEO  1.100 2008-09-12
#>  4   CRU  1.100 2000-07-09
#>  5   NAZ  1.000 1973-10-15
#>  6   HAP  1.100 1992-12-17
#>  7   FER  1.100 2008-09-03
#>  8   WOK  2.200 2004-08-27
#>  9   YEM  3.100 1997-02-13
#> 10   NIS  1.110 2016-06-16
#> 11   JNJ  1.000 1986-08-18
#> 12   AVO  2.200 1999-10-21
#> 13   DEV  1.100 2004-11-28
#> 14   DYN  1.100 1997-08-21
#> 15   VIC  1.000 1976-10-04
#> 16   TEC  1.220 2006-12-20
#> 17   EMU  1.100 1989-08-05
#> 18   NOK  2.100 2003-03-08
#> 19   ROM  1.220 2000-05-27
#> 20   WOW  2.200 2010-08-16
#> 21   LOU  2.200 2003-06-15
#> 22   JNT  1.000 1973-04-05
#> 23   CAF  1.120 2012-06-15
#> 24   UFO  1.211 2016-06-10
#> 25   PAU  2.000 1993-11-26
#> 26   OBI  1.200 1996-01-09
#> 27   WEJ  2.110 2016-04-08
#> 28   HUN  1.110 2010-11-26
#> 29   NOZ  2.100 2000-09-21
#> 30   HEK  1.000 1983-09-03
```

Now we're ready to apply the function to each row to obtain a group size on the date of birth.

``` r
now_with_group_size <- test_data %>% 
  rowwise() %>% 
  mutate(grp_size_at_birth = get_n_members(biograph, members, matgrp, birth))

print(now_with_group_size, n = 30)
#> Source: local data frame [30 x 4]
#> Groups: <by row>
#> 
#> # A tibble: 30 x 4
#>    sname matgrp      birth grp_size_at_birth
#>    <chr>  <dbl>     <date>             <int>
#>  1   PUN  2.000 1992-05-19                59
#>  2   WAO  2.200 2003-03-12                66
#>  3   DEO  1.100 2008-09-12                88
#>  4   CRU  1.100 2000-07-09                58
#>  5   NAZ  1.000 1973-10-15                44
#>  6   HAP  1.100 1992-12-17                40
#>  7   FER  1.100 2008-09-03                85
#>  8   WOK  2.200 2004-08-27                69
#>  9   YEM  3.100 1997-02-13                71
#> 10   NIS  1.110 2016-06-16                70
#> 11   JNJ  1.000 1986-08-18                68
#> 12   AVO  2.200 1999-10-21                44
#> 13   DEV  1.100 2004-11-28                70
#> 14   DYN  1.100 1997-08-21                51
#> 15   VIC  1.000 1976-10-04                44
#> 16   TEC  1.220 2006-12-20                58
#> 17   EMU  1.100 1989-08-05                28
#> 18   NOK  2.100 2003-03-08                41
#> 19   ROM  1.220 2000-05-27                35
#> 20   WOW  2.200 2010-08-16               108
#> 21   LOU  2.200 2003-06-15                68
#> 22   JNT  1.000 1973-04-05                41
#> 23   CAF  1.120 2012-06-15                29
#> 24   UFO  1.211 2016-06-10                44
#> 25   PAU  2.000 1993-11-26                70
#> 26   OBI  1.200 1996-01-09                31
#> 27   WEJ  2.110 2016-04-08                48
#> 28   HUN  1.110 2010-11-26                65
#> 29   NOZ  2.100 2000-09-21                40
#> 30   HEK  1.000 1983-09-03                60
```
