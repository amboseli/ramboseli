
# setup -------------------------------------------------------------------

# Install devtools if not already present
if (!("devtools" %in% installed.packages()[,"Package"]))
  install.packages("devtools")

# Install newest version of ramboseli if not already installed
if (!("ramboseli" %in% installed.packages()[,"Package"]))
  devtools::install_github("amboseli/ramboseli")

Sys.setenv(TZ = 'UTC')
list.of.packages <- list("foreach", "doSNOW", "parallel", "tidyverse",
                         "lubridate", "dbplyr", "purrrlyr", "RPostgreSQL",
                         "zoo", "ramboseli")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

# Run something like this in terminal (Mac) or PuTTY (Windows)
# Will need to modify login ("fac13") and enter your password
#
# ssh -f fac13@papio.biology.duke.edu -L 2222:localhost:5432 -N

babase <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "localhost",
  port = 2222,
  user = "fac13",
  dbname = "babase",
  password = rstudioapi::askForPassword("Database password"))

# Get local copy of biograph table
biograph_l <- collect(tbl(babase, "biograph"))

# Make a members subset that excludes behavioral observation gaps
members_l <- subset_members(babase)

# Subset other data sets used for sociality indices
focals_l <- subset_focals(babase, members_l)
females_l <- subset_females(members_l)
grooming_l <- subset_grooming(babase, members_l)

# Make an individual-year-of-life data set for adults
iyol <- make_iyol(babase, members_l, focals_l, grooming_l)

# Example of how to save / reload a data set
# saveRDS(iyol, "data/iyol_2018-02-02.RDS")
# iyol <- readRDS("data/iyol_2018-02-02.RDS")


# calculate-dsi -----------------------------------------------------------

## Restrict to groups where the animal was present for at least 60 days
iyol_dsi <- iyol %>%
  filter(days_present >= 60)

# Calculate within-group DSI subset for each row of data
# Warning: takes ~7 hours!!!!
# dsi <- dsi(iyol_dsi, biograph_l, members_l, focals_l, females_l, grooming_l,
#            min_cores_days = 60, within_grp = TRUE)

# Calculate population-level DSI subset for each row of data
# Warning: takes up to 30 hours!!!!
# Faster if run using parallel computation, but speed will depend on number of cores
# The option for parallel processing seems finicky and might not work on your computer
# If you're getting errors, try setting parallel to FALSE
dsi_pop <- dsi(iyol_dsi, biograph_l, members_l, focals_l, females_l, grooming_l,
               min_cores_days = 60, within_grp = FALSE, parallel = TRUE)

# Example of how to save / reload a data set
# saveRDS(dsi, "data/dsi_2018-02-02.RDS")
saveRDS(dsi_pop, "data/dsi-pop_2018-02-02.RDS")
# dsi <- readRDS("data/dsi_2018-02-02.RDS")
# dsi_pop <- readRDS("data/dsi-pop_2018-02-02.RDS")

# Summarize DSI variables for top partners in each year of life
# Takes about 3 or 4 minutes
# dsi_summary <- dsi_summary(dsi)
dsi_pop_summary <- dsi_summary(dsi_pop)

# Example of how to save / reload a data set
# saveRDS(dsi_summary, "data/dsi_summary_2018-02-02.RDS")
saveRDS(dsi_pop_summary, "data/dsi-pop_summary_2018-02-02.RDS")
# dsi_summary <- readRDS("data/dsi_summary_2018-02-02.RDS")
# dsi_pop_summary <- readRDS("data/dsi-pop_summary_2018-02-02.RDS")


# calculate-sci -----------------------------------------------------------

iyol_sci <- iyol %>%
  filter(days_present >= 60)

# Calculate SCI subset for each row of data
# Warning: takes ~50 minutes!!!!
sci <- sci(iyol_sci, members_l, focals_l, females_l, grooming_l,
           min_res_days = 60, parallel = TRUE)

# Example of how to save / reload a data set
saveRDS(sci, "data/sci_2018-02-02.RDS")
# sci <- readRDS("data/sci_2018-02-02.RDS")
