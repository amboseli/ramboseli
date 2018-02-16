
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

grooming_l <- subset_interactions(babase, members_l, my_acts = c("G"))
agonism_l <- subset_interactions(babase, members_l, my_acts = c("A", "AS"))

# Make an individual-year-of-life data set for adults
iyol <- make_iyol(babase, members_l, focals_l, grooming_l)

# Example of how to save / reload a data set
saveRDS(iyol, "data/iyol_2018-02-15.RDS")
# iyol <- readRDS("data/iyol_2018-02-15.RDS")


# calculate-dsi -----------------------------------------------------------

## Restrict to groups where the animal was present for at least 60 days
iyol_sub <- iyol %>%
  filter(days_present >= 60)


# Calculate population-level DSI subset for each row of data
# Warning: takes up to 30 hours!!!!
# Faster if run using parallel, but speed will depend on number of cores
# The option for parallel processing seems finicky
# If you're getting errors, try setting parallel to FALSE
dsi_pop <- dyadic_index(iyol_sub, biograph_l, members_l, focals_l, females_l,
                        grooming_l, min_cores_days = 60, within_grp = FALSE,
                        parallel = TRUE, directional = FALSE)

# Example of how to save / reload a data set
saveRDS(dsi_pop, "data/dsi-pop_2018-02-15.RDS")
# dsi_pop <- readRDS("data/dsi-pop_2018-02-15.RDS")

# Summarize DSI variables for top partners in each year of life
dsi_pop_summary <- dyadic_index_summary(dsi_pop)

saveRDS(dsi_pop_summary, "data/dsi-pop_summary_2018-02-15.RDS")
# dsi_pop_summary <- readRDS("data/dsi-pop_summary_2018-02-15.RDS")


# calculate-sci -----------------------------------------------------------

# Calculate SCI subset for each row of data
sci <- sci(iyol_sub, members_l, focals_l, females_l, grooming_l,
           min_res_days = 60, parallel = TRUE)

saveRDS(sci, "data/sci_2018-02-15.RDS")
# sci <- readRDS("data/sci_2018-02-15.RDS")


# calculate-agi -----------------------------------------------------------

# Calculate AGI subset for each row of data
# Warning: takes ~50 minutes!!!!
agi <- sci(iyol_sub, members_l, focals_l, females_l, agonism_l,
           min_res_days = 60, parallel = TRUE, directional = TRUE)

names(agi) <- str_replace(names(agi), pattern = "SCI_", replacement = "AGI_")

saveRDS(agi, "data/agi_2018-02-15.RDS")
# agi <- readRDS("data/agi_2018-02-15.RDS")


# directed-sci ------------------------------------------------------------

# Calculate AGI subset for each row of data
# Warning: takes ~50 minutes!!!!
sci_dir <- sci(iyol_sub, members_l, focals_l, females_l, grooming_l,
           min_res_days = 60, parallel = TRUE, directional = TRUE)

saveRDS(sci_dir, "data/sci-dir_2018-02-15.RDS")
# sci_dir <- readRDS("data/sci-dir_2018-02-15.RDS")
