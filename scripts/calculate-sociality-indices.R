
# setup -------------------------------------------------------------------

Sys.setenv(TZ = 'UTC')
list.of.packages <- list("tidyverse", "lubridate", "dbplyr", "purrrlyr",
                         "RPostgreSQL", "zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

babase <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "localhost",
  port = 2222,
  user = "fac13",
  dbname = "babase"
)

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

saveRDS(iyol, "data/iyol_2018-01-10.RDS")
# iyol <- readRDS("iyol_2018-01-10.RDS")


# calculate-dsi -----------------------------------------------------------

## Restrict to groups where the animal was present for at least 60 days
iyol <- iyol %>%
  filter(days_present >= 60)

temp <- iyol[1:10, ]

# Calculate DSI subset for each row of data
# Warning: takes ~7 hours!!!!
ptm <- proc.time()
dsi <- dsi(iyol, biograph_l, members_l, focals_l, females_l, grooming_l, min_cores_days = 60)
proc.time() - ptm

saveRDS(dsi, "data/dsi_2018-01-10.RDS")

# Summarize DSI variables for top partners in each year of life
# Takes about 3 or 4 minutes
dsi_summary <- dsi_summary(dsi)


# calculate-csi -----------------------------------------------------------

# Calculate CSI subset for each row of data
# Warning: takes ~1.5 hours!!!!
ptm <- proc.time()
csi <- csi(iyol, members_l, focals_l, females_l, grooming_l, min_res_days = 60)
proc.time() - ptm

saveRDS(csi, "data/csi_2018-01-10.RDS")

