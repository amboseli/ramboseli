
# setup -------------------------------------------------------------------

# Install devtools if not already present
if (!("devtools" %in% installed.packages()[,"Package"]))
  install.packages("devtools")

# Install newest version of ramboseli if not already installed
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
agonism_l <- subset_interactions(babase, members_l, my_acts = c("A", "AS", "DS", "OS"))

# Make an individual-year-of-life data set for adults
iyol <- make_iyol(babase, members_l, focals_l, grooming_l)

# Example of how to save / reload a data set
saveRDS(iyol, paste0("data/iyol_", Sys.Date(), ".RDS"))

## Restrict to groups where the animal was present for at least 60 days
iyol_sub <- iyol %>%
  filter(days_present >= 1)



# NOTE:
# All function calls below will be MUCH faster when parallel = TRUE.
# Speed will depend on number of cores; by default, all are used.
# But the option for parallel processing seems finicky.
# If you're getting errors, try setting parallel to FALSE.




######################################
## SOCIALITY INDICES FOR YEARS OF LIFE


# calculate-sci -----------------------------------------------------------

# Calculate grooming social connectedness index
sci <- sci(iyol_sub, members_l, focals_l, females_l, grooming_l,
           min_res_days = 1, parallel = TRUE)

saveRDS(sci, paste0("data/sci_", Sys.Date(), ".RDS"))


# directed-sci ------------------------------------------------------------

# Calculate DIRECTED grooming connectedness index
sci_dir <- sci(iyol_sub, members_l, focals_l, females_l, grooming_l,
           min_res_days = 1, parallel = TRUE, directional = TRUE)

saveRDS(sci_dir, paste0("data/sci-dir_", Sys.Date(), ".RDS"))


# calculate-agi -----------------------------------------------------------

# Calculate directed agonism connectedness index
agi <- sci(iyol_sub, members_l, focals_l, females_l, agonism_l,
           min_res_days = 1, parallel = TRUE, directional = TRUE, ncores = 3)

names(agi) <- str_replace(names(agi), pattern = "SCI_", replacement = "AGI_")

saveRDS(agi, paste0("data/agi_", Sys.Date(), ".RDS"))


# calculate-dsi -----------------------------------------------------------

# Calculate population-level dyadic grooming index
# Warning: takes a really long time!
dsi_pop <- dyadic_index(iyol_sub, biograph_l, members_l, focals_l, females_l,
                        grooming_l, min_cores_days = 1, within_grp = FALSE,
                        parallel = TRUE, directional = FALSE)

saveRDS(dsi_pop, paste0("data/dsi-pop_", Sys.Date(), ".RDS"))

# Summarize DSI variables for top partners in each year of life
dsi_pop_summary <- dyadic_index_summary(dsi_pop)

saveRDS(dsi_pop_summary, paste0("data/dsi-pop_summary_", Sys.Date(), ".RDS"))




#####################################
## SOCIALITY INDICES FOR TARGET DATES

# target-date-gc-dsi ------------------------------------------------------

#fGC samples
gc <- tbl(babase, in_schema("fecal", "prep")) %>%
  inner_join(tbl(babase, in_schema("fecal", "results")),
             by = "sid") %>%
  collect() %>%
  left_join(select(members_l, sname, date, grp, sex),
            by = c("sname", "date")) %>%
  filter(sex == "F" & gc > 0) %>%
  drop_na(gc) %>%
  select(sid, sname, sex, grp, date, gc)

gca <- make_target_date_df(gc, babase, members_l)

saveRDS(gca, file = paste0("data/gca_", Sys.Date(), ".RDS"))

# WARNING: this one takes several days to finish!

gca_dsi <- dyadic_index(gca, biograph_l, members_l, focals_l, females_l,
                        grooming_l, min_cores_days = 1, within_grp = FALSE,
                        parallel = TRUE, directional = FALSE)

saveRDS(gca_dsi, file = paste0("data/gca-dsi_", Sys.Date(), ".RDS"))


gca_dsi_summary <- dyadic_index_summary(gca_dsi)
saveRDS(gca_dsi_summary, file = paste0("data/gca-dsi-summary_", Sys.Date(), ".RDS"))


# target-date-offspring-dob -----------------------------------------------

biograph <- tbl(babase, "biograph")

kids <- tbl(babase, "parents") %>%
  select(mom, kid) %>%
  inner_join(select(biograph, kid = sname, kid_birth = birth, kid_sex = sex,
                    kid_statdate = statdate, matgrp), by = "kid") %>%
  arrange(mom, kid_statdate) %>%
  collect() %>%
  rename(date = kid_birth, sname = mom, grp = matgrp)

kids$sex <- "F"

kid_df <- make_target_date_df(kids, babase, members_l)

temp <- sci(kid_df, members_l, focals_l, females_l, grooming_l,
            min_res_days = 1, parallel = TRUE)
