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



# biographical-data-plhd --------------------------------------------------

# b <- read_csv("plhd_biographies.csv")
# study_end <- ymd("2018-12-31")
#
# b <- b %>%
#   filter(entrydate <= study_end) %>%
#   mutate(departtype = if_else(departdate > study_end, "O", departtype),
#          departdate = if_else(departdate > study_end, study_end, departdate))
#
# write_csv(b, "data/NewBabData.csv")
# write_csv2(b, "data/NewBabData_semicolon.csv")


# ---- DATABASE -----------------------------------------------------------

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

study_end <- ymd("2018-12-31")

# Get local copy of biograph table
biograph_l <- collect(tbl(babase, "biograph"))

# Truncate to study_end date
biograph_l <- biograph_l %>%
  filter(entrydate <= study_end) %>%
  mutate(status = if_else(statdate > study_end, 0L, status),
         statdate = if_else(statdate > study_end, study_end, statdate))

# Make a members subset that excludes behavioral observation gaps
members_l <- subset_members(babase)
members_l <- members_l %>%
  filter(date <= study_end)

# Subset other data sets used for sociality indices
focals_l <- subset_focals(babase, members_l)
females_l <- subset_females(members_l)
grooming_l <- subset_interactions(babase, members_l, my_acts = c("G"))

# Make an individual-year-of-life data set for adults
iyol <- make_iyol(babase, members_l, focals_l, grooming_l)

# Example of how to save / reload a data set
saveRDS(iyol, paste0("data/iyol_", Sys.Date(), ".RDS"))


# dsi ---------------------------------------------------------------------

iyol_sub <- iyol %>%
  filter(days_present >= 1)

# Calculate population-level dyadic grooming index
# Warning: takes a really long time!
dsi_pop <- dyadic_index(iyol_sub, biograph_l, members_l, focals_l, females_l,
                        grooming_l, min_cores_days = 1, within_grp = FALSE,
                        parallel = TRUE, directional = FALSE)

saveRDS(dsi_pop, paste0("data/dsi-pop_", Sys.Date(), ".RDS"))

# Summarize DSI variables for top partners in each year of life
dsi_pop_summary <- dyadic_index_summary(dsi_pop)

saveRDS(dsi_pop_summary, paste0("data/dsi-pop_summary_", Sys.Date(), ".RDS"))



# group_size --------------------------------------------------------------

bio_ex_grp <- make_iyol(babase, members_l, .by_grp = TRUE)


# ranks -------------------------------------------------------------------

ranks <- tbl(babase, "ranks") %>%
  filter(rnktype %in% c("ADF", "ADM")) %>%
  collect() %>%
  group_by(grp, rnkdate, rnktype) %>%
  mutate(proprank = 1 - (rank - 1) / (n() - 1)) %>%
  rename(date = rnkdate) %>%
  mutate(yearmon = as.character(as.yearmon(date))) %>%
  ungroup()

mean_ranks <- bio_ex_grp %>%
  inner_join(rename(ranks, obs_date = date), by = c("sname", "grp")) %>%
  filter(obs_date >= start & obs_date <= end) %>%
  group_by(sname, age_class, grp) %>%
  summarise(mean_rank = mean(rank, na.rm = TRUE),
            mean_prop_rank = mean(proprank, na.rm = TRUE),
            n_ranks = n())


# joins -------------------------------------------------------------------

si_df <- dsi_pop_summary %>%
  mutate(AllBonds_F = WeaklyBonded_F + StronglyBonded_F + VeryStronglyBonded_F,
         AllBonds_M = WeaklyBonded_M + StronglyBonded_M + VeryStronglyBonded_M)

surv_df <- si_df %>%
  left_join(mean_ranks, by = c("sname", "grp", "age_class")) %>%
  select(-contains("Bonded"), -contains("recip"), -n_ranks)

wt_vars <- vars(DSI_M, DSI_F, AllBonds_F, AllBonds_M, mean_rank, mean_prop_rank)

temp <- surv_df %>%
  mutate_at(wt_vars, list(wt = ~ . * days_present))

surv_df <- surv_df %>%
  mutate_at(wt_vars,
            funs(wt = . * days_present)) %>%
  group_by(sname, sex, birth, age_class, start, end, midpoint, birth_dates) %>%
  summarise_at(vars(contains("_wt")),
               funs(sum(., na.rm = TRUE) /
                      sum(days_present * ifelse(is.na(.), NA, 1), na.rm = TRUE))) %>%
  ungroup() %>%
  mutate_at(vars(contains("_wt")), funs(replace_na(., NA)))

names(surv_df) <- str_replace(names(surv_df), "_wt", "")


temp <- surv_df %>%
  mutate(obs_date = birth_dates + days(182)) %>%
  mutate(age_model = floor(as.numeric(obs_date - birth) / 365.25) + 0.5) %>%
  select(animid = sname, sex, age_class, birth, obs_date, age_model, start, end,
         !!!wt_vars)

make_age_seq <- function(df) {
  res <- seq(min(df$age_class), max(df$age_class), by = 1)
  return(res)
}

temp1 <- temp %>%
  group_by(animid) %>%
  nest() %>%
  mutate(age_class = map(data, make_age_seq)) %>%
  select(-data) %>%
  unnest(cols = c(age_class))

temp1 <- left_join(temp1, temp, by = c("animid", "age_class"))

temp1 <- temp1 %>%
  select(-birth, -sex) %>%
  group_by(animid) %>%
  inner_join(select(biograph_l, animid = sname, birth, sex),
             by = "animid") %>%
  ungroup()

temp1 <- temp1 %>%
  mutate(obs_date = if_else(is.na(obs_date), birth + years(age_class - 1) + days(182),
                            obs_date))

# temp1[temp1$sex == "M", ]$SCI_M <- NA
temp1[temp1$sex == "M", ]$DSI_M <- NA
temp1[temp1$sex == "M", ]$AllBonds_M <- NA


# WRITE DATA
temp1 %>%
  select(sex, animid, birth, obs_date, start, end, age_class, age_model,
         DSI_F, DSI_M, AllBonds_F, AllBonds_M,
         mean_rank, mean_prop_rank) %>%
  write_csv(paste0("~/Desktop/", Sys.Date(), "_dsi-tab.csv"))
