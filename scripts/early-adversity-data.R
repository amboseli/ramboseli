
# ---- SETUP ----------------------------------------------------------------

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

# Database connections
biograph <- tbl(babase, "biograph")
members <- tbl(babase, "members")
maturedates <- tbl(babase, "maturedates")
rankdates <- tbl(babase, "rankdates")

md_females <- maturedates %>%
  semi_join(filter(biograph, sex == "F"), by = "sname")

rd_males <- rankdates %>%
  semi_join(filter(biograph, sex == "M"), by = "sname")

# Get local copy of biograph table
biograph_l <- collect(tbl(babase, "biograph"))


# ---- SUBJECTS -------------------------------------------------------------

# The subjects should be:
#   - either male or female (both sexes included)
#   - born into matgroups < 3.0
#   - exclude animals born into group 1.3 (Proton's group)
#   - had a bstatus < 1 (0 or 0.5 is OK)
#   - lived to at least age 4. I think sometimes you define age differently than I do, but this is the code I use to calculate subject age >=4: (statdate - birth)/365.25) >=4
#   - PID is not null (the subject must have a known mother)
#   - have complete information for all 6 sources of adversity (see below)

subjects <- biograph_l %>%
  filter(matgrp < 3 & matgrp != 1.3 & bstatus < 1 & !is.na(pid) & (statdate - birth) / 365.25 >= 4) %>%
  select(sname, name, pid, birth, bstatus, sex, matgrp, statdate)


# ---- ADVERSITIES ----------------------------------------------------------

# The early adversities should be defined as:


# adv-group-density -------------------------------------------------------

# Occurs when the the subject's group size (defined as the number of sexually mature males and females)
# on the day they were born is in the highest quartile. Here and for other adversities that use quartiles (maternal rank & maternal
# social isolation), the quartiles should be defined using the current data set, not the cut-offs in Tung et al. 2016.

# Find all adult members in each group on each date
adult_members <- members %>%
  filter(grp < 3 & grp != 1.3) %>%
  inner_join(select(biograph, sname, sex), by = "sname") %>%
  left_join(select(md_females, sname, matured), by = "sname") %>%
  left_join(select(rd_males, sname, ranked), by = "sname") %>%
  filter((sex == "F" & date >= matured) | (sex == "M" & date >= ranked)) %>%
  collect()

# Get total counts of all adult members in each group (<3) on each date
adult_sex_counts <- adult_members %>%
  group_by(grp, date, sex) %>%
  summarise(n_animals = n()) %>%
  arrange(grp, date) %>%
  spread(sex, n_animals, fill = 0) %>%
  ungroup() %>%
  mutate(n_adults = F + M) %>%
  select(grp, date, n_adults)

density_adv <- subjects %>%
  left_join(adult_sex_counts, by = c("birth" = "date", "matgrp" = "grp")) %>%
  mutate(thresh_density = quantile(n_adults, probs = 0.75),
         adv_density = n_adults > thresh_density) %>%
  select(sname, adv_density, density = n_adults, thresh_density)


# adv-drought -------------------------------------------------------------

# Occurs when rainfall in the subject's first year of life is less than or equal to 200mm

amb_rain <- collect(tbl(babase, "min_maxs"))

# There are two duplicated dates
# Take sum of measurements
amb_rain <- amb_rain %>%
  mutate(date_of = as.Date(wrdaytime)) %>%
  group_by(date_of) %>%
  summarise(rain = sum(rain, na.rm = TRUE))

rain_1_yr <- subjects %>%
  select(sname, start_rain = birth) %>%
  mutate(end_rain = start_rain + years(1) - days(1))

rain_1_yr$data <- list(amb_rain)

rain_1_yr <- rain_1_yr %>%
  unnest() %>%
  filter(date_of >= start_rain & date_of <= end_rain) %>%
  group_by(sname) %>%
  summarise(total_rain = sum(rain)) %>%
  mutate(adv_rain = total_rain <= 200)

rain_adv <- rain_1_yr %>%
  select(sname, adv_rain, rain = total_rain)


# adv-maternal-loss -------------------------------------------------------

# Occurs when the subject's mother dies before the subject reaches age 4

moms <- subjects %>%
  mutate(kid_sname = sname,
         mom_sname = str_sub(pid, 1, 3))

mom_adv <- moms %>%
  mutate(kid_4th_bday = birth + years(4)) %>%
  select(kid_sname, mom_sname, kid_4th_bday) %>%
  inner_join(biograph_l, by = c("mom_sname" = "sname")) %>%
  mutate(adv_mom = statdate < kid_4th_bday) %>%
  select(sname = kid_sname, adv_mom)


# adv-competing-sib -------------------------------------------------------

# Occurs when a maternal sibling is born alive within 1.5 years of the subject's birth.
# "Born alive" is defined as any individual who lived for at least 1 day.

pregs <- tbl(babase, "pregs") %>%
  collect() %>%
  mutate(mom_sname = str_sub(pid, 1, 3))

sibs <- moms %>%
  inner_join(select(pregs, pid, kid_parity = parity), by = "pid") %>%
  select(mom_sname, kid_sname, kid_birth = birth, kid_parity) %>%
  inner_join(pregs, by = "mom_sname") %>%
  inner_join(select(biograph_l, pid, sib_sname = sname, sib_birth = birth, sib_statdate = statdate), by = "pid") %>%
  filter(kid_sname != sib_sname) %>%
  rename(sib_parity = parity)

next_sib <- sibs %>%
  filter(sib_parity >= kid_parity) %>%
  group_by(kid_sname) %>%
  top_n(-1, sib_parity) %>%
  ungroup()

sib_adv <- next_sib %>%
  mutate(adv_date = kid_birth + days(548),
         adv_sib = sib_birth < adv_date) %>%
  select(sname = kid_sname, adv_sib) %>%
  right_join(select(subjects, sname)) %>%
  replace_na(list(adv_sib = FALSE))


# adv-maternal-rank -------------------------------------------------------

# Occurs when the mother's rank in the month of the offspring's birth is in the bottom quartile for the data set.

ranks <- tbl(babase, "ranks") %>%
  filter(rnktype == "ADF") %>%
  group_by(grp, rnkdate, rnktype) %>%
  mutate(proprank = 1 - (rank - 1) / (n() - 1)) %>%
  collect() %>%
  make_date_cols(rnkdate) %>%
  ungroup()

mom_rank <- moms %>%
  select(sname, mom_sname, birth, matgrp) %>%
  make_date_cols(birth) %>%
  inner_join(rename(ranks, "mom_sname" = "sname"),
             by = c("mom_sname", "year_of", "month_of", "matgrp" = "grp"))

mom_rank_adv <- mom_rank %>%
  mutate(thresh_rank = quantile(proprank, probs = 0.25),
         adv_mom_rank = proprank < thresh_rank) %>%
  select(sname, adv_mom_rank, mom_rank = proprank, thresh_rank)


# adv-maternal-dsi --------------------------------------------------------

# Occurs when the mother's average DSI to adult females (DSI-F) across the subject's first 2 years of
# life is in the bottom quartile. If less than 2 years of data is available for a mother, we should still
# include them in the data set. I think the minimum amount of data I would use to calculate DSI would be 6 months,
# and these 6 months could fall anywhere in the 2-year window. For instance, If a subject was born on 1 Jan 2016,
# the 2-year window would end 31-Dec 2018. If the mom only had 6 months of data available from the last 6 months of
# the 2-year period (31-Jun 2018 to 31-Dec 2018), I would still include her.

mom_dsi_df <- moms %>%
  select(kid_sname = sname, mom_sname, birth) %>%
  mutate(date = birth + years(2) - days(1)) %>%
  select(sname = mom_sname, date) %>%
  arrange(sname, date)

mom_dsi_df$sex <- "F"

# Data sets for calculating DSI
members_l <- subset_members(babase)
focals_l <- subset_focals(babase, members_l)
females_l <- subset_females(members_l)
grooming_l <- subset_interactions(babase, members_l, my_acts = c("G"))

mom_dsi_df <- make_target_date_df(mom_dsi_df, babase, members_l, window_length = 2)

# Remove mom/grp combinations with fewer than 6-months of data, and data from non-study groups
mom_dsi_df <- mom_dsi_df %>%
  filter(days_present >= 183 & grp < 3 & grp != 1.3)

# Calculate mom's DSI
# Warning: takes a while, around 2 minutes per core on your machine
mom_dsi_2yrs <- dyadic_index(mom_dsi_df, biograph_l, members_l, focals_l, females_l,
                             grooming_l, min_cores_days = 1, within_grp = FALSE,
                             parallel = TRUE, directional = FALSE)

mom_dsi_summary <- dyadic_index_summary(mom_dsi_2yrs)

mom_dsi_wt_avg <- mom_dsi_summary %>%
  mutate_at(vars(starts_with("DSI")), funs(wt = . * days_present)) %>%
  group_by(sname, start, end) %>%
  summarise_at(vars(ends_with("_wt")), funs(sum(., na.rm = TRUE) / sum(days_present))) %>%
  ungroup()

mom_dsi_adv <- mom_dsi_wt_avg %>%
  mutate(thresh_dsi = quantile(DSI_F_wt, probs = 0.25),
         adv_mom_dsi = DSI_F_wt < thresh_dsi) %>%
  select(mom_sname = sname, birth = start, adv_mom_dsi, mom_dsi = DSI_F_wt, thresh_dsi) %>%
  right_join(select(moms, sname, birth, mom_sname), by = c("mom_sname", "birth")) %>%
  select(sname, adv_mom_dsi, mom_dsi, thresh_dsi)


# adv-join-gc -------------------------------------------------------------

subjects <- subjects %>%
  left_join(density_adv, by = "sname") %>%
  left_join(rain_adv, by = "sname") %>%
  left_join(mom_adv, by = "sname") %>%
  left_join(sib_adv, by = "sname") %>%
  left_join(mom_rank_adv, by = "sname") %>%
  left_join(mom_dsi_adv, by = "sname")

subjects <- drop_na(subjects)

subjects <- subjects %>%
mutate(adv_cumulative = rowSums(.[str_detect(names(subjects), "adv_")])) %>%
arrange(birth)



# ---- WRITE-ADVERSITY-FILE -----------------------------------------------

# write_csv(subjects, paste0("early-adversity-subjects_", Sys.Date(), ".csv"))
saveRDS(mom_dsi_2yrs, paste0("data/mom_dsi_2yrs", Sys.Date(), ".RDS"))
saveRDS(mom_dsi_summary, paste0("data/mom_dsi_summary_", Sys.Date(), ".RDS"))



# ---- COVARIATES-FOR-GC --------------------------------------------------

# Make members and grooming subsets that exclude behavioral observation gaps AND that include juvenile data
members_l2 <- subset_members(babase, .adults_only = FALSE)
grooming_l2 <- subset_interactions(babase, members_l2, my_acts = c("G"), .adults_only = FALSE)

# Get GC data from database
gc <- tbl(babase, in_schema("fecal", "prep")) %>%
  inner_join(tbl(babase, in_schema("fecal", "results")),
             by = "sid") %>%
  collect()

# Add information about grp and sex from members, and restrict to measurable samples from our subjects
# Note that the inner_join on the subset of members removes fGC samples collected from individuals during
# behave_gaps, group fissions, non-study groups, etc.
gc <- gc %>%
  inner_join(select(members_l2, sname, date, grp, sex),
             by = c("sname", "date")) %>%
  filter(gc > 0 & sname %in% subjects$sname) %>%
  drop_na(gc) %>%
  select(sid, sname, sex, grp, date, gc)

# Discard samples collected before indiviual's 4th birthday
gc <- gc %>%
  inner_join(select(biograph_l, sname, birth), by = "sname") %>%
  mutate(age_sample = as.numeric(date - birth) / 365.25) %>%
  filter(age_sample >= 4) %>%
  distinct(sid, .keep_all = TRUE)

# gc-cov-repstat ----------------------------------------------------------

## Reproductive state on the day the sample was collected (pregnant, lactating, cycling).

repstats <- collect(tbl(babase, "repstats_grp"))
repstats$state <- factor(repstats$state, labels = c("Cycling", "Lactating", "Pregnant"))

# Consider week after birth as Pregnant rather than Lactating
# because GC values remain high
temp <- repstats %>%
  mutate(old_state = state,
         flag_rep = (state == "Lactating" & dins <= 7),
         state = as.character(state),
         state = if_else(flag_rep, "Pregnant", state),
         state = factor(state, levels = c("Cycling", "Lactating", "Pregnant")))

# Find unique repstat blocks and assign an ID
temp <- temp %>%
  arrange(sname, date) %>%
  group_by(sname) %>%
  mutate(tdiff = as.numeric(date - lag(date)),
         bid = if_else(is.na(tdiff) | state != lag(state) |
                         (!is.na(tdiff) & tdiff > 1 &
                            (pid != lag(pid) | is.na(pid) | is.na(lag(pid)))),
                       rid, 0L))

temp <- temp %>%
  mutate(bid = if_else(bid == 0, NA_integer_, bid))

repstats <- tidyr::fill(temp, bid, .direction = "down")

gc <- gc %>%
  left_join(select(repstats, sname, state, date, grp), by = c("sname", "grp", "date"))


# gc-cov-grp-density ------------------------------------------------------

#  measured as the number of adult members of the subject’s social group on the day the sample was collected.
#  Note this is a change. Since we are no longer using monthly mean GCs, we will also not use monthly means of numbers of group members.

gc <- gc %>%
  left_join(adult_sex_counts, by = c("date", "grp"))


# gc-cov-grp-density-squared ----------------------------------------------

#  to account for non-linear relationships between GCs and group density.
#  Same note as above re: monthly means versus counts from individual days.

gc <- gc %>%
  mutate(n_adults_squared = n_adults^2)


# gc-cov-maxtemp ----------------------------------------------------------

## Mean maximum temperature in the 30 days preceding sample collection (following Gesquire et al. 2008)

amb_temp <- tbl(babase, "min_maxs") %>%
  collect() %>%
  mutate(date = as.Date(wrdaytime)) %>%
  select(date, wstation, tempmin, tempmax)

# Deal with a couple of duplicated dates
amb_temp <- amb_temp %>%
  group_by(date, wstation) %>%
  summarise(tempmax = mean(tempmax, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na(tempmax)

# Model to correct for weather station effects
tmax_mod <- lme4::lmer(tempmax ~ 1 + (1 | wstation), data = amb_temp)

# Augment original data set with corrected values
tmax_aug <- tbl_df(augment(tmax_mod, amb_temp)) %>%
  mutate(tempmax_cor = .fixed + .resid) %>%
  select(date, wstation, tempmax_obs = tempmax, tempmax_cor)

# Generate full sequence of dates and fill with NA values
amb_temp <- tmax_aug %>%
  complete(date = full_seq(date, 1))

# Calculate mean tempmax over 30-day sliding window that is right-aligned to sample collection date
amb_temp$mean_tmax_30d <- RcppRoll::roll_meanr(amb_temp$tempmax_obs, n = 30, na.rm = TRUE)

# Join to GC data
gc <- gc %>%
  left_join(select(amb_temp, date, mean_tmax_30d), by = c("date"))


# gc-cov-season -----------------------------------------------------------

## To account for seasonal variation, we will try running three versions. We’ll see which explains the most variation, and go with that one.
#  Season
#  Season + delta rainfall (to account for how far off ‘normal’ they were experiencing at a given point in time)
#  Total rainfall for the three months preceding sample collection

# Helper function to get season
get_season <- function(d) {

  if (!(is.Date(d) | is.POSIXct(d))) {
    stop("The season function only works on dates.")
  }
  month_of <- lubridate::month(d)
  season <- ifelse(month_of >= 6 & month_of <= 10, "Dry", "Wet")

  return(season)
}

# Function to calculate rain average rain during a given date window
get_rain_normal <- function(rain_df, date_col, rain_col, d_start, d_end) {

  if (d_end < d_start) message("Error: end date before start date.")

  yday_start <- yday(d_start)
  yday_end <- yday(d_end)

  # Make complete sequence of dates and fill with zeros
  rain_df <- rain_df %>%
    rename(date_of = !!date_col) %>%
    complete(date_of = full_seq(date_of, 1), fill = list(rain = 0)) %>%
    mutate(yday = yday(!!date_col))

  if (yday_end > yday_start) {

    r <- rain_df %>%
      filter(yday >= yday_start & yday <= yday_end) %>%
      mutate(year_of = year(!!date_col)) %>%
      group_by(year_of) %>%
      summarise(sum_rain = sum(!!rain_col, na.rm = TRUE),
                n = n())

    # Remove any years with truncated data
    r <- r %>%
      filter(n == median(n))

    return(mean(r$sum_rain))

  } else { # Time interval is split across calendar years
    r <- rain_df %>%
      filter(yday >= yday_start | yday <= yday_end) %>%
      mutate(year_of = year(!!date_col),
             year_of = if_else(yday >= yday_start, year_of,
                               if_else(yday <= yday_end, year_of - 1, -999), -9999)) %>%
      group_by(year_of) %>%
      summarise(sum_rain = sum(!!rain_col, na.rm = TRUE),
                n = n())

    # Remove any years with truncated data
    r <- r %>%
      filter(n == median(n))

    return(mean(r$sum_rain))
  }

}

get_rain_total <- function(rain_df, date_col, rain_col, d_start, d_end) {
  if (d_end < d_start) message("Error: end date before start date.")

  # Make complete sequence of dates and fill with zeros
  rain_total <- rain_df %>%
    rename(date_of = !!date_col) %>%
    filter(date_of >= d_start & date_of <= d_end) %>%
    summarise(sum_rain = sum(!!rain_col, na.rm = TRUE)) %>%
    pull(sum_rain)

  return(rain_total)
}

# Warning: takes a while to complete
# Only need to do it once per unique date, not per sample, since rain anomaly will be the same
rain_anom <- gc %>%
  distinct(date, .keep_all = TRUE) %>%
  rowwise() %>%
  mutate(start_date = date %m-% months(3),
         end_date = date,
         rain_normal = get_rain_normal(amb_rain, quo(date_of), quo(rain), start_date, end_date),
         rain_total = get_rain_total(amb_rain, quo(date_of), quo(rain), start_date, end_date)) %>%
  ungroup()

rain_anom <- rain_anom %>%
  mutate(rain_anom_3mo = rain_total - rain_normal)

gc <- gc %>%
  mutate(season = get_season(date)) %>%
  left_join(select(rain_anom, date, rain_anom_3mo, rain_total), by = "date")


# gc-cov-hybrid-score -----------------------------------------------------

#  The model using hybrid score will only be included in the supplement,
#  and is really to show that it doesn’t matter. It will also markedly shrink the sample size.

hyb <- tbl(babase, "hybridgene_scores") %>%
  filter(hgaid == 2) %>%
  rename(hyb_score = score, hyb_lower_conf = lower_conf, hyb_upper_conf = upper_conf) %>%
  collect()

gc <- gc %>%
  left_join(select(hyb, sname, hyb_score), by = "sname")


# gc-cov-hydroyear --------------------------------------------------------

get_hydroyear <- function(d) {

  if (!(is.Date(d) | is.POSIXct(d))) {
    stop("The hydroyear function only works on dates.")
  }
  year_of <- lubridate::year(d)
  month_of <- lubridate::month(d)

  year_of <- ifelse(month_of >= 11, year_of + 1, year_of)

  return(year_of)
}

gc <- gc %>%
  mutate(hydroyear = get_hydroyear(date))


# compare-season-models ---------------------------------------------------

library(lme4)
library(glmmTMB)
library(MuMIn)

m1 <- glmmTMB(log(gc) ~ state + age_sample + n_adults + n_adults_squared + mean_tmax_30d + season +
             (1 | sname) + (1 | grp) + (1 | hydroyear),
           data = filter(gc, sex == "F"))
m2 <- glmmTMB(log(gc) ~ state + age_sample + n_adults + n_adults_squared + mean_tmax_30d + season + rain_anom_3mo +
             (1 | sname) + (1 | grp) + (1 | hydroyear),
           data = filter(gc, sex == "F"))
m3 <- glmmTMB(log(gc) ~ state + age_sample + n_adults + n_adults_squared + mean_tmax_30d + rain_total +
             (1 | sname) + (1 | grp) + (1 | hydroyear),
           data = filter(gc, sex == "F"))

model.sel(m1, m2, m3)


# gc-cov-adult-maternal-relatives -----------------------------------------

# Number of adult maternal female relatives present in the group at the time of sample collection.
# This is a count of mother, adult sisters, and adult daughters (i.e. adult female relatives with R >= 0.25).
# We’re doing this to help mitigate the problem of the relationship between maternal presence (an original covariate)
# and the adversity measure. This also follows both Archie et al. 2014 and Lea et al. 2014, who used aggregate measures
# of maternal relative presence. Note that we should plot the relationship between this measure and both maternal loss
# and the overall adversity index, just so we have some idea of if/how they are related.

md_females_l <- collect(md_females)

# Moms: check members table to see if mom present in same group on same date
mom_present <- gc %>%
  inner_join(select(moms, sname, mom_sname), by = "sname") %>%
  semi_join(select(members_l2, mom_sname = sname, date, grp),
             by = c("mom_sname", "grp", "date")) %>%
  mutate(mom_present = TRUE)

mom_absent <- gc %>%
  inner_join(select(moms, sname, mom_sname), by = "sname") %>%
  anti_join(select(members_l2, mom_sname = sname, date, grp),
            by = c("mom_sname", "grp", "date")) %>%
  mutate(mom_present = FALSE)

# combine
mom_present <- bind_rows(mom_present, mom_absent) %>%
  arrange(sid) %>%
  select(sid, sname, grp, date, mom_present)


# Sisters
sisters <- sibs %>%
  inner_join(select(biograph_l, sib_sname = sname, sib_sex = sex)) %>%
  filter(sib_sex == "F") %>%
  select(sname = kid_sname, sib_sname, sib_birth)

n_adult_sisters <- gc %>%
  select(sid, sname, grp, date) %>%
  inner_join(sisters, by = "sname") %>%  # Combine with GC data
  inner_join(select(md_females_l, sib_sname = sname, sib_matured = matured), by = "sib_sname") %>% # Get sister maturedates
  filter(date >= sib_matured) %>% # Restrict to matured sisters only
  semi_join(select(members_l2, sib_sname = sname, date, grp), # Filtering join combines with members
            by = c("sib_sname", "grp", "date")) %>%
  group_by(sid, sname, grp, date) %>%
  summarise(n_adult_sisters = n())

# combine
n_adult_sisters <- gc %>%
  left_join(n_adult_sisters, by = c("sid", "sname", "grp", "date")) %>%
  replace_na(list(n_adult_sisters = 0)) %>%
  arrange(sid) %>%
  select(sid, sname, grp, date, n_adult_sisters)


# Daughters
daughters <- biograph_l %>%
  filter(sex == "F") %>%
  select(kid_sname = sname, kid_pid = pid, kid_birth = birth) %>%
  mutate(mom_sname = str_sub(kid_pid, 1, 3))

gc_daughters <- gc %>%
  select(sid, sname, grp, date) %>%
  inner_join(daughters, by = c("sname" = "mom_sname")) %>%
  inner_join(select(md_females_l, kid_sname = sname, kid_matured = matured), by = "kid_sname") %>% # Get daughter maturedates
  filter(date >= kid_matured) %>% # Restrict to matured daughters only
  semi_join(select(members_l2, kid_sname = sname, date, grp), # Combine with members
            by = c("kid_sname", "grp", "date"))

n_adult_daughters <- gc_daughters %>%
  group_by(sid, sname, grp, date) %>%
  summarise(n_adult_daughters = n())

# combine
n_adult_daughters <- gc %>%
  left_join(n_adult_daughters, by = c("sid", "sname", "grp", "date")) %>%
  replace_na(list(n_adult_daughters = 0)) %>%
  arrange(sid) %>%
  select(sid, sname, grp, date, n_adult_daughters)


# Combine all maternal relative info
maternal_kin <- mom_present %>%
  inner_join(n_adult_sisters, by = c("sid", "sname", "grp", "date")) %>%
  inner_join(n_adult_daughters, by = c("sid", "sname", "grp", "date")) %>%
  mutate(n_adult_mat_kin = mom_present + n_adult_sisters + n_adult_daughters) %>%
  select(sid, sname, grp, date, n_adult_mat_kin)

gc <- gc %>%
  left_join(maternal_kin, by = c("sid", "sname", "grp", "date"))


# gc-percent-cycling ------------------------------------------------------

## Percent of time cycling in previous year

repstats <- collect(tbl(babase, "repstats_grp"))
repstats$state <- factor(repstats$state, labels = c("Cycling", "Lactating", "Pregnant"))

# repstat on gc collection date
gc_repstats <- repstats %>%
  semi_join(gc, by = c("sname", "grp", "date"))

# Create 1-yr intervals
gc_repstats <- gc_repstats %>%
  mutate(start = date - years(1) + days(1))

temp <- gc_repstats %>%
  select(sname, start, end = date, c_state = state) %>%
  inner_join(select(repstats, sname, date, state), by = "sname") %>%
  filter(date >= start & date <= end)

pct_cycling <- temp %>%
  group_by(sname, start, end, state, .drop = FALSE) %>%
  tally(name = "n_days") %>%
  ungroup() %>%
  group_by(sname, start, end) %>%
  mutate(percent_days = n_days / sum(n_days)) %>%
  filter(state == "Cycling") %>%
  ungroup() %>%
  rename(n_days_cycling = n_days, percent_days_cycling = percent_days)

gc <- gc %>%
  left_join(select(pct_cycling, sname, percent_days_cycling, date = end),
            by = c("sname", "date"))


# gc-percent-infant-3mo ---------------------------------------------------

gc_mom_snames <- gc %>%
  filter(sex == "F") %>%
  pull(sname) %>%
  unique()

kids <- biograph_l %>%
  drop_na(sname) %>%
  select(kid_sname = sname, kid_pid = pid, kid_birth = birth) %>%
  mutate(mom_sname = str_sub(kid_pid, 1, 3)) %>%
  filter(mom_sname %in% gc_mom_snames)

kid_snames <- unique(kids$kid_sname)

kid_members <- biograph_l %>%
  filter(sname %in% kid_snames) %>%
  inner_join(members_l2) %>%
  select(kid_sname = sname, kid_birth = birth, date)

kid_members <- kid_members %>%
  filter(date <= kid_birth + months(3)) %>%
  left_join(select(kids, kid_sname, mom_sname))

gc_moms <- gc %>%
  select(sname, sex, date) %>%
  filter(sex == "F") %>%
  mutate(start = date - years(1) + days(1),) %>%
  rename(end = date) %>%
  distinct()

gc_moms_days_present <- gc_moms %>%
  inner_join(select(members_l2, sname, sex, date)) %>%
  filter(date >= start & date <= end) %>%
  group_by(sname, sex, start, end) %>%
  tally(name = "days_present")

gc_mom_infant_days <- gc_moms %>%
  left_join(select(kid_members, kid_sname, sname = mom_sname, date)) %>%
  filter(date >= start & date <= end) %>%
  group_by(sname, start, end, kid_sname) %>%
  tally(name = "days_with_infant")

# There are some cases where the female had multiple infants in same year
# Combine these by taking sum of days
gc_mom_infant_days <- gc_mom_infant_days %>%
  group_by(sname, start, end) %>%
  summarise(days_with_infant = sum(days_with_infant))

# Merge these to calculate percent days
pct_infant <- gc_moms_days_present %>%
  left_join(gc_mom_infant_days) %>%
  replace_na(list(days_with_infant = 0)) %>%
  mutate(percent_days_with_infant = days_with_infant / days_present) %>%
  ungroup()

gc <- gc %>%
  left_join(select(pct_infant, sname, percent_days_with_infant, date = end),
            by = c("sname", "date"))


# gc-cov-proprank ---------------------------------------------------------

# The subject's proportional dominance rank in the month the sample was collected.

ranks <- tbl(babase, "ranks")

ranks_l <- ranks %>%
  filter(rnktype %in% c("ADF", "ADM")) %>%
  collect() %>%
  group_by(grp, rnkdate, rnktype) %>%
  mutate(proprank = 1 - (rank - 1) / (n() - 1)) %>%
  make_date_cols(rnkdate) %>%
  ungroup()

# Proprank is NaN if there's only one individual of that age/sex class in the group
ranks_l <- replace_na(ranks_l, list(proprank = 1))

gc_rank <- gc %>%
  select(sname, grp, date) %>%
  make_date_cols(date) %>%
  left_join(ranks_l, by = c("year_of", "month_of", "sname", "grp")) %>%
  distinct()

gc <- gc %>%
  left_join(select(gc_rank, sname, grp, date, proprank), by = c("sname", "grp", "date"))


# gc-cov-dsi --------------------------------------------------------------

# The mediators should be:
# The subject's DSI-F in the year prior to sample collection. For subjects who are age 4, this mean that some of the grooming
# interactions would occur when the subject is age 3.
#
# The subject's DSI-M in the year prior to sample collection. For subjects who are age 4, this mean that some of the grooming
# interactions would occur when the subject is age 3. Note this can only be calculated for female subjects.

gcd <- make_target_date_df(select(gc, -birth), babase, members_l2, .adults_only = FALSE)



######### WARNING WARNING WARNING WARNING WARNING WARNING
#
# This one takes several days to finish
# And may require > 16 GB of memory!
gc_dsi <- dyadic_index(gcd, biograph_l, members_l2, focals_l, females_l,
                        grooming_l2, min_cores_days = 1, within_grp = FALSE,
                        parallel = TRUE, directional = FALSE)
#
######### WARNING WARNING WARNING WARNING WARNING WARNING



gc_dsi_summary <- dyadic_index_summary(gc_dsi)

# For animals in multiple groups during a given year,
# average of DSI in each group weighted by days_present.
# Also remove any DSI values based on fewer than 60 days present.
gc_dsi_wt_avg <- gc_dsi_summary %>%
  filter(days_present >= 60) %>%
  mutate_at(vars(starts_with("DSI")), funs(wt = . * days_present)) %>%
  group_by(sname, obs_date) %>%
  summarise_at(vars(contains("_wt")),
               list(~ sum(., na.rm = TRUE) /
                      sum(days_present * ifelse(is.na(.), NA, 1), na.rm = TRUE))) %>%
  ungroup() %>%
  mutate_at(vars(contains("_wt")), list(~ replace_na(., NA))) %>%
  rename(date = obs_date, DSI_F = DSI_F_wt, DSI_M = DSI_M_wt)

gc <- gc %>%
  left_join(select(gc_dsi_wt_avg, sname, date, DSI_F, DSI_M),
            by = c("sname", "date"))

saveRDS(gc_dsi, paste0("data/gc_dsi_", Sys.Date(), ".RDS"))
saveRDS(gc_dsi_summary, paste0("data/gc_dsi_summary_", Sys.Date(), ".RDS"))





# gc-cov-sci --------------------------------------------------------------

# The subject's SCI-F in the year prior to sample collection. For subjects who are age 4, this mean that some of the grooming
# interactions would occur when the subject is age 3.
#
# The subject's SCI-M in the year prior to sample collection. For subjects who are age 4, this mean that some of the grooming
# interactions would occur when the subject is age 3. Note this can only be calculated for female subjects.
#
gc_sci <- sci(gcd, members_l2, focals_l, females_l, grooming_l2,
              min_res_days = 60, parallel = TRUE)

gc_sci_summary <- gc_sci %>%
  select(sname, grp, days_present, obs_date, SCI_F, SCI_M)

# For animals in multiple groups during a given year,
# average of SCI in each group weighted by days_present.
# This dataset is already restricted to >= 60 days present.
gc_sci_wt_avg <- gc_sci_summary %>%
  mutate_at(vars(starts_with("SCI")), funs(wt = . * days_present)) %>%
  group_by(sname, obs_date) %>%
  summarise_at(vars(contains("_wt")),
               list(~ sum(., na.rm = TRUE) /
                      sum(days_present * ifelse(is.na(.), NA, 1), na.rm = TRUE))) %>%
  ungroup() %>%
  mutate_at(vars(contains("_wt")), list(~ replace_na(., NA))) %>%
  rename(date = obs_date, SCI_F = SCI_F_wt, SCI_M = SCI_M_wt)

gc <- gc %>%
  left_join(gc_sci_wt_avg, by = c("sname", "date"))

saveRDS(gc_sci, paste0("data/gc_sci_", Sys.Date(), ".RDS"))


# ---- WRITE-GC-COVARIATES-FILE -------------------------------------------

write_csv(gc, paste0("data/gc_covariates_", Sys.Date(), ".csv"))

subjects_small <- filter(subjects, sname %in% gc$sname)
write_csv(subjects_small, paste0("data/early-adversity-subjects_", Sys.Date(), ".csv"))
