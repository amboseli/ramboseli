
#' Obtain a subset of members table that excludes behavioral observation gaps.
#'
#' @param babase A DBI connection to the babase database
#'
#' @return A subset of the members table that excludes behavioral observation gaps.
#' @export
#'
#' @examples
subset_members <- function(babase) {

  if (class(babase) != "PostgreSQLConnection") {
    stop("Invalid connection to babase.")
  }

  # babase-tables -----------------------------------------------------------

  message("Creating connections to babase tables...")

  # Database connections
  biograph <- dplyr::tbl(babase, "biograph")
  maturedates <- dplyr::tbl(babase, "maturedates")
  rankdates <- dplyr::tbl(babase, "rankdates")
  groups_history <- dplyr::tbl(babase, "groups_history")
  behave_gaps <- dplyr::tbl(babase, "behave_gaps")
  members <- dplyr::tbl(babase, "members")

  md_females <- maturedates %>%
    dplyr::semi_join(dplyr::filter(biograph, sex == "F"), by = "sname")

  rd_males <- rankdates %>%
    dplyr::semi_join(dplyr::filter(biograph, sex == "M"), by = "sname")


  # For each animal/year-of-life, find all groups that the animal was a member of
  # members-query -----------------------------------------------------------

  message("Obtaining members data from all adult females (matured) and males (ranked)...")

  members_keep <- members %>%
    dplyr::inner_join(dplyr::select(biograph, sname, sex), by = "sname") %>%
    dplyr::left_join(dplyr::select(md_females, sname, matured), by = "sname") %>%
    dplyr::left_join(dplyr::select(rd_males, sname, ranked), by = "sname") %>%
    dplyr::inner_join(dplyr::select(groups_history, gid, permanent, impermanent, last_reg_census),
                      by = c("grp" = "gid")) %>%
    dplyr::filter(grp < 3 & grp != 1.3 & grp != 1.4 & date >= permanent &
                    (is.na(impermanent) | date <= impermanent) &
                    (is.na(last_reg_census) | date <= last_reg_census) &
                    ((sex == "F" & date >= matured) | (sex == "M" & date >= ranked)))

  # Find behavior gaps that overlap records in members_keep
  bg <- members_keep %>%
    dplyr::left_join(behave_gaps, by = "grp") %>%
    dplyr::filter(date >= gap_start & date <= gap_end)

  # Exclude those records using anti_join
  members_keep <- dplyr::anti_join(members_keep, bg, by = "membid") %>%
    dplyr::select(grp, sname, date, sex, matured, ranked) %>%
    dplyr::collect()

  message("Dealing with grooming on first-of-month issue...")

  # Second part
  members_remove <- members %>%
    dplyr::inner_join(dplyr::select(biograph, sname, sex), by = "sname") %>%
    dplyr::left_join(dplyr::select(md_females, sname, matured), by = "sname") %>%
    dplyr::left_join(dplyr::select(rd_males, sname, ranked), by = "sname") %>%
    dplyr::select(grp, sname, date, sex, matured, ranked) %>%
    dplyr::collect()

  members_remove <- members_remove %>%
    dplyr::filter(lubridate::day(date) == 1)


  # members-data-manip ------------------------------------------------------

  members_keep$yearmon <- as.character(zoo::as.yearmon(members_keep$date))
  members_remove$yearmon <- as.character(zoo::as.yearmon(members_remove$date))

  ## Check how many days an individual is present in a month
  members_keep <- members_keep %>%
    dplyr::group_by(sname, yearmon) %>%
    dplyr::mutate(month_days_present = n())

  members_keep$check <- "keep"
  members_remove$check <- "remove"

  members_l <- dplyr::bind_rows(members_keep, members_remove)

  ## How many groups per month/year
  members_l <- members_l %>%
    dplyr::group_by(sname, yearmon) %>%
    dplyr::mutate(yearmon_num_grp = n_distinct(grp))

  ## Check in what group an individual lives in  on the first of the month.
  members_l <- members_l %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(dplyr::select(members_remove, first_of_month_grp = grp, sname, yearmon),
                      by = c("sname", "yearmon")) %>%
    tidyr::drop_na(month_days_present)

  ## Make a column with all the groups an individual lives in during each yearmon
  members_l <- members_l %>%
    dplyr::group_by(sname, yearmon) %>%
    dplyr::mutate(grps = paste(sort(unique(grp)), collapse = " ")) %>%
    dplyr::ungroup()

  return(members_l)
}


#' Obtain a subset of focal samples that excludes behavioral observation gaps.
#'
#' @param babase A DBI connection to the babase database
#' @param members_l A subset of members table produced by the function 'subset_members'
#'
#' @return A subset of focal samples that excludes behavioral observation gaps.
#' @export
#'
#' @examples
subset_focals <- function(babase, members_l) {

  if (class(babase) != "PostgreSQLConnection") {
    stop("Invalid connection to babase.")
  }

  # babase-tables -----------------------------------------------------------

  message("Creating connections to babase tables...")

  # Database connections
  biograph <- dplyr::tbl(babase, "biograph")
  maturedates <- dplyr::tbl(babase, "maturedates")
  samples <- dplyr::tbl(babase, "samples")
  point_data <- dplyr::tbl(babase, "point_data")
  altos_points_84to96mar3 <- dplyr::tbl(babase, dbplyr::in_schema("babase_pending", "altos_points_84to96mar3"))
  altos_points_96marto99 <- dplyr::tbl(babase, dbplyr::in_schema("babase_pending", "altos_points_96marto99"))
  hooks_points_84to96 <- dplyr::tbl(babase, dbplyr::in_schema("babase_pending", "hooks_points_84to96"))
  hooks_points_96to99 <- dplyr::tbl(babase, dbplyr::in_schema("babase_pending", "hooks_points_96to99"))

  # Local
  biograph_l <- dplyr::collect(biograph)
  maturedates_l <- dplyr::collect(maturedates)

  # focals ------------------------------------------------------------------

  message("Finding all focal samples...")

  focals <- samples %>%
    dplyr::inner_join(point_data, by = "sid") %>%
    dplyr::group_by(date, grp, sname) %>%
    dplyr::summarise(n_samples = n_distinct(sid)) %>%
    dplyr::collect()

  focals_altos_84to96 <- altos_points_84to96mar3 %>%
    dplyr::group_by(date, grp, sname) %>%
    dplyr::summarise(n_samples = n_distinct(time)) %>%
    dplyr::collect()

  focals_altos_96to99 <- altos_points_96marto99 %>%
    dplyr::group_by(date, grp, sname) %>%
    dplyr::summarise(n_samples = n_distinct(time)) %>%
    dplyr::collect()

  focals_hooks_84to96 <- hooks_points_84to96 %>%
    dplyr::group_by(date, grp, sname) %>%
    dplyr::summarise(n_samples = n_distinct(time)) %>%
    dplyr::collect()

  focals_hooks_96to99 <- hooks_points_96to99 %>%
    dplyr::group_by(date, grp, sname) %>%
    dplyr::summarise(n_samples = n_distinct(time)) %>%
    dplyr::collect()

  focals <- dplyr::bind_rows(focals, focals_altos_84to96, focals_altos_96to99,
                             focals_hooks_84to96, focals_hooks_96to99)

  focals_l <- focals %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(biograph_l, by = "sname") %>%
    dplyr::filter(sex == "F") %>%
    dplyr::inner_join(maturedates_l, by = "sname") %>%
    dplyr::filter(matured <= date) %>%
    dplyr::group_by(date, grp) %>%
    dplyr::summarise(sum = sum(n_samples)) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup()

  ## Restrict the focal count data to the same date restrictions of members
  focals_l <- focals_l %>%
    dplyr::semi_join(members_l, by = c("grp", "date"))

  return(focals_l)

}


#' Obtain a subset of female count data that excludes behavioral observation gaps.
#'
#' @param members_l A subset of members table produced by the function 'subset_members'
#'
#' @return A subset of female count data that excludes behavioral observation gaps.
#' @export
#'
#' @examples
subset_females <- function(members_l) {

  message("Finding all females samples...")

  ## Get a count of number of adult females per day in a grp
  females_l <- members_l %>%
    dplyr::filter(sex == "F") %>%
    dplyr::group_by(grp, date) %>%
    dplyr::summarise(nr_females = n()) %>%
    dplyr::ungroup()

  return(females_l)

}


#' Obtain a subset of grooming data that excludes behavioral observation gaps.
#'
#' @param babase A DBI connection to the babase database
#' @param members_l A subset of members table produced by the function 'subset_members'
#'
#' @return A subset of grooming data that excludes behavioral observation gaps.
#' @export
#'
#' @examples
subset_grooming <- function(babase, members_l) {

  if (class(babase) != "PostgreSQLConnection") {
    stop("Invalid connection to babase.")
  }

  # babase-tables -----------------------------------------------------------

  message("Creating connections to babase tables...")

  # Database connections
  biograph <- dplyr::tbl(babase, "biograph")
  maturedates <- dplyr::tbl(babase, "maturedates")
  actor_actees <- dplyr::tbl(babase, "actor_actees")
  rankdates <- dplyr::tbl(babase, "rankdates")

  # Local
  maturedates_l <- dplyr::collect(maturedates)
  rankdates_l <- dplyr::collect(rankdates)

  # grooming-interactions-query ---------------------------------------------

  message("Obtaining and validating grooming data...")

  grooming <- actor_actees %>%
    dplyr::inner_join(dplyr::select(biograph, sname, actor_sex = sex),
                      by = c("actor" = "sname")) %>%
    dplyr::inner_join(dplyr::select(biograph, sname, actee_sex = sex),
                      by = c("actee" = "sname")) %>%
    dplyr::filter(act == "G" & actee_grp != 1.3 & actee_grp != 1.4 &
                    (actee_grp < 3 | actee_grp == 9) &
                    actor_grp != 1.3 & actor_grp != 1.4 & (actor_grp < 3 | actor_grp == 9)) %>%
    dplyr::collect()

  grooming <- grooming %>%
    dplyr::left_join(dplyr::select(maturedates_l, sname, actor_matured = matured),
                     by = c("actor" = "sname")) %>%
    dplyr::left_join(dplyr::select(maturedates_l, sname, actee_matured = matured),
                     by = c("actee" = "sname")) %>%
    dplyr::left_join(dplyr::select(rankdates_l, sname, actor_ranked = ranked),
                     by = c("actor" = "sname")) %>%
    dplyr::left_join(dplyr::select(rankdates_l, sname, actee_ranked = ranked),
                     by = c("actee" = "sname"))

  grooming <- grooming %>%
    dplyr::filter(((actor_sex == "F" & date >= actor_matured) |
                     (actor_sex == "M" & date >= actor_ranked)) &
                    ((actee_sex == "F" & date >= actee_matured) |
                       (actee_sex == "M" & date >= actee_ranked)))

  grooming <- grooming %>%
    dplyr::select(iid, sid, act, date, actor, actor_grp, actee, actee_grp,
                  actor_sex, actee_sex)

  grooming$yearmon <- as.character(zoo::as.yearmon(grooming$date))


  # grooming-data-manip -----------------------------------------------------

  ## Check grooming data for potential issues with 1st of month issue
  ## This problem is being fixed and this code will no longer be needed oncees the historic grooming data has been solved.
  ## Yearmon are unique year and month combinations, e.g. MAR2000 or APR2012
  ## The check is done per sname and yearmon combination.

  ## Potential issues in the grooming interactions are all grooming that took place prior to Jul 2006.
  potential_issues <- members_l %>%
    dplyr::filter(first_of_month_grp != grp & date < '2006-07-01')

  ## There is no issue if an individual is only present in 1 group for the whole months as 1st of the month group will be the same as the whole month group
  ## There is also no issue if an individual is present in 1 monitored group (<3) and one observed group, because grooming is only recorded in certain groups.
  no_issue_sname_yearmon <- potential_issues %>%
    dplyr::filter(first_of_month_grp > 8.9 & yearmon_num_grp == 2) %>%
    dplyr::mutate(grp = suppressWarnings(as.numeric(grps))) %>%
    dplyr::select(sname, yearmon, grp) %>%
    dplyr::distinct(sname, yearmon, grp)

  ## There is an issue if an indvidual is in more than two monitored groups.
  issue_sname_yearmon <- potential_issues %>%
    dplyr::filter((first_of_month_grp > 8.9 & yearmon_num_grp > 2) |
                    (first_of_month_grp < 9 & yearmon_num_grp > 1)) %>%
    dplyr::mutate(grp = suppressWarnings(as.numeric(grps))) %>%
    dplyr::select(sname, yearmon, grp) %>%
    dplyr::distinct(sname, yearmon, grp)

  no_issue_actor <- no_issue_sname_yearmon %>%
    dplyr::rename(actor = sname, no_issue_actor_grp = grp)
  no_issue_actee <- no_issue_sname_yearmon %>%
    dplyr::rename(actee = sname, no_issue_actee_grp = grp)
  issue_actor <- issue_sname_yearmon %>%
    dplyr::rename(actor = sname, issue_actor_grp = grp)
  issue_actee <- issue_sname_yearmon %>%
    dplyr::rename(actee = sname, issue_actee_grp = grp)

  issue_actor$flag_issue_actor <- 1
  issue_actee$flag_issue_actee <- 1
  no_issue_actor$flag_no_issue_actor <- 1
  no_issue_actee$flag_no_issue_actee <- 1

  grooming_l <- grooming %>%
    dplyr::left_join(no_issue_actor, by = c("actor", "yearmon")) %>%
    dplyr::left_join(no_issue_actee, by = c("actee", "yearmon")) %>%
    dplyr::left_join(issue_actor, by = c("actor", "yearmon")) %>%
    dplyr::left_join(issue_actee, by = c("actee", "yearmon"))

  grooming_l <- grooming_l %>%
    dplyr::mutate(actor_grp = dplyr::case_when(
      (is.na(flag_no_issue_actor) & is.na(flag_issue_actor) & actor_grp > 3) ~ actee_grp,
      (is.na(flag_no_issue_actor) & is.na(flag_issue_actor)) ~ actor_grp,
      !is.na(flag_no_issue_actor) ~ no_issue_actor_grp,
      (is.na(flag_no_issue_actor) & !is.na(flag_issue_actor) &
         is.na(flag_no_issue_actee) & is.na(flag_issue_actee)) ~ actee_grp,
      (is.na(flag_no_issue_actor) & !is.na(flag_issue_actor) &
         !is.na(flag_no_issue_actee)) ~ no_issue_actee_grp,
      (is.na(flag_no_issue_actor) & !is.na(flag_issue_actor) &
         !is.na(flag_issue_actee)) ~ 991,
      TRUE ~ 992
    ))

  grooming_l <- grooming_l %>%
    dplyr::mutate(actee_grp = dplyr::case_when(
      (is.na(flag_no_issue_actee) & is.na(flag_issue_actee) & actee_grp > 3) ~ actor_grp,
      (is.na(flag_no_issue_actee) & is.na(flag_issue_actee)) ~ actee_grp,
      !is.na(flag_no_issue_actee) ~ no_issue_actee_grp,
      (is.na(flag_no_issue_actee) & !is.na(flag_issue_actee) &
         is.na(flag_no_issue_actor) & is.na(flag_issue_actor)) ~ actor_grp,
      (is.na(flag_no_issue_actee) & !is.na(flag_issue_actee) &
         !is.na(flag_no_issue_actor)) ~ no_issue_actor_grp,
      (is.na(flag_no_issue_actee) & !is.na(flag_issue_actee) &
         !is.na(flag_issue_actor)) ~ 993,
      TRUE ~ 994
    ))

  grooming_l <- grooming_l %>%
    dplyr::select(iid, sid, act, actor, actee, actor_sex, actee_sex, date, yearmon,
                  actor_grp, actee_grp)

  ## Restrict the grooming data to the same data restrictions of members
  grp_dates <- dplyr::distinct(members_l, grp, date)

  temp1 <- grooming_l %>%
    dplyr::inner_join(grp_dates, by = c("date", "actee_grp" = "grp"))

  temp2 <- grooming_l %>%
    dplyr::inner_join(grp_dates, by = c("date", "actor_grp" = "grp"))

  grooming_l <- dplyr::bind_rows(temp1, temp2) %>%
    dplyr::distinct(iid, .keep_all = TRUE)

  # NOTE: Duplicated rows not removed in original code

  return(grooming_l)

}


#' Obtain a subset of agonism data that excludes behavioral observation gaps.
#'
#' @param babase A DBI connection to the babase database
#' @param members_l A subset of members table produced by the function 'subset_members'
#'
#' @return A subset of agonsism data that excludes behavioral observation gaps.
#' @export
#'
#' @examples
subset_agonism <- function(babase, members_l) {

  if (class(babase) != "PostgreSQLConnection") {
    stop("Invalid connection to babase.")
  }

  # babase-tables -----------------------------------------------------------

  message("Creating connections to babase tables...")

  # Database connections
  biograph <- dplyr::tbl(babase, "biograph")
  maturedates <- dplyr::tbl(babase, "maturedates")
  actor_actees <- dplyr::tbl(babase, "actor_actees")
  rankdates <- dplyr::tbl(babase, "rankdates")

  # Local
  maturedates_l <- dplyr::collect(maturedates)
  rankdates_l <- dplyr::collect(rankdates)


  # agonism-interactions-query ----------------------------------------------

  message("Obtaining and validating agonism data...")

  agonism <- actor_actees %>%
    dplyr::inner_join(dplyr::select(biograph, sname, actor_sex = sex),
                      by = c("actor" = "sname")) %>%
    dplyr::inner_join(dplyr::select(biograph, sname, actee_sex = sex),
                      by = c("actee" = "sname")) %>%
    dplyr::filter(act %in% c("AS", "OS", "DS", "A") & actee_grp != 1.3 & actee_grp != 1.4 &
                    (actee_grp < 3 | actee_grp == 9) &
                    actor_grp != 1.3 & actor_grp != 1.4 & (actor_grp < 3 | actor_grp == 9)) %>%
    dplyr::collect()

  agonism <- agonism %>%
    dplyr::left_join(dplyr::select(maturedates_l, sname, actor_matured = matured),
                     by = c("actor" = "sname")) %>%
    dplyr::left_join(dplyr::select(maturedates_l, sname, actee_matured = matured),
                     by = c("actee" = "sname")) %>%
    dplyr::left_join(dplyr::select(rankdates_l, sname, actor_ranked = ranked),
                     by = c("actor" = "sname")) %>%
    dplyr::left_join(dplyr::select(rankdates_l, sname, actee_ranked = ranked),
                     by = c("actee" = "sname"))

  agonism <- agonism %>%
    dplyr::filter(((actor_sex == "F" & date >= actor_matured) |
                     (actor_sex == "M" & date >= actor_ranked)) &
                    ((actee_sex == "F" & date >= actee_matured) |
                       (actee_sex == "M" & date >= actee_ranked)))

  agonism <- agonism %>%
    dplyr::select(iid, sid, act, date, actor, actor_grp, actee, actee_grp,
                  actor_sex, actee_sex)

  agonism$yearmon <- as.character(zoo::as.yearmon(agonism$date))


  # agonism-data-manip ------------------------------------------------------

  agonism <- agonism %>%
    dplyr::select(iid, sid, act, actor, actee, actor_sex, actee_sex, date, yearmon,
                  actor_grp, actee_grp)

  ## Restrict the grooming data to the same data restrictions of members
  grp_dates <- dplyr::distinct(members_l, grp, date)

  temp1 <- agonism %>%
    dplyr::inner_join(grp_dates, by = c("date", "actee_grp" = "grp"))

  temp2 <- agonism %>%
    dplyr::inner_join(grp_dates, by = c("date", "actor_grp" = "grp"))

  agonism <- dplyr::bind_rows(temp1, temp2) %>%
    dplyr::distinct(iid, .keep_all = TRUE)

  # Remove trailing space for act "A "
  agonism$act <- str_trim(agonism$act)

  return(agonism)

}


#' Create a data frame for each adult individual year of life
#'
#' @param babase A DBI connection to the babase database
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param focals_l A subset of focals produced by the function 'subset_focals'
#' @param grooming_l A subset of grooming data produced by the function 'subset_grooming'
#'
#' @return A tibble with one row per animal and year of life, with contextual data
#' @export
#'
#' @examples
make_iyol <- function(babase, members_l, focals_l, grooming_l) {

  if (class(babase) != "PostgreSQLConnection") {
    stop("Invalid connection to babase.")
  }
  # babase-tables -----------------------------------------------------------

  message("Creating connections to babase tables...")

  # Database connections
  biograph <- dplyr::tbl(babase, "biograph")
  maturedates <- dplyr::tbl(babase, "maturedates")
  rankdates <- dplyr::tbl(babase, "rankdates")

  # Local
  biograph_l <- dplyr::collect(biograph)

  md_females <- maturedates %>%
    dplyr::semi_join(dplyr::filter(biograph, sex == "F"), by = "sname")

  rd_males <- rankdates %>%
    dplyr::semi_join(dplyr::filter(biograph, sex == "M"), by = "sname")

  # Find last date
  last_date <- min(max(members_l$date), max(focals_l$date),
                   max(grooming_l$date))


  # individ-year-of-life ----------------------------------------------------

  message("Creating individual-year-of-life data set...")

  iyol <- biograph %>%
    dplyr::left_join(dplyr::select(md_females, sname, matured), by = "sname") %>%
    dplyr::left_join(dplyr::select(rd_males, sname, ranked), by = "sname") %>%
    dplyr::select(sname, sex, birth, statdate, matured, ranked) %>%
    dplyr::collect()

  iyol <- iyol %>%
    dplyr::mutate(first_start_date = dplyr::case_when(
      sex == "F" ~ matured,
      sex == "M" ~ ranked
    )) %>%
    tidyr::drop_na(first_start_date) %>%
    dplyr::select(sname, sex, birth, first_start_date, statdate, -ranked, -matured)

  make_bday_seq <- function(df) {
    res <- as.character(seq(df$birth, df$statdate, by = "year"))
    return(res)
  }

  iyol <- iyol %>%
    purrrlyr::by_row(make_bday_seq, .collate = "rows") %>%
    dplyr::select(-.row) %>%
    dplyr::rename(birth_dates  = .out) %>%
    dplyr::mutate(birth_dates = lubridate::ymd(birth_dates))

  iyol <- iyol %>%
    dplyr::filter(lubridate::year(birth_dates) >= lubridate::year(first_start_date) - 1)

  iyol <- iyol %>%
    dplyr::mutate(start = dplyr::case_when(
      first_start_date >= birth_dates ~ first_start_date,
      TRUE ~ birth_dates))

  iyol <- iyol %>%
    dplyr::mutate(end = birth_dates + lubridate::years(1) - lubridate::days(1))

  iyol <- iyol %>%
    dplyr::mutate(end = dplyr::case_when(
      is.na(end) ~ birth_dates - lubridate::days(1) + lubridate::years(1),
      end > statdate ~ statdate,
      TRUE ~ end))

  iyol[iyol$end > last_date, ]$end <- last_date

  iyol <- iyol %>%
    dplyr::filter(start <= end)

  # NOTE: Lots of extra rows with nonsensical dates in original xdata
  # Start date after end date
  # E.g., for sname == CLE
  # Likely to be silently dropped later, so maybe not a problem

  ## Check in which groups the individual was present in the focal year
  ## and create one row per focal year per group
  temp <- iyol %>%
    dplyr::left_join(dplyr::select(members_l, sname, date, grp), by = c("sname")) %>%
    dplyr::filter(date >= start & date <= end) %>%
    dplyr::distinct(sname, start, grp)

  zdata <- iyol %>%
    dplyr::inner_join(temp, by = c("sname", "start"))

  ## And check how many days the focal was present in the group in a focal year
  zdata <- zdata %>%
    dplyr::inner_join(dplyr::select(members_l, sname, grp, date), by = c("sname", "grp")) %>%
    dplyr::filter(date >= start & date <= end) %>%
    dplyr::group_by(sname, grp, start, end) %>%
    dplyr::summarise(days_present = n()) %>%
    dplyr::arrange(sname, grp, start, end)

  iyol <- zdata %>%
    dplyr::inner_join(iyol, by = c("sname", "start", "end")) %>%
    dplyr::arrange(sname, grp, start, end)

  iyol <- iyol %>%
    dplyr::mutate(midpoint = start + floor((end - start) / 2),
                  age_start_yrs = (lubridate::interval(birth, start) %/% lubridate::days(1)) / 365.25,
                  age_class = floor(plyr::round_any(age_start_yrs, 0.01)) + 1)

  # NOTES:
  # Original scipt produces error in dates for RUT
  # One day difference for AGE due to weirdness with interpolation
  # - AGE has one entry in members after statdate
  # tmp1 <- zdata %>%
  #   tbl_df %>%
  #   dplyr::mutate(grp = as.double(grp))
  # setdiff(select(tmp1, -end), select(iyol, -end))

  iyol <- dplyr::ungroup(iyol)

  return(iyol)
}


#' Calculate DSI variables from individual-year-of-life data
#'
#' @param my_iyol Individual-year-of-life data.
#' @param biograph_l A local copy of the biograph table
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param focals_l A subset of focals produced by the function 'subset_focals'
#' @param females_l A subset of female counts produced by the function 'subset_females'
#' @param grooming_l A subset of grooming data produced by the function 'subset_grooming'
#' @param min_cores_days The minimum number of coresidence days needed for dyad to be included. Defaults to 60 days.
#' @param within_grp Logical value indicating whether regressions should be fit relative to dyads in entire population (default) or to dyads within-group
#'
#' @return The input data with an additional list columsn containing the full DSI subset and the focal DSI variables.
#' @export
#'
#' @examples
dsi <- function(my_iyol, biograph_l, members_l, focals_l, females_l, grooming_l,
                min_cores_days = 60, within_grp = FALSE, parallel = FALSE,
                ncores = NULL) {

  ptm <- proc.time()

  # Return an empty tibble if the subset is empty
  if (is.null(my_iyol) |
      !all(names(my_iyol) %in% c("sname", "grp", "start", "end", "days_present", "sex",
                                 "birth", "first_start_date", "statdate", "birth_dates",
                                 "midpoint", "age_start_yrs", "age_class")) |
      min_cores_days < 0) {
    stop("Problem with input data. Use the 'make_iyol' function to create the input.")
  }

  if (parallel) {
    avail_cores <- detectCores()
    if (!is.null(ncores)) {
      if (ncores > avail_cores) {
        message(paste0("Ignoring 'ncores' argument because only ", avail_cores,
                       " cores are available."))
        ncores <- avail_cores
      }
    }
    else {
      message(paste0("Using all available cores: ", avail_ncores,
                     ". Use 'ncores' to specify number of cores to use."))
      ncores <- avail_cores
    }

    cl <- makeCluster(ncores)
    registerDoSNOW(cl)
    pb <- txtProgressBar(min = 0, max = nrow(my_iyol), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    subset <- foreach(i = 1:nrow(my_iyol), .options.snow = opts,
                       .packages = c('tidyverse')) %dopar% {
                         get_dyadic_subset(my_iyol[i, ], biograph_l,
                                           members_l, focals_l, females_l,
                                           grooming_l, min_cores_days,
                                           within_grp)
                       }
    close(pb)
    stopCluster(cl)
    my_iyol <- add_column(my_iyol, subset)
  }
  else {
    if (!is.null(ncores)) {
      message("Ignoring 'ncores' argument because 'parallel' set to FALSE.")
    }
    my_iyol$subset <- list(NULL)
    pb <- txtProgressBar(min = 0, max = nrow(my_iyol), style = 3) # Progress bar
    for (i in 1:nrow(my_iyol)) {
      my_iyol[i, ]$subset <- list(get_dyadic_subset(my_iyol[i, ], biograph_l,
                                                    members_l, focals_l, females_l,
                                                    grooming_l, min_cores_days,
                                                    within_grp))
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }

  my_iyol <- my_iyol %>%
    dplyr::mutate(dsi = purrr::pmap(list(sname, grp, subset), get_focal_dsi))

  tdiff <- (proc.time() - ptm)["elapsed"] / 60
  message(paste0("Elapsed time: ", round(tdiff, 3), " minutes (",
                 round(tdiff / 60, 3), ") hours."))

  return(my_iyol)
}


#' Obtain dyadic grooming subsets for the animal's year of life
#'
#' @param df One row individual-year-of-life data
#' @param biograph_l A local copy of the biograph table
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param focals_l A subset of focals produced by the function 'subset_focals'
#' @param females_l A subset of female counts produced by the function 'subset_females'
#' @param grooming_l A subset of grooming data produced by the function 'subset_grooming'
#' @param min_cores_days The minimum number of coresidence days needed for dyad to be included. Defaults to 60 days.
#' @param within_grp Logical value indicating whether regressions should be fit relative to dyads in entire population (default) or to dyads within-group
#'
#' @return The input row with an additional list column containing the subset
#'
#' @examples
get_dyadic_subset <- function(df, biograph_l, members_l, focals_l, females_l,
                              grooming_l, min_cores_days = 60, within_grp = FALSE) {

  # Find and return all co-residence dates for focal_sname and partner_sname in my_members
  get_overlap_dates <- function(focal_sname, partner_sname, focal_grp, partner_grp) {

    focal_dates <- my_members %>%
      dplyr::filter(sname == focal_sname & grp == focal_grp) %>%
      dplyr::pull(date)

    partner_dates <- my_members %>%
      dplyr::filter(sname == partner_sname & grp == partner_grp) %>%
      dplyr::pull(date)

    overlap_dates <- dplyr::intersect(focal_dates, partner_dates)

    return(overlap_dates)
  }

  # Return total count of focals during co-residence dates
  get_focal_counts <- function(coresidence_dates, focal_grp) {

    res <- my_focals %>%
      dplyr::filter(date %in% coresidence_dates & grp == focal_grp)

    return(sum(res$sum))
  }

  # Return average number of females present in grp during co-residence dates
  get_female_counts <- function(coresidence_dates, focal_grp) {

    res <- my_females %>%
      dplyr::filter(date %in% coresidence_dates & grp == focal_grp)

    return(mean(res$nr_females))
  }

  # Return grooming by actor to actee during co-residence dates
  get_grooming <- function(my_actor, my_actee, focal_grp, partner_grp) {

    res <- my_grooming %>%
      dplyr::filter(actor == my_actor & actor_grp == focal_grp & actee == my_actee & actee_grp == partner_grp)

    return(nrow(res))
  }

  my_grp <- df$grp
  my_sname <- df$sname
  my_start <- df$start
  my_end <- df$end

  # Put some subsets in environment for faster performance
  if (within_grp) {
    my_members <- dplyr::filter(members_l, grp == my_grp & date >= my_start & date <= my_end)
    my_focals <- dplyr::filter(focals_l, grp == my_grp & date >= my_start & date <= my_end)
    my_females <- dplyr::filter(females_l, grp == my_grp & date >= my_start & date <= my_end)
    my_grooming <- grooming_l %>%
      dplyr::filter((actor_grp == my_grp | actee_grp == my_grp) & date >= my_start & date <= my_end)

    # Find all distinct members WITHIN GROUP between start and end dates
    # For each animal in each group durign this time, calculate:
    # number of days present, first date, last date
    my_subset <- my_members %>%
      dplyr::rename(sname_sex = sex) %>%
      dplyr::inner_join(select(df, -sname, -sex), by = c("grp")) %>%
      dplyr::group_by(sname, grp, sname_sex) %>%
      dplyr::summarise(days_present = n(),
                       start = min(date),
                       end = max(date))
  }
  else {
    my_members <- dplyr::filter(members_l, date >= my_start & date <= my_end)
    my_focals <- dplyr::filter(focals_l, date >= my_start & date <= my_end)
    my_females <- dplyr::filter(females_l, date >= my_start & date <= my_end)
    my_grooming <- grooming_l %>%
      dplyr::filter(date >= my_start & date <= my_end)

    # Find all distinct members IN POPULATION between start and end dates
    # For each animal in each group durign this time, calculate:
    # number of days present, first date, last date
    my_subset <- my_members %>%
      dplyr::rename(sname_sex = sex) %>%
      dplyr::group_by(sname, grp, sname_sex) %>%
      dplyr::summarise(days_present = n(),
                       start = min(date),
                       end = max(date))
  }

  # For each of these records, find all possible dyad partners
  # Store as new list column and unnest to expand
  dyads <- my_subset %>%
    dplyr::mutate(partner = list(my_subset$sname[my_subset$sname != sname])) %>%
    tidyr::unnest()

  # Get sex and grp of partner
  # Remove dyads not in same groups
  dyads <- dyads %>%
    dplyr::inner_join(dplyr::select(my_subset, partner = sname,
                                    partner_sex = sname_sex, partner_grp = grp),
                      by = "partner") %>%
    dplyr::filter(grp == partner_grp)

  # Note that the step above created duplicated dyads
  # e.g., sname A and partner B, sname B and partner A
  # If DSI is symmetric, these can be removed
  # That's true here, so remove duplicate dyads
  my_subset <- dyads %>%
    dplyr::rowwise() %>%
    dplyr::mutate(tmp = paste(grp, sort(c(sname, partner)), collapse = '')) %>%
    dplyr::distinct(tmp, .keep_all = TRUE) %>%
    dplyr::select(-tmp) %>%
    dplyr::ungroup()

  # Remove male-male dyads
  my_subset <- my_subset %>%
    dplyr::filter(!(sname_sex == "M" & partner_sex == "M"))

  ## Co-residence dates
  # Find all dates during which focal and partner co-resided in my_grp
  # Get a count of these dates
  my_subset <- my_subset %>%
    dplyr::mutate(coresidence_dates = purrr::pmap(list(sname, partner, grp, partner_grp),
                                                  get_overlap_dates)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(coresidence_days = length(coresidence_dates)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(coresidence_days >= min_cores_days)

  ## Focal counts
  # Get total count of focals during each dyad's co-residence dates
  my_subset <- my_subset %>%
    dplyr::mutate(n_focals = purrr::pmap_dbl(list(coresidence_dates, grp),
                                             get_focal_counts)) %>%
    dplyr::filter(n_focals > 0)

  ## Female counts
  # Get average number of females in group during the dyad's co-residence dates
  my_subset <- my_subset %>%
    dplyr::mutate(n_females = purrr::pmap_dbl(list(coresidence_dates, grp),
                                              get_female_counts)) %>%
    dplyr::filter(n_females > 0)

  # Filter and calculate variables
  my_subset <- my_subset %>%
    dplyr::mutate(OE = (n_focals / n_females) / coresidence_days,
                  log2OE = log2(OE)) %>%
    dplyr::filter(!is.na(OE))

  # Exit if no data meet the criteria, exit without proceeding further
  # Return NULL
  if (nrow(my_subset) == 0) {
    return(dplyr::tbl_df(NULL))
  }

  ## Grooming between dyad during the period of interest
  # Since grooming dates are 1st of month for many years, it does not work
  # to restrict this to co-resident dates only. Instead, use start and end.
  # Co-residence is (usually) implied by the grooming interaction.
  # g_given is grooming given by sname to partner
  # g_received is grooming received by sname from partner
  my_subset <- my_subset %>%
    dplyr::mutate(g_given = purrr::pmap_dbl(list(sname, partner, grp, partner_grp),
                                            get_grooming),
                  g_received = purrr::pmap_dbl(list(partner, sname, partner_grp, grp),
                                               get_grooming),
                  g_total = g_given + g_received)

  # Calculate variables
  my_subset <- my_subset %>%
    dplyr::mutate(g_adj = g_total / coresidence_days,
                  log2_g_adj = log2(g_adj))

  # Classify dyads by dyad type ("F-F" or "F-M"), and nest by dyad type
  my_subset <- my_subset %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dyad = paste(sort(c(sname, partner)), collapse = '-'),
                  dyad_type = paste(sort(c(sname_sex, partner_sex)), collapse = '-')) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dyad_type != "M-M") %>%
    dplyr::group_by(dyad_type) %>%
    tidyr::nest()

  # Fit regression separately for the two dyad types and get residuals
  my_subset <- my_subset %>%
    dplyr::mutate(data = purrr::map(data, fit_dsi_regression)) %>%
    tidyr::unnest()

  # Reorganize columns
  my_subset <- my_subset %>%
    dplyr::select(-coresidence_dates) %>%
    dplyr::select(sname, sname_sex, partner, partner_sex, everything()) %>%
    dplyr::arrange(sname, partner)

  return(my_subset)
}

#' Fit DSI regression on subset of data
#'
#' @param df A subset of data on which to fit a regression of grooming on observer effort.
#'
#' @return The input data with an additional column containing the regression residuals.
#'
#' @examples
fit_dsi_regression <- function(df) {

  # There will be lots of zeros in most subsets
  # If present, remove these before fitting regression model
  zero_subset <- dplyr::filter(df, g_adj == 0)

  if (nrow(zero_subset) > 0) {

    # Fit regression to non-zero values
    nonzero_subset <- dplyr::filter(df, g_adj != 0)
    nonzero_subset$res_g_adj <- as.numeric(residuals(lm(data = nonzero_subset, log2_g_adj ~ log2OE)))

    # Assign a value of -Inf for the residuals of zero values (for calculating quantiles)
    zero_subset$res_g_adj <- -Inf

    # Combine with zero and non-zero subsets
    df <- dplyr::bind_rows(zero_subset, nonzero_subset)
  }
  else {
    df$res_g_adj <- as.numeric(residuals(lm(data = df, log2_g_adj ~ log2OE)))
  }

  return(df)

}


#' Calculate DSI variables for focal animal from a DSI subset
#'
#' @param my_sname
#' @param my_subset
#'
#' @return The input data with an additional list column containing the DSI variables.
#'
#' @examples
get_focal_dsi <- function(my_sname, my_grp, my_subset) {

  # Return an empty tibble if the subset is empty
  if (nrow(my_subset) == 0) {
    return(dplyr::tbl_df(NULL))
  }

  # Calculate 50 and 90 percentiles from full set of residuals
  # This represents all dyads of a given type in the group during the year
  # Including the "zero-grooming" values set to -Inf
  percs <- my_subset %>%
    dplyr::group_by(dyad_type) %>%
    dplyr::filter(g_total > 0) %>%
    dplyr::summarise(perc_50 = quantile(res_g_adj, probs = 0.5),
                     perc_90 = quantile(res_g_adj, probs = 0.9))

  # Add percentile columns to my_subset
  my_subset <- dplyr::left_join(my_subset, percs, by = "dyad_type")

  # The focal DSI subset should contain only dyads that include my_sname
  # This can be in either the sname or partner column
  # Categorize as very strongly bonded, strongly bonded, weakly bonded, or not bonded
  # Also calculate the bond-strength percentile from the
  # empirical cummulative distribution function
  focal_dsi <- my_subset %>%
    dplyr::filter(grp == my_grp & (sname == my_sname | partner == my_sname)) %>%
    dplyr::mutate(bond_strength = dplyr::case_when(
      res_g_adj >= perc_90 ~ "VeryStronglyBonded",
      res_g_adj >= perc_50 ~ "StronglyBonded",
      res_g_adj >= -9999999 ~ "WeaklyBonded",
      TRUE ~ "NotBonded"))

  return(focal_dsi)
}


#' Create summary data from DSI input
#'
#' @param df DSI input data produced by the 'dsi' function.
#'
#' @return An individual-year-of-life data set with summarized DSI variables.
#' @export
#'
#' @examples
dsi_summary <- function(df) {

  # Return an empty tibble if the subset is empty
  if (is.null(df) |
      !all(names(df) %in% c("sname", "grp", "start", "end", "days_present", "sex",
                            "birth", "first_start_date", "statdate", "birth_dates",
                            "midpoint", "age_start_yrs", "age_class", "subset", "dsi"))) {
    stop("Problem with input data. Use the 'dsi' function to create the input.")
  }

  df$dsi_sum <- list(NULL)
  pb <- txtProgressBar(min = 0, max = nrow(df), style = 3) # Progress bar
  for (i in 1:nrow(df)) {
    df[i, ]$dsi_sum <- list(dsi_row_summary(df$dsi[[i]]))
    setTxtProgressBar(pb, i)
  }
  close(pb)

  df <- df %>%
    dplyr::select(-subset, -dsi) %>%
    tidyr::unnest()

  dsi_strength <- df %>%
    select(-top_partners, -r_quantity, -r_reciprocity) %>%
    unnest() %>%
    select(-n)

  dsi_strength <- dsi_strength %>%
    mutate(DSI_type = case_when(
      sex == "M" & dyad_type == "F-M" ~ "DSI_F",
      sex == "F" & dyad_type == "F-M" ~ "DSI_M",
      sex == "F" & dyad_type == "F-F" ~ "DSI_F"),
      sex = fct_recode(sex, Male = "M", Female = "F")) %>%
    select(-dyad_type) %>%
    spread(DSI_type, r_strength) %>%
    select(sname, grp, start, end, DSI_F, DSI_M)

  dsi_quantity <- df %>%
    select(-top_partners, -r_strength, -r_reciprocity) %>%
    unnest() %>%
    mutate(DSI_type = case_when(
      sex == "M" & dyad_type == "F-M" ~ "F",
      sex == "F" & dyad_type == "F-M" ~ "M",
      sex == "F" & dyad_type == "F-F" ~ "F"),
      sex = fct_recode(sex, Male = "M", Female = "F")) %>%
    select(-dyad_type) %>%
    gather(bond_cat, n_bonds, contains("Bonded")) %>%
    unite(var, bond_cat, DSI_type) %>%
    spread(var, n_bonds, fill = 0) %>%
    select(sname, grp, start, end, ends_with("_M"), ends_with("_F"))

  dsi_recip <- df %>%
    select(-top_partners, -r_quantity, -r_strength) %>%
    unnest() %>%
    select(-n)

  dsi_recip <- dsi_recip %>%
    mutate(DSI_type = case_when(
      sex == "M" & dyad_type == "F-M" ~ "recip_F",
      sex == "F" & dyad_type == "F-M" ~ "recip_M",
      sex == "F" & dyad_type == "F-F" ~ "recip_F"),
      sex = fct_recode(sex, Male = "M", Female = "F")) %>%
    select(-dyad_type) %>%
    spread(DSI_type, r_reciprocity) %>%
    select(sname, grp, start, end, recip_F, recip_M)

  dsi_summary <- df %>%
    select(-top_partners, -starts_with("r_")) %>%
    left_join(dsi_strength, by = c("sname", "grp", "start", "end")) %>%
    left_join(dsi_quantity, by = c("sname", "grp", "start", "end")) %>%
    left_join(dsi_recip, by = c("sname", "grp", "start", "end"))

  return(dsi_summary)
}


#' Summarize a single DSI subset.
#' Calculate top partners, bond quantity, bond strength, and bond reciprocity
#' for each individual-year-of-life separately for the focal animal's top 3
#' grooming partners, separately for F-M and F-F dyads (if applicable)
#'
#' @param df A DSI subset
#'
#' @return The input data with additional list columns.
#'
#' @examples
dsi_row_summary <- function(df) {

  # Return an empty tibble if the subset is empty
  if (nrow(df) == 0) {
    return(dplyr::tbl_df(NULL))
  }

  # Remove any M-M dyads
  df <- df %>%
    dplyr::filter(!(sname_sex == "M" & partner_sex == "M"))

  # Relationship quantity is the number of bonds in each bond-strength category
  r_quantity <- df %>%
    dplyr::group_by(dyad_type) %>%
    dplyr::count(bond_strength) %>%
    tidyr::spread(bond_strength, n)

  # Top partners are the top three grooming partners
  # For males, this means the top three females only
  # For females, this is calculated separately for top three male and female partners
  top_partners <- df %>%
    dplyr::filter(res_g_adj > -9999) %>%
    dplyr::group_by(dyad_type) %>%
    dplyr::arrange(-res_g_adj) %>%
    dplyr::slice(1:3)

  # Relationship strength is the mean of the DSI for the top three partners
  r_strength <- top_partners %>%
    dplyr::filter(res_g_adj > -9999) %>%
    dplyr::group_by(dyad_type) %>%
    dplyr::summarise(r_strength = mean(res_g_adj, na.rm = TRUE),
                     n = n())

  # Reciprocity is the mean of grooming asymmetry for the top three partners
  r_reciprocity <- top_partners %>%
    dplyr::mutate(recip = 1 - abs((g_given - g_received) / (g_given + g_received))) %>%
    dplyr::group_by(dyad_type) %>%
    dplyr::summarise(r_reciprocity = mean(recip, na.rm = TRUE),
                     n = n())

  res <- tibble(top_partners = list(top_partners),
                r_quantity = list(r_quantity),
                r_strength = list(r_strength),
                r_reciprocity = list(r_reciprocity))

  return(res)
}


get_mem_dates <- function(my_sub, members_l, df, sel = NULL) {

  mem_dates <- my_sub %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(df, by = "grp") %>%
    dplyr::filter(date >= start & date <= end)

  # Remove all rows for dates when the particular animal wasn't present in grp
  remove_rows <- mem_dates %>%
    dplyr::anti_join(members_l, by = c("sname", "grp", "date"))

  # Take set difference and calculate summary
  mem_dates <- mem_dates %>%
    dplyr::setdiff(remove_rows) %>%
    dplyr::select(sname, grp, date, !!sel)

  return(mem_dates)
}


get_groom_dates <- function(my_sub, members_l, df, my_sex_var, my_role, my_sex) {

  groom_dates <- my_sub %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(df, by = c("sname" = my_role)) %>%
    dplyr::filter(date >= start & date <= end & UQ(my_sex_var) == my_sex)

  # Remove all rows for dates when the particular animal wasn't present in grp
  remove_rows <- groom_dates %>%
    dplyr::anti_join(members_l, by = c("sname", "grp", "date"))

  # Take set difference and calculate summary
  groom_dates <- groom_dates %>%
    dplyr::setdiff(remove_rows) %>%
    dplyr::select(sname, grp, date, iid)

  return(groom_dates)
}


#' Obtain composite grooming subsets for the animal's year of life
#'
#' @param df One row individual-year-of-life data
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param focals_l A subset of focals produced by the function 'subset_focals'
#' @param females_l A subset of female counts produced by the function 'subset_females'
#' @param grooming_l A subset of grooming data produced by the function 'subset_grooming'
#' @param min_res_days The minimum number of residence days needed to be included. Defaults to 60 days.
#'
#' @return The input row with an additional list column containing the subset
#'
#' @examples
get_sci_subset <- function(df, members_l, focals_l, females_l, grooming_l, min_res_days = 60) {

  zero_daily_count <- 1/365.25
  log_zero_daily_count <- log2(zero_daily_count)

  my_subset <- members_l %>%
    dplyr::inner_join(select(df, -sname, -grp), by = c("sex")) %>%
    dplyr::filter(date >= start & date <= end) %>%
    dplyr::group_by(sname, grp) %>%
    dplyr::summarise(days_present = n(),
                     start = min(date),
                     end = max(date))

  ## Focal counts
  # Get all focals during relevant time period in grp
  my_focals <- get_mem_dates(my_subset, members_l, focals_l, sel = quo(sum)) %>%
    dplyr::group_by(grp, sname) %>%
    dplyr::summarise(n_focals = sum(sum))

  ## Female counts
  my_females <- get_mem_dates(my_subset, members_l, females_l, sel = quo(nr_females)) %>%
    dplyr::group_by(grp, sname) %>%
    dplyr::summarise(mean_f_count = mean(nr_females))

  # Join back to my_subset to add n_focals column
  my_subset <- my_subset %>%
    dplyr::left_join(my_focals, by = c("grp", "sname")) %>%
    dplyr::left_join(my_females, by = c("grp", "sname"))

  # Filter and calculate variables
  my_subset <- my_subset %>%
    dplyr::filter(days_present >= min_res_days & mean_f_count > 0) %>%
    dplyr::mutate(OE = (n_focals / mean_f_count) / days_present,
                  log2OE = log2(OE)) %>%
    dplyr::filter(!is.na(OE))

  ## Grooming given to females
  gg_f <- get_groom_dates(my_subset, members_l, grooming_l, quo(actee_sex), "actor", "F") %>%
    dplyr::group_by(grp, sname) %>%
    dplyr::summarise(GtoF = n())

  ## Grooming received from females
  gr_f <- get_groom_dates(my_subset, members_l, grooming_l, quo(actor_sex), "actee", "F") %>%
    dplyr::group_by(grp, sname) %>%
    dplyr::summarise(GfromF = n())

  my_subset <- my_subset %>%
    dplyr::left_join(gg_f, by = c("grp", "sname")) %>%
    dplyr::left_join(gr_f, by = c("grp", "sname"))

  my_subset <- my_subset %>%
    tidyr::replace_na(list(GtoF = 0, GfromF = 0))

  # Calculate variables
  my_subset <- my_subset %>%
    dplyr::mutate(GtoF_daily = GtoF / days_present,
                  log2GtoF_daily = dplyr::case_when(
                    GtoF == 0 ~ log_zero_daily_count,
                    TRUE ~ log2(GtoF_daily)),
                  GfromF_daily = GfromF / days_present,
                  log2GfromF_daily = dplyr::case_when(
                    GfromF == 0 ~ log_zero_daily_count,
                    TRUE ~ log2(GfromF_daily)))

  my_subset$resGtoF <- as.numeric(residuals(lm(data = my_subset, log2GtoF_daily ~ log2OE)))
  my_subset$resGfromF <- as.numeric(residuals(lm(data = my_subset, log2GfromF_daily ~ log2OE)))
  my_subset$SCI_F <- (my_subset$resGtoF + my_subset$resGfromF) / 2

  # Run for females only
  if (df$sex == "F") {

    ## Grooming given to males
    gg_m <- get_groom_dates(my_subset, members_l, grooming_l, quo(actee_sex), "actor", "M") %>%
      dplyr::group_by(grp, sname) %>%
      dplyr::summarise(GtoM = n())

    ## Grooming received from males
    gr_m <- get_groom_dates(my_subset, members_l, grooming_l, quo(actor_sex), "actee", "M") %>%
      dplyr::group_by(grp, sname) %>%
      dplyr::summarise(GfromM = n())

    my_subset <- my_subset %>%
      dplyr::left_join(gg_m, by = c("grp", "sname")) %>%
      dplyr::left_join(gr_m, by = c("grp", "sname"))

    my_subset <- my_subset %>%
      tidyr::replace_na(list(GtoM = 0, GfromM = 0))

    # Calculate variables
    my_subset <- my_subset %>%
      dplyr::mutate(GtoM_daily = GtoM / days_present,
                    log2GtoM_daily = dplyr::case_when(
                      GtoM == 0 ~ log_zero_daily_count,
                      TRUE ~ log2(GtoM_daily)),
                    GfromM_daily = GfromM / days_present,
                    log2GfromM_daily = dplyr::case_when(
                      GfromM == 0 ~ log_zero_daily_count,
                      TRUE ~ log2(GfromM_daily)))

    my_subset$resGtoM <- as.numeric(residuals(lm(data = my_subset, log2GtoM_daily ~ log2OE)))
    my_subset$resGfromM <- as.numeric(residuals(lm(data = my_subset, log2GfromM_daily ~ log2OE)))
    my_subset$SCI_M <- (my_subset$resGtoM + my_subset$resGfromM) / 2
  }

  return(my_subset)
}

#' Calculate SCI variables from individual-year-of-life data
#'
#' @param my_iyol Individual-year-of-life data.
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param focals_l A subset of focals produced by the function 'subset_focals'
#' @param females_l A subset of female counts produced by the function 'subset_females'
#' @param grooming_l A subset of grooming data produced by the function 'subset_grooming'
#' @param min_res_days The minimum number of coresidence days needed for dyad to be included. Defaults to 60 days.
#'
#' @return The input data with an additional list columsn containing the full SCI subset and variables.
#' @export
#'
#' @examples
sci <- function(my_iyol, members_l, focals_l, females_l, grooming_l,
                min_res_days = 60, parallel = FALSE, ncores = NULL) {

  ptm <- proc.time()

  # Return an empty tibble if the subset is empty
  if (is.null(my_iyol) |
      !all(names(my_iyol) %in% c("sname", "grp", "start", "end", "days_present", "sex",
                                 "birth", "first_start_date", "statdate", "birth_dates",
                                 "midpoint", "age_start_yrs", "age_class")) |
      min_res_days < 0) {
    stop("Problem with input data. Use the 'make_iyol' function to create the input.")
  }

  if (parallel) {
    avail_cores <- detectCores()
    if (!is.null(ncores)) {
      if (ncores > avail_cores) {
        message(paste0("Ignoring 'ncores' argument because only ", avail_cores,
                       " cores are available."))
        ncores <- avail_cores
      }
    }
    else {
      message(paste0("Using all available cores: ", avail_ncores,
                     ". Use 'ncores' to specify number of cores to use."))
      ncores <- avail_cores
    }

    cl <- makeCluster(ncores)
    registerDoSNOW(cl)
    pb <- txtProgressBar(min = 0, max = nrow(my_iyol), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    subset <- foreach(i = 1:nrow(my_iyol), .options.snow = opts,
                      .packages = c('tidyverse')) %dopar% {
                        get_sci_subset(my_iyol[i, ], members_l, focals_l,
                                       females_l, grooming_l, min_res_days)
                      }
    close(pb)
    stopCluster(cl)
    my_iyol <- add_column(my_iyol, subset)
  }
  else {
    if (!is.null(ncores)) {
      message("Ignoring 'ncores' argument because 'parallel' set to FALSE.")
    }
    my_iyol$subset <- list(NULL)
    pb <- txtProgressBar(min = 0, max = nrow(my_iyol), style = 3) # Progress bar
    for (i in 1:nrow(my_iyol)) {
      my_iyol[i, ]$subset <- list(get_sci_subset(my_iyol[i, ], members_l,
                                                 focals_l, females_l,
                                                 grooming_l,
                                                 min_res_days))
      setTxtProgressBar(pb, i)
      close(pb)
    }
  }

  sci_focal <- my_iyol %>%
    unnest() %>%
    mutate(focal = (sname == sname1 & grp == grp1)) %>%
    filter(focal) %>%
    select(sname, grp, start, end, SCI_M, SCI_F)

  res <- left_join(my_iyol, sci_focal, by = c("sname", "grp", "start", "end"))

  tdiff <- (proc.time() - ptm)["elapsed"] / 60
  message(paste0("Elapsed time: ", round(tdiff, 3), " minutes (",
                 round(tdiff / 60, 3), ") hours."))

  return(res)
}
