
#' Obtain a subset of members table that excludes behavioral observation gaps.
#'
#' @param babase A DBI connection to the babase database
#' @param .adults_only Logical indicating whether to include adults only. Default is TRUE
#'
#' @return A subset of the members table that excludes behavioral observation gaps.
#' @export
#'
#' @examples
subset_members <- function(babase, .adults_only = TRUE) {

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

  if (.adults_only) {
    message("Obtaining members data from all adult females (matured) and males (ranked)...")
  }
  else {
    message("Obtaining members data from all females and males...")
  }

  members_keep <- members %>%
    dplyr::inner_join(dplyr::select(biograph, sname, sex), by = "sname") %>%
    dplyr::left_join(dplyr::select(md_females, sname, matured), by = "sname") %>%
    dplyr::left_join(dplyr::select(rd_males, sname, ranked), by = "sname") %>%
    dplyr::inner_join(dplyr::select(groups_history, gid, permanent, impermanent, last_reg_census),
                      by = c("grp" = "gid")) %>%
    dplyr::filter(grp < 3 & grp != 1.3 & grp != 1.4 & date >= permanent &
                    (is.na(impermanent) | date <= impermanent) &
                    (is.na(last_reg_census) | date <= last_reg_census))

  if (.adults_only) {
    members_keep <- members_keep %>%
      dplyr::filter((sex == "F" & date >= matured) | (sex == "M" & date >= ranked))
    }

  # Find behavior gaps that overlap records in members_keep
  bg <- members_keep %>%
    dplyr::left_join(behave_gaps, by = "grp") %>%
    dplyr::filter(date >= gap_start & date <= gap_end)

  # Exclude those records using anti_join
  members_keep <- dplyr::anti_join(members_keep, bg, by = "membid") %>%
    dplyr::select(grp, sname, date, sex, matured, ranked) %>%
    dplyr::collect()

  message("Dealing with first-of-month issue...")

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

  ## Check in what group an individual lives in on the first of the month.
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


#' Obtain a subset of adult female focal samples that excludes behavioral observation gaps.
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


#' Obtain a subset of adult female count data that excludes behavioral observation gaps.
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
    dplyr::filter(sex == "F" & date >= matured) %>%
    dplyr::group_by(grp, date) %>%
    dplyr::summarise(nr_females = n()) %>%
    dplyr::ungroup()

  return(females_l)

}


#' Obtain a subset of grooming data that excludes behavioral observation gaps.
#'
#' @param babase A DBI connection to the babase database
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param .adults_only Logical indicating whether to include adults only. Default is TRUE
#'
#' @return A subset of grooming data that excludes behavioral observation gaps.
#' @export
#'
#' @examples
subset_interactions <- function(babase, members_l, my_acts = NULL, .adults_only = TRUE) {

  if (class(babase) != "PostgreSQLConnection") {
    stop("Invalid connection to babase.")
  }


  # verify-acts -------------------------------------------------------------

  # Get acts table, trim "act" column, which currently includes spaces
  acts_l <- dplyr::tbl(babase, "acts") %>%
    dplyr::collect() %>%
    dplyr::mutate(act = stringr::str_trim(act))

  all_acts <- acts_l$act

  # Exit with error if user specifies an act that doesn't exist.
  if (!all(my_acts %in% all_acts)) {
    stop(paste0("Unrecognized acts: ",
                paste(my_acts[!my_acts %in% all_acts], collapse = ", "),
                "."))
  }

  # Grooming can't be combined with other interactions because of 1st of month issue
  if (length(my_acts) > 1 & "G" %in% my_acts) {
    stop("Grooming can't be combined with other kinds of act because of 1st of month issue. Subset grooming separately.")
  }

  my_classes <- acts_l %>%
    dplyr::filter(act %in% my_acts) %>%
    dplyr::pull(class) %>%
    unique()

  # Check if the acts chosen by the user all belong to one kind of behavior
  # E.g., just grooming or just agonism
  # Warn (but don't exit) if behavior classes are mixed
  if (length(my_classes) > 1) {
    warning("Interactions include different classes of behavior. Output might be nonsensical!")
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

  # interactions-query ------------------------------------------------------

  message("Obtaining and validating interaction data...")

  inter <- actor_actees %>%
    dplyr::mutate(act = TRIM(act)) %>%
    dplyr::inner_join(dplyr::select(biograph, sname, actor_sex = sex),
                      by = c("actor" = "sname")) %>%
    dplyr::inner_join(dplyr::select(biograph, sname, actee_sex = sex),
                      by = c("actee" = "sname")) %>%
    dplyr::filter(act %in% my_acts & actee_grp != 1.3 & actee_grp != 1.4 &
                    (actee_grp < 3 | actee_grp == 9) &
                    actor_grp != 1.3 & actor_grp != 1.4 & (actor_grp < 3 | actor_grp == 9)) %>%
    dplyr::collect()

  inter <- inter %>%
    dplyr::left_join(dplyr::select(maturedates_l, sname, actor_matured = matured),
                     by = c("actor" = "sname")) %>%
    dplyr::left_join(dplyr::select(maturedates_l, sname, actee_matured = matured),
                     by = c("actee" = "sname")) %>%
    dplyr::left_join(dplyr::select(rankdates_l, sname, actor_ranked = ranked),
                     by = c("actor" = "sname")) %>%
    dplyr::left_join(dplyr::select(rankdates_l, sname, actee_ranked = ranked),
                     by = c("actee" = "sname"))

  if (.adults_only) {
    inter <- inter %>%
      dplyr::filter(((actor_sex == "F" & date >= actor_matured) |
                       (actor_sex == "M" & date >= actor_ranked)) &
                      ((actee_sex == "F" & date >= actee_matured) |
                         (actee_sex == "M" & date >= actee_ranked)))
  }

  inter$yearmon <- as.character(zoo::as.yearmon(inter$date))

  if (.adults_only) {
    inter$is_actor_adult <- TRUE
    inter$is_actee_adult <- TRUE
  }
  else {
    inter <- inter %>%
      dplyr::mutate(is_actor_adult = (actor_sex == "F" & date >= actor_matured) |
               (actor_sex == "M" & date >= actor_ranked),
             is_actee_adult = (actee_sex == "F" & date >= actee_matured) |
               (actee_sex == "M" & date >= actee_ranked)) %>%
      tidyr::replace_na(list(is_actor_adult = FALSE, is_actee_adult = FALSE))
  }

  inter <- inter %>%
    dplyr::select(iid, sid, act, actor, actee, actor_sex, actee_sex, date,
                  yearmon, actor_grp, actee_grp, is_actor_adult, is_actee_adult)

  # If user requested grooming data, deal with first-of-month issue

  if (my_acts[1] == "G") {

    message("Dealing with grooming on first-of-month issue...")

    # grooming-data-manip -----------------------------------------------------

    ## Check grooming data for potential issues with 1st of month issue
    ## This problem is being fixed and this code will no longer be needed once the historic grooming data has been solved.
    ## Yearmon are unique year and month combinations, e.g. MAR2000 or APR2012
    ## The check is done per sname and yearmon combination.

    ## Potential issues in the grooming interactions are all grooming that took place prior to Jul 2006.
    potential_issues <- members_l %>%
      dplyr::filter(first_of_month_grp != grp & date < '2006-07-01')

    ## There is no issue if an individual is only present in 1 group for the whole month as 1st of the month group will be the same as the whole month group
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

    grooming_l <- inter %>%
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

    grooming_l <- select(grooming_l, -contains("issue"))

    inter <- grooming_l
  }

  ## Restrict the grooming data to the same data restrictions of members
  grp_dates <- dplyr::distinct(members_l, grp, date)

  temp1 <- inter %>%
    dplyr::inner_join(grp_dates, by = c("date", "actee_grp" = "grp"))

  temp2 <- inter %>%
    dplyr::inner_join(grp_dates, by = c("date", "actor_grp" = "grp"))

  inter <- dplyr::bind_rows(temp1, temp2) %>%
    dplyr::distinct(iid, .keep_all = TRUE)

  # NOTE: Duplicated rows not removed in original code

  return(inter)

}


#' Create a data frame for each individual year of life (by default, for adults only)
#'
#' @param babase A DBI connection to the babase database
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param focals_l A subset of focals produced by the function 'subset_focals'
#' @param interactions_l A subset of interaction data produced by the function 'subset_interactions'
#' @param .by_grp Logical indicating whether to separate by group. Default is TRUE
#' @param .adults_only Logical indicating whether to include adults only. Default is TRUE
#'
#' @return A tibble with one row per animal (and optionally, per group) and year of life, with contextual data
#' @export
#'
#' @examples
make_iyol <- function(babase, members_l, focals_l = NULL, interactions_l = NULL,
                      .by_grp = TRUE, .adults_only = TRUE) {

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
  last_date <- max(members_l$date)

  if (!is.null(focals_l)) {
    last_date <- min(last_date, max(focals_l$date))
  }

  if (!is.null(interactions_l$date)) {
    last_date <- min(last_date, max(interactions_l$date))
  }


  # individ-year-of-life ----------------------------------------------------

  message("Creating individual-year-of-life data set...")

  iyol <- biograph %>%
    dplyr::left_join(dplyr::select(md_females, sname, matured), by = "sname") %>%
    dplyr::left_join(dplyr::select(rd_males, sname, ranked), by = "sname") %>%
    dplyr::select(sname, sex, birth, statdate, matured, ranked) %>%
    dplyr::collect()

  if (.adults_only) {
    iyol <- iyol %>%
      dplyr::mutate(first_start_date = dplyr::case_when(
        sex == "F" ~ matured,
        sex == "M" ~ ranked
      )) %>%
      tidyr::drop_na(first_start_date) %>%
      dplyr::select(sname, sex, birth, first_start_date, statdate, -ranked, -matured)
  }
  else {
    iyol <- iyol %>%
      dplyr::mutate(first_start_date = dplyr::case_when(
        sex == "F" ~ birth,
        sex == "M" ~ birth
      )) %>%
      tidyr::drop_na(first_start_date) %>%
      dplyr::select(sname, sex, birth, first_start_date, statdate, -ranked, -matured)
  }

  make_bday_seq <- function(df) {
    res <- as.character(seq(df$birth, df$statdate, by = "year"))
    return(res)
  }

  iyol <- iyol %>%
    dplyr::filter(!is.na(sname)) %>%
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

  if (any(iyol$end > last_date)) {
    iyol[iyol$end > last_date, ]$end <- last_date
  }

  iyol <- iyol %>%
    dplyr::filter(start <= end)

  # NOTE: Lots of extra rows with nonsensical dates in original xdata
  # Start date after end date
  # E.g., for sname == CLE
  # Likely to be silently dropped later, so maybe not a problem

  if (.by_grp) {
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
  }
  else {
    ## Check how many days the focal was present in ANY group in a focal year
    temp <- iyol %>%
      dplyr::inner_join(dplyr::select(members_l, sname, date), by = c("sname")) %>%
      dplyr::filter(date >= start & date <= end) %>%
      dplyr::group_by(sname, start, end) %>%
      dplyr::summarise(days_present = n()) %>%
      dplyr::arrange(sname, start, end)

    iyol <- temp %>%
      dplyr::inner_join(iyol, by = c("sname", "start", "end")) %>%
      dplyr::arrange(sname, start, end)
  }

  # Calculate date variables
  iyol <- iyol %>%
    dplyr::mutate(midpoint = start + floor((end - start) / 2),
                  age_start_yrs = as.numeric(start - birth) / 365.25,
                  age_class = floor(plyr::round_any(age_start_yrs, 0.005)) + 1)

  # NOTES:
  # Original SCI scipt produces error in dates for RUT
  # One day difference for AGE due to weirdness with interpolation
  # - AGE has one entry in members after statdate
  # tmp1 <- zdata %>%
  #   tbl_df %>%
  #   dplyr::mutate(grp = as.double(grp))
  # setdiff(select(tmp1, -end), select(iyol, -end))

  iyol <- dplyr::ungroup(iyol)

  return(iyol)
}


#' Create a data frame with year-long intervals prior to specific target dates
#'
#' @param target_df A data frame that includes the columns sname, sex, grp, and date
#' @param babase A DBI connection to the babase database
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param window_length Length in years of the time window for the social index
#' @param .by_grp Logical indicating whether to separate by group. Default is TRUE
#' @param .adults_only Logical indicating whether to include adults only. Default is TRUE
#'
#' @return A tibble with one row per animal (and optionally, per group) and target date, with contextual data
#' @export
#'
#' @examples
make_target_date_df <- function(target_df, babase, members_l, window_length = 1, .by_grp = TRUE,
                                .adults_only = TRUE) {

  if (class(babase) != "PostgreSQLConnection") {
    stop("Invalid connection to babase.")
  }

  # Return an empty tibble if the subset is empty
  if (is.null(target_df) |
      !all(c("sname", "sex", "date") %in% names(target_df))) {
    stop("Problem with input data. Target data frame must include rows 'sname', 'sex', and 'date'.")
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
    dplyr::semi_join(dplyr::filter(biograph, sex == "F"), by = "sname") %>%
    collect()

  rd_males <- rankdates %>%
    dplyr::semi_join(dplyr::filter(biograph, sex == "M"), by = "sname") %>%
    collect()

  # Find last date
  last_date <- max(members_l$date)

  message("Creating target-date data set...")

  target_df <- target_df %>%
    dplyr::left_join(biograph_l, by = c("sname", "sex")) %>%
    dplyr::left_join(dplyr::select(md_females, sname, matured), by = "sname") %>%
    dplyr::left_join(dplyr::select(rd_males, sname, ranked), by = "sname") %>%
    dplyr::select(sname, obs_date = date, sex, birth, statdate, matured, ranked)

  if (.adults_only) {
    target_df <- target_df %>%
      dplyr::mutate(first_start_date = dplyr::case_when(
        sex == "F" ~ matured,
        sex == "M" ~ ranked
      )) %>%
      tidyr::drop_na(first_start_date) %>%
      dplyr::select(sname, obs_date, sex, birth, first_start_date, statdate, -ranked, -matured)
  }
  else {
    target_df <- target_df %>%
      dplyr::mutate(first_start_date = dplyr::case_when(
        sex == "F" ~ birth,
        sex == "M" ~ birth
      )) %>%
      tidyr::drop_na(first_start_date) %>%
      dplyr::select(sname, obs_date, sex, birth, first_start_date, statdate, -ranked, -matured)
  }

  # target_df <- target_df %>%
  #   dplyr::mutate(first_start_date = dplyr::case_when(
  #     sex == "F" ~ matured,
  #     sex == "M" ~ ranked
  #   )) %>%
  #   dplyr::select(sname, obs_date, sex, birth, first_start_date, statdate, -ranked, -matured)

  target_df <- target_df %>%
    dplyr::mutate(start = dplyr::case_when(
      first_start_date >= obs_date - lubridate::years(window_length) + days(1) ~ first_start_date,
      TRUE ~ obs_date - lubridate::years(window_length) + days(1)))

  target_df <- target_df %>%
    dplyr::mutate(end = obs_date) %>%
    select(sname, sex, birth, obs_date, first_start_date, statdate, start, end)

  target_df <- target_df %>%
    dplyr::filter(start <= end) %>%
    arrange(sname, obs_date)

  # .by_grp <- TRUE

  if (.by_grp) {
    ## Check in which groups the individual was present in the focal year
    ## and create one row per focal year per group
    temp <- target_df %>%
      dplyr::left_join(dplyr::select(members_l, sname, date, grp), by = c("sname")) %>%
      dplyr::filter(date >= start & date <= end) %>%
      dplyr::distinct(sname, start, end, grp)

    zdata <- target_df %>%
      dplyr::inner_join(temp, by = c("sname", "start", "end")) %>%
      tibble::rownames_to_column()

    ## And check how many days the focal was present in the group in a focal year
    zdata <- zdata %>%
      dplyr::inner_join(dplyr::select(members_l, sname, grp, date), by = c("sname", "grp")) %>%
      dplyr::filter(date >= start & date <= end) %>%
      dplyr::group_by(sname, grp, start, end, rowname) %>%
      dplyr::summarise(days_present = n()) %>%
      dplyr::arrange(sname, grp, start, end)

    target_df <- zdata %>%
      dplyr::inner_join(target_df, by = c("sname", "start", "end")) %>%
      dplyr::arrange(sname, grp, start, end) %>%
      dplyr::select(-rowname)
  }
  else {
    ## Check how many days the focal was present in ANY group in a focal year
    # temp <- target_df %>%
    #   dplyr::inner_join(dplyr::select(members_l, sname, date), by = c("sname")) %>%
    #   dplyr::filter(date >= start & date <= end) %>%
    #   dplyr::group_by(sname, start, end) %>%
    #   dplyr::summarise(days_present = n()) %>%
    #   dplyr::arrange(sname, start, end)
    #
    # target_df <- temp %>%
    #   dplyr::inner_join(target_df, by = c("sname", "start", "end")) %>%
    #   dplyr::arrange(sname, start, end)

    stop("Not Yet Completed.")
  }

  # Calculate date variables
  target_df <- target_df %>%
    dplyr::mutate(midpoint = start + floor((end - start) / 2),
                  age_start_yrs = as.numeric(start - birth) / 365.25,
                  age_class = floor(plyr::round_any(age_start_yrs, 0.005)) + 1)

  target_df <- dplyr::ungroup(target_df) %>%
    distinct()

  return(target_df)
}
