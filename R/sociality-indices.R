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


get_interaction_dates <- function(my_sub, members_l, df, my_sex_var, my_role, my_sex) {

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


#' Obtain SCI subsets for the animal's year of life
#'
#' @param df One row individual-year-of-life data
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param focals_l A subset of focals produced by the function 'subset_focals'
#' @param females_l A subset of female counts produced by the function 'subset_females'
#' @param interactions_l A subset of interactions data produced by the function 'subset_interactions'
#' @param min_res_days The minimum number of residence days needed to be included. Defaults to 60 days.
#' @param directional Logical value indicating whether to preserve directionality
#'
#' @return The input row with an additional list column containing the subset
#'
#' @examples
get_sci_subset <- function(df, members_l, focals_l, females_l, interactions_l,
                           min_res_days, directional, legacy_sci) {

  zero_daily_count <- 1/365.25
  log_zero_daily_count <- log2(zero_daily_count)
  my_sname <- df$sname

  # Allow focal animal only to be a non-adult (for early adversity analysis)
  my_members <- members_l %>%
    filter(sname == my_sname | (sex == "F" & date >= matured) | (sex == "M" & date >= ranked))

  # Get all members of same sex as the focal animal during relevant time period
  my_subset <- my_members %>%
    dplyr::inner_join(select(df, -sname, -grp), by = c("sex")) %>%
    dplyr::filter(date >= start & date <= end) %>%
    dplyr::group_by(sname, grp) %>%
    dplyr::summarise(days_present = n(),
                     start = min(date),
                     end = max(date))

  # Allow focal animal only to be a non-adult (for early adversity analysis)
  my_interactions <- interactions_l %>%
    dplyr::filter((is_actor_adult & is_actee_adult) |
                    (is_actor_adult & actee == my_sname) |
                    (is_actee_adult & actor == my_sname))

  ## Focal counts
  # Get all focals during relevant time period in grp
  my_focals <- get_mem_dates(my_subset, my_members, focals_l, sel = quo(sum))

  ## Observation days
  # Focal animal was present and at least one focal sample was collected
  obs_days <- my_focals %>%
    group_by(grp, sname) %>%
    summarise(days_observed = n())

  my_subset <- my_subset %>%
    left_join(obs_days, by = c("sname", "grp"))

  my_focals <- my_focals %>%
    dplyr::group_by(grp, sname) %>%
    dplyr::summarise(n_focals = sum(sum))

  ## Female counts
  my_females <- get_mem_dates(my_subset, my_members, females_l, sel = quo(nr_females)) %>%
    dplyr::group_by(grp, sname) %>%
    dplyr::summarise(mean_f_count = mean(nr_females))

  # Join back to my_subset to add n_focals column
  my_subset <- my_subset %>%
    dplyr::left_join(my_focals, by = c("grp", "sname")) %>%
    dplyr::left_join(my_females, by = c("grp", "sname"))

  if (nrow(my_subset) == 0 | nrow(my_focals) == 0 | nrow(my_females) == 0) {
    return(dplyr::tbl_df(NULL))
  }

  # Filter and calculate variables
  my_subset <- my_subset %>%
    dplyr::filter(days_present >= min_res_days & mean_f_count > 0) %>%
    dplyr::mutate(OE = (n_focals / mean_f_count) / days_present,
                  log2OE = log2(OE)) %>%
    dplyr::filter(!is.na(OE))

  ## Interactions given to females by each actor of focal's sex
  gg_f <- get_interaction_dates(my_subset, my_members, my_interactions,
                                quo(actee_sex), "actor", "F") %>%
    dplyr::group_by(grp, sname) %>%
    dplyr::summarise(ItoF = n())

  ## Interactions received from females by each actee of focal's sex
  gr_f <- get_interaction_dates(my_subset, my_members, my_interactions,
                                quo(actor_sex), "actee", "F") %>%
    dplyr::group_by(grp, sname) %>%
    dplyr::summarise(IfromF = n())

  # Calculate variables for interactions with males only if:
  # - the interactions are grooming AND the focal animal is female OR
  # - the interactions are anything but grooming
  include_males <- my_interactions$act[[1]] != "G" | (my_interactions$act[[1]] == "G" & df$sex == "F")

  if (include_males) {
    ## Interactions given to males by each actor of focal's sex
    gg_m <- get_interaction_dates(my_subset, my_members, my_interactions,
                                  quo(actee_sex), "actor", "M") %>%
      dplyr::group_by(grp, sname) %>%
      dplyr::summarise(ItoM = n())

    ## Interactions received from males by each actee of focal's sex
    gr_m <- get_interaction_dates(my_subset, my_members, my_interactions,
                                  quo(actor_sex), "actee", "M") %>%
      dplyr::group_by(grp, sname) %>%
      dplyr::summarise(IfromM = n())
  }

  my_subset <- my_subset %>%
    dplyr::left_join(gg_f, by = c("grp", "sname")) %>%
    dplyr::left_join(gr_f, by = c("grp", "sname"))

  my_subset <- my_subset %>%
    tidyr::replace_na(list(ItoF = 0, IfromF = 0))

  if (include_males) {
    my_subset <- my_subset %>%
      dplyr::left_join(gg_m, by = c("grp", "sname")) %>%
      dplyr::left_join(gr_m, by = c("grp", "sname"))

    my_subset <- my_subset %>%
      tidyr::replace_na(list(ItoM = 0, IfromM = 0))
  }

  # Calculate variables, first for interactions with females only
  my_subset <- my_subset %>%
    dplyr::mutate(ItoF_daily = ItoF / days_present,
                  log2ItoF_daily = dplyr::case_when(
                    ItoF == 0 ~ log_zero_daily_count,
                    TRUE ~ log2(ItoF_daily)),
                  IfromF_daily = IfromF / days_present,
                  log2IfromF_daily = dplyr::case_when(
                    IfromF == 0 ~ log_zero_daily_count,
                    TRUE ~ log2(IfromF_daily)))

  if (include_males) {
    my_subset <- my_subset %>%
      dplyr::mutate(ItoM_daily = ItoM / days_present,
                    log2ItoM_daily = dplyr::case_when(
                      ItoM == 0 ~ log_zero_daily_count,
                      TRUE ~ log2(ItoM_daily)),
                    IfromM_daily = IfromM / days_present,
                    log2IfromM_daily = dplyr::case_when(
                      IfromM == 0 ~ log_zero_daily_count,
                      TRUE ~ log2(IfromM_daily)))
  }

  if (legacy_sci) {

    my_subset$SCI_F_Dir <- as.numeric(residuals(lm(data = my_subset, log2ItoF_daily ~ log2OE)))
    my_subset$SCI_F_Rec <- as.numeric(residuals(lm(data = my_subset, log2IfromF_daily ~ log2OE)))

    if (include_males) {
      my_subset$SCI_M_Dir <- as.numeric(residuals(lm(data = my_subset, log2ItoM_daily ~ log2OE)))
      my_subset$SCI_M_Rec <- as.numeric(residuals(lm(data = my_subset, log2IfromM_daily ~ log2OE)))
    }

    if (!directional) {
      my_subset$SCI_F <- (my_subset$SCI_F_Dir + my_subset$SCI_F_Rec) / 2
      if (include_males) {
        my_subset$SCI_M <- (my_subset$SCI_M_Dir + my_subset$SCI_M_Rec) / 2
      }
    }
  }

  return(my_subset)
}

#' Calculate Social Connectedness Index variables from individual-year-of-life data
#'
#' @param my_iyol Individual-year-of-life data.
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param focals_l A subset of focals produced by the function 'subset_focals'
#' @param females_l A subset of female counts produced by the function 'subset_females'
#' @param interactions_l A subset of interactions data produced by the function 'subset_interactions'
#' @param min_res_days The minimum number of coresidence days needed for dyad to be included. Defaults to 60 days.
#' @param parallel Logical value indicating whether to process in parallel
#' @param ncores Integer value indicating how many cores to use in parallel processing
#' @param directional Logical value indicating whether to preserve directionality
#'
#' @return The input data with an additional list columsn containing the full subset and variables.
#' @export
#'
#' @examples
sci <- function(my_iyol, members_l, focals_l, females_l, interactions_l,
                min_res_days = 60, parallel = FALSE, ncores = NULL,
                directional = FALSE, legacy_sci = FALSE) {

  ptm <- proc.time()

  # Return an empty tibble if the subset is empty
  if (is.null(my_iyol) |
      !all(names(my_iyol) %in% c("sname", "grp", "start", "end", "days_present", "sex",
                                 "birth", "first_start_date", "statdate", "birth_dates",
                                 "midpoint", "age_start_yrs", "age_class", "obs_date")) |
      min_res_days < 0) {
    stop("Problem with input data. Use the 'make_iyol' or 'make_target_df' function to create the input.")
  }

  if (parallel) {
    avail_cores <- detectCores()
    if (!is.null(ncores)) {
      if (ncores > avail_cores) {
        message(paste0("Ignoring 'ncores' argument because only ", avail_cores,
                       " cores are available."))
        ncores <- avail_cores
      }
    } else {
      message(paste0("Using all available cores: ", avail_cores,
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
                                       females_l, interactions_l, min_res_days,
                                       directional, legacy_sci)
                      }
    close(pb)
    stopCluster(cl)
    my_iyol <- add_column(my_iyol, subset)
  } else {
    if (!is.null(ncores)) {
      message("Ignoring 'ncores' argument because 'parallel' set to FALSE.")
    }
    my_iyol$subset <- list(NULL)
    pb <- txtProgressBar(min = 0, max = nrow(my_iyol), style = 3) # Progress bar
    for (i in 1:nrow(my_iyol)) {
      my_iyol[i, ]$subset <- list(get_sci_subset(my_iyol[i, ], members_l,
                                                 focals_l, females_l,
                                                 interactions_l, min_res_days,
                                                 directional, legacy_sci))
      setTxtProgressBar(pb, i)
      close(pb)
    }
  }

  if (!legacy_sci) {

    sci_males <- my_iyol %>%
      tidyr::unnest() %>%
      dplyr::filter(sex == "M") %>%
      dplyr::mutate(SCI_F_Dir = lm(log2ItoF_daily ~ log2OE)$residuals,
             SCI_F_Rec = lm(log2IfromF_daily ~ log2OE)$residuals,
             SCI_F = (SCI_F_Dir + SCI_F_Rec) / 2) %>%
      dplyr::group_by(sname, sex, grp, age_class, start, end) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::starts_with("SCI")), list(scale_num)) %>%
      dplyr::ungroup()

    sci_females <- my_iyol %>%
      tidyr::unnest() %>%
      dplyr::filter(sex == "F") %>%
      dplyr::mutate(SCI_F_Dir = lm(log2ItoF_daily ~ log2OE)$residuals,
                    SCI_F_Rec = lm(log2IfromF_daily ~ log2OE)$residuals,
                    SCI_F = (SCI_F_Dir + SCI_F_Rec) / 2,
                    SCI_M_Dir = lm(log2ItoM_daily ~ log2OE)$residuals,
                    SCI_M_Rec = lm(log2IfromM_daily ~ log2OE)$residuals,
                    SCI_M = (SCI_M_Dir + SCI_M_Rec) / 2) %>%
      dplyr::group_by(sname, sex, grp, age_class, start, end) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::starts_with("SCI")), list(scale_num)) %>%
      dplyr::ungroup()

    temp_iyol <- dplyr::bind_rows(sci_females, sci_males) %>%
      dplyr::group_by(sname, grp, start, end, days_present, sex, birth, first_start_date, statdate, birth_dates, midpoint, age_start_yrs, age_class) %>%
      tidyr::nest(.key = "subset") %>%
      dplyr::arrange(sname, grp, age_class)

  }

  sci_focal <- temp_iyol %>%
    tidyr::unnest() %>%
    dplyr::mutate(focal = (sname == sname1 & grp == grp1)) %>%
    dplyr::filter(focal) %>%
    dplyr::select(sname, grp, start, end, dplyr::contains("SCI_"))

  res <- dplyr::left_join(temp_iyol, sci_focal, by = c("sname", "grp", "start", "end"))

  attr(res, "directional") <- directional

  tdiff <- (proc.time() - ptm)["elapsed"] / 60
  message(paste0("Elapsed time: ", round(tdiff, 3), " minutes (",
                 round(tdiff / 60, 3), ") hours."))

  return(res)
}


#' Calculate dyadic index variables from individual-year-of-life data
#'
#' @param my_iyol Individual-year-of-life data.
#' @param biograph_l A local copy of the biograph table
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param focals_l A subset of focals produced by the function 'subset_focals'
#' @param females_l A subset of female counts produced by the function 'subset_females'
#' @param agonism_l A subset of agonism data produced by the function 'subset_agonism'
#' @param min_cores_days The minimum number of coresidence days needed for dyad to be included. Defaults to 60 days.
#' @param within_grp Logical value indicating whether regressions should be fit relative to dyads in entire population (default) or to dyads within-group
#' @param parallel Logical value indicating whether to process in parallel
#' @param ncores Integer value indicating how many cores to use in parallel processing
#' @param directional Logical value indicating whether to preserve directionality
#'
#' @return The input data with an additional list columsn containing the full DSI subset and the focal DSI variables.
#' @export
#'
#' @examples
dyadic_index <- function(my_iyol, biograph_l, members_l, focals_l, females_l, interactions_l,
                         min_cores_days = 60, within_grp = FALSE, parallel = FALSE,
                         ncores = NULL, directional = FALSE) {

  ptm <- proc.time()

  # Return an empty tibble if the subset is empty
  if (is.null(my_iyol) |
      !all(names(my_iyol) %in% c("sname", "grp", "start", "end", "days_present", "sex",
                                 "birth", "first_start_date", "statdate", "birth_dates",
                                 "midpoint", "age_start_yrs", "age_class", "obs_date")) |
      min_cores_days < 0) {
    stop("Problem with input data. Use the 'make_iyol' or 'make_target_df' function to create the input.")
  }

  if (parallel) {
    avail_cores <- detectCores()
    if (!is.null(ncores)) {
      if (ncores > avail_cores) {
        message(paste0("Ignoring 'ncores' argument because only ", avail_cores,
                       " cores are available."))
        ncores <- avail_cores
      }
    } else {
      message(paste0("Using all available cores: ", avail_cores,
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
                                          interactions_l, min_cores_days,
                                          within_grp, directional)
                      }
    close(pb)
    stopCluster(cl)
    my_iyol <- add_column(my_iyol, subset)
  } else {
    if (!is.null(ncores)) {
      message("Ignoring 'ncores' argument because 'parallel' set to FALSE.")
    }
    my_iyol$subset <- list(NULL)
    pb <- txtProgressBar(min = 0, max = nrow(my_iyol), style = 3) # Progress bar
    for (i in 1:nrow(my_iyol)) {
      my_iyol[i, ]$subset <- list(get_dyadic_subset(my_iyol[i, ], biograph_l,
                                                    members_l, focals_l, females_l,
                                                    interactions_l, min_cores_days,
                                                    within_grp, directional))
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }

  # Apply universal slope correction
  # This replaces all the res_i_adj values in each subset
  my_iyol <- apply_universal_slope(my_iyol)

  # Create focal summaries
  my_iyol <- my_iyol %>%
    dplyr::mutate(di = purrr::pmap(list(sname, grp, subset), get_focal_index))

  attr(my_iyol, "directional") <- directional

  tdiff <- (proc.time() - ptm)["elapsed"] / 60
  message(paste0("Elapsed time: ", round(tdiff, 3), " minutes (",
                 round(tdiff / 60, 3), ") hours."))

  return(my_iyol)
}


#' Apply universal slope correction to DSI
#'
#' @param data A full DSI data set including the subset list-column
#'
#' @return The full DSI data set with the res_i_adj values replaced by their universal-slope equivalents.
#'
#' @examples
apply_universal_slope <- function(data) {

  # Apply universal slope correction
  keep <- data %>%
    tidyr::unnest() %>%
    dplyr::filter(log2_i_adj > -999)

  dsi_universal_slopes <- keep %>%
    dplyr::group_by(sex, dyad_type) %>%
    dplyr::summarise(univ = list(lm(log2_i_adj ~ log2OE))) %>%
    dplyr::mutate(coefs = map(univ, broom::tidy)) %>%
    dplyr::select(-univ) %>%
    tidyr::unnest() %>%
    dplyr::select(sex, dyad_type, term, estimate) %>%
    dplyr::spread(term, estimate) %>%
    dplyr::rename("B0" = `(Intercept)`, "B1" = "log2OE")

  universal_dsi_slope_values <- function(my_df, focal_sname, focal_grp, focal_sex) {

    keep_out <- my_df %>%
      dplyr::filter(i_total == 0)

    sv <- dsi_universal_slopes %>%
      dplyr::filter(sex == focal_sex)

    res <- my_df %>%
      dplyr::inner_join(sv, by = "dyad_type")

    res <- res %>%
      dplyr::filter(i_adj > 0) %>%
      dplyr::group_by(dyad_type) %>%
      dplyr::mutate(res_i_adj = log2_i_adj - (B0 + (B1 * log2OE))) %>%
      dplyr::ungroup()

    res <- res %>%
      dplyr::group_by(dyad_type) %>%
      dplyr::mutate(res_i_adj = scale_num(res_i_adj)) %>%
      dplyr::select(-B0, -B1, -sex) %>%
      dplyr::bind_rows(keep_out) %>%
      dplyr::arrange(sname, grp, dyad_type, partner)

  }

  data_univ <- data %>%
    dplyr::mutate(subset = purrr::pmap(.l = list(subset, sname, grp, sex), .f = universal_dsi_slope_values))

  return(data_univ)
}


#' Obtain dyadic interaction subsets for the animal's year of life
#'
#' @param df One row individual-year-of-life data
#' @param biograph_l A local copy of the biograph table
#' @param members_l A subset of members table produced by the function 'subset_members'
#' @param focals_l A subset of focals produced by the function 'subset_focals'
#' @param females_l A subset of female counts produced by the function 'subset_females'
#' @param interactions_l A subset of interaction data
#' @param min_cores_days The minimum number of coresidence days needed for dyad to be included. Defaults to 60 days.
#' @param within_grp Logical value indicating whether regressions should be fit relative to dyads in entire population (default) or to dyads within-group
#' @param directional Logical value indicating whether to preserve directionality
#'
#' @return The input row with an additional list column containing the subset
#'
#' @examples
get_dyadic_subset <- function(df, biograph_l, members_l, focals_l, females_l,
                              interactions_l, min_cores_days, within_grp,
                              directional) {

  # Find and return all co-residence dates for focal_sname and partner_sname in my_members
  get_overlap_dates <- function(focal_sname, partner_sname, focal_grp, partner_grp) {

    focal_dates <- my_members %>%
      dplyr::filter(sname == focal_sname & grp == focal_grp) %>%
      dplyr::pull(date)

    partner_dates <- my_members %>%
      dplyr::filter(sname == partner_sname & grp == partner_grp) %>%
      dplyr::pull(date)

    overlap_dates <- dplyr::intersect(focal_dates, partner_dates)

    overlap_dates <- my_focals %>%
      dplyr::filter(date %in% overlap_dates & grp == focal_grp) %>%
      dplyr::pull(date)

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
  get_interactions <- function(my_actor, my_actee, focal_grp, partner_grp) {

    res <- my_interactions %>%
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

    # This line allows for the focal animal only to be represented as a non-adult
    my_members <- dplyr::filter(my_members, (sex == "F" & (date >= matured | sname == my_sname)) |
                                  (sex == "M" & (date >= ranked | sname == my_sname)))

    my_focals <- dplyr::filter(focals_l, grp == my_grp & date >= my_start & date <= my_end)
    my_females <- dplyr::filter(females_l, grp == my_grp & date >= my_start & date <= my_end)
    my_interactions <- interactions_l %>%
      dplyr::filter((actor_grp == my_grp | actee_grp == my_grp) & date >= my_start & date <= my_end)

    # This line allows for the focal animal only to be represented as a non-adult
    my_interactions <- my_interactions %>%
      dplyr::filter((is_actor_adult & is_actee_adult) |
                      (is_actor_adult & actee == my_sname) |
                      (is_actee_adult & actor == my_sname))

    # Find all distinct members WITHIN GROUP between start and end dates
    # For each animal in each group durign this time, calculate:
    # number of days present, first date, last date
    my_subset <- my_members %>%
      dplyr::rename(sname_sex = sex) %>%
      dplyr::inner_join(select(df, -sname, -sex), by = c("grp")) %>%
      dplyr::group_by(sname, grp, sname_sex) %>%
      dplyr::summarise(days_present = dplyr::n(),
                       start = min(date),
                       end = max(date))
  } else {
    my_members <- dplyr::filter(members_l, date >= my_start & date <= my_end)

    # This line allows for the focal animal only to be represented as a non-adult
    my_members <- dplyr::filter(my_members, (sex == "F" & (date >= matured | sname == my_sname)) |
                                  (sex == "M" & (date >= ranked | sname == my_sname)))

    my_focals <- dplyr::filter(focals_l, date >= my_start & date <= my_end)
    my_females <- dplyr::filter(females_l, date >= my_start & date <= my_end)

    my_interactions <- dplyr::filter(interactions_l, date >= my_start & date <= my_end)

    # This line allows for the focal animal only to be represented as a non-adult
    my_interactions <- my_interactions %>%
      dplyr::filter((is_actor_adult & is_actee_adult) |
                      (is_actor_adult & actee == my_sname) |
                      (is_actee_adult & actor == my_sname))

    if (any(purrr::map_int(list(my_members, my_females, my_focals, my_interactions),
                           nrow) == 0)) {
      return(dplyr::tbl_df(NULL))
    }

    # Find all distinct members IN POPULATION between start and end dates
    # For each animal in each group durign this time, calculate:
    # number of days present, first date, last date
    my_subset <- my_members %>%
      dplyr::rename(sname_sex = sex) %>%
      dplyr::group_by(sname, grp, sname_sex) %>%
      dplyr::summarise(days_present = dplyr::n(),
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

  # Remove male-male dyads for grooming
  if (interactions_l$act[[1]] == "G") {
    my_subset <- my_subset %>%
      dplyr::filter(!(sname_sex == "M" & partner_sex == "M"))
  }

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

  # Remove coresidence_dates to make object simpler
  my_subset <- dplyr::select(my_subset, -coresidence_dates)

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

  ## Interactions between dyad during the period of interest
  # Since grooming dates are 1st of month for many years, it does not work
  # to restrict this to co-resident dates only. Instead, use start and end.
  # Co-residence is (usually) implied by the grooming interaction.
  # i_given is behavior given by sname to partner
  # i_received is behavior received by sname from partner
  my_subset <- my_subset %>%
    dplyr::mutate(i_given = purrr::pmap_dbl(list(sname, partner, grp, partner_grp),
                                     get_interactions),
           i_received = purrr::pmap_dbl(list(partner, sname, partner_grp, grp),
                                        get_interactions),
           i_total = i_given + i_received)

  if (!directional) {

    # Calculate variables
    my_subset <- my_subset %>%
      dplyr::mutate(i_adj = i_total / coresidence_days,
                    log2_i_adj = log2(i_adj))

    # Classify dyads by dyad type ("F-F", "F-M", or "M-M"), and nest by dyad type
    # Since this is not directional, "F-M" and "M-F" are combined into one category: F-M
    my_subset <- my_subset %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dyad = paste(sort(c(sname, partner)), collapse = '-'),
                    dyad_type = paste(sort(c(sname_sex, partner_sex)), collapse = '-')) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dyad_type) %>%
      tidyr::nest()

    # Fit regression separately for the two dyad types and get residuals
    my_subset <- my_subset %>%
      dplyr::mutate(data = purrr::map(data, fit_dyadic_regression)) %>%
      tidyr::unnest()

    # Reorganize columns
    my_subset <- my_subset %>%
      dplyr::select(sname, sname_sex, partner, partner_sex, everything()) %>%
      dplyr::arrange(sname, partner)
  } else {
    # Behaviors for which direction needs to be preserved, e.g., agonism

    # Duplicate each row to have one for sname->partner and one for partner->sname:
    sub_d1 <- my_subset %>%
      dplyr::rename_at(vars(contains("partner")), funs(sub('partner', 'anim2', .))) %>%
      dplyr::rename("anim1" = "sname", "anim1_grp" = "grp", "anim1_sex" = "sname_sex") %>%
      dplyr::select(contains("anim1"), contains("anim2"), everything())

    # Reverse the relevant columns
    sub_d2 <- my_subset %>%
      dplyr::rename_at(vars(contains("partner")), funs(sub('partner', 'anim1', .))) %>%
      dplyr::rename("anim2" = "sname", "anim2_grp" = "grp", "anim2_sex" = "sname_sex",
             "i_given" = "i_received", "i_received" = "i_given") %>%
      dplyr::select(contains("anim1"), contains("anim2"), everything())

    # Combine the two sets
    my_subset <- dplyr::bind_rows(sub_d1, sub_d2)

    # Calculate variables
    my_subset <- my_subset %>%
      dplyr::mutate(i_adj = i_given / coresidence_days,
                    log2_i_adj = log2(i_adj))

    # Directional dyad type: F-F, F-M, M-M, or M-F
    # Since this is directional, "F-M" and "M-F" are distinguished
    # Nest by dyad type
    my_subset <- my_subset %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dyad = paste(anim1, anim2, sep = '-'),
                    dyad_type = paste(anim1_sex, anim2_sex, sep = '-')) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dyad_type) %>%
      tidyr::nest()

    # Fit regression separately for the different dyad types and get residuals
    my_subset <- my_subset %>%
      dplyr::mutate(data = purrr::map(data, fit_dyadic_regression)) %>%
      tidyr::unnest()

    # Reorganize columns
    my_subset <- my_subset %>%
      dplyr::select(sname = anim1, grp = anim1_grp, sname_sex = anim1_sex,
                    partner = anim2, partner_grp = anim2_grp,
                    partner_sex = anim2_sex, everything()) %>%
      dplyr::arrange(sname, partner)
  }

  return(my_subset)
}

#' Fit dyadic index regression on subset of data
#'
#' @param df A subset of data on which to fit a regression of interactions on observer effort.
#'
#' @return The input data with an additional column containing the regression residuals.
#'
#' @examples
fit_dyadic_regression <- function(df) {

  if (all(df$i_adj == 0)) {
    return(tbl_df(NULL))
  }

  # There will be lots of zeros in most subsets
  # If present, remove these before fitting regression model
  zero_subset <- dplyr::filter(df, i_adj == 0)

  if (nrow(zero_subset) > 0) {

    # Fit regression to non-zero values
    nonzero_subset <- dplyr::filter(df, i_adj != 0)
    nonzero_subset$res_i_adj <- as.numeric(residuals(lm(data = nonzero_subset, log2_i_adj ~ log2OE)))

    # Assign a value of -Inf for the residuals of zero values (for calculating quantiles)
    zero_subset$res_i_adj <- -Inf

    # Combine with zero and non-zero subsets
    df <- dplyr::bind_rows(zero_subset, nonzero_subset)
  } else {
    df$res_i_adj <- as.numeric(residuals(lm(data = df, log2_i_adj ~ log2OE)))
  }

  return(df)

}


#' Calculate dyadic variables for focal animal from a dyadic subset
#'
#' @param my_sname
#' @param my_grp
#' @param my_subset
#'
#' @return The input data with an additional list column containing the dyadic variables.
#'
#' @examples
get_focal_index <- function(my_sname, my_grp, my_subset) {

  # Return an empty tibble if the subset is empty
  if (nrow(my_subset) == 0) {
    return(dplyr::tbl_df(NULL))
  }

  # Calculate 50 and 90 percentiles from full set of residuals
  # This represents all dyads of a given type in the group during the year
  # NOT including the "zero-interaction" values set to -Inf
  percs <- my_subset %>%
    dplyr::group_by(dyad_type) %>%
    dplyr::filter(i_adj > 0) %>%
    dplyr::summarise(perc_50 = quantile(res_i_adj, probs = 0.5),
                     perc_90 = quantile(res_i_adj, probs = 0.9))

  # Add percentile columns to my_subset
  my_subset <- dplyr::left_join(my_subset, percs, by = "dyad_type")

  # The focal subset should contain only dyads that include my_sname
  # This can be in either the sname or partner column
  # Categorize as very strongly bonded, strongly bonded, weakly bonded, or not bonded
  focal_di <- my_subset %>%
    dplyr::filter(grp == my_grp & (sname == my_sname | partner == my_sname)) %>%
    dplyr::mutate(bond_strength = dplyr::case_when(
      res_i_adj >= perc_90 ~ "VeryStronglyBonded",
      res_i_adj >= perc_50 ~ "StronglyBonded",
      res_i_adj >= -9999999 ~ "WeaklyBonded",
      TRUE ~ "NotBonded"))

  return(focal_di)
}


#' Create summary data from dyadic index input
#'
#' @param df Dyadic index input data produced by the 'dyadic_index' function.
#'
#' @return An individual-year-of-life data set with summarized dyadic variables.
#' @export
#'
#' @examples
dyadic_index_summary <- function(df) {

  # Return an empty tibble if the subset is empty
  if (is.null(df) |
      !all(names(df) %in% c("sname", "grp", "start", "end", "days_present", "sex",
                            "birth", "first_start_date", "statdate", "birth_dates",
                            "midpoint", "age_start_yrs", "age_class", "subset", "di", "obs_date"))) {
    stop("Problem with input data. Use the 'dyadic_index' function to create the input.")
  }

  directional <- attr(df, "directional")

  df$di_sum <- list(NULL)
  pb <- txtProgressBar(min = 0, max = nrow(df), style = 3) # Progress bar
  for (i in 1:nrow(df)) {
    df[i, ]$di_sum <- list(dyadic_row_summary(df$di[[i]], df$sname[[i]], directional))
    setTxtProgressBar(pb, i)
  }
  close(pb)

  df <- df %>%
    dplyr::select(-subset, -di) %>%
    tidyr::unnest()

  di_strength <- df %>%
    dplyr::select(-top_partners, -r_quantity, -r_reciprocity) %>%
    tidyr::unnest() %>%
    dplyr::select(-n)

  di_recip <- df %>%
    dplyr::select(-top_partners, -r_quantity, -r_strength) %>%
    tidyr::unnest() %>%
    dplyr::select(-n)

  if (directional) {
    di_strength <- di_strength %>%
      dplyr::mutate(DSI_type = case_when(
        sex == "M" & dyad_type %in% c("F-M", "M-F") ~ "DSI_F",
        sex == "M" & dyad_type == "M-M" ~ "DSI_M",
        sex == "F" & dyad_type %in% c("F-M", "M-F") ~ "DSI_M",
        sex == "F" & dyad_type == "F-F" ~ "DSI_F"),
        sex = forcats::fct_recode(sex, Male = "M", Female = "F"),
        DSI_type = paste(DSI_type, direction, sep = "_")) %>%
      dplyr::select(-dyad_type, -direction) %>%
      tidyr::spread(DSI_type, r_strength) %>%
      dplyr::select(sname, grp, start, end, contains("DSI"))

    di_quantity <- df %>%
      dplyr::select(-top_partners, -r_strength, -r_reciprocity) %>%
      tidyr::unnest() %>%
      dplyr::mutate(DSI_type = case_when(
        sex == "M" & dyad_type %in% c("F-M", "M-F") ~ "F",
        sex == "M" & dyad_type == "M-M" ~ "M",
        sex == "F" & dyad_type %in% c("F-M", "M-F") ~ "M",
        sex == "F" & dyad_type == "F-F" ~ "F"),
        sex = forcats::fct_recode(sex, Male = "M", Female = "F"),
        DSI_type = paste(DSI_type, direction, sep = "_")) %>%
      dplyr::select(-dyad_type, -direction) %>%
      tidyr::gather(bond_cat, n_bonds, contains("Bonded")) %>%
      tidyr::unite(var, bond_cat, DSI_type) %>%
      tidyr::spread(var, n_bonds, fill = 0) %>%
      dplyr::select(sname, grp, start, end, ends_with("_Dir"), ends_with("_Rec"))

    di_recip <- di_recip %>%
      dplyr::mutate(DSI_type = case_when(
        sex == "M" & dyad_type %in% c("F-M", "M-F") ~ "recip_F",
        sex == "M" & dyad_type == "M-M" ~ "recip_M",
        sex == "F" & dyad_type %in% c("F-M", "M-F") ~ "recip_M",
        sex == "F" & dyad_type == "F-F" ~ "recip_F"),
        sex = forcats::fct_recode(sex, Male = "M", Female = "F"),
        DSI_type = paste(DSI_type, direction, sep = "_")) %>%
      dplyr::select(-dyad_type, -direction) %>%
      tidyr::spread(DSI_type, r_reciprocity) %>%
      dplyr::select(sname, grp, start, end, contains("recip"))
  } else {
    di_strength <- di_strength %>%
      dplyr::mutate(DSI_type = case_when(
        sex == "M" & dyad_type == "M-M" ~ "DSI_M",
        sex == "M" & dyad_type == "F-M" ~ "DSI_F",
        sex == "F" & dyad_type == "F-M" ~ "DSI_M",
        sex == "F" & dyad_type == "F-F" ~ "DSI_F"),
        sex = forcats::fct_recode(sex, Male = "M", Female = "F")) %>%
      dplyr::select(-dyad_type) %>%
      tidyr::spread(DSI_type, r_strength) %>%
      dplyr::select(sname, grp, start, end, one_of("DSI_F", "DSI_M"))

    di_quantity <- df %>%
      dplyr::select(-top_partners, -r_strength, -r_reciprocity) %>%
      tidyr::unnest() %>%
      dplyr::mutate(DSI_type = case_when(
        sex == "M" & dyad_type == "M-M" ~ "M",
        sex == "M" & dyad_type == "F-M" ~ "F",
        sex == "F" & dyad_type == "F-M" ~ "M",
        sex == "F" & dyad_type == "F-F" ~ "F"),
        sex = forcats::fct_recode(sex, Male = "M", Female = "F")) %>%
      dplyr::select(-dyad_type) %>%
      tidyr::gather(bond_cat, n_bonds, contains("Bonded")) %>%
      tidyr::unite(var, bond_cat, DSI_type) %>%
      tidyr::spread(var, n_bonds, fill = 0) %>%
      dplyr::select(sname, grp, start, end, ends_with("_M"), ends_with("_F"))

    di_recip <- di_recip %>%
      dplyr::mutate(DSI_type = case_when(
        sex == "M" & dyad_type == "M-M" ~ "recip_M",
        sex == "M" & dyad_type == "F-M" ~ "recip_F",
        sex == "F" & dyad_type == "F-M" ~ "recip_M",
        sex == "F" & dyad_type == "F-F" ~ "recip_F"),
        sex = forcats::fct_recode(sex, Male = "M", Female = "F")) %>%
      dplyr::select(-dyad_type) %>%
      tidyr::spread(DSI_type, r_reciprocity) %>%
      dplyr::select(sname, grp, start, end, one_of("recip_F", "recip_M"))
  }

  di_summary <- df %>%
    dplyr::select(-top_partners, -starts_with("r_")) %>%
    dplyr::left_join(di_strength, by = c("sname", "grp", "start", "end")) %>%
    dplyr::left_join(di_quantity, by = c("sname", "grp", "start", "end")) %>%
    dplyr::left_join(di_recip, by = c("sname", "grp", "start", "end"))

  return(di_summary)
}


#' Summarize a single DSI subset.
#' Calculate top partners, bond quantity, bond strength, and bond reciprocity
#' for each individual-year-of-life separately for the focal animal's top 3
#' grooming partners, separately for F-M and F-F dyads (if applicable)
#'
#' @param df A DSI subset
#' @param focal Focal animal
#' @param directional Logical value indicating whether to preserve directionality
#'
#' @return The input data with additional list columns.
#' @export
#'
#' @examples
dyadic_row_summary <- function(df, focal, directional) {

  # Return an empty tibble if the subset is empty
  if (nrow(df) == 0) {
    return(dplyr::tbl_df(NULL))
  }

  # Reciprocity is the mean of interaction asymmetry for the top three partners
  if (directional) {

    df <- df %>%
      dplyr::mutate(direction = dplyr::if_else(sname == focal, "Dir", "Rec"))

    # Relationship quantity is the number of bonds in each bond-strength category
    r_quantity <- df %>%
      dplyr::group_by(dyad_type, direction) %>%
      dplyr::count(bond_strength) %>%
      tidyr::spread(bond_strength, n)

    # Top partners are the top three interaction partners
    # This is calculated separately for each dyad type
    top_partners <- df %>%
      dplyr::filter(res_i_adj > -9999) %>%
      dplyr::arrange(dyad_type, direction, desc(res_i_adj)) %>%
      dplyr::group_by(dyad_type, direction) %>%
      dplyr::slice(1:3)

    # Relationship strength is the mean of the index value for the top three partners
    r_strength <- top_partners %>%
      dplyr::filter(res_i_adj > -9999) %>%
      dplyr::group_by(dyad_type, direction) %>%
      dplyr::summarise(r_strength = mean(res_i_adj, na.rm = TRUE),
                       n = n())

    r_reciprocity <- top_partners %>%
      dplyr::mutate(recip = (i_received - i_given) / (i_given + i_received)) %>%
      dplyr::group_by(dyad_type, direction) %>%
      dplyr::summarise(r_reciprocity = mean(recip, na.rm = TRUE),
                       n = n())
  } else {
    # Relationship quantity is the number of bonds in each bond-strength category
    r_quantity <- df %>%
      dplyr::group_by(dyad_type) %>%
      dplyr::count(bond_strength) %>%
      tidyr::spread(bond_strength, n)

    # Top partners are the top three interaction partners
    # This is calculated separately for each dyad type
    top_partners <- df %>%
      dplyr::filter(res_i_adj > -9999) %>%
      dplyr::arrange(dyad_type, desc(res_i_adj)) %>%
      dplyr::group_by(dyad_type) %>%
      dplyr::slice(1:3)

    # Relationship strength is the mean of the index value for the top three partners
    r_strength <- top_partners %>%
      dplyr::filter(res_i_adj > -9999) %>%
      dplyr::group_by(dyad_type) %>%
      dplyr::summarise(r_strength = mean(res_i_adj, na.rm = TRUE),
                       n = n())

    r_reciprocity <- top_partners %>%
      dplyr::mutate(recip = 1 - abs((i_given - i_received) / (i_given + i_received))) %>%
      dplyr::group_by(dyad_type) %>%
      dplyr::summarise(r_reciprocity = mean(recip, na.rm = TRUE),
                       n = n())
  }

  res <- tibble(top_partners = list(top_partners),
                r_quantity = list(r_quantity),
                r_strength = list(r_strength),
                r_reciprocity = list(r_reciprocity))

  return(res)
}
