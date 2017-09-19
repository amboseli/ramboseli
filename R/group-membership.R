
#' get_n_members
#'
#' @param biograph A DBI connection to the babase table biograph, or an equivalent biograph table in your R environment
#' @param members A DBI connection to the babase table members, or an equivalent biograph table in your R environment
#' @param my_grp The group of interest
#' @param my_date The date of interest
#' @param my_sex  The sex of interest. If null, assumes all individuals.
#'
#' @return A count of individuals in the group on that date.
#' @export
#'
#' @examples
get_n_members <- function(biograph, members, my_grp, my_date, my_sex = NULL) {

  n_members <- members %>%
    dplyr::filter(date == my_date & grp == my_grp) %>%
    dplyr::inner_join(biograph, by = "sname") %>%
    dplyr::collect()

  if (!is.null(my_sex)) {
    n_members <- n_members %>%
      dplyr::filter(sex == my_sex)
  }

  n_members <- n_members %>%
    nrow()

  return(n_members)

}
