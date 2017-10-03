Sys.setenv(TZ = 'UTC')
list.of.packages <- list("ramboseli", "tidyverse", "forcats")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(unlist(new.packages))
lapply(list.of.packages, require, character.only = T)

babase <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         host = "localhost",
                         port = 2222,
                         user = "fac13",
                         dbname = "babase")

ranks <- collect(tbl(babase, "ranks"))
rank_dates <- collect(tbl(babase, "rankdates"))

ranks_w_dates <- ranks %>%
  left_join(rank_dates, by = "sname")

prop_ranks <- ranks_w_dates %>%
  filter(!(rnktype == "ALM" & rnkdate < ranked)) %>%
  group_by(grp, rnkdate, rnktype) %>%
  mutate(n_grp = n(),
         prop_rank = case_when(
           n_grp == 1 ~ 1,
           TRUE ~ 1 - (rank - 1) / (n_grp - 1)))

prop_ranks$rnktype <- forcats::fct_recode(prop_ranks$rnktype, ADM = "ALM")
