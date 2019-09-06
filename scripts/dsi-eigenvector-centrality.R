library("tidyverse")
library("tidygraph")
library("ggraph")
library("graphlayouts")

# Read a DSI object produced by the function ramboseli::dyadic_index()
dsi <- readRDS("dsi_2019-04-08.RDS") # Change to name of your data

get_centrality <- function(my_focal, my_grp, my_subset) {

  my_network <- my_subset %>%
    rename(from = sname, to = partner) %>%
    mutate(grp_fct = factor(grp),
           from = paste(from, grp, sep = "_"),
           to = paste(to, grp, sep = "_")) %>%
    filter(res_i_adj > -10)

  grps <- bind_rows(select(my_network, name = from, grp),
                    select(my_network, name = to, grp)) %>%
    distinct(name, grp)

  sxs <- bind_rows(select(my_network, name = from, sex = sname_sex),
                   select(my_network, name = to, sex = partner_sex)) %>%
    distinct(name, sex)

  f_graph <- as_tbl_graph(my_network, directed = FALSE, my_network) %>%
    activate(nodes) %>%
    left_join(grps, by = "name") %>%
    left_join(sxs, by = "name") %>%
    mutate(sname = str_sub(name, 1, 3))

  # Calculate centrality metrics
  res <- f_graph %>%
    morph(to_split, grp) %>%
    mutate(eigen_wt = centrality_eigen(weights = res_i_adj)
           # eigen = centrality_eigen(),
           # betweenness = centrality_betweenness(normalized = TRUE),
           # degree = centrality_degree(normalized = TRUE)
           ) %>%
    unmorph()

  return(res)
}

# Apply the weighted eigenvector centrality function to each network (contained in the 'subset' column)
# Warning: will take a while if calculating for thousands of networks
dsi <- dsi %>%
  mutate(centrality = suppressMessages(pmap(list(sname, grp, subset), get_centrality)))


# example-table-summary ---------------------------------------------------

# Pull out the centrality value for to the focal individual in each year of life by matching sname and grp
dsi %>%
  select(-di, -subset) %>%
  mutate(network = map(centrality, as_tibble)) %>%
  select(-centrality) %>%
  unnest() %>%
  mutate(name = str_sub(name, 1, 3)) %>%
  filter(sname == name & grp == grp1) %>%
  select(sname, grp, start, end, sex, age_class, eigen_wt)
