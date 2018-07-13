library(tidyverse)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(scales)
library(rcartocolor)
# library(patchwork)
library(RColorBrewer)

dsi_pop <- readRDS("data/dsi-pop_2018-06-08.RDS")

focal_col <- carto_pal(12, "Bold")[5]
partner_col <- carto_pal(12, "Bold")[2]



# sample ------------------------------------------------------------------

# my_row <- dsi_pop[6, ]
my_row <- dsi_pop[13, ]
# my_row <- dsi_pop %>%
#   filter(sex == "F") %>%
#   sample_n(1)

my_focal <- my_row$sname[[1]]
my_grp <- my_row$grp[[1]]
my_subset <- my_row$subset[[1]]

top_partners <- my_row$di[[1]] %>%
  dplyr::filter(res_i_adj > -9999) %>%
  dplyr::arrange(dyad_type, desc(res_i_adj)) %>%
  dplyr::group_by(dyad_type) %>%
  dplyr::slice(1:3) %>%
  filter(dyad_type == "F-F")

top_partners <- c(pull(top_partners, partner), pull(top_partners, sname)) %>%
  unique()

top_partners <- top_partners[top_partners != my_focal]

temp <- my_subset %>%
  rename(from = sname, to = partner)

temp$grp_fct <- as.factor(temp$grp)

temp <- filter(temp, dyad_type == "F-F" & res_i_adj > -10)

grps <- bind_rows(select(temp, name = from, grp), select(temp, name = to, grp)) %>%
  distinct(name, grp)

sxs <- bind_rows(select(temp, name = from, sex = sname_sex),
                 select(temp, name = to, sex = partner_sex)) %>%
  distinct(name, sex)

f_graph <- as_tbl_graph(temp, directed = TRUE, temp) %>%
  activate(nodes) %>%
  mutate(is_focal = as.factor(if_else(name == my_focal, "Focal",
                                      if_else(name %in% top_partners, "Top", "Not")))) %>%
  left_join(grps)

focal_x <- ggraph::create_layout(f_graph, "kk") %>%
  filter(name == my_focal & grp == my_grp) %>%
  pull(x)

focal_y <- ggraph::create_layout(f_graph, "kk") %>%
  filter(name == my_focal & grp == my_grp) %>%
  pull(y)

partner_x <- ggraph::create_layout(f_graph, "kk") %>%
  filter(name %in% top_partners & grp == my_grp) %>%
  pull(x)

partner_y <- ggraph::create_layout(f_graph, "kk") %>%
  filter(name %in% top_partners & grp == my_grp) %>%
  pull(y)


# plots -------------------------------------------------------------------

# Number of Focals
p_focals <- ggraph(f_graph, layout = 'kk') +
  geom_edge_link0(aes(width = n_focals, color = n_focals)) +
  geom_node_point(aes(fill = is_focal), shape = 21,
                  color = "black", size = 1.5, show.legend = FALSE) +
  scale_fill_manual(values = c(focal_col, "white", "white")) +
  # geom_node_point(show.legend = FALSE, shape = 21,
  #                 color = "black", size = 1.5, fill = "white") +
  scale_edge_color_gradientn(colours = rcartocolor::carto_pal(7, "OrYel"),
                             name = NULL, limits = c(0, max(my_subset$n_focals))) +
  scale_edge_width(range = c(0.1, 0.5), guide = FALSE) +
  coord_equal() +
  theme_void() +
  labs(subtitle = "Number of Focal Samples") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate(geom = "text", x = focal_x + 0.45, y = focal_y, label = my_focal,
           color = focal_col, size = 3, hjust = 0) +
  # annotate(geom = "text", x = partner_x + 0.25, y = partner_y + 0.25, label = top_partners,
  #          color = partner_col, size = 3, hjust = 0) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25, "cm"))

# Number of Females
p_females <- ggraph(f_graph, layout = 'kk') +
  geom_edge_link0(aes(width = n_females, color = n_females)) +
  geom_node_point(aes(fill = is_focal), shape = 21,
                  color = "black", size = 1.5, show.legend = FALSE) +
  scale_fill_manual(values = c(focal_col, "white", "white")) +
  # geom_node_point(show.legend = FALSE, shape = 21,
  #                 color = "black", size = 1.5, fill = "white") +
  scale_edge_color_gradientn(colours = rcartocolor::carto_pal(7, "Mint"),
                             name = NULL, limits = c(0, max(my_subset$n_females))) +
  scale_edge_width(range = c(0.1, 0.5), guide = FALSE) +
  coord_equal() +
  theme_void() +
  labs(subtitle = "Number of Females") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate(geom = "text", x = focal_x + 0.45, y = focal_y, label = my_focal,
           color = focal_col, size = 3, hjust = 0) +
  # annotate(geom = "text", x = partner_x + 0.25, y = partner_y + 0.25, label = top_partners,
  #          color = partner_col, size = 3, hjust = 0) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25, "cm"))

# Co-residence Days
p_cores <- ggraph(f_graph, layout = 'kk') +
  geom_edge_link0(aes(width = coresidence_days, color = coresidence_days)) +
  geom_node_point(aes(fill = is_focal), shape = 21,
                  color = "black", size = 1.5, show.legend = FALSE) +
  scale_fill_manual(values = c(focal_col, "white", "white")) +
  # geom_node_point(show.legend = FALSE, shape = 21,
  #                 color = "black", size = 1.5, fill = "white") +
  scale_edge_color_gradientn(colours = rcartocolor::carto_pal(7, "Magenta"),
                             name = NULL, limits = c(0, 366)) +
  scale_edge_width(range = c(0.1, 0.5), guide = FALSE) +
  coord_equal() +
  theme_void() +
  labs(subtitle = "Co-residence Days") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate(geom = "text", x = focal_x + 0.45, y = focal_y, label = my_focal,
           color = focal_col, size = 3, hjust = 0) +
  # annotate(geom = "text", x = partner_x + 0.25, y = partner_y + 0.25, label = top_partners,
  #          color = partner_col, size = 3, hjust = 0) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25, "cm"))

# OE
p_oe <- ggraph(f_graph, layout = 'kk') +
  geom_edge_link0(aes(width = OE, color = OE)) +
  geom_node_point(aes(fill = is_focal), shape = 21,
                  color = "black", size = 1.5, show.legend = FALSE) +
  scale_fill_manual(values = c(focal_col, "white", "white")) +
  # geom_node_point(show.legend = FALSE, shape = 21,
  #                 color = "black", size = 1.5, fill = "white") +
  scale_edge_color_gradientn(colours = rev(viridis::cividis(20)),
                             # colours = rcartocolor::carto_pal(7, "Teal"),
                             name = NULL) +
  scale_edge_width(range = c(0.25, 1), guide = FALSE) +
  coord_equal() +
  theme_void() +
  labs(subtitle = "Observer Effort") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate(geom = "text", x = focal_x + 0.45, y = focal_y, label = my_focal,
           color = focal_col, size = 3, hjust = 0) +
  # annotate(geom = "text", x = partner_x + 0.25, y = partner_y + 0.25, label = top_partners,
  #          color = partner_col, size = 3, hjust = 0) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25, "cm"))

# Observer Effort
p1 <- plot_grid(plot_grid(p_focals, p_females, p_cores, ncol = 1, scale = 0.9),
                p_oe, ncol = 2)

# p2 ----------------------------------------------------------------------


# Total Grooming
p_groom <- ggraph(f_graph, layout = 'kk') +
  geom_edge_link0(aes(width = i_total, color = i_total)) +
  geom_node_point(aes(fill = is_focal), shape = 21,
                  color = "black", size = 1.5, show.legend = FALSE) +
  scale_fill_manual(values = c(focal_col, "white", "white")) +
  # geom_node_point(show.legend = FALSE, shape = 21,
  #                 color = "black", size = 1.5, fill = "white") +
  scale_edge_color_gradientn(colours = carto_pal(7, "Emrld"),
                             name = NULL, limits = c(0, max(my_subset$i_total))) +
  scale_edge_width(range = c(0.25, 1), guide = FALSE) +
  coord_equal() +
  theme_void() +
  labs(subtitle = "Number of Grooming Interactions") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate(geom = "text", x = focal_x + 0.45, y = focal_y, label = my_focal,
           color = focal_col, size = 3, hjust = 0) +
  # annotate(geom = "text", x = partner_x + 0.25, y = partner_y + 0.25, label = top_partners,
  #          color = partner_col, size = 3, hjust = 0) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25, "cm"))

# Daily Grooming Rate
p_groomrate <- ggraph(f_graph, layout = 'kk') +
  geom_edge_link0(aes(width = i_adj, color = i_adj)) +
  geom_node_point(aes(fill = is_focal), shape = 21,
                  color = "black", size = 1.5, show.legend = FALSE) +
  scale_fill_manual(values = c(focal_col, "white", "white")) +
  # geom_node_point(show.legend = FALSE, shape = 21,
  #                 color = "black", size = 1.5, fill = "white") +
  scale_edge_color_gradientn(colours = rcartocolor::carto_pal(7, "Burg"),
                             name = NULL, limits = c(0, max(my_subset$i_adj))) +
  scale_edge_width(range = c(0.25, 1), guide = FALSE) +
  coord_equal() +
  theme_void() +
  labs(subtitle = "Daily Grooming Rate") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate(geom = "text", x = focal_x + 0.45, y = focal_y, label = my_focal,
           color = focal_col, size = 3, hjust = 0) +
  # annotate(geom = "text", x = partner_x + 0.25, y = partner_y + 0.25, label = top_partners,
  #          color = partner_col, size = 3, hjust = 0) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25, "cm"))


# Adjusted grooming
p2 <- plot_grid(plot_grid(p_groom, p_cores, ncol = 1, scale = 0.9), p_groomrate, ncol = 2)

# p3 ----------------------------------------------------------------------

# Log Adjusted Grooming
p_log_groom <- ggraph(f_graph, layout = 'kk') +
  geom_edge_link0(aes(width = log2_i_adj, color = log2_i_adj)) +
  geom_node_point(aes(fill = is_focal), shape = 21,
                  color = "black", size = 1.5, show.legend = FALSE) +
  scale_fill_manual(values = c(focal_col, "white", "white")) +
  # geom_node_point(show.legend = FALSE, shape = 21,
  #                 color = "black", size = 1.5, fill = "white") +
  scale_edge_color_gradientn(colours = rcartocolor::carto_pal(7, "Burg"),
                             name = NULL) +
  scale_edge_width(range = c(0.25, 1), guide = FALSE) +
  coord_equal() +
  theme_void() +
  labs(subtitle = "Log Daily Grooming Rate") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate(geom = "text", x = focal_x + 0.45, y = focal_y, label = my_focal,
           color = focal_col, size = 3, hjust = 0) +
  # annotate(geom = "text", x = partner_x + 0.25, y = partner_y + 0.25, label = top_partners,
  #          color = partner_col, size = 3, hjust = 0) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25, "cm"))

# Log OE
p_log_oe <- ggraph(f_graph, layout = 'kk') +
  geom_edge_link0(aes(width = log2OE, color = log2OE)) +
  geom_node_point(aes(fill = is_focal), shape = 21,
                  color = "black", size = 1.5, show.legend = FALSE) +
  scale_fill_manual(values = c(focal_col, "white", "white")) +
  # geom_node_point(show.legend = FALSE, shape = 21,
  #                 color = "black", size = 1.5, fill = "white") +
  scale_edge_color_gradientn(colours = rev(viridis::cividis(20)),
                             # colours = rcartocolor::carto_pal(7, "Teal"),
                             name = NULL) +
  scale_edge_width(range = c(0.25, 1), guide = FALSE) +
  coord_equal() +
  theme_void() +
  labs(subtitle = "Log Observer Effort") +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  annotate(geom = "text", x = focal_x + 0.45, y = focal_y, label = my_focal,
           color = focal_col, size = 3, hjust = 0) +
  # annotate(geom = "text", x = partner_x + 0.25, y = partner_y + 0.25, label = top_partners,
  #          color = partner_col, size = 3, hjust = 0) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25, "cm"))


# DSI
lim <- max(abs(my_subset[my_subset$i_total > 0, ]$res_i_adj), na.rm = TRUE)
p_dsi <- ggraph(f_graph, layout = 'kk') +
  geom_edge_link0(aes(width = res_i_adj, color = res_i_adj)) +
  geom_node_point(aes(fill = is_focal), show.legend = FALSE, shape = 21,
                  color = "black", size = 2.5) +
  scale_fill_manual(values = c(focal_col, "white", partner_col)) +
  scale_edge_color_gradientn(colours = rcartocolor::carto_pal(7, "Purp"),
                             name = NULL, limits = c(-lim, lim)) +
  scale_edge_width(range = c(0.25, 1), guide = FALSE) +
  coord_equal() +
  theme_void() +
  labs(subtitle = "Dyadic Grooming Strength") +
  annotate(geom = "text", x = focal_x + 0.45, y = focal_y, label = my_focal,
           color = focal_col, size = 3, hjust = 0) +
  annotate(geom = "text", x = partner_x, y = partner_y + 0.25, label = top_partners,
           color = partner_col, size = 3, hjust = 0.5) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.25, "cm"),
        plot.subtitle = element_text(hjust = 0.5))

# DSI
p3 <- plot_grid(plot_grid(p_log_groom, p_log_oe, ncol = 1, scale = 0.9), p_dsi, ncol = 2)



# timeline ----------------------------------------------------------------

temp <- select(my_row, -subset, -di)

my_date_seq <- seq.Date(from = temp$birth, to = temp$statdate, by = "1 year")

col1 <- brewer.pal(12, "Paired")[2]
col2 <- brewer.pal(12, "Paired")[1]
col3 <- carto_pal(12, "Bold")[4]

p_time <- ggplot(temp) +
  geom_segment(aes(x = start, xend = end, y = 0, yend = 0),
               size = 2, color = focal_col) +
  geom_segment(aes(x = birth, xend = statdate, y = 0, yend = 0)) +
  geom_point(aes(x = birth, y = 0), shape = 21, color = "black", size = 3.5,
             fill = col1) +
  geom_point(aes(x = statdate, y = 0), shape = 21, color = "black", size = 3.5,
             fill = col1) +
  scale_x_date(breaks = my_date_seq, labels = seq(0, length(my_date_seq) - 1)) +
  annotate(geom = "point", x = my_date_seq[-1], y = 0, size = 0.75) +
  annotate(geom = "text", label = "Birth", x = temp$birth, y = 0.5,
           hjust = 0.5, color = col1, size = 3.5) +
  annotate(geom = "text", label = "Censored", x = temp$statdate, y = 0.5,
           hjust = 0.5, color = col1, size = 3.5) +
  annotate(geom = "text", label = temp$birth, x = temp$birth, y = 0.25,
           hjust = 0.5, color = col1, size = 3.5) +
  annotate(geom = "text", label = temp$statdate, x = temp$statdate, y = 0.25,
           hjust = 0.5, color = col1, size = 3.5) +
  annotate(geom = "text", label = "7th Year of Life", x = temp$start + months(6), y = 0.5,
           hjust = 0.5, color = focal_col, size = 3.5) +
  annotate(geom = "text", label = temp$start, x = temp$start - months(4), y = 0.25,
           hjust = 1, color = focal_col, size = 3.5) +
  annotate(geom = "text", label = temp$end, x = temp$end + months(4), y = 0.25,
           hjust = 0, color = focal_col, size = 3.5) +
  annotate(geom = "segment", x = temp$start - months(3), xend = temp$start,
           y = 0.25, yend = 0.25,
           color = focal_col) +
  annotate(geom = "segment", x = temp$end + months(3), xend = temp$end,
           y = 0.25, yend = 0.25,
           color = focal_col) +
  annotate(geom = "segment", x = temp$start, xend = temp$start,
           y = 0.2, yend = 0.3,
           color = focal_col) +
  annotate(geom = "segment", x = temp$end, xend = temp$end,
           y = 0.2, yend = 0.3,
           color = focal_col) +
  labs(x = "Age", y = NULL) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# combined ----------------------------------------------------------------

plot_grid(plot_grid(NULL, p_time, NULL, nrow = 1, rel_widths = c(1, 2, 1),
                    labels = c("", "A", "")),
          plot_grid(p2, p1, p3, nrow = 1, labels = c("B", "C", "D"), scale = 0.95),
          nrow = 2, rel_heights = c(1, 5))

# 10 x 24
