iyol <- readRDS("data/iyol_2018-01-10.RDS")
sci <- readRDS("data/sci_2018-01-10.RDS")
dsi <- readRDS("data/dsi_2018-01-10.RDS")
dsi_summary <- readRDS("data/dsi_summary_2018-01-10.RDS")

library(scales)

# plotting ----------------------------------------------------------------

temp <- dsi %>%
  select(-subset) %>%
  unnest()

temp$dsi <- temp$res_g_adj
temp[is.infinite(temp$dsi), ]$dsi <- NA

ggplot(filter(temp, res_g_adj > -9999),
       aes(x = start, y = res_g_adj, group = dyad)) +
  geom_path(alpha = 0.05) +
  facet_wrap(~grp) +
  theme_minimal()

my_sname <- "LAZ"
ggplot(filter(temp, sname == my_sname & dyad_type != "M-M"),
       aes(x = age_class, color = dyad_type)) +
  geom_point(aes(y = dsi, group = dyad), size = 0.75) +
  geom_line(aes(y = dsi, group = dyad)) +
  geom_line(aes(y = res_g_adj, group = dyad), lty = 3) +
  facet_wrap(~dyad) +
  theme_journal() +
  scale_color_brewer(palette = "Set1", name = "Dyad Type") +
  scale_fill_brewer(palette = "Set1", name = "Dyad Type") +
  labs(x = "Year of Life", y = "DSI", title = paste0("Dyadic bonds for ", my_sname))

ggplot(filter(temp, res_g_adj > -9999), aes(x = res_g_adj)) +
  geom_histogram(alpha = 0.5, color = "black") +
  facet_wrap(~grp, scales = "free_y") +
  theme_minimal()

ggplot(filter(temp, res_g_adj > -9999),
       aes(x = start, y = res_g_adj)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours = viridis::viridis(11, option = "magma"),
                       trans = sqrt_trans()) +
  facet_wrap(~grp) +
  theme_minimal()


# dsi_sci -----------------------------------------------------------------

sci_focal <- sci %>%
  unnest() %>%
  mutate(focal = (sname == sname1 & grp == grp1)) %>%
  filter(focal) %>%
  select(sname, grp, start, end, sex, SCI_F, SCI_M) %>%
  gather(connection, sci_value, SCI_M, SCI_F)


# Strength

dsi_strength <- dsi_summary %>%
  select(-top_partners, -r_quantity, -r_reciprocity) %>%
  unnest() %>%
  select(-n)

temp1 <- inner_join(dsi_strength, sci_focal,
                    by = c("sname", "grp", "start", "end", "sex"))

temp1 <- temp1 %>%
  mutate(sex = fct_recode(sex, Males = "M", Females = "F"))

temp1_f <- temp1 %>%
  filter(sex == "Females") %>%
  mutate(dyad_type = fct_recode(dyad_type,
                                "Top Male Partners" = "F-M",
                                "Top Female Partners" = "F-F"))

temp1_m <- temp1 %>%
  filter(sex == "Males") %>%
  mutate(dyad_type = fct_recode(dyad_type, "Top Female Partners" = "F-M"))

strength_p1 <- ggplot(temp1_f, aes(x = r_strength, y = sci_value)) +
  geom_point(alpha = 0.15) +
  facet_grid(connection ~ dyad_type) +
  stat_smooth(method = "lm", color = "firebrick", fill = "firebrick") +
  theme_journal() +
  labs(x = "Mean Bond Strength (DSI) with Top 3 Partners",
       y = "Social Connectedness Index",
       title = "Females")

strength_p2 <- ggplot(temp1_m, aes(x = r_strength, y = sci_value)) +
  geom_point(alpha = 0.15) +
  facet_grid(connection ~ dyad_type) +
  stat_smooth(method = "lm", color = "firebrick", fill = "firebrick") +
  theme_journal() +
  labs(x = "Mean Bond Strength (DSI) with Top 3 Partners",
       y = "Social Connectedness Index",
       title = "Males")

cowplot::plot_grid(strength_p1, strength_p2, ncol = 2, rel_widths = c(1.75, 1),
                   labels = c("a", "b"))


# Quantity

dsi_quantity <- dsi_summary %>%
  select(-top_partners, -r_strength, -r_reciprocity) %>%
  unnest() %>%
  gather(r_class, n_partners, Bonded, Neither, StronglyBonded)

dsi_quantity <- dsi_quantity %>%
  mutate(r_class = fct_recode(r_class, "Not Bonded" = "Neither",
                              "Strongly Bonded" = "StronglyBonded"))

dsi_quantity$r_class <- factor(dsi_quantity$r_class,
                               levels = c("Not Bonded", "Bonded",
                                          "Strongly Bonded"))

temp2 <- inner_join(dsi_quantity, sci_focal,
                    by = c("sname", "grp", "start", "end", "sex"))

temp2 <- temp2 %>%
  mutate(sex = fct_recode(sex, Males = "M", Females = "F"))

temp2_f <- temp2 %>%
  filter(sex == "Females") %>%
  mutate(dyad_type = fct_recode(dyad_type,
                                "Male Partners" = "F-M",
                                "Female Partners" = "F-F"))

temp2_m <- temp2 %>%
  filter(sex == "Males") %>%
  mutate(dyad_type = fct_recode(dyad_type, "Female Partners" = "F-M"))

quantity_p1 <- ggplot(filter(temp2_f, dyad_type == "Female Partners"),
                      aes(x = n_partners, y = sci_value)) +
  geom_point(alpha = 0.15) +
  facet_grid(connection ~ r_class, scales = "free_x") +
  stat_smooth(method = "lm", color = "firebrick", fill = "firebrick") +
  theme_journal() +
  labs(x = "Relationship Quantity (# of Bonds)",
       y = "Social Connectedness Index",
       title = "Females", subtitle = "Female Partners")

quantity_p2 <- ggplot(filter(temp2_f, dyad_type == "Male Partners"),
                      aes(x = n_partners, y = sci_value)) +
  geom_point(alpha = 0.15) +
  facet_grid(connection ~ r_class, scales = "free_x") +
  stat_smooth(method = "lm", color = "firebrick", fill = "firebrick") +
  theme_journal() +
  labs(x = "Relationship Quantity (# of Bonds)",
       y = "Social Connectedness Index",
       title = "Females", subtitle = "Male Partners")

quantity_p3 <- ggplot(filter(temp2_m, dyad_type == "Female Partners"),
                      aes(x = n_partners, y = sci_value)) +
  geom_point(alpha = 0.15) +
  facet_grid(connection ~ r_class, scales = "free_x") +
  stat_smooth(method = "lm", color = "firebrick", fill = "firebrick") +
  theme_journal() +
  labs(x = "Relationship Quantity (# of Bonds)",
       y = "Social Connectedness Index",
       title = "Males", subtitle = "Female Partners")

quantity_p4 <- ggplot(filter(temp2_m, dyad_type == "Male Partners"),
                      aes(x = n_partners, y = sci_value)) +
  geom_blank() +
  theme_journal() +
  theme(panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "", title = "Males", subtitle = "Male Partners")

cowplot::plot_grid(cowplot::plot_grid(quantity_p1, quantity_p2, ncol = 1,
                                      labels = c("a", "b")),
                   cowplot::plot_grid(quantity_p3, quantity_p4, ncol = 1,
                                      labels = c("c", "d")),
                   ncol = 2)


# Reciprocity

dsi_reciprocity <- dsi_summary %>%
  select(-top_partners, -r_quantity, -r_strength) %>%
  unnest() %>%
  select(-n)

temp3 <- inner_join(dsi_reciprocity, sci_focal,
                    by = c("sname", "grp", "start", "end", "sex"))

temp3 <- temp3 %>%
  mutate(sex = fct_recode(sex, Males = "M", Females = "F"))

temp3_f <- temp3 %>%
  filter(sex == "Females") %>%
  mutate(dyad_type = fct_recode(dyad_type,
                                "Top Male Partners" = "F-M",
                                "Top Female Partners" = "F-F"))

temp3_m <- temp3 %>%
  filter(sex == "Males") %>%
  mutate(dyad_type = fct_recode(dyad_type, "Top Female Partners" = "F-M"))

recip_p1 <- ggplot(temp3_f, aes(x = r_reciprocity, y = sci_value)) +
  geom_point(alpha = 0.15) +
  facet_grid(connection ~ dyad_type) +
  stat_smooth(method = "lm", color = "firebrick", fill = "firebrick") +
  theme_journal() +
  labs(x = "Mean Reciprocity with Top 3 Partners",
       y = "Social Connectedness Index",
       title = "Females")

recip_p2 <- ggplot(temp3_m, aes(x = r_reciprocity, y = sci_value)) +
  geom_point(alpha = 0.15) +
  facet_grid(connection ~ dyad_type) +
  stat_smooth(method = "lm", color = "firebrick", fill = "firebrick") +
  theme_journal() +
  labs(x = "Mean Reciprocity with Top 3 Partners",
       y = "Social Connectedness Index",
       title = "Males")

cowplot::plot_grid(recip_p1, recip_p2, ncol = 2, rel_widths = c(1.75, 1),
                   labels = c("a", "b"))




# sci ---------------------------------------------------------------------

sci_yearly <- sci %>%
  unnest() %>%
  mutate(focal = (sname == sname1 & grp == grp1)) %>%
  filter(focal)

sci_yearly$sex <- factor(sci_yearly$sex, labels = c("Females", "Males"))

temp <- sci_yearly %>%
  gather(SCI_type, value, SCI_F, SCI_M)

temp$SCI_type <- fct_recode(temp$SCI_type,
                            "Connectedness To Females" = "SCI_F",
                            "Connectedness To Males" = "SCI_M")

myseed <- 2010
set.seed(myseed)
ggplot(temp, aes(x = age_class, y = value)) +
  geom_point(color = "gray50", size = 0.75, alpha = 0.25,
             position = position_jitter(width = 0.2, height = 0)) +
  stat_smooth(method = "loess",
              color = "firebrick", fill = "firebrick") +
  facet_grid(sex ~ SCI_type) +
  theme_journal_x2() +
  labs(x = "Age", y = "SCI Value")




# dsi_age -----------------------------------------------------------------

dsi_strength <- dsi_strength %>%
  mutate(partner_type = case_when(
    sex == "F" & dyad_type == "F-F" ~ "Female Partners",
    sex == "F" & dyad_type == "F-M" ~ "Male Partners",
    sex == "M" & dyad_type == "F-M" ~ "Female Partners"),
    sex = fct_recode(sex, "Males" = "M", "Females" = "F"))

ggplot(dsi_strength, aes(x = age_class, y = r_strength)) +
  geom_point(color = "gray50", size = 0.75, alpha = 0.25,
             position = position_jitter(width = 0.2, height = 0)) +
  stat_smooth(method = "loess",
              color = "firebrick", fill = "firebrick") +
  facet_grid(sex ~ partner_type) +
  theme_journal_x2() +
  labs(x = "Age", y = "Mean DSI with Top 3 Partners")



dsi_quantity_f <- filter(dsi_quantity, sex == "F")
dsi_quantity_m <- filter(dsi_quantity, sex == "M")

dsi_quantity_f <- dsi_quantity_f %>%
  filter(sex == "F") %>%
  mutate(sex = fct_recode(sex,
                          "Females" = "F"),
         dyad_type = fct_recode(dyad_type,
                                "Male Partners" = "F-M",
                                "Female Partners" = "F-F"))

dsi_quantity_m <- dsi_quantity_m %>%
  filter(sex == "M") %>%
  mutate(sex = fct_recode(sex,
                          "Males" = "M"),
         dyad_type = fct_recode(dyad_type,
                                "Female Partners" = "F-M"))

p1 <- ggplot(dsi_quantity_f, aes(x = age_class, y = n_partners)) +
  geom_point(color = "gray50", size = 0.75, alpha = 0.25,
             position = position_jitter(width = 0.2, height = 0.2)) +
  stat_smooth(method = "loess",
              color = "firebrick", fill = "firebrick") +
  facet_grid(r_class ~ dyad_type, scale = "free_y") +
  theme_journal() +
  labs(title = "Females", x = "Age", y = "Number of Partners")

p2 <- ggplot(dsi_quantity_m, aes(x = age_class, y = n_partners)) +
  geom_point(color = "gray50", size = 0.75, alpha = 0.25,
             position = position_jitter(width = 0.2, height = 0.2)) +
  stat_smooth(method = "loess",
              color = "firebrick", fill = "firebrick") +
  facet_grid(r_class ~ dyad_type, scale = "free_y") +
  theme_journal() +
  labs(title = "Males", x = "Age", y = "Number of Partners")

cowplot::plot_grid(p1, p2, ncol = 2, rel_widths = c(1.75, 1),
                   labels = c("a", "b"))

p1 <- ggplot(dsi_quantity_f, aes(x = age_class, y = n_partners)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours = viridis::viridis(11, option = "magma"),
                       trans = sqrt_trans(), guide = FALSE) +
  stat_smooth(method = "loess",
              color = "white", fill = "white") +
  facet_grid(r_class ~ dyad_type, scale = "free_y") +
  theme_journal() +
  labs(title = "Females", x = "Age", y = "Number of Partners")

p2 <- ggplot(dsi_quantity_m, aes(x = age_class, y = n_partners)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours = viridis::viridis(11, option = "magma"),
                       trans = sqrt_trans(), guide = FALSE) +
  stat_smooth(method = "loess",
              color = "white", fill = "white") +
  facet_grid(r_class ~ dyad_type, scale = "free_y") +
  theme_journal() +
  labs(title = "Males", x = "Age", y = "Number of Partners")

cowplot::plot_grid(p1, p2, ncol = 2, rel_widths = c(1.75, 1),
                   labels = c("a", "b"))



dsi_reciprocity <- dsi_reciprocity %>%
  mutate(partner_type = case_when(
    sex == "F" & dyad_type == "F-F" ~ "Female Partners",
    sex == "F" & dyad_type == "F-M" ~ "Male Partners",
    sex == "M" & dyad_type == "F-M" ~ "Female Partners"),
    sex = fct_recode(sex, "Males" = "M", "Females" = "F"))

ggplot(dsi_reciprocity, aes(x = age_class, y = r_reciprocity)) +
  # geom_point(color = "gray50", size = 0.75, alpha = 0.25,
  #            position = position_jitter(width = 0.2, height = 0)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours = viridis::viridis(11, option = "magma"),
                       trans = sqrt_trans(), guide = FALSE) +
  stat_smooth(method = "loess",
              color = "white", fill = "white") +
  # stat_smooth(method = "loess",
  #             color = "firebrick", fill = "firebrick") +
  facet_grid(sex ~ partner_type) +
  theme_journal_x2() +
  labs(x = "Age", y = "Mean Reciprocity with Top 3 Partners")
