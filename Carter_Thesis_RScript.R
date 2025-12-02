# 1. Read data and inspect ----

library(tidyverse)
library(car)
library(emmeans)

df <- read_csv("df.csv")

cat("Columns present in file:\n")
print(colnames(df))

glimpse(df)
summary(df)
str(df)


# 2. Clean + prepare data ----

df <- df %>%
  mutate(
    site        = factor(site),
    species     = factor(species),
    bottom_type = factor(bottom_type),   # mesh vs hard
    box_id      = factor(box_id),
    group       = paste(species, site, bottom_type, sep = "_")
  )


# 3. T-tests for Furcellaria (Hard vs Mesh) ----

furcell <- df %>% filter(species == "f_lumbricalis")
furcell_s1 <- furcell %>% filter(site == "S1")

### ASSUMPTION TESTS 
# Normality of each bottom_type group
shapiro.test(furcell_s1$growth_f[furcell_s1$bottom_type == "mesh"])
shapiro.test(furcell_s1$growth_f[furcell_s1$bottom_type == "hard"])

# Homogeneity of variances
fligner.test(growth_f ~ bottom_type, data = furcell_s1)

### T-tests
t_furcell_s1_growth <- t.test(growth_f ~ bottom_type, data = furcell_s1)
t_furcell_s1_dgr    <- t.test(dgr ~ bottom_type, data = furcell_s1)
t_furcell_s1_epi    <- t.test(ep_cov_mean ~ bottom_type, data = furcell_s1)

t_furcell_s1_growth
t_furcell_s1_dgr
t_furcell_s1_epi

# 4. Linear models (ANOVAs) for Mesh subset only-----

#extract mesh containers only
mesh_df <- df %>% filter(bottom_type == "mesh")

mesh_df$species <- factor(mesh_df$species)
mesh_df$site    <- factor(mesh_df$site)

# Daily Growth Rate (DGR) model 

m_dgr <- lm(dgr ~ species * site, data = mesh_df)

### Residuals assumption checks 
shapiro.test(residuals(m_dgr))
fligner.test(residuals(m_dgr) ~ interaction(mesh_df$species, mesh_df$site))

qqnorm(residuals(m_dgr))
qqline(residuals(m_dgr))
plot(m_dgr, which = 1)

### ANOVA
Anova(m_dgr, type = 2)

### Post-hoc
emm_dgr  <- emmeans(m_dgr, ~ species * site)
pairs(emm_dgr, adjust = "tukey")


# Accuulated growth model

m_growth <- lm(growth_f ~ species * site, data = mesh_df)

### Assumptions
shapiro.test(residuals(m_growth))
fligner.test(residuals(m_growth) ~ interaction(mesh_df$species, mesh_df$site))

qqnorm(residuals(m_growth))
qqline(residuals(m_growth))
plot(m_growth, which = 1)

### ANOVA
Anova(m_growth, type = 2)

### Post-hoc
emm_growth <- emmeans(m_growth, ~ species * site)
pairs(emm_growth, adjust = "tukey")


# Epizoan cover model

m_epizoan <- lm(ep_cov_mean ~ species * site, data = mesh_df)

### Assumptions
shapiro.test(residuals(m_epizoan))
fligner.test(residuals(m_epizoan) ~ interaction(mesh_df$species, mesh_df$site))

qqnorm(residuals(m_epizoan))
qqline(residuals(m_epizoan))
plot(m_epizoan, which = 1)

### ANOVA
Anova(m_epizoan, type = 2)

### Post-hoc
emm_epizoan <- emmeans(m_epizoan, ~ species * site)
pairs(emm_epizoan, adjust = "tukey")

# 5. DGR Plot ----

library(dplyr)
library(ggplot2)
library(forcats)

# Prepare dataframe
df_dgr <- df %>%
  filter(
    (species == "f_vesiculosus" & bottom_type == "mesh") |
      (species == "f_lumbricalis" & ((site == "S1" & bottom_type %in% c("mesh","hard")) | site == "S2"))
  ) %>%
  mutate(
    group = case_when(
      species == "f_vesiculosus" & site == "S1" ~ "F. vesiculosus S1",
      species == "f_vesiculosus" & site == "S2" ~ "F. vesiculosus S2",
      species == "f_lumbricalis" & site == "S1" & bottom_type == "mesh" ~ "F. lumbricalis S1 mesh",
      species == "f_lumbricalis" & site == "S1" & bottom_type == "hard" ~ "F. lumbricalis S1 hard",
      species == "f_lumbricalis" & site == "S2" ~ "F. lumbricalis S2"
    ),
    site_color = ifelse(site == "S1", "S1", "S2")
  )

# Set factor levels for x-axis order
df_dgr$group <- factor(df_dgr$group,
                       levels = c("F. lumbricalis S1 hard",
                                  "F. lumbricalis S1 mesh",
                                  "F. vesiculosus S1",
                                  "F. lumbricalis S2",
                                  "F. vesiculosus S2"))

# Significance letters
anova_letters_dgr <- tibble(
  group = c("F. vesiculosus S1","F. vesiculosus S2","F. lumbricalis S1 mesh","F. lumbricalis S2"),
  label = c("b","b","a","c"),
  fontface = "plain"
)
ttest_letters_dgr <- tibble(
  group = c("F. lumbricalis S1 mesh", "F. lumbricalis S1 hard"),
  label = c("1","1"),
  fontface = "italic"
)
letters_dgr <- bind_rows(anova_letters_dgr, ttest_letters_dgr) %>%
  rowwise() %>%
  mutate(ypos = max(df_dgr$dgr[df_dgr$group == group], na.rm = TRUE) + 0.05) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = levels(df_dgr$group)))

# Abbreviated labels for plotting
plot_labels <- c(
  "F. vesiculosus S1" = "italic('F. ves.')",
  "F. lumbricalis S1 mesh" = "italic('F. lum.')~'mesh'",
  "F. lumbricalis S1 hard" = "italic('F. lum.')~'hard'",
  "F. vesiculosus S2" = "italic('F. ves.')",
  "F. lumbricalis S2" = "italic('F. lum.')"
)

# DGR plot
DGR_plot <- ggplot(df_dgr, aes(x = group, y = dgr)) +
  geom_boxplot(aes(fill = site_color, color = site_color), outlier.shape = 21) +
  geom_text(data = letters_dgr, aes(x = group, y = ypos, label = label, fontface = fontface),
            inherit.aes = FALSE) +
  scale_x_discrete(labels = function(x) parse(text = plot_labels[x])) +
  scale_fill_manual(values = c("S1" = "lightblue", "S2" = "lightgreen")) +
  scale_color_manual(values = c("S1" = "black", "S2" = "black")) +
  labs(x = "Treatment group", y = "Initial growth rate (% d'¹)", fill = "Site") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.5))

print(DGR_plot)


# 6. Accumulated growth plot -----

# Significance letters
anova_letters_growth <- tibble(
  group = c("F. vesiculosus S1","F. vesiculosus S2","F. lumbricalis S1 mesh","F. lumbricalis S2"),
  label = c("b","b","a","c"),
  fontface = "plain"
)
ttest_letters_growth <- tibble(
  group = c("F. lumbricalis S1 mesh", "F. lumbricalis S1 hard"),
  label = c("1","2"),
  fontface = "italic"
)
letters_growth <- bind_rows(anova_letters_growth, ttest_letters_growth) %>%
  rowwise() %>%
  mutate(ypos = max(df_dgr$growth_f[df_dgr$group == group], na.rm = TRUE) + 0.05) %>%
  ungroup()

# Accumulated biomass plot
Growth_plot <- ggplot(df_dgr, aes(x = group, y = growth_f)) +
  geom_boxplot(aes(fill = site_color, color = site_color), outlier.shape = 21) +
  geom_text(data = letters_growth, aes(x = group, y = ypos, label = label, fontface = fontface),
            inherit.aes = FALSE) +
  scale_x_discrete(labels = function(x) parse(text = plot_labels[x])) +
  scale_fill_manual(values = c("S1" = "lightblue", "S2" = "lightgreen")) +
  scale_color_manual(values = c("S1" = "black", "S2" = "black")) +
  labs(x = "Treatment group", y = "Final accumulated biomass (g)", fill = "Site") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.5))

print(Growth_plot)

# 7. Epizoan cover plot -----
# Significance letters
anova_letters_ep <- tibble(
  group = c("F. vesiculosus S1","F. vesiculosus S2","F. lumbricalis S1 mesh","F. lumbricalis S2"),
  label = c("a","a","b","b"),
  fontface = "plain"
)
ttest_letters_ep <- tibble(
  group = c("F. lumbricalis S1 mesh", "F. lumbricalis S1 hard"),
  label = c("1","2"),
  fontface = "italic"
)
letters_ep <- bind_rows(anova_letters_ep, ttest_letters_ep) %>%
  rowwise() %>%
  mutate(ypos = max(df_dgr$ep_cov_mean[df_dgr$group == group], na.rm = TRUE) + 0.5) %>%
  ungroup()

# Epizoan cover plot
EP_plot <- ggplot(df_dgr, aes(x = group, y = ep_cov_mean)) +
  geom_boxplot(aes(fill = site_color, color = site_color), outlier.shape = 21) +
  geom_text(data = letters_ep, aes(x = group, y = ypos, label = label, fontface = fontface),
            inherit.aes = FALSE) +
  scale_x_discrete(labels = function(x) parse(text = plot_labels[x])) +
  scale_fill_manual(values = c("S1" = "lightblue", "S2" = "lightgreen")) +
  scale_color_manual(values = c("S1" = "black", "S2" = "black")) +
  labs(x = "Treatment group", y = "Epizoan cover (%)", fill = "Site") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 0.5))

print(EP_plot)

# 8. Linear Modles, epizoan cover v biomass ----


# Helper function to run a simple linear regression and extract stats 
get_lm_stats <- function(data) {
  m <- lm(growth_f ~ ep_cov_mean, data = data)
  tibble(
    adj_r2 = summary(m)$adj.r.squared,
    p_val  = summary(m)$coefficients[2, 4]
  )
}


# Fucus

df_fucus <- df %>%
  filter(species == "f_vesiculosus")

fucus_site_stats <- df_fucus %>%
  group_by(site) %>%
  group_modify(~ get_lm_stats(.x))

print(fucus_site_stats)

# Labels for plot legend
fucus_labels <- c(
  "S1" = "S1 adj R² = -0.08 p = 0.47",
  "S2" = "S2 adj R² = 0.18 p = 0.27"
)

# Plot
lm_fucus <- ggplot(df_fucus, aes(ep_cov_mean, growth_f, color = site)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Mean epizoan cover (%)",
    y = "Accumulated biomass (g)",
    title = expression(italic("Fucus vesiculosus")),
    color = ""
  ) +
  scale_color_manual(values = c("S1" = "#1f78b4", "S2" = "#33a02c"),
                     labels = fucus_labels) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white")
  )

print(lm_fucus)

# Furcellaria 

df_furc <- df %>%
  filter(species == "f_lumbricalis", bottom_type == "mesh")

furc_site_stats <- df_furc %>%
  group_by(site) %>%
  group_modify(~ get_lm_stats(.x))

print(furc_site_stats)

furc_labels <- c(
  "S1" = "S1 adj R² = -0.19 p = 0.60",
  "S2" = "S2 adj R² = -0.16 p = 0.55"
)

lm_furc_mesh <- ggplot(df_furc, aes(ep_cov_mean, growth_f, color = site)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Mean epizoan cover (%)",
    y = "Accumulated biomass (g)",
    title = expression(italic("Furcellaria lumbricalis")),
    color = ""
  ) +
  scale_color_manual(values = c("S1" = "#1f78b4", "S2" = "#33a02c"),
                     labels = furc_labels) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.85, 0.25),
    legend.background = element_rect(fill = "white")
  ) +
  ylim(190, 630)

print(lm_furc_mesh)

# Furcellaria S1

df_furc1 <- df %>%
  filter(species == "f_lumbricalis", site == "S1")

furc_bottom_stats <- df_furc1 %>%
  group_by(bottom_type) %>%
  group_modify(~ get_lm_stats(.x))

print(furc_bottom_stats)

bottom_labels <- c(
  "hard" = "Hard adj R² = -0.28 p = 0.74",
  "mesh" = "Mesh adj R² = -0.19 p = 0.60"
)

lm_furc_bottom <- ggplot(df_furc1, aes(ep_cov_mean, growth_f, color = bottom_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Mean epizoan cover (%)",
    y = "Accumulated biomass (g)",
    title = expression(italic("Furcellaria lumbricalis")),
    color = ""
  ) +
  scale_color_manual(values = c("hard" = "#1f78b4", "mesh" = "#33a02c"),
                     labels = bottom_labels) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.85, 0.25),
    legend.background = element_rect(fill = "white")
  ) +
  ylim(-30, 630)

print(lm_furc_bottom)

# Fucus v Furcellaria, mesh, both sites

mesh_species_stats <- mesh_df %>%
  group_by(species) %>%
  group_modify(~ get_lm_stats(.x))

print(mesh_species_stats)

species_labels <- c(
  "f_lumbricalis" = "F. lumbricalis adj R² = 0.16 p = 0.14",
  "f_vesiculosus" = "F. vesiculosus adj R² = -0.11 p = 0.80"
)

lm_mesh_species <- ggplot(mesh_df, aes(ep_cov_mean, growth_f, color = species)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Mean epizoan cover (%)",
    y = "Accumulated biomass (g)",
    title = "Species comparison",
    color = ""
  ) +
  scale_color_manual(values = c("f_lumbricalis" = "#1f78b4",
                                "f_vesiculosus" = "#33a02c"),
                     labels = species_labels) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white")
  )

print(lm_mesh_species)
