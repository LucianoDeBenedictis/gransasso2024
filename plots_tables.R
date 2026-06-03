#' ---
#' title: "Plots and tables"
#' author: "Giacomo Cangelmi, Luciano L.M. De Benedictis, Mirko Legnaro Diamanti"
#' output: pdf_document
#' ---

# SCRIPT FOR DATA NOTE GRAPHS
# libraries ----
library(openxlsx)
library(tidyverse)
library(patchwork)

# ggplot theme ----

my_theme <- function(...){
  theme_minimal() +
    theme(axis.title.y = element_text(size = 5),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(face = "italic", size = 10),
          axis.text.x = element_text(size = 12),
          legend.position = "none", plot.title.position = "panel",
          title = element_text(size = 17),
          strip.text.y = element_text(size = 12, face = "bold"),
          strip.placement = "outside",
          panel.grid.major.y = element_blank(),
          ...)
}


# DATA IMPORT ----
sheets <- loadWorkbook('data/Data2024.xlsx') |> 
  names() |> 
  set_names()

data <- map(sheets, \(x) readWorkbook('data/Data2024.xlsx', sheet = x))

# "MOST FREQUENT SPECIES" plot ----

get_frequency <- function(x, cutoff = 0){
  x |>
    pivot_longer(!plotID) |> 
    filter_out(value == 0) |> 
    count(name) |> 
    arrange(desc(n)) |> 
    filter(n > cutoff) |> 
    mutate(name = str_replace_all(name, "(?<=\\w)\\.(?=\\w)", " "))
}

## Plants data
# Tree layer

tree_freq <- data$GS_2024_trees |> 
  get_frequency(cutoff = 1)

# Shrubs

shrub_freq <- data$GS_2024_shrubs |> 
  get_frequency(cutoff = 1)

# Herbs

herb_freq <- data$GS_2024_herbs |> 
  get_frequency(cutoff = 16)

## Lichens data

lichen_freq <- data$GS_2024_lichens |> 
  get_frequency(cutoff = 14)

## Fungi data

fungi_freq <- data$GS_2024_fungi |> 
  get_frequency(cutoff = 5)


## Dataset binding
freq_all <- bind_rows(tree_freq, shrub_freq, herb_freq, lichen_freq, fungi_freq, .id = "group") |> 
  mutate(group = recode_values(group,
    "1" ~ "Tree\nlayer",
    "2" ~ "Shrub\nlayer",
    "3" ~ "Herb\nlayer",
    "4" ~ "Lichens",
    "5" ~ "Fungi")) |> 
  mutate(group = as_factor(group))

# "Most frequent species" Graph
gg_spp_freq <- freq_all |> 
  ggplot(aes(x = fct_reorder(name, n), y = n, fill = group)) +
  geom_bar(stat = 'identity', width = 0.75) +
  scale_fill_manual(values = c(
    "Tree\nlayer" = "#315b25",
    "Shrub\nlayer" = "#4d7b4d",
    "Herb\nlayer" = "#7B9E7E",
    "Lichens" = "#F0E68C",
    "Fungi" = "#8F4A4A")) +
  coord_flip() +
  labs(y = "Sampling unit frequency", x = "") +
  ylim(c(0, 20)) +
  facet_grid(group ~ ., scales = "free", space = "free",
             switch = "both") +
  my_theme()

gg_spp_freq

# Save graph - EPS format
ggsave("Fig3.eps", 
       plot = gg_spp_freq,
       width = 119, 
       height = 73, 
       scale = 1.64, 
       units = "mm", 
       bg = "white")

# Save graph - PNG format
ggsave(
  filename = "most_frequent_species.png",
  plot = gg_spp_freq,
  width = 119, 
  height = 73, 
  scale = 1.64, 
  units = "mm", 
  bg = "white")

# "FOREST STRUCTURE" plot ----
forest_str <- data$GS_2024_structure |> 
  separate_wider_position(plotID, c(site = 2, plot = 1)) %>%
  mutate(site = recode_values(site,
                              "PR" ~ "Prati di Tivo",
                              "IN" ~ "Incodaro",
                              "VE" ~ "Venaquaro"))

# Number of records per each tree species in each study site
forest_str |> 
  count(site, treesp) |> 
  pivot_wider(names_from = site, values_from = n) |> 
  arrange(treesp)

# Creation of a common legend among study site
species_str <- forest_str |> 
  distinct(treesp) |> 
  arrange(treesp) |> 
  pull()

species_str

col_leg <- scale_fill_manual(
  name   = "Species",
  limits = species_str,   
  drop   = FALSE,        
  values = c(
    "Abies alba" = "#0072B2",
    "Acer pseudoplatanus" = "#E69F00",
    "Fagus sylvatica" = "#56B4E9",
    "Ilex aquifolium" = "#000000",
    "Taxus baccata" =  "#D55E00"
  )
)

# Ranges, means, medians, quantiles and histograms of DBH in...

my_summary <- function(x, var){
  x |>
    summarise(
      min    = min({{var}}, na.rm = T),
      max    = max({{var}}, na.rm = T),
      mean   = mean({{var}}, na.rm = T),
      median = median({{var}}, na.rm = T),
      q25    = quantile({{var}}, probs = 0.25, na.rm = T),
      q75    = quantile({{var}}, probs = 0.75, na.rm = T)
    )
}

hist_str <- function(x, where, what, xlim, ylim, binwidth, xlab){
  x |> 
    filter(site == where) |> 
    ggplot(aes({{what}}, fill = treesp)) + 
    geom_histogram(color = "#636363", binwidth = binwidth, linewidth = 0.2,
                   show.legend = T) + 
    col_leg + 
    labs(x = xlab, y = "Frequency", fill = "Species", title = where)  +
    scale_x_continuous(limits = xlim, oob = scales::squish) +
    ylim(ylim) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.4), 
          legend.text = element_text(face = "italic"),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank())
}

# ...entire dataset

forest_str |> 
  my_summary(treedb)

xlim_db <- range(forest_str$treedb)
ylim_db <- c(0, 100)

# ...by site

forest_str |> 
  group_by(site) |> 
  my_summary(treedb)

# ... Incodaro study site

hist_IN <- forest_str |> 
  hist_str("Incodaro", treedb, xlim_db, ylim_db, 4, "Tree DBH (cm)")

hist_IN

# ... Venaquaro study site

hist_VE <- forest_str |> 
  hist_str("Venaquaro", treedb, xlim_db, ylim_db, 4, "Tree DBH (cm)")

hist_VE

# ... Prati di Tivo study site

hist_PR <- forest_str |> 
  hist_str("Prati di Tivo", treedb, xlim_db, ylim_db, 4, "Tree DBH (cm)")

hist_PR

# Ranges, means, medians, quantiles and histograms of height measured in...
# ... entire dataset
forest_str |> 
  my_summary(treeht)

xlim_ht <- range(forest_str$treeht, na.rm = T)
ylim_ht <- c(0,13)

# ... by site

forest_str |> 
  group_by(site) |> 
  my_summary(treeht)

# ... Incodaro study site

hist_ht_IN <- forest_str |> 
  hist_str("Incodaro", treeht, xlim_ht, ylim_ht, 2, "Tree height (m)")+
  ggtitle(NULL)

hist_ht_IN

# ... Venaquaro study site
hist_ht_VE <- forest_str |> 
  hist_str("Venaquaro", treeht, xlim_ht, ylim_ht, 2, "Tree height (m)")+
  ggtitle(NULL)

hist_ht_VE

# ... Prati di Tivo study site
hist_ht_PR <- forest_str |> 
  hist_str("Prati di Tivo", treeht, xlim_ht, ylim_ht, 2, "Tree height (m)")+
  ggtitle(NULL)

hist_ht_PR


# Combined DBH and heights histograms of the three sites
combi_plots <- wrap_plots(hist_IN, hist_VE, hist_PR,
                          hist_ht_IN, hist_ht_VE, hist_ht_PR,
                          ncol = 3, nrow = 2,
                          guides = "collect", axes = "collect")

combi_plots

# Save graph - EPS format
ggsave("Fig4.eps", 
       combi_plots,
       width = 119, 
       height = 73, 
       scale = 1.7, 
       units = "mm", 
       bg = "white")
# Save graph - PNG format
ggsave(
  filename = "forest_structure.png",
  combi_plots,
  width = 119, 
  height = 73, 
  scale = 1.7, 
  units = "mm", 
  bg = "white")

# DEADWOOD table ----

# entire dataset
data$GS_2024_deadwood |> 
  pivot_longer(diam1:height) |> 
  group_by(name) |> 
  my_summary(value)

#by site
data$GS_2024_deadwood |> 
  pivot_longer(diam1:height) |> 
  mutate(siteID = str_sub(plotID, 1, 2)) |> 
  select(!c(plotID, lynID)) |> 
  group_by(name, siteID) |> 
  my_summary(value)
