#' ---
#' title: "Richness plots"
#' author: "Luciano L.M. De Benedictis, Mirko Legnaro Diamanti, Giacomo Cangelmi"
#' output: pdf_document
#' ---


# libraries ----
library(tidyverse)
library(readxl)

# data ----

data <- map(1:5, \(x) {
  #read all three taxa
  read_excel("data/Data2024.xlsx", sheet = x) |> 
    #pivot
    pivot_longer(-1, names_to = "species") |> 
    #rename first column
    rename(plot = 1)
    }
  )

#keep copy with separate layers
datal <- data

#first three sheets are vascular plants, join them together
data <- bind_rows(data[1:3]) |> 
  list() |> 
  c(data[4:5])

data <- data |> 
  #bind list to single df
  bind_rows(.id = "taxon") |> 
  #rename IDs
  mutate(taxon = fct_recode(taxon,
                            "Vascular plants" = "1",
                            "Saproxylic fungi" = "2",
                            "Epiphytic lichens" = "3") |> 
           fct_relevel("Saproxylic fungi", after = 2),
         #extract genus
         genus = word(species, 1),
         #extract epithets
         epit = word(species, 2),
         #extract site
         site = substr(plot, 1, 2) |> 
           fct_recode("Prati di Tivo" = "PR",
                      "Incodaro" = "IN",
                      "Venaquaro" = "VE")
  ) |> 
  arrange(taxon, species) |> 
  #these were wide matrices, keep only observed entries
  filter(value > 0)

#repeat on layer data
datal <- datal |> 
  #bind list to single df
  bind_rows(.id = "taxon") |> 
  #rename IDs
  mutate(taxon = fct_recode(taxon,
                            "Tree" = "1",
                            "Shrub" = "2",
                            "Herb" = "3",
                            "Saproxylic fungi" = "4",
                            "Epiphytic lichens" = "5") |> 
           fct_relevel("Saproxylic fungi", after = 4),
         #extract genus
         genus = word(species, 1),
         #extract epithets
         epit = word(species, 2),
         #extract site
         site = substr(plot, 1, 2) |> 
           fct_recode("Prati di Tivo" = "PR",
                      "Incodaro" = "IN",
                      "Venaquaro" = "VE")
  ) |> 
  arrange(taxon, species) |> 
  #these were wide matrices, keep only observed entries
  filter(value > 0)

#distinct species names
distinct(data, species) |> pull() |> sort()

#entity richness
data |> 
  group_by(taxon) |> 
  summarise(n_distinct(species), .groups = "drop")

#distinct genera
distinct(data, genus) |> pull() |> sort()

#genus richness
data |> 
  #Xylariaceae is not a genus, don't count
  filter_out(genus == "Xylariaceae") |> 
  group_by(taxon) |> 
  summarise(n_distinct(genus), .groups = "drop")

#distinct epithets
distinct(data, epit) |> pull() |> sort()

#check incerta
incerta_index <- c("", "sp.", "spp.")

incerta <- data |> 
  filter(epit %in% incerta_index) |> 
  distinct(taxon, species, genus)

incerta

#check congenerics of incerta

data |> 
  filter(genus %in% incerta$genus) |> 
  distinct(species, taxon) |> 
  print(n = 50)

#those that have no congenerics

keep_genus <- data |> 
  filter(genus %in% incerta$genus) |> 
  distinct(genus, species, taxon) |> 
  add_count(genus) |> 
  filter(n == 1)

keep_genus

#' These should be kept because they are the only ones in their genus. Now, let's check the ones with more entries:

data |> 
  filter(genus %in% incerta$genus) |> 
  distinct(genus, species, taxon) |> 
  add_count(genus) |> 
  filter(n > 1) |> 
  print(n = 50)

#' Of those, *Lepraria* is kept because it was different from *Lepraria rigidula*, while *Physcia* and *Caloplaca* should be removed.

keep_genus <- c(keep_genus$genus, "Lepraria")

#keep only those incerta
incerta <- incerta |> 
  filter(!(genus %in% keep_genus))

data <- data |> 
  anti_join(incerta)

datal <- datal |> 
  anti_join(incerta)

# mean richness per area --------------------------------------------------

data |> 
  summarise(richness = n_distinct(species), .by = c(taxon, site, plot)) |> 
  summarise(avg = mean(richness), .by =c(taxon, site)) |> 
  mutate(avg = round(avg))

# mean, min, max per layer and area ---------------------------------------

datal |> 
  summarise(richness = n_distinct(species),
            .by = c(taxon, site, plot)) |> 
  summarise(avg = mean(richness),
            min = min(richness),
            max = max(richness),
            .by =c(taxon, site)) |> 
  mutate(avg = round(avg)) |> 
  arrange(taxon, site)

# plot! -------------------------------------------------------------------

richness <- data |>  
  group_by(site, taxon) |> 
  summarise(rich = n_distinct(species), .groups = "drop")

richness |> 
  arrange(taxon, desc(rich))

richness |> 
  ggplot(aes(x = site, y = rich, fill = taxon)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_discrete(palette = c("#7B9E7E", "#F0E68C", "#8F4A4A")) +
  theme_minimal() +
  labs(
    x = "Site", 
    y = "Number of species",
    fill = "Taxon"
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 15)),
    panel.grid.major.x = element_blank()
  )

ggsave("richness_stack.png", width = 119, height = 73, scale = 1.2, units = "mm", bg = "white")
ggsave("Fig2.eps", width = 119, height = 73, scale = 1.2, units = "mm", bg = "white")
