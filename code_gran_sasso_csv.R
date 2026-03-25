## The following script assumes that the CSVs are in a "data" subfolder within the project folder
## Steps shown should be taken as suggestions only, and can be modified or combined in different ways

#load necessary packages
library(tidyverse)

#list all CSVs
files <- list.files("data", full.names = T) |> 
  #only CSV
  str_subset("\\.csv$") |> 
  #exclude metadata
  str_subset("metadata", negate = T)

#give nicer names to files
files <- files |> 
  set_names(\(x) str_extract(x, "(?<=GS_2024_).*(?=\\.csv$)"))

#import files into named list
data <- files |> 
  map(read_csv)

#biodiversity data is in wide format, you can use this to convert to long format

biodiv_long <- data |> 
  #select biodiversity sheets
  discard_at(c("deadwood", "structure")) |> 
  map(\(x) x |> 
        pivot_longer(!plotID, names_to = "species") |> 
        filter(value != 0))

#rename value to cover or frequency

biodiv_long[c("trees", "shrubs", "herbs")] <- biodiv_long |> 
  keep_at(c("trees", "shrubs", "herbs")) |> 
  map(\(x) x |> 
        rename(cover = value))

biodiv_long[c("fungi", "lichens")] <- biodiv_long |> 
  keep_at(c("fungi", "lichens")) |> 
  map(\(x) x |> 
        rename(frequency = value))

#join vascular plants into a single tibble, with a layer column

plants <- biodiv_long |> 
  keep_at(c("trees", "shrubs", "herbs")) |> 
  bind_rows(.id = "layer") |> 
  mutate(layer = str_remove(layer, "s$"))
