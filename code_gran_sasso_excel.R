## The following script assumes that the excel sheet is in a "data" subfolder within the project folder
## Steps shown should be taken as suggestions only, and can be modified or combined in different ways

#load necessary packages
library(tidyverse)
library(readxl)

#path to file within project directory
path <- "data/Data2024.xlsx"

#get sheets names
sheets <- excel_sheets("data/Data2024.xlsx") |> 
  #exclude metadata sheet
  str_subset("metadata", negate = T) |> 
  set_names()

#import sheets into named list
data <- sheets |> 
  map(\(x) read_excel(path, x))

#biodiversity data is in wide format, you can use this to convert to long format

biodiv_long <- data |> 
  #select biodiversity sheets
  keep_at(1:5) |> 
  map(\(x) x |> 
        pivot_longer(!plotID, names_to = "species") |> 
        filter(value != 0))

#rename value to cover or frequency

biodiv_long[1:3] <- biodiv_long[1:3] |> 
  map(\(x) x |> 
        rename(cover = value))

biodiv_long[4:5] <- biodiv_long[4:5] |> 
  map(\(x) x |> 
        rename(frequency = value))

#join vascular plants into a single tibble, with a layer column

plants <- biodiv_long |> 
  keep_at(1:3) |> 
  bind_rows(.id = "layer") |> 
  mutate(layer = str_extract(layer, "(?<=GS_2024_).*(?=s$)"))
