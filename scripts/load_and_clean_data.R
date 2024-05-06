suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(scales)) # for the label_comma() function
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(gridExtra))

# data import
data <- read_csv('dataset/educ.csv', show_col_types = FALSE)
continent <- read_csv('dataset/continents2.csv', show_col_types = FALSE)
pisa <- read_csv('dataset/pisa-scores-by-country-2024.csv', show_col_types = FALSE)
```


# data cleanig

# rename columns to make it easier to merge
continent <- continent |>
  rename(
    country = name,
    sub_region = `sub-region`
  )

# data merging
merged_data <- merge(data, continent, by = "country")
merged_data2 <- merge(merged_data, pisa, by = "country", all.x = TRUE)
merged_data3 <- merge(continent, literacy_clean, by = "country")
```