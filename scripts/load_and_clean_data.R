# This file is purely as an example. 
# There are a few places

library(tidyverse)

data <- read_csv('dataset/educ.csv')
continent <- read_csv('dataset/continents2.csv')

## CLEAN the data

continent <- continent|>
  rename(
    country = name
  )
continent

merged_data <- merge(data, continent, by = "country")

merged_data <- merged_data |> rename(sub_region = `sub-region`)


# plot of % of people who have not completed primary school vs year, and by country
grouped3 <- merged_data |>
  #filter(survey == 'DHS') |>
  #filter(region_group %in% c('Central and Southern Asia', 'Europe and Northern America', 'Sub-Saharan Africa')) |>
  group_by(`sub_region`, year) |>
  summarise(mean_primary_completion_rate = mean(comp_prim_v2_m, na.rm = TRUE),
            percent_people_not_in_primary_school = mean(eduout_upsec_m, na.rm = TRUE),
            mean_educ_years_20_24 = mean(eduyears_2024_m, na.rm = TRUE),
            attend_higher_education = mean(attend_higher_1822_m, na.rm = TRUE),
            count = n()
  ) |>
  ggplot(aes(x = year, y = percent_people_not_in_primary_school, color = sub_region)) +
  geom_line()
grouped3


# plot of average years of education for 20 to 24 year olds vs year by continent
grouped6 <- merged_data |>
  #filter(survey == 'DHS') |>
  #filter(region_group %in% c('Central and Southern Asia', 'Europe and Northern America', 'Sub-Saharan Africa')) |>
  group_by(region, year) |>
  summarise(mean_primary_completion_rate = mean(comp_prim_v2_m, na.rm = TRUE),
            percent_people_not_in_primary_school = mean(eduout_upsec_m, na.rm = TRUE),
            mean_educ_years_20_24 = mean(eduyears_2024_m, na.rm = TRUE),
            attend_higher_education = mean(attend_higher_1822_m, na.rm = TRUE),
            number_complete_prim_1524 = mean(comp_prim_1524_no, na.rm = TRUE),
            count = n()
  ) |>
  ggplot(aes(x = year, y = mean_educ_years_20_24, color = region)) +
  geom_line()
grouped6


#bar chart of rural and urban areas in northern africa vs. % of people who completed primary school
grouped7 <- merged_data |>
  filter(sub_region %in% c('Northern Africa')) |>
  group_by(sub_region, Location) |>
  summarise(
    mean_primary_completion_rate = mean(comp_prim_v2_m, na.rm = TRUE),
    count = n()
  ) |>
  ggplot(
    aes(x = Location, y = mean_primary_completion_rate, fill = Location) 
  )+geom_col() +ylim(c(0,1))

grouped7


# same as previous graph but in sub-saharan africa and not northern africa
grouped8 <- merged_data |>
  filter(sub_region %in% c('Sub-Saharan Africa')) |>
  group_by(sub_region, Location) |>
  summarise(
    mean_primary_completion_rate = mean(comp_prim_v2_m, na.rm = TRUE),
    count = n()
  ) |>
  ggplot(
    aes(x = Location, y = mean_primary_completion_rate, fill = Location) 
  )+geom_col() +ylim(c(0,1))

grouped8


