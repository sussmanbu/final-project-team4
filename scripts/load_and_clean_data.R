# This file is purely as an example. 
# There are a few places

library(tidyverse)
library(tidymodels)
library(broom)

# chunk1
data <- read_csv('dataset/educ.csv')
continent <- read_csv('dataset/continents2.csv')

#chunk2
continent <- continent|>
  rename(
    country = name
  )
continent

merged_data <- merge(data, continent, by = "country")

merged_data <- merged_data |> rename(sub_region = `sub-region`)

#chunk3
afg <- data |>
  filter(iso_code == 'AFG')
afg

#chunk4
grouped <- data |>
  group_by(iso_code, Sex) |>
  summarise(mean = mean(comp_prim_v2_m, na.rm = TRUE), count = n())

grouped


#chunk5
grouped2 <- data |>
  group_by(iso_code) |>
  summarise(mean_primary_completion_rate = mean(comp_prim_v2_m, na.rm = TRUE),
            mean_educ_years_20_24 = mean(eduyears_2024_m, na.rm = TRUE),
            attend_higher_education = mean(attend_higher_1822_m, na.rm = TRUE),
            count = n())
grouped2

#chunk6
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

#chunk7
grouped5 <- merged_data |>
  #filter(survey == 'DHS') |>
  #filter(region_group %in% c('Central and Southern Asia', 'Europe and Northern America', 'Sub-Saharan Africa')) |>
  group_by(`sub_region`, year) |>
  summarise(mean_primary_completion_rate = mean(comp_prim_v2_m, na.rm = TRUE),
            percent_people_not_in_primary_school = mean(eduout_upsec_m, na.rm = TRUE),
            mean_educ_years_20_24 = mean(eduyears_2024_m, na.rm = TRUE),
            attend_higher_education = mean(attend_higher_1822_m, na.rm = TRUE),
            number_complete_prim_1524 = mean(comp_prim_1524_no, na.rm = TRUE),
            count = n()
  ) |>
  ggplot(aes(x = year, y = mean_primary_completion_rate, color = sub_region)) +
  geom_line()
grouped5

#chunk8
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

#chunk9
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

#chunk10
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

#chunk11
grouped9 <- merged_data |>
  #filter(survey == 'DHS') |>
  filter(sub_region %in% c('Northern Africa')) |>
  group_by(iso_code) |>
  summarise(mean_primary_completion_rate = mean(comp_prim_v2_m, na.rm = TRUE),
            percent_people_not_in_primary_school = mean(eduout_upsec_m, na.rm = TRUE),
            mean_educ_years_20_24 = mean(eduyears_2024_m, na.rm = TRUE),
            attend_higher_education = mean(attend_higher_1822_m, na.rm = TRUE),
            number_complete_prim_1524 = mean(comp_prim_1524_no, na.rm = TRUE),
            count = n()
  ) |>
  ggplot(aes(x = iso_code, y = mean_primary_completion_rate, fill = iso_code)) +
  geom_col() +ylim(c(0,1))
grouped9

#chunk12
grouped10 <- merged_data |>
  #filter(survey == 'DHS') |>
  filter(sub_region %in% c('Latin America and the Caribbean')) |>
  group_by(iso_code) |>
  summarise(mean_primary_completion_rate = mean(comp_prim_v2_m, na.rm = TRUE),
            percent_people_not_in_primary_school = mean(eduout_upsec_m, na.rm = TRUE),
            mean_educ_years_20_24 = mean(eduyears_2024_m, na.rm = TRUE),
            attend_higher_education = mean(attend_higher_1822_m, na.rm = TRUE),
            number_complete_prim_1524 = mean(comp_prim_1524_no, na.rm = TRUE),
            count = n()
  ) |>
  ggplot(aes(x = iso_code, y = mean_primary_completion_rate, fill = iso_code)) +
  geom_col() +ylim(c(0,1)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
grouped10

#chunk13
grouped11 <- merged_data |>
  #filter(survey == 'DHS') |>
  filter(sub_region %in% c('Latin America and the Caribbean')) |>
  group_by(iso_code) |>
  summarise(mean_primary_completion_rate = mean(comp_prim_v2_m, na.rm = TRUE),
            obs_primary_completion_rate = sum(!is.na(comp_prim_v2_m)),
            percent_people_not_in_primary_school = mean(eduout_upsec_m, na.rm = TRUE),
            mean_educ_years_20_24 = mean(eduyears_2024_m, na.rm = TRUE),
            attend_higher_education = mean(attend_higher_1822_m, na.rm = TRUE),
            number_complete_prim_1524 = mean(comp_prim_1524_no, na.rm = TRUE),
            count = n()
  ) |>
  ggplot(aes(x = iso_code, y = obs_primary_completion_rate, fill = iso_code)) + geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

grouped11


#chunk14
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

grouped2 <- data |>
  group_by(iso_code) |>
  summarise(mean_primary_completion_rate = mean(comp_prim_v2_m, na.rm = TRUE),
            mean_educ_years_20_24 = mean(eduyears_2024_m, na.rm = TRUE),
            attend_higher_education = mean(attend_higher_1822_m, na.rm = TRUE),
            count = n()) |>
  ungroup()

grouped2

lm_model <- 
  linear_reg() |> 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  fit(mean_primary_completion_rate ~ mean_educ_years_20_24, data = grouped2)

lm_form_fit

broom::glance(lm_form_fit)


