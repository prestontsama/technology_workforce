library(tidyverse)
library(readabs)
library(readxl)

# Load a list of technology occupations
technology_occupations <- read_excel("Inputs/technology_worker_definition.xlsx", sheet = "occupation") %>%
  mutate(occupation_code = as.character(occupation_code))

# Load a list of technology industries
technology_industries <- read_excel("Inputs/technology_worker_definition.xlsx", sheet = "industry")

# Read in employment by occupation, by state
EQ08 <- read_lfs_datacube(cube = "EQ08",
                          path ="Inputs"
                          )

# Read in employment by occupation, by state
EQ06 <- read_lfs_datacube(cube = "EQ06",
                          path ="Inputs"
)




# Split the occupation variable into code and name
EQ08_1 <- EQ08 %>%
  separate(
    occupation_of_main_job__anzsco_2013_v1.2,
    into = c("occupation_code", "occupation_name"),
    sep = " ",
    extra = "merge"
  ) %>%
  mutate(date1 = date) %>%
  separate(
    date1,
    into = c("year", "other"),
    sep = "-",
    extra = "merge"
  ) 

EQ08_1 <- EQ08_1 %>%
  left_join(technology_occupations %>% select(occupation_code, occupation_group), by = "occupation_code")

tech <- EQ08_1 %>%
  filter(!is.na(occupation_group)) %>%
  group_by(date) %>%
  summarise(
    tech = sum(employed_total_000)
  ) %>%
  ungroup() %>%
  separate(
    date,
    into = c("year", "other"),
    sep = "-",
    extra = "merge"
  ) %>%
  group_by(year) %>%
  summarise(
    tech_occ = mean(tech)
  )

options(scipen = 999)



EQ06_1 <- EQ06 %>%
  mutate(employed = employed_full_time_000 + employed_part_time_000) %>%
  filter(industry_group_of_main_job__anzsic_2006_rev.2.0 %in% technology_industries$industry) %>%
  group_by(date) %>%
  summarise(
    tech = sum(employed)
  ) %>%
  ungroup() %>%
  separate(
    date,
    into = c("year", "other"),
    sep = "-",
    extra = "merge"
  ) %>%
  group_by(year) %>%
  summarise(
    tech_ind = mean(tech)
  )

 tech_workers <- tech %>%
   left_join(EQ06_1, by = "year") %>%
   mutate(tech_workers = tech_occ + tech_ind)