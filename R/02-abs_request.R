# I think we take the ABS data and calculate
# I think it will be create varible if industry = tech or occ = tech, deals with double ups autmatically



# Load a list of technology occupations
tech_occupations <- read_excel("Inputs/technology_worker_definition.xlsx", sheet = "occupation") %>%
  mutate(occupation_code = as.character(occupation_code))

# Load a list of technology industries
tech_industries <- read_excel("Inputs/technology_worker_definition.xlsx", sheet = "industry")

# Load a list of technology occupations
abs <- read_excel("Inputs/Deloitte_Access_Economics_Labour Force_LS007237.xlsx", sheet = "Table 1", skip = 8) %>%
  rename(
    industry_code = `Industry group of main/last job: ANZSIC (2006) Rev.2.0 - Codes`,
    industry_name = `Industry group of main/last job: ANZSIC (2006) Rev.2.0 - Labels`,
    occupation_code = `Occupation of main/last job: ANZSCO (2013) v1.2 - Codes`,
    occupation_name = `Occupation of main/last job: ANZSCO (2013) v1.2 - Labels`  
  ) %>%
  filter(industry_code %in% tech_industries$Industry_code2 | industry_code == "TOTAL") %>%
  filter(occupation_code != "TOTAL")


tech <- abs %>%
  mutate(tech = case_when(
    occupation_code %in% tech_occupations$occupation_code & industry_code == "TOTAL" ~ 1, # include all tech workers in total
    industry_code %in% tech_industries$Industry_code2 ~ 2, # incldue all workers in tech ind
    .default = 0
  )) %>%
  mutate(tech1 = case_when(
    industry_code %in% tech_industries$Industry_code2 & occupation_code %in% tech_occupations$occupation_code ~ 0,
    .default = tech
  )) %>%
  mutate(id = case_when(
    tech1 == 1 ~ "tech_occ",
    tech1 == 2 ~ "tech_ind",
    .default = NA
  )) %>%  
  group_by(id) %>%
  summarise(
    workers = sum(AUSTRALIA)
  )
  





tech <- abs %>%
  mutate(tech = case_when(
    occupation_code %in% tech_occupations$occupation_code~ 1, # include all tech workers in total
    .default = 0
  )) %>%
  group_by(tech) %>%
  summarise(
    workers = sum(AUSTRALIA)
  )




tech <- abs %>%
  mutate(tech = case_when(
    occupation_code %in% tech_occupations$occupation_code & industry_code == "TOTAL" ~ 1, # include all tech workers in total
    industry_code %in% tech_industries$Industry_code2 ~ 2, # incldue all workers in tech ind
    .default = 0
  )) %>%
  mutate(tech1 = case_when(
    industry_code %in% tech_industries$Industry_code2 & occupation_code %in% tech_occupations$occupation_code ~ 0,
    .default = tech
  )) %>%
  group_by(industry_name) %>%  
  summarise(
    workers = sum(AUSTRALIA)
  )