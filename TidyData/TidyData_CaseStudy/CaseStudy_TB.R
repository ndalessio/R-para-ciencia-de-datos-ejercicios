library(readr)
library(tidyverse)
who <- read_csv("TB_notifications_2020-05-20.csv")
View(who)

names(who)

########## In this dataset all cases of TB are new cases:

# new_sp - TB new case that could be diagnosed by smear smear was positive.   
# new_sn - TB new case that couldn't be diagnosed by smear, it was negative.
# new_su - TB new case, smear unknown.
# new_ep - Extrapulmonary TB new case.
# ret_rel - Relapse new case of TB.
# new_oth - new others  
# ( f ) femenine and ( m ) masculine
# ages 0-14 1524 25-34 35-44 45-54 55-64 m65
# ret_taf treatment after failure cases
# ret_tad
# ret_oth

### TIDYING DATA

# 1.pivot_longer () : we gather in a column named keys those varibles that look like they were actually values.
#   we group the values in a column called "cases".

# 2. If we check the values in key, we can find that there is an inconsistency: all names are new_something 
# but newrel_f014 doesn't have the _ after the word "new". We fix that:

# 3. muate() + stringr::str_replace(): We can separate the values in each code with two passes of separate(). The first pass will split the codes
# at each underscore. - This is amazing!

# 4. We drop iso2 and iso3 because we already have "country". We also drop "new"

# 5. separate(): We separate "agesex" varible into two differents variables:

who_tb <-
    who %>% 
    pivot_longer(
      cols = new_sp_m04:newrel_f65,
      names_to = "key",
      values_to = "cases",
      values_drop_na = TRUE
    ) %>% 
    mutate(
      key = stringr::str_replace(key, "newrel", "new_rel")
      ) %>% 
    separate(key, c("new", "var", "sexage"), sep = "_") %>% 
    select(-new, -iso2, -iso3) %>% 
    separate(sexage, c("sex", "age"), sep = 1)

#####################################################################
####################### Exercises ###################################
#####################################################################

# 1. Was ok to set values_drop_na = TRUE? It depends what missing values represent. If they represent no data
# it's ok. But, if they represent no cases of TB, we need them.

# 1. CHECKING IF THERE ARE CASES = 0, indicating no cases of TB:

who_tb %>% 
    filter(cases == 0) %>% 
    nrow()

# There are 23731 rows were cases = 0.

# 2. EXPLICIT MISSING VALUES. In a combination of (country, year), what's the proportion of NA?

gather(who, new_sp_m014:newrel_f65, key="key", value="cases") %>% 
  group_by(country, year) %>% 
  mutate(prop_missing = sum(is.na(cases)/n())) %>% 
  filter(prop_missing > 0, prop_missing < 1) %>% 
  select(country, year, key, cases, prop_missing)

# They might have some explicit missing values, but not all. 

# 3. IMPLICIT MISSING VALUES.

nrow(who)
#8286

who %>% 
  complete(country, year) %>% 
  nrow()
#8502

# There are more complete cases than nrows in who, which means there are implicit missing values in the data.
# But, what do they mean? To figure that out we use anti_join().

anti_join(complete(who, country, year), who, by = c("country", "year")) %>% 
    select(country, year) %>% 
    group_by(country) %>% 
    summarise(min_year = min(year), max_year = max(year))

#Implicit missing values is data that doesn't exist, because countries in those years didn't exist.


# 5.For each country, year, and sex compute the total number of cases of TB. Make an informative 
# visualization of the data.

who_tb %>% 
    group_by(country, year, sex) %>% 
    filter(year > 1995) %>% 
    summarise(cases = sum(cases)) %>% 
    unite(contry_sex, country, sex, remove = FALSE) %>% 
    ggplot(aes(x = year, y = cases, group = contry_sex, color= sex)) +
    geom_line()

who_tb %>% 
  count(sex)

# hmmm need to check sex. 



























    