library(tidyverse)

## Exercise 12.2.2

table2_cases <- filter(table2, type == "cases") %>% 
    rename(cases = count) %>% 
    arrange(country, year)

table2_population <- filter(table2, type == "population") %>% 
    rename(population = count) %>% 
    arrange(country, year)

t2_cases_per_cap <- tibble(
    year = table2_cases$year,
    country = table2_cases$country,
    cases = table2_cases$cases,
    population = table2_population$population
) %>% 
    mutate(cases_per_cap = (cases / population) * 1000)
    
t2_cases_per_cap <- t2_cases_per_cap %>% 
    mutate(type = "cases_per_cap") %>% 
    rename(count = cases_per_cap)

bind_rows(table2, t2_cases_per_cap) %>% 
    arrange(country, year,type, count)
## 

# table4a contains cases and table4b contains population

table4c <- 
    tibble(
      country = table4a$country,
      "1999" = table4a[["1999"]] / table4b[["1999"]] * 1000,
      "2000" = table4a[["2000"]] / table4b[["2000"]] * 1000
    )

## Recreate the plot showing change in cases over time using table2 instead of table1. What do you need to do first?
library(ggplot2)

