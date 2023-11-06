library(readr)
library(dplyr)
library(lubridate)
library(stringr)

import::from("sjmisc", "frq")

data <- 
  bind_rows(
    bind_rows(
      
      bind_rows(
        
        read_csv("data-raw/complaints/animal-compliance-april-to-june-2016.zip") %>% 
          mutate(year = 2016, quarter = quarter(ymd("2016-04-01"))),
        
        read_csv("data-raw/complaints/animal-compliance-july-to-september-2016.zip") %>% 
          mutate(year = 2016, quarter = quarter(ymd("2016-07-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-october-to-december-2016.zip") %>% 
          mutate(year = 2016, quarter = quarter(ymd("2016-10-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-january-to-march-2017.zip") %>% 
          mutate(year = 2017, quarter = quarter(ymd("2017-01-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-april-to-june-2017.zip") %>% 
          mutate(year = 2017, quarter = quarter(ymd("2017-04-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-july-to-september-2017.zip") %>% 
          mutate(year = 2017, quarter = quarter(ymd("2017-07-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-october-to-december-2017.zip") %>% 
          mutate(year = 2017, quarter = quarter(ymd("2017-10-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-jan-to-mar-2018.zip") %>% 
          mutate(year = 2018, quarter = quarter(ymd("2018-01-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-apr-to-jun-2018.zip") %>% 
          mutate(year = 2018, quarter = quarter(ymd("2018-04-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-jul-to-sep-2018.zip") %>% 
          mutate(year = 2018, quarter = quarter(ymd("2018-07-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-oct-to-dec-2018.zip") %>% 
          mutate(year = 2018, quarter = quarter(ymd("2018-10-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-jan-mar-2019.zip") %>% 
          mutate(year = 2019, quarter = quarter(ymd("2019-01-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-apr-jun-2019.zip") %>% 
          mutate(year = 2019, quarter = quarter(ymd("2019-04-01"))),
        
        read_csv("data-raw/complaints/cars-bis-open-data-animal-related-complaints-jul-to-sep-2019.zip") %>% 
          mutate(year = 2019, quarter = quarter(ymd("2019-07-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-oct-to-dec-2019.zip") %>% 
          mutate(year = 2019, quarter = quarter(ymd("2019-10-01")))
        
      ) %>% 
        select(-`Office: Responsible Office`), 
      
      bind_rows(
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-jan-to-mar-2020.zip") %>% 
          mutate(year = 2020, quarter = quarter(ymd("2020-01-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-apr-to-jun-2020.zip") %>% 
          mutate(year = 2020, quarter = quarter(ymd("2020-04-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-jul-to-sep-2020.zip") %>% 
          mutate(year = 2020, quarter = quarter(ymd("2020-07-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-complaints-apr-to-jun-2022.zip") %>% 
          mutate(year = 2021, quarter = quarter(ymd("2021-04-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-jul-sep-2021.zip") %>% 
          mutate(year = 2021, quarter = quarter(ymd("2021-07-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-oct-to-dec-2021.zip") %>% 
          mutate(year = 2021, quarter = quarter(ymd("2021-10-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-jan-to-mar-2022.zip") %>% 
          mutate(year = 2022, quarter = quarter(ymd("2022-01-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-complaints-apr-to-jun-2022.csv.zip") %>% 
          mutate(year = 2022, quarter = quarter(ymd("2022-04-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-jul-to-sep-2022.csv.zip") %>% 
          mutate(year = 2022, quarter = quarter(ymd("2022-07-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-oct-to-dec-2022.csv.zip") %>%
          mutate(year = 2022, quarter = quarter(ymd("2022-10-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-jan-to-mar-2023.csv.zip") %>%
          mutate(year = 2023, quarter = quarter(ymd("2022-01-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-apr-to-jun-2023.csv.zip") %>%
          mutate(year = 2023, quarter = quarter(ymd("2022-04-01"))),
        
        read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-jul-to-sep-2023.csv.zip") %>%
          mutate(year = 2023, quarter = quarter(ymd("2022-07-01")))
        
      )  %>% 
        select(-Index) 
      
    ) %>% 
      rename(
        nature = `Category: Nature`, 
        type = `Category: Type`,
        reporting_level = `Category: Reporting Level`,
        suburb = `Location: Suburb`
      ), 
    
    bind_rows(
      
      read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-oct-to-dec-2020.zip") %>% 
        mutate(year = 2020, quarter = quarter(ymd("2020-10-01"))),
      
      read_csv("data-raw/complaints/cars-srsa-open-data-animal-related-complaints-jan-mar-2021.zip") %>% 
        mutate(year = 2021, quarter = quarter(ymd("2021-01-01")))
    ) %>% 
      select(-Index) %>% 
      rename(
        nature = `Nature`, 
        type = `Type`,
        reporting_level = `Reporting Level`,
        suburb = `Suburb`)
  ) %>% 
  relocate(year, quarter) %>% 
  arrange(year, quarter)

data %>% 
  filter(! nature == "Animal") %>% 
  nrow()
  
data <- data %>% 
  filter(nature == "Animal") %>% 
  select(-nature) 

data %>% 
  filter(type == "Attack" & reporting_level == "Not An Attack") %>% 
  nrow()

data <- data %>% 
  filter(! (type == "Attack" & reporting_level == "Not An Attack") )
  

names(data)

data %>% 
  mutate(date = str_c(year, "-", quarter)) %>% 
  {length(unique(.$date))}

data %>% 
  mutate(date = str_c(year, "-", quarter)) %>% 
  frq(date)

frq(data, type, sort.frq = "desc")
frq(data, reporting_level, sort.frq = "desc")

temp <- data %>% 
  janitor::tabyl(reporting_level, type)
