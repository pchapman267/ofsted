library(dplyr)
library(janitor)

gias <- read.csv(paste0("http://ea-edubase-api-prod.azurewebsites.net/edubase/edubasealldata",gsub("-","",Sys.Date()),".csv")) %>%
  clean_names() %>%
  mutate(
    open_date = as.Date(open_date, format = "%d-%m-%Y"),
    close_date = as.Date(close_date, format = "%d-%m-%Y")
  ) %>%
  select(urn, open_date, close_date, type_of_establishment_name)
