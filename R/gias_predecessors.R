library(dplyr)
library(janitor)

links <- read.csv(paste0("https://ea-edubase-api-prod.azurewebsites.net/edubase/downloads/public/links_edubasealldata",gsub("-","",Sys.Date()),".csv"))

successor_links <- links %>%
  filter(grepl("Suc", LinkType)) %>%
  select(URN, LinkURN, LinkType)

predecessor <- successor_links %>%
  # Drop circular links
  filter(!(URN %in% c(122057, 122060, 135073, 147355))) %>%
  # Add flag for Merge
  group_by(LinkURN) %>%
  mutate(n_lurn = n()) %>%
  ungroup() %>%
  mutate(
  MergeFlag = ifelse(n_lurn > 1, 1, 0),
  LinkType
  ) %>% 
  # Add flag for split
  group_by(URN) %>%
  mutate(n_urn = n()) %>%
  ungroup() %>%
  mutate(
    SplitFlag = ifelse(n_urn > 1, 1,0),
    LinkType = "Predecessor",
    Level = 1
  ) %>%
  # Remove any splits or mergers
  filter(
    SplitFlag == 0, 
    MergeFlag == 0
  ) %>%
  select(
  URN = LinkURN,
  LinkURN = URN,
  LinkType,
  Level
  )

predecessors <- predecessor

rows <- TRUE

while (rows == TRUE) {
  
  predecessor_next_level <- predecessors %>%
    filter(Level == max(Level, na.rm = TRUE)) %>%
    select(URN, LinkURN, Level) %>%
    left_join(select(predecessor, URN, LinkURN), by = c("LinkURN" = "URN")) %>%
    filter(!is.na(LinkURN.y)) %>% 
    filter(LinkURN != LinkURN.y) %>% 
    mutate(
      Level = max(Level, na.rm = TRUE) + 1,
      LinkType = "Predecessor"
    ) %>%
    select(-LinkURN) %>% 
    rename(LinkURN = LinkURN.y)
  
  predecessors <- bind_rows(predecessors, predecessor_next_level)
  
  rows <- nrow(predecessor_next_level) > 0 

}

# Remove any urns duplicated in history
predecessors <- predecessors %>% 
  group_by(URN, LinkURN) %>%
  filter(Level == min(Level, na.rm = TRUE)) %>%
  ungroup()