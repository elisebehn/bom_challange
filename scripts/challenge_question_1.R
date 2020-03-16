library(tidyverse)
bom_data <- read_csv("data/BOM_data.csv")
bom_stations <- read_csv("data/BOM_stations.csv")

view(bom_stations)
view(bom_data)


ques_1 <- bom_data %>% 
  separate(Temp_min_max,into = c("mean_temp","max_temp")) %>% 
  filter(mean_temp != "" , max_temp != "", Rainfall != "-") %>% 
  group_by(Station_number) %>% 
  summarise(num_row = n())
  
           