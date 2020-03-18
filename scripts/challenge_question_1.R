library(tidyverse) #grab tidyverse
bom_data <- read_csv("data/BOM_data.csv") # read in data
bom_stations <- read_csv("data/BOM_stations.csv")

view(bom_stations)
view(bom_data)

#quest 1 
ques_1 <- bom_data %>%    #label 
  separate(Temp_min_max,into = c("min_temp","max_temp")) %>%  #separate the data into new columns
  filter(min_temp != "" , max_temp != "", Rainfall != "-") %>%  #filter out data with no entry
  group_by(Station_number) %>%  #group data by station number
  summarise(num_row = n()) #summarise number of rows
  
#question 2

lowest_av_temp <- bom_data %>% 
  separate(Temp_min_max,into = c("min_temp","max_temp"), sep="/")  %>%  
  filter(min_temp  != "", min_temp != "-" , max_temp != "", max_temp != "-") %>% 
  mutate(temp_diff = as.numeric(max_temp) - as.numeric(min_temp)) %>% 
  group_by(Month) %>% 
  summarise(average = mean(temp_diff)) %>% 
  arrange(average) %>% 
  slice(1)
  
 
  



#QUESTION 3

#first i need to arrange the bom station data into tidy data
stations_state_tidy <- bom_stations %>%
  gather(key = Station_number, value = value, -info) %>% 
  spread(key = info, value = value) %>%
  mutate(Station_number =as.numeric(Station_number))

#separate the values in the bom data so you can use them for the calculation
bom_data_sep_all <- bom_data %>% 
  separate(Temp_min_max,into = c("min_temp","max_temp"), sep="/")

#combine both data sets from above so you can do the calc
combined_data <-  full_join(stations_state_tidy, bom_data_sep_all, by = c ("Station_number" = "Station_number") )

#change values into numeric form 
answer_3 <- combined_data %>% 
  mutate(min_temp = as.numeric(min_temp)) %>% 
  mutate(max_temp = as.numeric(max_temp)) %>% 
  mutate(difference_temp = max_temp - min_temp) %>% 
  group_by(state) %>% 
  summarise(average = mean(difference_temp, na.rm = TRUE)) %>% 
  arrange(average) %>% 
  slice(1)

#ANSWER - QUEENSLAND 7.36 HAD THE LOWEST 



#question 4

#average solar exposure 



solar_averg_ <- combined_data %>% 
  mutate(Solar_expo = as.numeric(Solar_exposure),
           lon = as.numeric(lon)) %>% 
  filter(Solar_expo !="na") %>% #remove all na fields
  group_by(Station_number, lon) %>%  #group by station
  summarise(average = mean(Solar_expo)) %>%  #summarise whilst calculating the average 
  arrange(lon) %>% #arrange in order of lon
  ungroup() %>% filter(lon == min(lon) | lon == max(lon)) # if there is a grouping a filter will look into min and max of each group
  

 





