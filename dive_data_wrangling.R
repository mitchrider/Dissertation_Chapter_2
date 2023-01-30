## Dive Data Wrangling ## 
# This code will process dive data from WICO Histos files to calculate...
# 1. Proportion of time at the surface
# 2. Average Dive Duration
# 3. Average Surface Duration

#load libraries
pacman::p_load(dplyr, ggplot2, RchivalTag, suncalc, lubridate)

#load the dive counts data
#histos_data <- read.csv("Data/Test_data/172446-Histos.csv")
histos_data <- dir(pattern = "*-Histos.csv", recursive = T) %>% 
  purrr::map_dfr(read.csv) %>% bind_rows()

#reformat the histos data to get percent time above 2m 
histos_data_reform <- histos_data %>% 
  filter(HistType == "TAD") %>% dplyr::select(1,7,16:29) %>% 
  mutate(Bin14 = as.integer(Bin14)) %>% 
  tidyr::pivot_longer(
    cols = starts_with("Bin"),
    names_to = "Depth_Bin",
    values_to = "Count",
    values_drop_na = F) %>% 
  mutate(datetime = as.POSIXct(Date, format = "%m/%d/%Y %H:%M"),
         Date = as.Date(Date, format = "%m/%d/%Y %H:%M"),
         Depth_Bin = case_when(Depth_Bin == "Bin1" ~ 0,
                               Depth_Bin == "Bin2" ~ 2,
                               Depth_Bin == "Bin3" ~ 5,
                               Depth_Bin == "Bin4" ~ 10,
                               Depth_Bin == "Bin5" ~ 25,
                               Depth_Bin == "Bin6" ~ 50,
                               Depth_Bin == "Bin7" ~ 100,
                               Depth_Bin == "Bin8" ~ 200,
                               Depth_Bin == "Bin9" ~ 300,
                               Depth_Bin == "Bin10" ~ 400,
                               Depth_Bin == "Bin11" ~ 500,
                               Depth_Bin == "Bin12" ~ 750,
                               Depth_Bin == "Bin13" ~ 1000,
                               Depth_Bin == "Bin14" ~ 2000)) 

#make sure to filter out instances of possible mortality or tag detachment
#plot out TAD both above and below 2 m
histos_data_reform %>% 
  filter(!Depth_Bin %in% c(0,2)) %>% tidyr::drop_na(Count) %>% 
  group_by(DeployID, datetime) %>% 
  summarise(tad_below_2 = sum(Count)) %>% 
  ggplot(., aes(x=datetime, y = tad_below_2)) +
  geom_line() + facet_wrap(~DeployID, scales = "free")


#make sure to only diurnal time bins 
TAD_data <- histos_data_reform %>% 
  mutate(hours = hour(datetime)) %>% 
  mutate(tperiod = case_when(hours == 0 ~ "20:00 - 2:00",
                             hours == 6 ~ "2:00 - 8:00",
                             hours == 12 ~ "8:00 - 14:00",
                             hours == 18 ~ "14:00 - 20:00")) %>%
  mutate(diel_period = ifelse(tperiod %in% c("20:00 - 2:00", "2:00 - 8:00"),
                              "20:00 - 8:00", "8:00 - 20:00")) %>% 
  filter(diel_period == "8:00 - 20:00") %>% 
  group_by(datetime, DeployID) %>% 
  summarise(TAD = sum(Count))

#plot out distribution
hist(TAD_data$TAD, breaks = 100)



## Get Average Surface and Dive Duration ## 
#load behavior data
#behavior_data <- read.csv("Data/172446-Behavior.csv")
behavior_data <- dir(pattern = "*-Behavior.csv", recursive = T) %>% 
  purrr::map_dfr(read.csv) %>% bind_rows()
str(behavior_data)

#load location data
#location_data <- read.csv("Data/post172446_interpolated.csv")
location_data <- dir(pattern = "*_interpolated.csv", recursive=T) %>% 
  purrr::map_dfr(readr::read_csv) %>% bind_rows()
str(location_data)

daily_locs <- location_data %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(id, date) %>% 
  summarise(lat = mean(y), lon = mean(x)) %>% 
  rename(DeployID = id)

#calculate average surface duration for diurnal activity
formatted_behavior_data <- behavior_data %>% 
  filter(What %in% c("Surface", "Dive")) %>%
  select(DeployID, What, Start, End, Shallow, DurationMin, DurationMax) %>% 
  mutate(date = as.Date(End, format= "%H:%M:%S %d-%b-%Y")) %>% 
  left_join(., daily_locs, by = c("date", "DeployID")) 

#join sunset and sunriese times to dive behavior  
diel_periods <- getSunlightTimes(data = formatted_behavior_data, keep = c("sunrise", "sunset"))

#bind formatted behavior with diel periods
behavior_diel <- bind_cols(formatted_behavior_data, diel_periods[,c("sunrise","sunset")])
  
#determine diel period of interval
behavior_day<-behavior_diel %>% mutate(end_datetime = as.POSIXct(End, format= "%H:%M:%S %d-%b-%Y")) %>% 
  mutate(diel_period = ifelse(end_datetime > sunrise & end_datetime < sunset, "day", "night")) %>% 
  filter(diel_period == "day") 

#plot out distribution of surface time
hist(behavior_day$Shallow, breaks = 1000)

#plot out distribution of dive time 
hist(behavior_day$DurationMax[behavior_day$What == "Dive"], breaks = 1000)













