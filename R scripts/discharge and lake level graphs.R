#2/6/2020
#Ellie Sovcik
#This script reads in discharge and lake level data from USGS data and graphs it

library("tidyverse")
library("lubridate")
library("xts")
library("cowplot")

setwd("C:/Users/esovc/ownCloud3/Shared/BREE/Watershed Data/Miss_Delta")

#Load in discharge data
discharge <- read_csv("Data/USGS Data/swanton_discharge_data.csv", col_types = cols()) %>% 
  mutate(r_timestamp = mdy_hm(Datetime, tz = "America/New_York"))
#remove Halloween storm from the data to better show seasonal discahrge
dis_cut <- read_csv("Data/USGS Data/swanton_discharge_data.csv", col_types = cols()) %>% 
  mutate(r_timestamp = mdy_hm(Datetime, tz = "America/New_York")) %>% 
  filter(r_timestamp < ymd_hms("2019-11-01 00:00:00", tz = "America/New_York"))
#Load in lake level data
lake <- read_csv("Data/USGS Data/lake_level_burlington_data.csv", col_types = cols()) %>% 
  mutate(r_timestamp = mdy(Date, tz = "America/New_York")) %>% 
  filter(r_timestamp < ymd_hms("2019-11-01 00:00:00", tz = "America/New_York")) 
  # mutate(r_timestamp = as.character(r_timestamp))


#Calulate average discharge per day of data (to simplify and match lake data frequency)
# dis_avg <- 
#   dis_cut %>%
#   group_by(date(r_timestamp)) %>% 
#   # group_by(month(r_timestamp), day(r_timestamp)) %>% 
#   summarize(dis_mean = mean(discharge_cfs, na.rm = TRUE)) %>% 
#   rename(r_timestamp = "date(r_timestamp)") %>% 
#   mutate(r_timestamp = as.character(r_timestamp))

dis_avg <- 
  dis_cut %>%
  group_by(date(r_timestamp)) %>% 
  # group_by(month(r_timestamp), day(r_timestamp)) %>% 
  summarize(dis_mean = mean(discharge_cfs, na.rm = TRUE)) %>% 
  rename(r_timestamp = "date(r_timestamp)")

#Bind discharge and lake level data
# Bind data together
comb <- bind_cols(lake, dis_avg) %>% 
  # Add a month column & make it a factor instead of number
  mutate(month = as.factor(month(r_timestamp))) %>% 
  # Convert cubic ft per sec to cubic meters per sec
  mutate(dis_mean_cms = dis_mean*0.0283168) %>% 
  # Convert lake level in feet to meters
  mutate(lake_level_m = `lake level_ft`*0.3048)


# Plot mean daily discharge and lake level separately and then combine into one figure
# Discharge here is cms and lake level is in m
  # Set theme
  theme1 <- 
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11)) 
    
  # Daily mean river discharge
  pl_dis <- comb %>%
    ggplot(aes(x = r_timestamp, y = dis_mean_cms)) +
    geom_line() +
    labs(x = "Time", y = expression(Discharge~(m^{3}~s^{-1}))) +
    scale_x_datetime(date_breaks = "1 month",
                     date_labels = "%b") +
    geom_vline(xintercept=as.numeric(as.POSIXct("2019-05-17 12:00:00")), color = "coral1", size = 1) +
    geom_vline(xintercept=as.numeric(as.POSIXct("2019-07-03 12:00:00")), color = "darkolivegreen4", size = 1) +
    geom_vline(xintercept=as.numeric(as.POSIXct("2019-08-14 12:00:00")), color = "turquoise3", size = 1) +
    geom_vline(xintercept=as.numeric(as.POSIXct("2019-10-21 12:00:00")), color = "mediumorchid1", size = 1) +
    theme1 +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  
  # Lake surface level
  pl_lake <- comb %>%
    ggplot(aes(x = r_timestamp, y = lake_level_m)) +
    geom_line() +
    labs(x = "Time", y = "Lake surface \n elevation (m)") +
    scale_x_datetime(date_breaks = "1 month",
                     date_labels = "%b") +
    geom_vline(xintercept=as.numeric(as.POSIXct("2019-05-17 12:00:00")), color = "coral1", size = 1) +
    geom_vline(xintercept=as.numeric(as.POSIXct("2019-07-03 12:00:00")), color = "darkolivegreen4", size = 1) +
    geom_vline(xintercept=as.numeric(as.POSIXct("2019-08-14 12:00:00")), color = "turquoise3", size = 1) +
    geom_vline(xintercept=as.numeric(as.POSIXct("2019-10-21 12:00:00")), color = "mediumorchid1", size = 1) +
    theme1 +
    theme(axis.title.x = element_blank())
  
  # Combine plots into 1 using cowplot
  plot1 <- plot_grid(pl_dis, pl_lake, ncol = 1, align = "hv",
                     labels = "auto")
  save_plot("Plots/figure2_discharge_lakelevel.png", plot1, base_width = 6, base_height = 4.5, dpi = 150)


# Older plot versions ----
#Plot discharge
discharge %>% 
  ggplot(aes(x = r_timestamp, y = discharge_cfs)) +
    geom_path(size = .5)  +
  labs(x = "Time", y = "Discharge (cfs)") +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-05-17 12:00:00")), color = "coral1", size = 1) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-07-03 12:00:00")), color = "darkolivegreen4", size = 1) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-08-14 12:00:00")), color = "turquoise3", size = 1) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-10-21 12:00:00")), color = "mediumorchid1", size = 1)

#Plot discharge without Halloween storm
dis_avg %>% 
  ggplot(aes(x = r_timestamp, y = dis_mean)) +
   geom_vline(xintercept=ymd("2019-05-17"), color = "coral1", size = 1) +
    geom_vline(xintercept=ymd("2019-07-03"), color = "darkolivegreen4", size = 1) +
    geom_vline(xintercept=ymd("2019-08-14"), color = "turquoise3", size = 1) +
    geom_vline(xintercept=ymd("2019-10-21"), color = "mediumorchid1", size = 1) +
  geom_line(size = .5) + 
  labs(x = "Time", y = "Discharge (cfs)") +
  theme_bw() + theme(panel.grid = element_blank()) 
   
#Plot lake data
lake %>% 
  ggplot(aes(x = Date, y = `lake level_ft`, group = 1)) +
  geom_point(size = 1) +
  geom_line() +
  labs(x = "Time", y = "Lake level (ft)") +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept=as.numeric(31), color = "coral1", size = 1) +
  geom_vline(xintercept=as.numeric(78), color = "darkolivegreen4", size = 1) +
  geom_vline(xintercept=as.numeric(120), color = "turquoise3", size = 1) +
  geom_vline(xintercept=as.numeric(188), color = "mediumorchid1", size = 1) 


#Facet_wrap discharge and lake level data

#create labels for vaiable names
labs <- c('lake level_ft' = "Lake Level (ft)", dis_mean = "Q (cfs)")
#plot them!
comb %>% 
  gather(key = "var", value = "level", c(`lake level_ft`, dis_mean)) %>% 
  ggplot(aes(x = r_timestamp, y = level, group = var)) +
  facet_wrap(~var, ncol = 1, scales = "free_y", labeller = labeller(var = labs)) +
  geom_line() + 
  geom_vline(xintercept=as.numeric(31), color = "coral1", size = 1) +
  geom_vline(xintercept=as.numeric(78), color = "darkolivegreen4", size = 1) +
  geom_vline(xintercept=as.numeric(120), color = "turquoise3", size = 1) +
  geom_vline(xintercept=as.numeric(188), color = "mediumorchid1", size = 1) +
  labs(x = "Date", y = NULL) +
  theme_bw() + theme(panel.grid = element_blank()) 


