#Analysis of figures for manuscript
#Igrena Aponte - 10/19/21

#Figure 2 from ES thesis 
#Add discharge time series for Wade and Hungerford to subplot a.
# Code from R script - discharge and lake level graphs 
# Adapted by DWK on 10/8/21

library("tidyverse")
library("lubridate")
# library("xts")
# library("cowplot")
library("patchwork")

# setwd("C:/Users/esovc/ownCloud3/Shared/BREE/Watershed Data/Miss_Delta")

#Load in Miss Riv @ Swanton dam discharge data
# discharge <- read_csv("Data/USGS Data/swanton_discharge_data.csv", col_types = cols()) %>% 
#   mutate(r_timestamp = mdy_hm(Datetime, tz = "America/New_York")) %>% 
#   mutate(site = "Swanton") %>% 
#   # Convert from cfs to cms
#   mutate(q_cms = discharge_cfs*0.0283168) %>% 
#   select(r_timestamp, q_cms, site)

# remove Halloween storm from the data to better show seasonal discahrge
q_miss <- read_csv("Data/USGS Data/swanton_discharge_data.csv", col_types = cols()) %>% 
  mutate(r_timestamp = mdy_hm(Datetime, tz = "America/New_York")) %>% 
  filter(r_timestamp < ymd_hms("2019-11-01 00:00:00", tz = "America/New_York")) %>% 
  mutate(site = "Missisquoi") %>% 
  # Convert from cfs to cms
  mutate(q_cms = discharge_cfs*0.0283168) %>% 
  select(r_timestamp, q_cms, site)

# Load in lake level data
lake <- read_csv("Data/USGS Data/lake_level_burlington_data.csv", col_types = cols()) %>% 
  mutate(r_timestamp = mdy(Date, tz = "America/New_York")) %>% 
  filter(r_timestamp < ymd_hms("2019-11-01 00:00:00", tz = "America/New_York")) %>% 
  # Convert lake left from ft to m
  mutate(lake_level_m = `lake level_ft`*0.3048) %>% 
  select(-c(Date, month, `lake level_ft`))

# Hungerford and Wade 2019 discharge data
# Hford
q_hford <- read_csv("Data/hungerford_2019_best_q.csv", col_types = cols()) %>% 
  mutate(site = "Hungerford") %>% 
  rename(q_cms = HF_best_q, timestamp = r_timestamp) %>% 
  select(-c(X1, hobo_stage, offset, hobo_stage_int, corr_stage)) %>% 
  mutate(timestamp = ymd_hms(timestamp, tz = "America/New_York")) %>% 
  filter(timestamp < ymd_hms("2019-11-01 00:00:00", tz = "America/New_York")) %>% 
  rename(r_timestamp = timestamp)
# Wade
q_wade <- read_csv("Data/wade_2019_best_q.csv", col_types = cols()) %>% 
  mutate(site = "Wade") %>% rename(q_cms = best_q, timestamp = r_timestamp) %>% 
  select(site, timestamp, q_cms) %>% 
  mutate(timestamp = ymd_hms(timestamp, tz = "America/New_York")) %>% 
  filter(timestamp < ymd_hms("2019-11-01 00:00:00", tz = "America/New_York")) %>% 
  rename(r_timestamp = timestamp)

# Combine and calculate average discharge per day of data (to simplify and match lake data frequency)
dis_avg <- 
  q_miss %>% 
  bind_rows(q_hford) %>% 
  bind_rows(q_wade) %>% 
  group_by(site, date(r_timestamp)) %>% 
  summarize(q_cms_mean = mean(q_cms, na.rm = TRUE)) %>% 
  rename(r_timestamp = "date(r_timestamp)") %>% 
  pivot_wider(names_from = site, values_from = q_cms_mean)

#Bind discharge and lake level data
# Bind data together
comb <- 
  dis_avg %>% 
  left_join(lake, by = "r_timestamp") %>% 
  filter(r_timestamp >= ymd("2019-04-17"))



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
  pivot_longer(cols = c(Hungerford, Wade, Missisquoi), names_to = "site", values_to = "q_cms") %>% 
  mutate(site = factor(site, levels = c("Missisquoi", "Hungerford", "Wade"))) %>% 
  ggplot(aes(x = r_timestamp, y = q_cms, group = site, linetype = site)) +
  geom_line() +
  labs(x = "Time", y = expression(Discharge~(m^{3}~s^{-1}))) +
  # Transform y-axis to log10
  scale_y_log10() +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b") +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash")) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-05-17 12:00:00")), color = "coral1", size = 1) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-07-03 12:00:00")), color = "darkolivegreen4", size = 1) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-08-14 12:00:00")), color = "turquoise3", size = 1) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-10-21 12:00:00")), color = "mediumorchid1", size = 1) +
  theme1 +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank())

# Lake surface level
pl_lake <- comb %>%
  ggplot(aes(x = r_timestamp, y = lake_level_m)) +
  geom_line() +
  labs(x = "Time", y = "Lake surface\nelevation (m)") +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-05-17 12:00:00")), color = "coral1", size = 1) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-07-03 12:00:00")), color = "darkolivegreen4", size = 1) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-08-14 12:00:00")), color = "turquoise3", size = 1) +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-10-21 12:00:00")), color = "mediumorchid1", size = 1) +
  theme1 +
  theme(axis.title.x = element_blank())

# Combine plots into 1
plot1 <- pl_dis / pl_lake + plot_annotation(tag_levels = "a")
# plot1 <- plot_grid(pl_dis, pl_lake, ncol = 1, align = "hv",
#                    labels = "auto")
ggsave("Plots/figure2_discharge_lakelevel.png", plot1, width = 6, height = 4, units = "in", dpi = 150)

#/#/#/#/#/#/#

#Plot as bar plots similar to Figures 3e-h in ED thesis, except that instead of CV%, 
#plot means as bar height and std dev as error bars; might want to include Figures 
#3a-d in Supp Info if we describe any aspect of longitudinal variability or signal 
#(e.g., if we want to show where we think lake might start to influence river vars near river mouth); 
#include this stacked bar plot as a subplot of the figure with discharge and lake levels



