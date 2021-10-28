#Analysis of figures for manuscript
#Igrena Aponte - 10/19/21

#Figure 2 from ES thesis 
#Add discharge time series for Wade and Hungerford to subplot a.
# Code from R script - discharge and lake level graphs 
# Adapted by DWK on 10/8/21

library("tidyverse")
library("lubridate")
#library("xts")
library("cowplot")
library("patchwork")

# setwd("C:/Users/esovc/ownCloud3/Shared/BREE/Watershed Data/Miss_Delta")

#----Plot 01----

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

#----Plot 02----

#Read in data
#code extracted from the script: combined data analysis.R
#lines 11 - 74
may <- read_csv("Data/alldata_compiled_2019-05-17.csv", col_types = cols()) %>%
  mutate(r_timestamp = as.character(r_timestamp)) %>% 
  # Remove low DOC outlier in May
  mutate(NPOC_mgCL = ifelse(NPOC_mgCL < 1, NA, NPOC_mgCL))
july <- read_csv("Data/alldata_compiled_2019-07-03.csv", col_types = cols()) %>%
  mutate(r_timestamp = as.character(r_timestamp))
# august <- read_csv("Data/alldata_compiled_2019-08-14.csv",
#                    col_types = cols(NO3_UM = "n", NH4_UM = "d",PO4_UM = "d", 
#                                     NO3_mgNL = "d", NH4_mgNL = "d", PO4_mgPL = "d")) %>%
#   mutate(r_timestamp = as.character(r_timestamp))
# Here is a weird f&cking workaround for the issue above w/ August data - angry face!
august <- read_csv("Data/alldata_compiled_2019-08-14.csv", col_types = cols()) %>% 
  mutate(r_timestamp = as.character(r_timestamp),
         NO3_UM = as.numeric(NO3_UM),
         NH4_UM = as.numeric(NH4_UM),
         PO4_UM = as.numeric(PO4_UM), 
         NO3_mgNL = as.numeric(NO3_mgNL), 
         NH4_mgNL = as.numeric(NH4_mgNL), 
         PO4_mgPL = as.numeric(PO4_mgPL))
october <- read_csv("Data/alldata_compiled_2019-10-21.csv", col_types = cols()) %>%
  mutate(r_timestamp = as.character(r_timestamp)) 


# Bind data together
comb <- bind_rows(may, july, august, october) %>% 
  # Format date if not already done
  mutate(r_timestamp = ymd_hms(r_timestamp)) %>% 
  # Add a month column & make it a factor instead of number
  mutate(month = as.factor(month(r_timestamp)))
# rm(may, july, august, october)

#Calculate N and P fractions (that were not measured)
comb <- comb %>% 
  #Calculate Particulate N 
  mutate(PN = TN_mgNL - TDN_mgNL) %>% 
  #Calculate DON
  mutate(DON = TDN_mgNL - NH4_mgNL - NO3_mgNL) %>% 
  #Calculate Particulate P
  mutate(PP = TP_mgPL - TDP_mgPL) %>% 
  #Calculate DOP
  mutate(DOP = TDP_mgPL - PO4_mgPL) %>% 
  #Make negative values 0
  mutate(PN = replace(PN, PN < 0, 0),
         PP = replace(PP, PP < 0, 0)) %>% 
  #Calculate molar ratios
  mutate(TN_TP = TN_UM / TP_UM,
         DIN_SRP = (NO3_UM + NH4_UM) / PO4_UM,
         DOC_TN = NPOC_mgCL * 1000/12.011 / TN_UM,
         DOC_TP = NPOC_mgCL * 1000/12.011 / TP_UM,
         DOC_DIN = NPOC_mgCL * 1000/12.011 / (NO3_UM + NH4_UM),
         DOC_SRP = NPOC_mgCL * 1000/12.011 / PO4_UM) 


# Calculate summary stats for each month
month_sum <- comb %>% 
  gather(key = "var", value = "value", c(temp_c, do_mgl, do_per, spc_uscm, ph, NO3_UM:Fe_ppb_0.45, PN:DOC_TP)) %>% 
  group_by(transect, month, var) %>% 
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            CV = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE),
            CV_per = CV*100) %>% 
  filter(transect != "Dam") %>% 
  ungroup()


#Ploting Data 
month_sum %>% 
  filter(transect == "Main") %>% 
  filter(var %in% c("do_per", "ph", "spc_uscm", "temp_c")) %>% 
  # Re-order the variables so that temperature is plotted on top
  mutate(var = factor(var, levels = c("temp_c", "do_per", "ph", "spc_uscm"),
                      # I provide labels here for the facetted plots that include symbols and then use labeller = label_parsed below to parse them into label text
                      labels = c("Temp.~(degree*C)", "DO~('%'*~saturation)", "pH", "Sp.~cond.~(mu*S~cm^{-1})"))) %>% 
  # Provide abbreviated month labels for legend instead of numbers
  mutate(month = factor(month, levels = c("5", "7", "8", "10"),
                        labels = c("May", "Jul", "Aug", "Oct"))) %>%
  ggplot(aes(x = month, y = mean, fill = month)) +  
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
  facet_wrap(~var, ncol=1,scales = "free_y")

#----Plot 3 ---- 

#Nitrogen Dynamics plot 

#----Suplemental information: Plots---- 



