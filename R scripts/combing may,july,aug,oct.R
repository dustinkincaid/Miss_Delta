#plotting may, july, august, and october data on the same plots
#Ellie Sovcik
#09/12/19
install.packages("tidyverse")
#Load packages
library("tidyverse")
library("lubridate")


setwd('C:/Users/esovc/ownCloud3/Shared/BREE/Watershed Data/Miss_Delta')

# Read in data
may <- read_csv("Data/alldata_compiled_2019-05-17.csv", col_types = cols()) %>%
      mutate(r_timestamp = as.character(r_timestamp))
july <- read_csv("Data/alldata_compiled_2019-07-03.csv", col_types = cols()) %>%
     mutate(r_timestamp = as.character(r_timestamp))
august <- read_csv("Data/alldata_compiled_2019-08-14.csv", 
                   col_types = cols(NO3_UM = "d", NH4_UM = "d",PO4_UM = "d", 
                                    NO3_mgNL = "d", NH4_mgNL = "d", PO4_mgPL = "d")) %>%
     mutate(r_timestamp = as.character(r_timestamp))
october <- read_csv("Data/alldata_compiled_2019-10-21.csv", col_types = cols()) %>%
      # October r_timestamp is in a different format (this takes that into consideration?)
      #mutate(r_timestamp = mdy_hms(r_timestamp)) %>%
      mutate(r_timestamp = as.character(r_timestamp)) 
     
  
#pull out August mainstream/ exclude Dead Creek
aug_main <- august %>% filter(transect == "Main")
#pull out October mainstem/ exclude Dead Creek
oct_main <- october %>% filter(transect == "Main")
# Bind data together
comb <- bind_rows(may, july, aug_main, oct_main) %>% 
  # Format date if not already done
   mutate(r_timestamp = ymd_hms(r_timestamp)) %>% 
  # Add a month column & make it a factor instead of number
  mutate(month = as.factor(month(r_timestamp)))

#pull out Dead Creek Data from August and October
  aug_dead <- august %>% filter(transect == "Dead Creek")
  oct_dead <- october %>% filter(transect == "Dead Creek")
#bind Dead Creek data together
  dead <- bind_rows(aug_dead, oct_dead) %>%
    # Add a month column & make it a factor instead of number
    mutate(month = as.factor(month(r_timestamp)))
  
# Plot data
# Plotting Nitrate sensor data
comb %>%
  ggplot(aes(x = dist_cum_m, y = scan_NO3_mgNL, group = month, color = month)) +
    geom_line() + 
  geom_vline(xintercept = 6750) +
  theme_bw() + theme(panel.grid = element_blank()) +
  xlab("Distance (m)") + 
  ylab(bquote(~NO[3]~ (mg N/L^-1)))
# Plotting DOC(mg/L) data
comb %>%
  ggplot(aes(x = dist_cum_m, y = scan_DOC_mgCL, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "DOC (mg/L)") +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750)
# Plotting temp data
comb %>%
  ggplot(aes(x = dist_cum_m, y = temp_c, group = month, color = month)) +
  geom_line() + labs(x = "distance (m)", y = "Temperature (C)") +
    theme_bw() + theme(panel.grid = element_blank()) +
    geom_vline(xintercept = 6750)
# Plotting DO percent data
comb %>%
  ggplot(aes(x = dist_cum_m, y = do_per, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "DO %") +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750)
# Plotting DO (mg/L) data
comb %>%
  ggplot(aes(x = dist_cum_m, y = do_mgl, group = month, color = month)) +
  geom_line() + labs(x = "distance (m)", y = "DO (mg/L)") +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750)
# Plotting pH data
comb %>%
  ggplot(aes(x = dist_cum_m, y = ph, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "pH") +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750)
# Plotting fdom data
comb %>%
  ggplot(aes(x = dist_cum_m, y = fdom_qsu, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "fdom") +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750)
#Plotting turbidity data
comb %>%
  ggplot(aes(x = dist_cum_m, y = turb_NTU_scan, group = month, color = month)) +
  geom_line() + labs(x = "distance (m)", y = "Turbidity") +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750)
# Plotting TOC(mg/L) data
comb %>%
  ggplot(aes(x = dist_cum_m, y = toc_mgL_scan, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "TOC (mg/L)") +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750)

#Plotting longitudinal profiles for all months of temp, DO, pH, and specific conductivity
sensor_labels <- c(temp_c = "Temperature (C)", do_mgl = "DO (mg/L)", 
                   ph = "pH", spc_uscm = "Specific Conductivity (??S/cm)")

comb %>% 
  gather(key = "var", value = "value", c(temp_c, do_mgl, ph, spc_uscm)) %>% 
  ggplot(aes(x = dist_cum_m, y = value, group = month, color = month)) +
  geom_line() +
  facet_wrap(~var, ncol = 1, scales = "free_y", labeller = labeller(var = sensor_labels)) +
  labs(x = "Distance (m)", y = NULL) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750) +
  scale_linetype_discrete(name = "Month", 
                      breaks= c("5", "7", "8", "10"), 
                      labels = c("May", "July", "August", "October"))

# Plotting Nutrient Grab Sample Data

#Plotting Dissolved Nutrient Data
diss_labels <- c(NH4_mgNL = "NH4 (mg/L N)", NO3_mgNL = "NO3 (mg/L N)", 
                 PO4_mgPL = "PO4 (mg/L P)")
comb %>%
  # Gather all the nutrient time series into long format
  gather(key = "var", value = "value", c(NO3_mgNL:PO4_mgPL)) %>% 
  #filter out NA values
   filter(!is.na("NO3 (mg/L N)")) %>%
#Plot!
  ggplot(aes(x = dist_cum_m, y = value, group = month, color = month)) +
  geom_point() + geom_line() +
  facet_wrap(~var, ncol = 1, scales = "free_y", labeller = labeller(var = diss_labels)) +
  labs(x = "Distance (m)", y = NULL) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750) 

#Plotting Total Dissolved Nutrient Data
tot_diss_labels <- c(TDN_UM = "TN uMol/L N", TDP_UM = "TP uMol/L P",
                     TDN_mgNL = "TN mg/L N", TDP_mgPL = "TP mg/L P")
comb %>%
  # Gather all the nutrient time series into long format
  gather(key = "var", value = "value", c(TDN_UM:TDP_mgPL)) %>% 
  #filter out NA values
  filter(!is.na("NO3 (mg/L N)")) %>%
  #Plot!
  ggplot(aes(x = dist_cum_m, y = value, group = month, color = month)) +
  geom_point() + geom_line() +
  facet_wrap(~var, ncol = 1, scales = "free_y", labeller = labeller(var = tot_diss_labels)) +
  labs(x = "Distance (m)", y = NULL) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750) 

#Plotting Totals
tot_labels <- c(TDN_UM = "TN uMol/L N", TDP_UM = "TP uMol/L P",
                     TDN_mgNL = "TN mg/L N", TDP_mgPL = "TP mg/L P")
comb %>%
  # Gather all the nutrient time series into long format
  gather(key = "var", value = "value", c(TDN_UM:TDP_mgPL)) %>% 
  #filter out NA values
  filter(!is.na("NO3 (mg/L N)")) %>%
  #Plot!
  ggplot(aes(x = dist_cum_m, y = value, group = month, color = month)) +
  geom_point() + geom_line() +
  facet_wrap(~var, ncol = 1, scales = "free_y", labeller = labeller(var = tot_diss_labels)) +
  labs(x = "Distance (m)", y = NULL) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750) 

#Plotting DOC grab samples
comb %>%
  #Plot!
  ggplot(aes(x = dist_cum_m, y = NPOC_mgCL, group = month, color = month)) +
  geom_point() + geom_line() +
  labs(x = "Distance (m)", y = NULL) +
  #Scaling y to omit low outlier from May
  scale_y_continuous(limits = c(2, 7)) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750) 
#Plotting DOC grabs and sensor data
comb %>%
  ggplot(aes(x = dist_cum_m, y = doc_mgL_scan, group = month, color = month)) +
  geom_line() +
  geom_point(aes(y = NPOC_mgCL)) + 
  labs(x = "distance (m)", y = NULL) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750)

#TSS Data from each transect and combined into one plot
# Plotting TSS (mg/L) data
comb %>%
  filter(!is.na(tss_mgL)) %>%
  ggplot(aes(x = dist_cum_m, y = tss_mgL, group = month, color = month)) +
  geom_point(size = 2) + geom_line() +
  labs(x = "Distance (m)", y = "TSS (mg/L)") +
#Scaling y to omit high outlier from May
  scale_y_continuous(limits = c(0, 7.5)) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_vline(xintercept = 6750)
#Plotting may TSS alone
  may %>% 
    filter(!is.na(tss_mgL)) %>%
    ggplot(aes(x = dist_cum_m, y = tss_mgL)) +
       geom_point(color = "coral1", size = 3) + geom_line(color = "coral1") +
     labs(x = "Distance (m)", y = "TSS (mg/L)") +
  #Scaling y to omit high outlier from May
     scale_y_continuous(limits = c(0, 7.5)) +
   theme_bw() + theme(panel.grid = element_blank()) +
    geom_vline(xintercept = 6750)
#Plotting july TSS alone
  july %>% 
    filter(!is.na(tss_mgL)) %>%
    ggplot(aes(x = dist_cum_m, y = tss_mgL)) + 
      geom_point(color = "seagreen3", size = 3) + geom_line(color = "seagreen3") +
      labs(x = "Distance (m)", y = "TSS (mg/L)") +
  theme_bw() + theme(panel.grid = element_blank()) +
    geom_vline(xintercept = 6750)
#Plotting August TSS alone
  aug_main %>% 
    filter(!is.na(tss_mgL)) %>%
    ggplot(aes(x = dist_cum_m, y = tss_mgL)) +
     geom_point(color = "turquoise3", size = 3) + geom_line(color = "turquoise3") +
      labs(x = "Distance (m)", y = "TSS (mg/L)") +
    theme_bw() + theme(panel.grid = element_blank()) +
      geom_vline(xintercept = 6750)
  
#Plotting TSS data for each month with turbidity s::can data
#May Data
  may %>%
    filter(!is.na(tss_mgL)) %>%
    ggplot(aes(x = dist_cum_m, y = turb_NTU_scan, color = "coral1")) +
    geom_line() + 
    geom_point(aes(y = tss_mgL)) + geom_line(aes(y = tss_mgL)) +
    labs(x = "distance (m)", y = NULL) +
  #scale out outlier
    scale_y_continuous(limits = c(0, 7.5)) +
    theme_bw() + theme(panel.grid = element_blank()) +
    geom_vline(xintercept = 6750)
#July Data
  july %>%
    filter(!is.na(tss_mgL)) %>%
    ggplot(aes(x = dist_cum_m, y = turb_NTU_scan)) +
    geom_line(color = "seagreen3") + 
    geom_point(aes(y = tss_mgL), color = "seagreen3") + geom_line(aes(y = tss_mgL), color = "seagreen4") +
    labs(x = "distance (m)", y = NULL) +
    theme_bw() + theme(panel.grid = element_blank()) +
    geom_vline(xintercept = 6750)
#August Data
  aug_main %>%
    filter(!is.na(tss_mgL)) %>%
    ggplot(aes(x = dist_cum_m, y = turb_NTU_scan)) +
    geom_line(color = "turquoise3") + 
    geom_point(aes(y = tss_mgL), color = "turquoise3") + geom_line(aes(y = tss_mgL), color = "turquoise4") +
    labs(x = "distance (m)", y = NULL) +
    theme_bw() + theme(panel.grid = element_blank()) +
    geom_vline(xintercept = 6750)
  

# Plot Dead Creek Run data
  # Plotting Nitrate sensor data
  dead %>%
    ggplot(aes(x = dist_cum_m, y = no3_mgL_scan, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "NO3 (mg/L)") +
    theme_bw() + theme(panel.grid = element_blank())
  # Plotting DOC(mg/L) data
  dead %>%
    ggplot(aes(x = dist_cum_m, y = doc_mgL_scan, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "DOC (mg/L)") +
    theme_bw() + theme(panel.grid = element_blank()) 
  # Plotting temp data
  dead %>%
    ggplot(aes(x = dist_cum_m, y = temp_c, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "Temperature (C)") +
    theme_bw() + theme(panel.grid = element_blank()) 
  # Plotting DO percent data
  dead %>%
    ggplot(aes(x = dist_cum_m, y = do_per, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "DO %") +
    theme_bw() + theme(panel.grid = element_blank()) 
  # Plotting DO (mg/L) data
  dead %>%
    ggplot(aes(x = dist_cum_m, y = do_mgl, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "DO (mg/L)") +
    theme_bw() + theme(panel.grid = element_blank()) 
  # Plotting pH data
  dead %>%
    ggplot(aes(x = dist_cum_m, y = ph, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "pH") +
    theme_bw() + theme(panel.grid = element_blank()) 
  # Plotting fdom data
  dead %>%
    ggplot(aes(x = dist_cum_m, y = fdom_qsu, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "fdom") +
    theme_bw() + theme(panel.grid = element_blank()) 
  #Plotting turbidity data
  dead %>%
    ggplot(aes(x = dist_cum_m, y = turb_NTU_scan, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "Turbidity") +
    theme_bw() + theme(panel.grid = element_blank()) 
  # Plotting TOC(mg/L) data
  dead %>%
    ggplot(aes(x = dist_cum_m, y = toc_mgL_scan, group = month, color = month)) +
    geom_line() + labs(x = "distance (m)", y = "TOC (mg/L)") +
    theme_bw() + theme(panel.grid = element_blank()) 
  # Plotting TSS (mg/L) data
  dead %>%
    filter(!is.na(tss_mgL)) %>%
    ggplot(aes(x = dist_cum_m, y = tss_mgL, group = month, color = month)) +
    geom_point(size = 2) + geom_line() +
    labs(x = "Distance (m)", y = "TSS (mg/L)") +
    theme_bw() + theme(panel.grid = element_blank()) 
  
  # Plotting Nutrient Grab Sample Data
  
  #Plotting Dissolved Nutrient Data
  diss_labels_dead <- c(NH4_mgNL = "NH4 (mg/L N)", NO3_mgNL = "NO3 (mg/L N)", 
                   PO4_mgPL = "PO4 (mg/L P)")
  dead %>%
    # Gather all the nutrient time series into long format
    gather(key = "var", value = "value", c(NO3_mgNL:PO4_mgPL)) %>% 
    #filter out NA values
    filter(!is.na("NO3 (mg/L N)")) %>%
    #Plot!
    ggplot(aes(x = dist_cum_m, y = value, group = month, color = month)) +
    geom_point() + geom_line() +
    facet_wrap(~var, ncol = 1, scales = "free_y", labeller = labeller(var = diss_labels_dead)) +
    labs(x = "Distance (m)", y = NULL) +
    theme_bw() + theme(panel.grid = element_blank()) 
  
  #Plotting Total Dissolved Nutrient Data
  tot_diss_labels_dead <- c(TDN_UM = "TN uMol/L N", TDP_UM = "TP uMol/L P",
                       TDN_mgNL = "TN mg/L N", TDP_mgPL = "TP mg/L P")
  dead %>%
    # Gather all the nutrient time series into long format
    gather(key = "var", value = "value", c(TDN_UM:TDP_mgPL)) %>% 
    #filter out NA values
    filter(!is.na("NO3 (mg/L N)")) %>%
    #Plot!
    ggplot(aes(x = dist_cum_m, y = value, group = month, color = month)) +
    geom_point() + geom_line() +
    facet_wrap(~var, ncol = 1, scales = "free_y", labeller = labeller(var = tot_diss_labels_dead)) +
    labs(x = "Distance (m)", y = NULL) +
    theme_bw() + theme(panel.grid = element_blank()) 
  
  #Plotting Totals
  tot_labels_dead <- c(TDN_UM = "TN uMol/L N", TDP_UM = "TP uMol/L P",
                  TDN_mgNL = "TN mg/L N", TDP_mgPL = "TP mg/L P")
  dead %>%
    # Gather all the nutrient time series into long format
    gather(key = "var", value = "value", c(TDN_UM:TDP_mgPL)) %>% 
    #filter out NA values
    filter(!is.na("NO3 (mg/L N)")) %>%
    #Plot!
    ggplot(aes(x = dist_cum_m, y = value, group = month, color = month)) +
    geom_point() + geom_line() +
    facet_wrap(~var, ncol = 1, scales = "free_y", labeller = labeller(var = tot_labels_dead)) +
    labs(x = "Distance (m)", y = NULL) +
    theme_bw() + theme(panel.grid = element_blank()) 
  
#Plotting DOC grab sample data 
  dead %>%
    #Plot!
    ggplot(aes(x = dist_cum_m, y = NPOC_mgCL, group = month, color = month)) +
    geom_point() + geom_line() +
    labs(x = "Distance (m)", y = NULL) +
    theme_bw() + theme(panel.grid = element_blank())
#Plotting DOC grabs and sensor data
  dead %>%
    ggplot(aes(x = dist_cum_m, y = doc_mgL_scan, group = month, color = month)) +
    geom_line() +
    geom_point(aes(y = NPOC_mgCL)) + 
    labs(x = "distance (m)", y = NULL) +
    theme_bw() + theme(panel.grid = element_blank())
  
  