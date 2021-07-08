# 12/14/2020
# Ellie Sovcik
# Updated on 3/8/21 by Dustin Kincaid (calculated catchment-normalized fluxes)

# This script will compile MissDelta nutrient data with Wade and Hungerford data
# The grab sample data will compared to one another based on the season they were collected


# Load packages
library("tidyverse")
library("lubridate")
library("cowplot")
library("patchwork")

# setwd('C:/Users/esovc/ownCloud3/Shared/BREE/Watershed Data/Miss_Delta')
# If you open the R project Miss_delta.Rproj and work on scripts through there, you don't need to set your working directory as you do above

# Read in and tidy data
# CHEMISTRY
# MissDelta
may <- read_csv("Data/alldata_compiled_2019-05-17.csv", 
                col_types = cols(NO3_UM = "d", NH4_UM = "d",PO4_UM = "d", 
                                 NO3_mgNL = "d", NH4_mgNL = "d", PO4_mgPL = "d")) %>%
  mutate(r_timestamp = as.character(r_timestamp))
july <- read_csv("Data/alldata_compiled_2019-07-03.csv", col_types = cols()) %>%
  mutate(r_timestamp = as.character(r_timestamp))
august <- read_csv("Data/alldata_compiled_2019-08-14.csv", 
                   col_types = cols(NO3_UM = "d", NH4_UM = "d",PO4_UM = "d", 
                                    NO3_mgNL = "d", NH4_mgNL = "d", PO4_mgPL = "d")) %>%
  mutate(r_timestamp = mdy_hm(r_timestamp, tz = "America/New_York")) %>% 
  mutate(r_timestamp = as.character(r_timestamp))
october <- read_csv("Data/alldata_compiled_2019-10-21.csv", col_types = cols()) %>%
  mutate(r_timestamp = as.character(r_timestamp))

# Bind data together
chem_md <- bind_rows(may, july, august, october) %>% 
  # Format date if not already done
  mutate(r_timestamp = ymd_hms(r_timestamp, tz = "America/New_York")) %>% 
  # Add a month column & make it a factor instead of number
  mutate(month = as.factor(month(r_timestamp))) %>% 
  # Calculate other concentrations
  mutate(DIN_mgNL = NO3_mgNL + NH4_mgNL,
         DON_mgNL = TDN_mgNL - DIN_mgNL,
         PN_mgNL = TN_mgNL - TDN_mgNL,
         DOP_mgPL = TDP_mgPL - PO4_mgPL,
         PP_mgPL = TP_mgPL - TDP_mgPL)
rm(may, july, august, october)

# Hungerford and Wade grab sample chemistry
wshed_all <- read_csv("Data/2014to2019_waterChem_BREEstreams.csv", col_types = cols()) %>% 
  mutate(timestamp_chem = mdy_hm(timestamp_chem, tz = "America/New_York")) %>%
  rename(r_timestamp = "timestamp_chem",
         NO3_mgNL = "NO3_mgL", 
         NPOC_mgCL = "DOC_mgL",
         NH4_mgNL = "NH4_mgL",
         PO4_mgPL = "SRP_mgL",
         TDN_mgNL = "TDN_mgL",
         TDP_mgPL = "TDP_mgL",
         TP_mgPL = "TP_mgL",
         TN_mgNL = "TN_mgL") %>% 
  # Keep only the important columns
  select(site, r_timestamp, turb_exo2_FNU, SampleID, NPOC_mgCL, NO3_mgNL, NH4_mgNL, PO4_mgPL, TDN_mgNL, TDP_mgPL, TN_mgNL, TP_mgPL) %>% 
  # Calculate other concentrations
  mutate(DIN_mgNL = NO3_mgNL + NH4_mgNL,
         DON_mgNL = TDN_mgNL - DIN_mgNL,
         PN_mgNL = TN_mgNL - TDN_mgNL,
         DOP_mgPL = TDP_mgPL - PO4_mgPL,
         PP_mgPL = TP_mgPL - TDP_mgPL)

wshed_trim <- wshed_all %>% 
  # Let's just keep times surrounding our sampling trips in Miss Delta
  filter((r_timestamp >= ymd_hms("2019-05-05 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-05-30 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-06-26 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-07-16 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-08-07 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-08-21 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-10-14 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-11-07 23:59:59", tz = "America/New_York"))
  )

# DISCHARGE
# Read in Q data from Hungerford & Wade so we can look at where the chem grabs fall on the hydrograph (to avoid using a sample collected during an event)
q_hford_19 <- read_csv("Data/hungerford_2019_best_q.csv", col_types = cols()) %>% mutate(site = "Hungerford") %>% rename(q_cms = HF_best_q) %>% 
  select(-c(X1, hobo_stage, offset, hobo_stage_int, corr_stage)) %>% mutate(r_timestamp = ymd_hms(r_timestamp, tz = "America/New_York"))
q_wade_19 <- read_csv("Data/wade_2019_best_q.csv", col_types = cols()) %>% mutate(site = "Wade") %>% rename(q_cms = best_q) %>% 
  select(site, r_timestamp, q_cms) %>% mutate(r_timestamp = ymd_hms(r_timestamp, tz = "America/New_York"))

# Bind Q data
q_wshed <- bind_rows(q_hford_19, q_wade_19) %>% 
  # Let's just keep times surrounding our sampling trips in Miss Delta
  filter((r_timestamp >= ymd_hms("2019-05-01 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-05-30 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-06-19 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-07-17 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-08-01 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-08-28 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-10-10 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-11-08 23:59:59", tz = "America/New_York")))    
rm(q_hford_19, q_wade_19)    

# Read in Missisquoi River Q data from Swanton USGS site
# And calculate Q daily mean
q_miss <- read_csv("Data/USGS Data/swanton_discharge_data.csv", col_types = cols()) %>% 
  mutate(r_timestamp = mdy_hm(Datetime, tz = "America/New_York")) %>% 
  group_by(date(r_timestamp)) %>% 
  # calculate the Q daily mean for each day in dataset 
  summarize(q_cfs = mean(discharge_cfs, na.rm = TRUE)) %>% 
  # Convert q from cfs to cms
  mutate(q_cms = q_cfs * 0.0283168) %>% 
  select(-q_cfs) %>% 
  rename(r_timestamp = "date(r_timestamp)") %>% 
  # filter out 5/17, 7/3, 8/14, and 10/21
  filter((r_timestamp >= ymd_hms("2019-05-17 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-05-17 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-07-03 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-07-03 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-08-14 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-08-14 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-10-21 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-10-21 23:59:59", tz = "America/New_York")))  



# Join watershed grab data to discharge to look at how the grabs map on to the hydrograph
wshed_plusQ <- full_join(q_wshed, wshed_trim) %>% 
  arrange(site, r_timestamp)

# Plot
wshed_plusQ %>% 
  mutate(month = as.factor(month(r_timestamp))) %>% 
  mutate(q_cms = ifelse(is.na(q_cms), lag(q_cms, n = 1), q_cms)) %>% 
  mutate(NO3_mgNL = ifelse(!is.na(NO3_mgNL), q_cms, NO3_mgNL)) %>% 
  ggplot() +
  facet_grid(site~month, scales = "free") +
  geom_line(aes(x = r_timestamp, y = q_cms)) +
  geom_point(aes(x = r_timestamp, y = NO3_mgNL), color = "red")

# Let's drop the 7/15, 10/17, and 11/16 samples & add a period & date corresponding to MissDelta sampling period and date
wshed_trim <- wshed_trim %>% 
  filter((r_timestamp >= ymd_hms("2019-05-05 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-05-30 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-06-26 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-07-12 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-08-11 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-08-13 23:59:59", tz = "America/New_York")) |
           (r_timestamp >= ymd_hms("2019-10-20 00:00:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-10-23 23:59:59", tz = "America/New_York"))) %>% 
  mutate(period = ifelse(month(r_timestamp) == 5, "May",
                         ifelse(month(r_timestamp) %in% c(6, 7), "July",
                                ifelse(month(r_timestamp) == 8, "August", "October")))) %>%     
  mutate(date_missDel = ifelse(month(r_timestamp) == 5, "2019-05-17",
                               ifelse(month(r_timestamp) %in% c(6, 7), "2019-07-03",
                                      ifelse(month(r_timestamp) == 8, "2019-08-14", "2019-10-21")))) %>% 
  mutate(date_missDel = ymd(date_missDel))

# How different do these concentrations look?
wshed_trim %>% 
  mutate(mon_day = paste(month(r_timestamp), mday(r_timestamp), sep = "_")) %>% 
  pivot_longer(cols = NPOC_mgCL:TP_mgPL, names_to = "var", values_to = "conc") %>% 
  mutate(mon_day = factor(mon_day, levels = c("5_7", "5_22", "6_26", "7_11", "8_12", "8_13", "10_22"))) %>% 
  ggplot(aes(x = mon_day, y = conc, fill = period)) + 
  facet_wrap(site~var, scales = "free") +
  geom_bar(stat = "identity")

# Let's get one concentration for each MissDel sampling date, so take the mean of the two samples where necessary
wshed_mean <-
  wshed_trim %>% 
  group_by(site, period, date_missDel) %>% 
  summarize(across(NPOC_mgCL:PP_mgPL, ~ mean(.x, na.rm = T))) %>% 
  arrange(site, date_missDel) %>% 
  ungroup() %>% 
  rename(date = date_missDel)

# We want to derive a single concentration from 4 locations for MissDelta: Swanton, Top, Fork, Bottom/Lake
md_main <-
  chem_md %>% 
  filter(transect != "Dead Creek") %>% 
  filter(!is.na(NO3_mgNL)) %>% 
  mutate(date = date(r_timestamp))

# Dam/Swanton
dam <- 
  md_main %>% 
  filter(transect == "Dam") %>% 
  mutate(section = "md_dam")

# Top MD
upper <- 
  md_main %>% 
  filter(transect == "Main") %>% 
  group_by(date) %>% 
  slice(1:3) %>% 
  ungroup() %>% 
  mutate(section = "md_upper")

# Fork MD
fork <- 
  md_main %>% 
  filter(transect == "Main") %>% 
  filter(dist_cum_m >= 6000 & dist_cum_m <= 6750) %>% 
  mutate(section = "md_fork")

# Lower/Lake MD
lower <- 
  md_main %>% 
  filter(transect == "Main") %>% 
  group_by(date) %>% 
  do(tail(., 2)) %>% 
  ungroup() %>% 
  mutate(section = "md_lower")

# Bind these together and take the mean for each section
md_mean <- 
  bind_rows(dam, upper, fork, lower) %>% 
  group_by(date, section) %>% 
  summarize(across(c(NO3_UM:Fe_ppb_0.45, DIN_mgNL:PP_mgPL), ~ mean(.x, na.rm = T))) %>% 
  rename(site = section) %>% 
  mutate(period = ifelse(month(date) == 5, "May",
                         ifelse(month(date) == 7, "July",
                                ifelse(month(date) == 8, "August", "October"))))
rm(md_main, dam, upper, fork, lower)

# Bind the Wade and Hford concentrations with MD concentrations
allData <- 
  bind_rows(wshed_mean, md_mean) %>% 
  mutate(site = ordered(site, levels = c("Wade", "Hungerford", "md_dam", "md_upper", "md_fork", "md_lower"))) %>% 
  mutate(period = ordered(period, levels = c("May", "July", "August", "October"))) %>% 
  arrange(site, date) %>% 
  mutate(catchment = ifelse(site == "Wade", "Wade",
                            ifelse(site == "Hungerford", "Hungerford", "Missisquoi"))) %>% 
  select(site, catchment, everything())

rm(wshed_mean, md_mean)

# Calculate flow-normalized concentrations (conc * discharge) 
# Add watershed areas
allData <- 
  allData %>% 
  mutate(catch_area_km2 = ifelse(site == "Hungerford", 43.8,
                                 ifelse(site == "Wade", 16.7, 2250.7))) %>% 
  select(site, catchment, catch_area_km2, date, period, everything()) %>% 
  # Drop concentrations in umol/L for now
  select(-c(ends_with("_UM")))

# Add discharge data
# To add Wade and Hford discharge (q), we first need to summarize the 15-min q time series as mean daily values
q_daily <-
  q_wshed %>% 
  mutate(date = date(r_timestamp)) %>% 
  group_by(site, date) %>% 
  summarize(q_cms = mean(q_cms, na.rm = TRUE)) %>% 
  ungroup()

# Join the two Q dfs together
q_all <-
  bind_rows(q_daily %>% 
              mutate(catchment = site) %>% 
              select(-site), 
            q_miss %>%
              mutate(catchment = "Missisquoi") %>% 
              rename(date = r_timestamp))

# Join q_all to allData & calculate catchment-area normalized flux
allData <-
  allData %>% 
  # Add Wade and Hford Q
  left_join(., q_all, by = c("catchment", "date")) %>% 
  select(site:period, q_cms, everything()) %>% 
  # Convert Q from m3/s to L/s
  mutate(q_Ls = q_cms * 1000) %>% 
  # Calculate instantaneous flux
  mutate_at(vars(c(NPOC_mgCL:Fe_ppb_0.45)),
            .funs = list("massPerS" = ~ . * q_Ls)) %>% 
  # Drop the q_Ls that we no longer need
  select(-q_Ls) %>% 
  # Rename the new variables correctly
  rename_at(vars(c(NPOC_mgCL_massPerS:tss_mgL_massPerS)),
            .funs = list(~paste(word(., 1, sep = "\\_"), "mgS", sep = "_"))) %>% 
  rename_at(vars(c(Al_ppb_0.45_massPerS:Fe_ppb_0.45_massPerS)),
            .funs = list(~paste(word(., 1, sep = "\\_"), "ugS", sep = "_"))) %>% 
  # Divide the instantaneous flux by catchment area to get catchment-area normalized flux
  mutate_at(vars(c(NPOC_mgS:Fe_ugS)),
            .funs = list("km2" = ~ . / catch_area_km2)) %>% 
  # Drop the instantaneous flux columns
  select(-c(ends_with("mgS"), ends_with("ugS")))

#----Abbot Clacs----
  
#----Subcatchment Leverage----


# Prep subcatchment & outlet data for Abbott calcs 
#----md_lower----
  temp <-
    allData %>% 
    mutate(q_sp = q_cms/catch_area_km2) %>% 
    pivot_longer(cols = (NPOC_mgCL:PP_mgPL), names_to = "var", values_to = "conc") %>% 
    select(-c(tss_mgL:Fe_ugS_km2)) %>% 
    select(-c(date, q_cms, catchment)) %>% 
    rename(subcatch = site, area_s = catch_area_km2, q_sp_s = q_sp, conc_s = conc)
  
  subcatch <-
    temp %>% 
    filter(subcatch != "md_lower")

#outflow = md_lower
  outlet <-
    temp %>% 
    filter(subcatch == "md_lower") %>% 
    rename(outlet = subcatch, area_o = area_s, q_sp_o = q_sp_s, conc_o = conc_s)
  
#Leverage
  leverage <-
    full_join(subcatch, outlet, by = c("period", "var")) %>% 
# cs_co = concentration subcatch-concentration outflow
#as_ao = area subcatch / area outflof
#qs_qo = specific discharge subcatch / specific discharge outflow
    mutate(cs_co = conc_s-conc_o) %>% 
    mutate(as_ao=area_s/area_o) %>% 
    mutate(qs_qo=q_sp_s/q_sp_o) %>% 
#sublev - subcatchment leverage calc. 
    mutate(sublev= cs_co*as_ao*qs_qo)
  
#Specific discharge comparison to evaluate values 
  q<-
    leverage %>% 
    select(period,subcatch,q_sp_s,outlet,q_sp_o) 
  
# Subc. Leverage Plots w/ md_lower as outflow
  
      # Color palettes
      cbp <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      plt<-c("#F0E442","#E69F00","#CC79A7","#56B4E9","#009E73")
      

#Subcatchmemnt leverage for N
      
      Nlev<-
      leverage %>%
      #Extract N sublev values
      select(period,var,subcatch,area_s,outlet,area_o,sublev) %>%
      filter(var %in% c("DIN_mgNL","DON_mgNL","NH4_mgNL","NO3_mgNL","PN_mgNL","TDN_mgNL","TN_mgNL"))
     
      #Plot
      Nlev %>% 
        filter(var %in% c("TN_mgNL", "PN_mgNL", "TDN_mgNL", "DON_mgNL","DIN_mgNL", "NO3_mgNL", "NH4_mgNL")) %>% 
        mutate(var=factor(var,levels = c("TN_mgNL", "PN_mgNL", "TDN_mgNL", "DON_mgNL","DIN_mgNL", "NO3_mgNL", "NH4_mgNL"))) %>%
        ggplot(aes(x=subcatch,y=sublev,fill=period))+
        facet_wrap(~var, ncol = 1)+
        geom_bar(stat = "identity",position = "dodge")+
        #scale_y_continuos(limits=c(,0.2))+
        ylim(0,0.2)+
        labs(tag= "outflow = md_lower",x="Site" ,y="Subcatchmemnt Leverage" )+
        theme_bw()

    
#Subcatchmemnt leverage for P
      
      Plev<-
        leverage %>%
        #Extract P sublev values
        select(period,var,subcatch,area_s,outlet,area_o,sublev) %>% 
        filter(var %in% c("DOP_mgPL","PO4_mgPL","PP_mgPL","TP_mgPL","TDP_mgPL"))
      
        #Plot
        Plev %>% 
        filter(var %in% c("DOP_mgPL","PO4_mgPL","PP_mgPL")) %>%
        mutate(var=factor(var,levels = c("DOP_mgPL","PO4_mgPL","PP_mgPL"))) %>% 
        ggplot(aes(x=subcatch,y=sublev,fill=period))+
        facet_wrap(~var, ncol = 1, scales = "fixed")+
        geom_bar(stat = "identity",position = "dodge")+
        ylim(-0.006,0.004)+
        labs(tag= "outflow = md_lower",x="Site" ,y="Subcatchmemnt Leverage" )+
        theme_bw() 
        
#Subcatchmemnt leverage for DOC
        Clev<-
        leverage %>%
          #Extract DOC sublev values
          select(period,var,subcatch,area_s,outlet,area_o,sublev) %>% 
          filter(var == "NPOC_mgCL")
        
        #Plot
        Clev %>% 
          mutate(var=factor(var,levels = "DOC_mgCL")) %>% 
          ggplot(aes(x=subcatch,y=sublev,fill=period))+
          geom_bar(stat = "identity",position = "dodge")+
          ylim(-0.08,0.08)+
          labs(tag= "outflow = md_lower",caption = "DOC",x="Site" ,y="Subcatchmemnt Leverage" )+
          theme_bw() 


        
#----md_fork----     
        
  #Prep subcatchment & outlet data for Abbott calcs
        temp <-
          allData %>% 
          mutate(q_sp = q_cms/catch_area_km2) %>% 
          pivot_longer(cols = (NPOC_mgCL:PP_mgPL), names_to = "var", values_to = "conc") %>% 
          select(-c(tss_mgL:Fe_ugS_km2)) %>% 
          select(-c(date, q_cms, catchment)) %>% 
          rename(subcatch = site, area_s = catch_area_km2, q_sp_s = q_sp, conc_s = conc)
        
        subcatch <-
          temp%>%
          filter(subcatch != "md_fork")
        #outflow = md_fork
        outlet <-
          temp %>%  
          filter(subcatch == "md_fork") %>% 
          rename(outlet = subcatch, area_o = area_s, q_sp_o = q_sp_s, conc_o = conc_s) 
        
        #Leverage
        forklev<-
          full_join(subcatch, outlet, by = c("period", "var")) %>% 
          # cs_co = concentration subcatch-concentration outflow
          #as_ao = area subcatch / area outflof
          #qs_qo = specific discharge subcatch / specific discharge outflow
          mutate(cs_co = conc_s-conc_o) %>% 
          mutate(as_ao=area_s/area_o) %>% 
          mutate(qs_qo=q_sp_s/q_sp_o) %>% 
          #sublev - subcatchment leverage calc. 
          mutate(sublev= cs_co*as_ao*qs_qo)
        
#Subcatchmemnt leverage for N
        
        Nlev<-
          forklev%>%
          #Extract N sublev values
          select(period,var,subcatch,area_s,outlet,area_o,sublev) %>%
          filter(var %in% c("DIN_mgNL","DON_mgNL","NH4_mgNL","NO3_mgNL","PN_mgNL","TDN_mgNL","TN_mgNL"))
        
        #Plot
        Nlev %>% 
          filter(var %in% c("TN_mgNL", "PN_mgNL", "TDN_mgNL", "DON_mgNL","DIN_mgNL", "NO3_mgNL", "NH4_mgNL")) %>% 
          filter(!subcatch =="md_lower") %>% 
          mutate(var=factor(var,levels = c("TN_mgNL", "PN_mgNL", "TDN_mgNL", "DON_mgNL","DIN_mgNL", "NO3_mgNL", "NH4_mgNL"))) %>%
          ggplot(aes(x=subcatch,y=sublev,fill=period))+
          facet_wrap(~var, ncol = 1, scales = "fixed")+
          geom_bar(stat = "identity",position = "dodge")+
          ylim(0,0.2)+
          labs(tag= "outflow = md_fork",x="Site" ,y="Subcatchmemnt Leverage" )+
          theme_bw()
        
        
#Subcatchmemnt leverage for P
        
        Plev<-
          forklev %>%
          #Extract P sublev values
          select(period,var,subcatch,area_s,outlet,area_o,sublev) %>% 
          filter(var %in% c("DOP_mgPL","PO4_mgPL","PP_mgPL","TP_mgPL","TDP_mgPL"))
        
        #Plot
        Plev %>% 
          filter(var %in% c("DOP_mgPL","PO4_mgPL","PP_mgPL")) %>%
          filter(!subcatch =="md_lower") %>%
          mutate(var=factor(var,levels = c("DOP_mgPL","PO4_mgPL","PP_mgPL"))) %>% 
          ggplot(aes(x=subcatch,y=sublev,fill=period))+
          facet_wrap(~var, ncol = 1, scales = "fixed")+
          geom_bar(stat = "identity",position = "dodge")+
          ylim(-0.006,0.004)+
          labs(tag= "outflow = md_fork",x="Site" ,y="Subcatchmemnt Leverage" )+
          theme_bw() 
        
#Subcatchmemnt leverage for DOC
        Clev<-
          forklev %>%
          #Extract DOC sublev values
          select(period,var,subcatch,area_s,outlet,area_o,sublev) %>% 
          filter(var == "NPOC_mgCL")
        
        #Plot
        Clev %>% 
          mutate(var=factor(var,levels = "DOC_mgCL")) %>% 
          filter(!subcatch =="md_lower") %>%
          ggplot(aes(x=subcatch,y=sublev,fill=period))+
          geom_bar(stat = "identity",position = "dodge")+
          ylim(-0.08,0.08)+
          labs(tag= "outflow = md_fork",caption = "DOC",x="Site" ,y="Subcatchmemnt Leverage" )+
          theme_bw() 
        
# Subcatchment synchrony ----
# Calculate the mean concentrations for each variable at each watershed (Wade, Hungerford, and the Miss Delta sampling points)
conc_means <-
  leverage %>% 
  group_by(subcatch, var) %>% 
  mutate(conc_s_mean = mean(conc_s, na.rm = TRUE),
            conc_o_mean = mean(conc_o, na.rm = TRUE)) %>% 
          
  mutate(x_xbar=conc_s-conc_s_mean) %>% #Subtract between x and x bar
  mutate(y_ybar=conc_o-conc_o_mean) %>% #Subtract between y and y bar
  mutate(x_y= x_xbar *y_ybar) %>% #Product of the subtractions
 #Separate arguments needed to calculate the subcatchment synchrony
  select("subcatch","period","var","conc_s","conc_o","x_xbar","outlet","y_ybar","x_y")
                
# Join the means to the leverage dataframe with the raw data
#temporal_synch <-
  #full_join(leverage, conc_means, by = c("subcatch", "var"))

  n_1<-3       
        
        Wadesynch<-
          conc_means %>% 
          filter(subcatch=="Wade") %>% 
          mutate(sum=c(sum(x_y[var == "DIN_mgNL"],sum(x_y[var == "DON_mgNL"],
                 sum(x_y[var == "DOP_mgPL"] ))))) %>%
          mutate(synch= sum/n_1)
          

            
          
          
          
          
          
          
        
    
    
  
        

        
        
        
        
      
    
      
      
      
      
      
      
     
      
    
    
      
  
      
    
    
      
      
   
    
    


    
      
  
  
