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

# Read in and tidy data----
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
    
    
    # Add in other variables measured with YSI at Wade and Hungerford (e.g., water temp)
    wshed_ysi <- read_csv("Data/allStreamData_2019_compiled_2021-06-25.csv", col_types = cols()) %>% 
      mutate(r_timestamp = ymd_hms(timestamp, tz = "America/New_York")) %>% 
      # rename(temp_c = temp, turb_exo2_FNU = turb, q_cms = q)
      rename(temp_c = temp,q_cms = q) %>% 
      select(-timestamp) %>% 
    # The YSI was often not logging when grab sample was taken so filter these rows out so
    # that the join to grab chem below works
      filter(!is.na(temp_c))
    
    # Calculate daily means
    wshed_ysi_daily <-
      wshed_ysi %>% 
      select(site, r_timestamp, everything()) %>% 
      group_by(site, date(r_timestamp)) %>% 
      summarize(across(q_cms:turb, ~ mean(.x, na.rm = T))) %>% 
      rename(date_wshed = `date(r_timestamp)`) %>% 
      ungroup()
    rm(wshed_ysi)
    
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
      
    
# Join dataframes ----
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
  rm(wshed_plusQ)
  
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
    mutate(date_missDel = ymd(date_missDel),
           date_wshed = date(r_timestamp)) %>% 
    # Drop the turb data that was with the grab sample chem; not sure where these values came from; use the turb from wshed_ysi
    select(-turb_exo2_FNU)
  
  # Add the other YSI variables from Wade and Hungerford
  wshed_trim <-
    left_join(wshed_trim, wshed_ysi_daily) %>% 
    rename(turb_exo2_FNU = turb)
  
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
    select(site, r_timestamp, date_missDel, period, q_cms:turb_exo2_FNU, NPOC_mgCL:PP_mgPL) %>%
    group_by(site, period, date_missDel) %>% 
    summarize(across(q_cms:PP_mgPL, ~ mean(.x, na.rm = T))) %>% 
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
    summarize(across(c(temp_c:Fe_ppb_0.45, DIN_mgNL:PP_mgPL), ~ mean(.x, na.rm = T))) %>% 
    rename(site = section) %>% 
    mutate(period = ifelse(month(date) == 5, "May",
                           ifelse(month(date) == 7, "July",
                                  ifelse(month(date) == 8, "August", "October")))) %>% 
    rename(DO_mgL = do_mgl, DO_sat = do_per, spCond = spc_uscm, pH = ph, fDOM = fdom_qsu) %>% 
    select(-c(scan_NO3_mgNL, scan_DOC_mgCL))
  rm(md_main, dam, upper, fork, lower)
  
# Bind the Wade and Hford concentrations with MD concentrations
  allData <- 
    bind_rows(wshed_mean, md_mean) %>% 
    mutate(site = ordered(site, levels = c("Wade", "Hungerford", "md_dam", "md_upper", "md_fork", "md_lower"))) %>% 
    mutate(period = ordered(period, levels = c("May", "July", "August", "October"))) %>% 
    arrange(site, date) %>% 
    mutate(catchment = ifelse(site == "Wade", "Wade",
                              ifelse(site == "Hungerford", "Hungerford", "Missisquoi"))) %>% 
    select(site, catchment, everything()) %>% 
    # Drop q_cms for now and add it below
    select(-q_cms)
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
  rm(q_daily, q_wshed, q_miss)
  
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
    select(-c(ends_with("mgS"), ends_with("ugS"))) %>% 
    # Calculate molar concentration, specifically micromoles (uM)
    mutate_at(vars(c(ends_with("mgNL"))),
              .funs = list("uM" = ~ . *1000/14.007)) %>% 
    mutate_at(vars(c(ends_with("mgPL"))),
              .funs = list("uM" = ~ . *1000/30.974)) %>% 
    # Now let's shorten those long columns names we just created
    rename_at(vars(c(ends_with("uM"))),
              .funs = list(~ paste(sub("\\_.*", "", .), "uM", sep = "_")))
  
  
  # ---- this is where Dustin stopped on 12/29/20 ----  
  
# ---- Ellie's graphs ----
  
  # graphing N concentrations for all seasons
  allData %>%
    # gather all nutrient data into long format
    pivot_longer(cols = c(NO3_mgNL, NH4_mgNL, TDN_mgNL, TN_mgNL, DON_mgNL, PN_mgNL), names_to = "var", values_to = "conc") %>% 
    # re-order the levels of our new column called var
    mutate(var = ordered(var, levels = c("NO3_mgNL", "NH4_mgNL", "TDN_mgNL", "TN_mgNL", "DON_mgNL", "PN_mgNL"))) %>% 
    ggplot(aes(x = var, y = conc, fill = site)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(position = "dodge", stat = 'identity')
  
    # stacked bar graph for N concentrations for each site, separated by month
    allData %>% 
      # gather all nutrient data into long format
      pivot_longer(cols = c(NO3_mgNL, NH4_mgNL, DON_mgNL, PN_mgNL), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("NO3_mgNL", "NH4_mgNL", "DON_mgNL", "PN_mgNL"))) %>%
     # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
      ggplot(aes(x = site, y = conc, fill = var)) +
        facet_wrap(~period, ncol = 1, scales = "free_y") +
      geom_bar(position = "stack", stat = 'identity')
  
  # graphing flow normalized concentrations for TDN, TN, TDP, and TP 
  allData %>%
    # gather all nutrient data into long format
    pivot_longer(cols = c(TDN_mgS_km2, TN_mgS_km2), names_to = "var", values_to = "conc") %>% 
    # re-order the levels of our new column called var
    mutate(var = ordered(var, levels = c("TDN_mgS_km2", "TN_mgS_km2"))) %>% 
    ggplot(aes(x = var, y = conc, fill = site)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(position = "dodge", stat = 'identity')
  
  
  # graphing flow normalized concentrations for N species for N03, NH4, DON, and PN
  allData %>%
    # gather all nutrient data into long format
    pivot_longer(cols = c(NO3_mgS_km2, NH4_mgS_km2, DON_mgS_km2, PN_mgS_km2), names_to = "var", values_to = "conc") %>% 
    # re-order the levels of our new column called var
    mutate(var = ordered(var, levels = c("NO3_mgS_km2", "NH4_mgS_km2", "DON_mgS_km2", "PN_mgS_km2"))) %>% 
    ggplot(aes(x = var, y = conc, fill = site)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(position = "dodge", stat = 'identity')
  
  # stacked bar graph for N concentrations for each site, separated by month--flow normalized concentrations
  allData %>% 
    # gather all nutrient data into long format
    pivot_longer(cols = c(NO3_mgS_km2, NH4_mgS_km2, DON_mgS_km2, PN_mgS_km2), names_to = "var", values_to = "conc") %>% 
    # re-order the levels of our new column called var
    mutate(var = ordered(var, levels = c("NO3_mgS_km2", "NH4_mgS_km2", "DON_mgS_km2", "PN_mgS_km2"))) %>%
    # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
    ggplot(aes(x = site, y = conc, fill = var)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(position = "stack", stat = "identity")

  # stacked bar graph for N concentrations for each site, separated by month--flow normalized concentrations
  # WITHOUT HUNGERFORD
  allData %>% 
    # gather all nutrient data into long format
    pivot_longer(cols = c(NO3_mgS_km2, NH4_mgS_km2, DON_mgS_km2, PN_mgS_km2), names_to = "var", values_to = "conc") %>% 
    # re-order the levels of our new column called var
    mutate(var = ordered(var, levels = c("NO3_mgS_km2", "NH4_mgS_km2", "DON_mgS_km2", "PN_mgS_km2"))) %>%
    # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
    #omit Hungerford from x axis
    filter(site %in% c("Wade", "md_dam", "md_fork", "md_upper", "md_lower")) %>% 
    ggplot(aes(x = site, y = conc, fill = var)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(position = "stack", stat = "identity")
  
  # graphing flow normalized concentrations for TDP and TP 
  allData %>%
    # gather all nutrient data into long format
    pivot_longer(cols = c(TDP_mgS_km2, TP_mgS_km2), names_to = "var", values_to = "conc") %>% 
    # re-order the levels of our new column called var
    mutate(var = ordered(var, levels = c("TDP_mgS_km2", "TP_mgS_km2"))) %>% 
    ggplot(aes(x = var, y = conc, fill = site)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(position = "dodge", stat = 'identity')
  
  # stacked bar graph for P concentrations for each site, separated by month--regular concentrations
  allData %>% 
    # gather all nutrient data into long format
    pivot_longer(cols = c(PO4_mgPL, DOP_mgPL, PP_mgPL), names_to = "var", values_to = "conc") %>% 
    # re-order the levels of our new column called var
    mutate(var = ordered(var, levels = c("PO4_mgPL", "DOP_mgPL", "PP_mgPL"))) %>%
    # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
    ggplot(aes(x = site, y = conc, fill = var)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(position = "stack", stat = "identity")
  
  # stacked bar graph for P concentrations for each site, separated by month--flow normalized concentrations
  allData %>% 
    # gather all nutrient data into long format
    pivot_longer(cols = c(PO4_mgS_km2, DOP_mgS_km2, PP_mgS_km2), names_to = "var", values_to = "conc") %>% 
    # re-order the levels of our new column called var
    mutate(var = ordered(var, levels = c("PO4_mgS_km2", "DOP_mgS_km2", "PP_mgS_km2"))) %>%
    # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
    ggplot(aes(x = site, y = conc, fill = var)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(position = "stack", stat = "identity")
  
  # stacked bar graph for P concentrations for each site, separated by month--flow normalized concentrations
  #WITHOUT HUNGERFORD
  allData %>% 
    # gather all nutrient data into long format
    pivot_longer(cols = c(PO4_mgS_km2, DOP_mgS_km2, PP_mgS_km2), names_to = "var", values_to = "conc") %>% 
    # re-order the levels of our new column called var
    mutate(var = ordered(var, levels = c("PO4_mgS_km2", "DOP_mgS_km2", "PP_mgS_km2"))) %>%
    # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
    filter(site %in% c("Wade", "md_dam", "md_fork", "md_upper", "md_lower")) %>% 
    ggplot(aes(x = site, y = conc, fill = var)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(position = "stack", stat = "identity")
  
  # graphing NPOC concentrations regular concentrattions 
  allData %>% 
    # gather all nutrient data into long format
    pivot_longer(cols = NPOC_mgCL, names_to = "var", values_to = "NPOC_mgCL") %>% 
    # filter out NA values
    filter(!is.na("NPOC_mgCL")) %>% 
    ggplot(aes(x = site, y = NPOC_mgCL, fill = var)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(stat = "identity") +
    ylab(expression(DOC~(mg~C~L^{-1})))
  
  # graphing NPOC concentrations for each site, separated by month--flow normalized concentrations
  allData %>% 
    # gather all nutrient data into long format
    pivot_longer(cols = NPOC_mgS_km2, names_to = "var", values_to = "conc") %>% 
    # filter out NA values
    filter(!is.na("NPOC_mgS_km2")) %>% 
    ggplot(aes(x = site, y = conc, fill = var)) +
    facet_wrap(~period, ncol = 1, scales = "free_y") +
    geom_bar(stat = "identity") +
    ylab(expression(DOC~(mg~C~L^{-1})))
  
  
#----Igrena's ----
#summer2021

----#Norm & Prop plots----  
#Normalized Concentration and Proportion Graphs
  
  #Nitrogen 
  
    #Nitrogen normalized concentrations (not stacked)
    allData %>%
      # gather all nutrient data into long format
      pivot_longer(cols = c(TN_mgS_km2, PN_mgS_km2, TDN_mgS_km2, DON_mgS_km2, NH4_mgS_km2, NO3_mgS_km2), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("TN_mgS_km2", "PN_mgS_km2", "TDN_mgS_km2", "DON_mgS_km2", "NH4_mgS_km2", "NO3_mgS_km2"))) %>% 
      #Plot
      ggplot(aes(x = var, y = conc, fill = site)) +
      facet_wrap(~period, ncol = 1, scales = "fixed") +
      geom_bar(position = "dodge", stat = 'identity')+
      labs(x="Variables",y="Instantaneous Yield (mgN/km2)")+
      theme_bw()
    
    #Nitrogen normalized concentrations (stacked)
  Nnorm<-
  allData %>% 
    # gather all nutrient data into long format
    pivot_longer(cols = c(NO3_mgS_km2, NH4_mgS_km2, DON_mgS_km2, PN_mgS_km2), names_to = "var", values_to = "conc") %>% 
    # re-order the levels of our new column called var
    mutate(var = ordered(var, levels = c("PN_mgS_km2", "DON_mgS_km2", "NH4_mgS_km2", "NO3_mgS_km2"))) %>%
    # Replace all negative concentrations with 0
    mutate(conc= ifelse(conc < 0, 0, conc)) %>% 
    #Plot
    ggplot(aes(x = site, y = conc, fill = var)) +
    facet_wrap(~period, ncol = 2, scales = "fixed") +
    geom_bar(position = "stack",stat = 'identity')+
    ylab(expression(Instantaneous~Yield~(mg~N~km^{-2})))+
    xlab("Site")+
    theme_bw()+
    #Add line and text to divide sites
    geom_vline(xintercept = 3.5, size=1.5)+
    geom_text(aes(x=2, y=125, label= "Headwater Catchments"),size=5, fontface = "bold",stat = "unique")+
    geom_text(aes(x=5, y=125,label= "Wetland Complex"),size=5, fontface = "bold",stat = "unique")+
    #Sites used to commppared %
    #Make square
    geom_rect(aes(xmin=2.5, xmax=6.5, ymin=0, ymax=50), alpha = 0.01)+
    #Fill colors and arrange axis
    scale_fill_manual(values=c("#3a0ca3","#118ab2","#06d6a0","#bee9e8"), labels=c("PN","DON","NH4","NO3"))+
    theme(legend.title=element_blank(),legend.position = "bottom", axis.text.x = element_text(angle = 90))
  
    
    # Dustin added this figure on 7/15/21
    # Let's look at how each N species (e.g., NO3, NH4, DON, & PN) contributes proportionally to the TN pool
    #Nitrogen normalized concentrations (proportions, stacked)
  Nprop<-
  allData %>% 
    # Let's replace all negative PN concentrations with 0
    mutate(PN_mgNL = ifelse(PN_mgNL < 0, 0, PN_mgNL)) %>% 
    # In theory, when we add NO3, NH4, DON, & PN concentrations, they should equal the TN concentration
    # However, because we measure NO3, NH4 and TN separately and there is some amount of error in the analytical process
    # they won't always equal the TN concentration we measured
    # So if we want to look at how each N species contributes to the TN pool, we should calculate a new TN conc using those species
    mutate(totalN = NO3_mgNL + NH4_mgNL + DON_mgNL + PN_mgNL,
           NO3_prop = NO3_mgNL/totalN,
           NH4_prop = NH4_mgNL/totalN,
           DON_prop = DON_mgNL/totalN,
           PN_prop = PN_mgNL/totalN) %>% 
    # Pivot all proportions into long format
    pivot_longer(cols = c(NO3_prop, NH4_prop, DON_prop, PN_prop), names_to = "var", values_to = "prop") %>% 
    # Re-order the levels of our new column called va
    mutate(var = ordered(var, levels = c("PN_prop", "DON_prop", "NH4_prop", "NO3_prop"))) %>%
    # Plot
    ggplot(aes(x = site, y = prop, fill = var)) +
    facet_wrap(~period, ncol = 2, scales = "fixed") +
    geom_bar(position = "stack", stat = 'identity')+
    labs(x="Site", y="Proportion of TN")+
    scale_fill_manual(values=c("#3a0ca3","#118ab2","#06d6a0","#bee9e8"),
                      labels=c("PN","DON","NH4","NO3"))+
    theme_bw()+
    theme(legend.title=element_blank(), axis.text.x = element_text(angle = 90))
      
  
    #Nitrogen normalized concentrations by month (stacked) without Hungerford
    allData %>% 
      # gather all nutrient data into long format
      pivot_longer(cols = c(NO3_mgS_km2, NH4_mgS_km2, DON_mgS_km2, PN_mgS_km2), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("PN_mgS_km2", "DON_mgS_km2", "NH4_mgS_km2", "NO3_mgS_km2"))) %>%
      # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
      #omit Hungerford from x axis
      filter(site %in% c("Wade", "md_dam", "md_fork", "md_upper", "md_lower")) %>% 
      #Plot
      ggplot(aes(x = site, y = conc, fill = var)) +
      facet_wrap(~period, ncol = 1, scales = "fixed") +
      geom_bar(position = "stack", stat = 'identity')+
      labs(x="Site",y="Instantaneous Yield (mgN/km2)")+
      scale_fill_manual(values=c("#515E63","#282846","#007580","#57837B"))+
      theme_bw()

#Phosphorus 
    
    #Phosphorus normalized concentrations (not stacked)
    allData %>%
      # gather all nutrient data into long format
      pivot_longer(cols = c(DOP_mgS_km2, PO4_mgS_km2,PP_mgS_km2,TDP_mgS_km2,TP_mgS_km2), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("TP_mgS_km2", "PP_mgS_km2","TDP_mgS_km2","DOP_mgS_km2","PO4_mgS_km2"))) %>% 
      #Plot
      ggplot(aes(x = var, y = conc, fill = site)) +
      facet_wrap(~period, ncol = 1, scales = "fixed") +
      geom_bar(position = "dodge", stat = 'identity')+
      labs(x="Variables",y="Instantaneous Yield (mgP/km2)")
      theme_bw()
    
    #Phosphorus normalized concentrations (stacked)
      Pnorm<-
      allData %>% 
        # gather all nutrient data into long format
        pivot_longer(cols = c(DOP_mgS_km2, PO4_mgS_km2,PP_mgS_km2), names_to = "var", values_to = "conc") %>% 
        # re-order the levels of our new column called var
        mutate(var = ordered(var, levels = c("PP_mgS_km2","DOP_mgS_km2","PO4_mgS_km2"))) %>%
        # Replace all negative concentrations with 0
        mutate(conc= ifelse(conc < 0, 0, conc)) %>% 
        #Plot
        ggplot(aes(x = site, y = conc, fill = var)) +
        facet_wrap(~period, ncol = 2, scales = "fixed") +
        geom_bar(position = "stack", stat = 'identity')+
        #labs(x="Site",y="Instantaneous Yield (mgP/km2)")+
        ylab(expression(Instantaneous~Yield~(mg~P~km^{-2})))+
        xlab("Site")+
        theme_bw()+
        #Add line and text to divide sites
        geom_vline(xintercept = 3.5, size=1.5)+
        geom_text(aes(x=2, y=1.5, label= "Headwater Catchments"),size=5, fontface = "bold",stat = "unique")+
        geom_text(aes(x=5, y=1.5,label= "Wetland Complex"),size=5, fontface = "bold",stat = "unique")+
        #Sites used to commppared %
        #Make square
        geom_rect(aes(xmin=2.5, xmax=6.5, ymin=0, ymax=0.95), alpha = 0.01)+
        #Fill colors and arrange axis
        scale_fill_manual(values=c("#3a0ca3","#118ab2","#06d6a0","#bee9e8"),labels=c("PP","DOP","PO4"))+
        theme(legend.title=element_blank(),legend.position = "bottom", axis.text.x = element_text(angle = 90))
      
    
    #Phosphorus normalized concentrations (proportions, stacked)
      Pprop<-
      allData %>% 
        # Let's replace all negative PN concentrations with 0
        mutate(PP_mgPL = ifelse(PP_mgPL < 0, 0, PP_mgPL),
               DOP_mgPL = ifelse(DOP_mgPL < 0, 0, DOP_mgPL)) %>% 
        # In theory, when we add NO3, NH4, DON, & PN concentrations, they should equal the TN concentration
        # However, because we measure NO3, NH4 and TN separately and there is some amount of error in the analytical process
        # they won't always equal the TN concentration we measured
        # So if we want to look at how each N species contributes to the TN pool, we should calculate a new TN conc using those species
        mutate(totalP = PO4_mgPL + DOP_mgPL + PP_mgPL,
               PO4_prop= PO4_mgPL/totalP,
               DOP_prop = DOP_mgPL/totalP,
               PP_prop = PP_mgPL/totalP) %>% 
        # Pivot all proportions into long format
        pivot_longer(cols = c(PO4_prop,DOP_prop, PP_prop), names_to = "var", values_to = "prop") %>% 
        # Re-order the levels of our new column called va
        mutate(var = ordered(var, levels = c("PP_prop", "DOP_prop", "PO4_prop"))) %>%
        # Plot
        ggplot(aes(x = site, y = prop, fill = var)) +
        facet_wrap(~period, ncol = 2, scales = "fixed") +
        geom_bar(position = "stack", stat = 'identity')+
        labs(x="Site", y="Proportion of TP")+
        scale_fill_manual(values=c("#3a0ca3","#118ab2","#06d6a0","#bee9e8"),
                          labels=c("PP","DOP","PO4"))+
        theme_bw()+
        theme(legend.title=element_blank(), axis.text.x = element_text(angle = 90))
  
    #Phosphorus normalized concentrations by month (stacked) without Hungerford
    allData %>% 
      # gather all nutrient data into long format
      pivot_longer(cols = c(DOP_mgS_km2, PO4_mgS_km2,PP_mgS_km2), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("PP_mgS_km2","DOP_mgS_km2","PO4_mgS_km2"))) %>%
      # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
      #omit Hungerford from x axis
      filter(site %in% c("Wade", "md_dam", "md_fork", "md_upper", "md_lower")) %>% 
      #Plot
      ggplot(aes(x = site, y = conc, fill = var)) +
      facet_wrap(~period, ncol = 2, scales = "fixed") +
      geom_bar(position = "stack", stat = 'identity')+
      labs(x="Site",y="Instantaneous Yield (mg P/km2)")+
      scale_fill_manual(values=c("#515E63","#282846","#007580","#57837B"))+
      theme_bw()

    
#Discharge and Stream Temperature----
    #discharge 
    #Plot
    q_plot<-
    allData %>% 
    ggplot(aes(x = site, y = q_cms)) +
      facet_wrap(~period, ncol = 2, scales = "fixed") +
      geom_bar(position = "stack", stat = 'identity',fill="#5B9433")+
      ylab(expression(Discharge~(cm^{3})))+
      xlab("Site")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.title.x=element_blank())
    
    #temperature
    #Plot
    temp_c<-
    allData %>% 
      ggplot(aes(x = site, y = temp_c,fill=temp_c)) +
      facet_wrap(~period, ncol = 2, scales = "fixed") +
      geom_bar(position = "stack", stat = 'identity',fill="#234B34")+
      ylab(expression(Temperature))+
      xlab("Site")+
      theme_bw()+
      theme(axis.text.x=element_blank(),
            axis.title.x=element_blank())
    
#Combine Nnorm/Nprop, Pnorm/Pprop, q_cms and, temp_c
    (q_plot+temp_c)/(Nnorm+Nprop)/(Pnorm+Pprop)
    #Save plot
    ggsave(filename = "plots_Igrena/Plot_NPcombined.pdf",width=8,height = 7,units="in",dpi = 150)

    
#Carbon (NPOC)
    
    #NPOC normalized concentrations by month
    allData %>% 
      # gather all nutrient data into long format
      pivot_longer(cols = NPOC_mgS_km2, names_to = "var", values_to = "conc") %>% 
      # filter out NA values
      filter(!is.na("NPOC_mgS_km2")) %>% 
      #Plot
      ggplot(aes(x = site, y = conc, fill = var)) +
      facet_wrap(~period, ncol = 1, scales = "fixed") +
      geom_bar(stat = "identity") +
      scale_fill_manual(values="#087E8B")+
      #labs(x="Site",y="Instantaneous Yield")+
      ylab(expression(DOC~(mg~C~L^{-1})))+
      xlab("Site")+
      theme_bw()
    
----#Relative Difference#----
    
    rels_vals<-
      allData %>% 
      select(site, period, TN_mgS_km2, NO3_mgS_km2, TP_mgS_km2, PO4_mgS_km2, NPOC_mgS_km2)
    
    #Reference md_lower 
    ref<-
      rels_vals%>% 
      filter(site == "md_lower") %>% 
      rename( lower = site , ref_TN = TN_mgS_km2, ref_NO3 = NO3_mgS_km2, ref_TP = TP_mgS_km2 , ref_PO4 = PO4_mgS_km2 , ref_DOC = NPOC_mgS_km2 )
    
    #Site values without md_lower
    temp_site<-
      rels_vals %>% 
      filter(site != "md_lower")
    
    #Join both tables 
    relat_diff<-
      full_join(temp_site,ref, by = "period") %>% 
      #Calculate relative difference for TN
      mutate(TN_diff=(TN_mgS_km2 - ref_TN)/ref_TN * 100) %>% 
      #Calculate relative difference for NO3
      mutate(NO3_diff=(NO3_mgS_km2 - ref_NO3)/ref_NO3 * 100) %>% 
      #Calculate relative difference for TP  
      mutate(TP_diff=(TP_mgS_km2 - ref_TP)/ref_TP * 100) %>% 
      #Calculate relative difference for PO4  
      mutate(PO4_diff=(PO4_mgS_km2 - ref_PO4)/ref_PO4 * 100) %>% 
      #Calculate relative difference for DOC  
      mutate(DOC_diff=(NPOC_mgS_km2 - ref_DOC)/ref_DOC * 100)
    
#Plotting Relative Difference
    #Total Nitrogen and NO3
    N_diff<-
    relat_diff %>% 
    select(site,period,TN_diff,NO3_diff,TP_diff,PO4_diff,DOC_diff) %>% 
    pivot_longer(cols = (TN_diff:DOC_diff), names_to = "var", values_to = "rel_diff") %>%
    filter(var %in% c("TN_diff","NO3_diff")) %>% 
    #Plot
    ggplot(aes(x = site, y = rel_diff, fill = var)) +
      facet_wrap(~period, ncol = 2, scales = "fixed") +
      geom_bar(position = "dodge", stat = 'identity')+
      labs(x="Site",y="% Relative Difference")+
      scale_fill_manual(values = c("#41878A","#7FC4C4"),
                        labels= c("NO3","TN"))+
      theme_bw()+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 90))
      
    
    #Total Phosphorus and PO4
    P_diff<-
    relat_diff %>% 
      select(site,period,TN_diff,NO3_diff,TP_diff,PO4_diff,DOC_diff) %>% 
      pivot_longer(cols = (TN_diff:DOC_diff), names_to = "var", values_to = "rel_diff") %>%
      filter(var %in% c("TP_diff","PO4_diff")) %>% 
      #Plot
      ggplot(aes(x = site, y = rel_diff, fill = var)) +
      facet_wrap(~period, ncol = 2, scales = "fixed") +
      geom_bar(position = "dodge", stat = 'identity')+
      scale_fill_manual(values = c("#5B9433","#234B34"),
                        labels= c("PO4","TP"))+
      labs(x="Site",y="% Relative Difference")+
      theme_bw()+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 90),        
            axis.title.y = element_blank())
    
    
    #Carbon (DOC)
    C_diff<-
    relat_diff %>% 
      select(site,period,TN_diff,NO3_diff,TP_diff,PO4_diff,DOC_diff) %>% 
      pivot_longer(cols = (TN_diff:DOC_diff), names_to = "var", values_to = "rel_diff") %>%
      filter(var %in% "DOC_diff") %>% 
      #Plot
      ggplot(aes(x = site, y = rel_diff, fill = var)) +
      facet_wrap(~period, ncol = 2, scales = "fixed") +
      geom_bar(position = "dodge", stat = 'identity')+
      labs(x="Site",y="% Relative Difference")+
      scale_fill_manual(values = c("#EAA046"),
                        labels="DOC")+
      theme_bw()+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 90),        
            axis.title.y = element_blank())
    
  #Join relative difference plots
    (N_diff+P_diff+C_diff)
    #Save plot
    ggsave(filename = "plots_Igrena/Plot_rela_diff_NPC.pdf",width=18,height = 6,units="in",dpi = 150)
    

#mg -> mol conversion
    
    #Filter all data to keep the regular and normalized concentrations for N,P and C
    Conc<-
      allData %>% 
      #remove variables
      select(-c(tss_mgL:Fe_ppb_0.45)) %>% 
      select(-c(tss_mgS_km2:Fe_ugS_km2)) %>% 
      select(!c(catchment,date)) %>% 
      #Arrange regular and normalized concentrations separately
      pivot_longer(cols = (NPOC_mgCL:PP_mgS_km2), names_to = "var", values_to = "conc") %>% 
      #Change the concentrations from mg to g 
      mutate(gconc=conc/1000)
    
----#Ratios and %----

# % to add on normalized concentration plots 
#Compare the difference in TN,NO3,TP and PO4 concentrations in percent across md_dam to md_lower
   
    dam<-
      allData %>% 
      select(site,period,TN_mgS_km2,NO3_mgS_km2,TP_mgS_km2,PO4_mgS_km2) %>% 
      filter(site %in% c("md_dam"))
    
    low<-
      allData %>% 
      select(site,period,TN_mgS_km2,NO3_mgS_km2,TP_mgS_km2,PO4_mgS_km2) %>% 
      filter(site %in% c("md_lower")) %>% 
      rename(lower=site,low_TN= TN_mgS_km2,low_NO3= NO3_mgS_km2,low_TP= TP_mgS_km2, low_PO4= PO4_mgS_km2)
    
    #Calculate the percent 
    Percent<- 
      full_join(dam,low, by = "period") %>% 
      mutate(TN_perc=(TN_mgS_km2-low_TN)*100) %>% 
      mutate(NO3_perc=(NO3_mgS_km2-low_NO3)*100) %>% 
      mutate(TP_perc=(TP_mgS_km2-low_TP)*100) %>% 
      mutate(PO4_perc=(PO4_mgS_km2-low_TN)*100)  #NOTE: When I multiply the difference by 100, some values are high  
             

#RAtios 
    
    #mg -> mol conversion
    
    #Filter molar concentrations
    temp_mol<-
      allData %>% 
      select(c(site,period,ends_with("_UM")))

    
Ratios<-
  #TN:TP
  temp_mol %>% 
      mutate(TN_TP=TN_uM/TP_uM) %>% 
  #NO3:SRP
      mutate(NO3_PO4=NO3_uM/PO4_uM) %>% 
  #TDN:TDP
      mutate(TDN_TDP=TDN_uM/TDP_uM)

#Ratio plots 


----#Regular Concentration Plots (N,P,DOC)#----
 
#Nitrogen 
    
    #Nitrogen concentrations by month (not stacked)
    allData %>%
      # gather all nutrient data into long format
      pivot_longer(cols = c(TN_mgNL, PN_mgNL, TDN_mgNL, DON_mgNL, NH4_mgNL, NO3_mgNL), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("TN_mgNL", "PN_mgNL", "TDN_mgNL", "DON_mgNL", "NH4_mgNL", "NO3_mgNL"))) %>% 
      #Plot
      ggplot(aes(x = var, y = conc, fill = site)) +
      facet_wrap(~period, ncol = 1, scales = "fixed") +
      geom_bar(position = "dodge", stat = 'identity')+
      labs(x="Varible",y="Concentration")+
      #scale_fill_manual(values=c("#FB7477","#FABE9E","#F8961E","#F9C74F","#90BE6D","#43AA8B","#577590"))+
      theme_bw()
    
    #Nitrogen concentrations by month (stacked)
    allData %>% 
      # gather all nutrient data into long format
      pivot_longer(cols = c(NO3_mgNL, NH4_mgNL, DON_mgNL, PN_mgNL), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("PN_mgNL", "DON_mgNL", "NH4_mgNL", "NO3_mgNL"))) %>%
      # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
      #Plot
      ggplot(aes(x = site, y = conc, fill = var)) +
      facet_wrap(~period, ncol = 2, scales = "fixed") +
      geom_bar(position = "stack", stat = 'identity')+
      labs(x="Site",y="Concentration")+
      scale_fill_manual(values=c("#515E63","#282846","#007580","#57837B"))+
      theme_bw()
    
    #Nitrogen concentrations by month (stacked) without Hungerford
    #Ellie also did this in her graphs, and I thought I would include it again to better visualize the other subcatchment concentrations. 
    allData %>% 
      # gather all nutrient data into long format
      pivot_longer(cols = c(NO3_mgNL, NH4_mgNL, DON_mgNL, PN_mgNL), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("PN_mgNL", "DON_mgNL", "NH4_mgNL", "NO3_mgNL"))) %>%
      # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
      #omit Hungerford from x axis
      filter(site %in% c("Wade", "md_dam", "md_fork", "md_upper", "md_lower")) %>% 
      #Plot
      ggplot(aes(x = site, y = conc, fill = var)) +
      facet_wrap(~period, ncol = 1, scales = "fixed") +
      geom_bar(position = "stack", stat = 'identity')+
      labs(x="Site",y="Concentration")+
      scale_fill_manual(values=c("#515E63","#282846","#007580","#57837B"))+
      theme_bw()
    
    
#Phosphorus 
    
    #Phosphorus concentrations by month (not stacked)
    allData %>%
      # gather all nutrient data into long format
      pivot_longer(cols = c(DOP_mgPL, PO4_mgPL,PP_mgPL,TDP_mgPL,TP_mgPL), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("TP_mgPL", "PP_mgPL","TDP_mgPL","DOP_mgPL","PO4_mgPL"))) %>% 
      #Plot
      ggplot(aes(x = var, y = conc, fill = site)) +
      facet_wrap(~period, ncol = 1, scales = "fixed") +
      geom_bar(position = "dodge", stat = 'identity')+
      labs(x="Varible",y="Concentration")+
      theme_bw()
    
    #Phosphorus concentrations by month (stacked)
    allData %>% 
      # gather all nutrient data into long format
      pivot_longer(cols = c(DOP_mgPL, PO4_mgPL,PP_mgPL), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("PP_mgPL","DOP_mgPL","PO4_mgPL"))) %>%
      # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
      #Plot
      ggplot(aes(x = site, y = conc, fill = var)) +
      facet_wrap(~period, ncol = 2, scales = "fixed") +
      geom_bar(position = "stack", stat = 'identity')+
      labs(x="Site",y="Concentration")+
      scale_fill_manual(values=c("#515E63","#282846","#007580","#57837B"))+
      theme_bw()
    
    #Phosphorus concentrations by month (stacked) without Hungerford
    allData %>% 
      # gather all nutrient data into long format
      pivot_longer(cols = c(DOP_mgPL, PO4_mgPL,PP_mgPL), names_to = "var", values_to = "conc") %>% 
      # re-order the levels of our new column called var
      mutate(var = ordered(var, levels = c("PP_mgPL","DOP_mgPL","PO4_mgPL"))) %>%
      # replace("PN_mgNL" <= 0, 0.01) %>%   need to figure out how to replace negative values
      #omit Hungerford from x axis
      filter(site %in% c("Wade", "md_dam", "md_fork", "md_upper", "md_lower")) %>% 
      #Plot
      ggplot(aes(x = site, y = conc, fill = var)) +
      facet_wrap(~period, ncol = 1, scales = "fixed") +
      geom_bar(position = "stack", stat = 'identity')+
      labs(x="Site",y="Concentration")+
      scale_fill_manual(values=c("#515E63","#282846","#007580","#57837B"))+
      theme_bw()

    
#Carbon (NPOC)
    
    #NPOC concentrations by month
    allData %>% 
      # gather all nutrient data into long format
      pivot_longer(cols = NPOC_mgCL, names_to = "var", values_to = "NPOC_mgCL") %>% 
      # filter out NA values
      filter(!is.na("NPOC_mgCL")) %>% 
      #Plot
      ggplot(aes(x = site, y = NPOC_mgCL, fill = var)) +
      facet_wrap(~period, ncol = 1, scales = "fixed") +
      geom_bar(stat = "identity") +
      scale_fill_manual(values="#087E8B")+
      #labs(x="Site",y="Concentration")+
      ylab(expression(DOC~(mg~C~L^{-1}))) +
      xlab("Site")+
      theme_bw()
    
    
    
  
 
  
