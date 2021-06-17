# Compile GPS data, YSI EXO2 data, s::can data, and chemistry data 
# from Missisquoi River Delta longitudinal survey on 2019-07-03 into one file


# NOTE: I had to remove the degree symbol from the name of the temperature column ("C-15F102377") 
# in the raw YSI CSV in order to be able to manipulate that variable


# To do:
  # Add TSS data
  # Add solute chemistry
  # Add predicted chemistry from s::can using PLSR calibration with lab grab sample chemistry


# Load packages
library("tidyverse")
library("lubridate")


# Read in files
  # GPS data (coordinates every 1 minute and distances traveled)
    # Change file name
    gps <- read_csv("Data/GPS Data/survey_distances_2019-07-03.csv", col_types = cols()) %>%
    # Convert timestamp
    mutate(r_timestamp = mdy_hm(r_timestamp, tz = "America/New_York"))
  
    
  # YSI EXO2 data
  # Change file name and filtered date range
  ysi <- read_csv("Data/YSI EXO2 Data/YSI_EXO2_Miss_Delta_20190703.csv", col_types = cols()) %>% 
    # Create timestamp & round to nearest minute
    mutate(r_timestamp = round_date(mdy_hms(paste(Date, Time), tz = "America/New_York"), "minute")) %>% 
    # Select columns to keep
    select(Date, Time, r_timestamp, "C-15F102377", "DO %-15F101915", "DO mg/L-15F101915", "SPC-uS/cm-15F102377",
           "TDS mg/L-15F102377", "pH-17D108473", "Chl ug/L-15F101336", "fDOM QSU-13M100089") %>% 
    # Rename columns
    rename(temp_c = "C-15F102377", do_per = "DO %-15F101915", do_mgl = "DO mg/L-15F101915", spc_uscm = "SPC-uS/cm-15F102377",
           tds_mgl = "TDS mg/L-15F102377", ph = "pH-17D108473", chla_ugl = "Chl ug/L-15F101336", fdom_qsu = "fDOM QSU-13M100089") %>% 
    # Filter readings to only include those during formal survey period
    filter(r_timestamp >= ymd_hms("2019-07-03 10:10:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-07-03 12:20:00", tz = "America/New_York"))
  
  
  # Grab sample solute chemistry
  # Dissolved nutrients
  chem_diss <- read_csv("Data/Lab Chemistry/MD19_Dissolved_set1_QC.csv") %>%
    # Format timestamp and make r_timestamp
    mutate(r_timestamp = mdy_hm(Datetime, tz = "America/New_York")) %>%
    # Rename columns
    rename(NO3_UM = "uMol/L N-NO3", NH4_UM = "uMol/L N-NH4", PO4_UM = "uMol/L P-PO4",
           NO3_mgNL = "mg/L N-NO3", NH4_mgNL = "mg/L N-NH4", PO4_mgPL = "mg/L P-PO4") 
  # # Get rid of unnecessary columns
  # select(-c("uMol/L N-NH4_corrected", "mg/L N-NH4_corrected"))
  # Total dissolved nutrients
  chem_totDiss <- read_csv("Data/Lab Chemistry/MD19_TDN-TDP_set1_QC.csv", col_types = cols(Time = "c")) %>%
    # Format timestamp and make r_timestamp
    mutate(r_timestamp = mdy_hm(paste(Date, Time), tz = "America/New_York")) %>%
    # Rename columns
    rename(TDN_UM = "TDN uMol/L N", TDP_UM = "TDP uMol/L P",
           TDN_mgNL = "TDN mg/L N", TDP_mgPL = "TDP mg/L P") %>%
    # Get rid of unnecessary columns
    select(-c(Date, Time))
  # Total nutrients
  chem_tot <- read_csv("Data/Lab Chemistry/MD19_TN-TP_set1_QC.csv", col_types = cols(Time = "c")) %>%
    # Format timestamp and make r_timestamp
    mutate(r_timestamp = mdy_hm(paste(Date, Time), tz = "America/New_York")) %>%
    # Rename columns
    rename(TN_UM = "TN uMol/L N", TP_UM = "TP uMol/L P",
           TN_mgNL = "TN mg/L N", TP_mgPL = "TP mg/L P") %>% 
    # Get rid of unnecessary columns
    select(-c(Date, Time))
  
  # Combine all chem files into one
  chem <- full_join(chem_diss, chem_totDiss, by = c("SampleID", "r_timestamp")) %>% 
    full_join(chem_tot, by = c("SampleID", "r_timestamp"))
  # May only
  chem_july <- chem %>% filter(month(r_timestamp) == 7) %>% select(r_timestamp, NO3_UM:TP_mgPL)
  
  
  # DOC
  doc <- read_csv("Data/DOC Data/NPOC_allSurveys_20191203.csv") %>%
    # Format timestamp and make r_timestamp
    mutate(r_timestamp = mdy_hm(Datetime, tz = "America/New_York")) %>%
    # Rename columns
    rename(NPOC_mgCL = "Result(NPOC)", transect = "Transect") %>% 
    # Get rid of BLK rows
    filter(!is.na(transect)) %>% 
    # Keep only relevant columns
    select(r_timestamp, NPOC_mgCL)
  
  # Keep only July DOC data
  doc_july <- doc %>% filter(month(r_timestamp) == 7)

  
  # TSS
  tss <- read_csv("Data/TSS/TSS_2019-07-19.csv", col_types = cols()) %>% 
    # Create timestamp
    mutate(r_timestamp = mdy_hms(paste(Date, Time), tz = "America/New_York")) %>% 
    # Filter to include samples from survey period
    filter(r_timestamp >= ymd_hms("2019-07-03 10:10:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-07-03 12:20:00", tz = "America/New_York")) %>% 
    # Rename columns
    rename(tss_mgL = "TSS (mg/L)") %>% 
    # Select columns to keep
    select(Date, Time, r_timestamp, tss_mgL)
  
  
  # Trace metals
  # First read in sample info to join to metals samples
  samples <- read_csv("Data/alldata_sample_info.csv") %>% 
    mutate(Date = mdy(Date),
           r_timestamp = ymd_hms(paste(Date, Time), tz = "America/New_York"))
  # Trace metals data
  metals <- read_csv("Data/Lab Chemistry/trace metals/BREE_traceMetals_MissDelta_compiled_2020-01-31.csv") %>% 
    # Drop unnecessary columns
    select(-c("ID for ICP-MS", "ICP-MS Run Date", "Sample ID", "Filter size", "Sample Time", Waterbody, Location1, Location2)) %>% 
    # Rename bottle no. column to SampleID & Sample Date to Date
    rename(SampleID = "Bottle no./ Field ID",
           Date = "Sample Date") %>% 
    # Format date column
    mutate(Date = mdy(Date)) %>% 
    # Rename metals colummns to add filter size suffix
    # First trip "ppb" and space after
    rename_at(.vars = vars(ends_with("ppb")),
              .funs = list(~ trimws(sub("ppb", "", .)))) %>% 
    # Add suffix
    rename_at(.vars = vars(-c(SampleID, Date)),
              .funs = list(~ paste0(., "_ppb_0.45")))
  # Join metals to sample info
  metals <- full_join(samples, metals, by = c("SampleID", "Date")) %>% 
    # Keep only this month's data
    filter(month(Date) == 7) %>% 
    # Drop unncessary columns
    select(-c(SampleID:Transect))  
  
  
  # Predicted chemistry from s::can global calibration
    # Global calibration predictions
    # scan <- read_csv("Data/scan Data/2019-07-03 scan data/scan_globalcalibration_20190703.csv", col_types = cols()) %>%
    #   # Rename columns
    #   rename(Date.Time = "Date/Time", turb_NTU_scan = "Turbid. [NTUeq]466.67-0.00_1", no3_mgL_scan = "NO3-Neq [mg/l]23.33-0.00_1",
    #          toc_mgL_scan = "TOCeq [mg/l]58.33-0.00_1", doc_mgL_scan = "DOCeq [mg/l]30.00-0.00_1", suva254_absm_scan = "SAC254T [Abs/m]NaN-NaN_1") %>% 
    #   # Convert the timestamp column & round to nearest minute
    #   mutate(r_timestamp = round_date(parse_date_time(Date.Time, "%Y.%m.%d %H:%M:%S", tz = "America/New_York"), "minute")) %>%
    #   # Filter readings to only include those during formal survey period
    #   filter(r_timestamp >= ymd_hms("2019-07-03 10:10:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-07-03 12:20:00", tz = "America/New_York")) %>% 
    #   # Select columns to keep
    #   select(r_timestamp, Date.Time, turb_NTU_scan, no3_mgL_scan, toc_mgL_scan, doc_mgL_scan, suva254_absm_scan)
    
    # PLSR predicted concentrations
    scan_no3 <- read_csv("Data/plsr/predicted_time_series/MD19_no3_predicted_time_series_5cmpt.csv") %>% 
      select(timestamp_chem, predicted_param) %>% 
      rename(r_timestamp = timestamp_chem, scan_NO3_mgNL = predicted_param) %>% 
      mutate(r_timestamp = ymd_hms(r_timestamp, tz = "America/New_York")) %>% 
      # Keep only this month's data
      filter(month(r_timestamp) == 7)
    scan_doc <- read_csv("Data/plsr/predicted_time_series/MD19_doc_predicted_time_series_2cmpt_jul_cal.csv") %>% 
      select(timestamp_chem, predicted_param) %>% 
      rename(r_timestamp = timestamp_chem, scan_DOC_mgCL = predicted_param) %>% 
      mutate(r_timestamp = ymd_hms(r_timestamp, tz = "America/New_York")) %>% 
      # Keep only this month's data
      filter(month(r_timestamp) == 7)      
  
  
# Compile files into one file
  alldata <- 
    # Join gps and ysi data frames
    full_join(gps, ysi, by = "r_timestamp") %>% 
    # Add the scan data
    full_join(scan_no3, by = "r_timestamp") %>% 
    full_join(scan_doc, by = "r_timestamp") %>% 
    # Add the nutrient data
    full_join(chem_july, by = "r_timestamp") %>% 
    # Add the tss data
    full_join(tss %>% select(-c(Date, Time)), by = "r_timestamp") %>% 
    # Add the DOC data
    full_join(doc_july, by = "r_timestamp") %>% 
    # Add the trace metals data
    full_join(metals, by = "r_timestamp") %>%    
    # Give the grab sample a transect == Dam
    mutate(transect = replace_na(transect, "Dam")) %>% 
    # Give the grab sample an arbitrary distance of -1000 m
    mutate(dist_cum_m = replace_na(dist_cum_m, -1000))

# Write the alldata file as CSV
  alldata %>% 
    # Convert the r_timestamp to character to avoid issues with timezone conversions in CSV
    mutate(r_timestamp = as.character(r_timestamp)) %>% 
    # Write as CSV
    write_csv("Data/alldata_compiled_2019-07-03.csv")
  
  