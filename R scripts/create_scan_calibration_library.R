# This script combines spectral data collected using the Wade_MP s::can in the Missisquoi River deltaic wetland 
# and solute chemistry from grab samples to create a calibration library to be used to predict 
# chemistry from s::can spectral time series (PLSR models)


# Load packages
  library("tidyverse")
  library("lubridate")
  library("data.table")
  
  
# Load function for cleaning s::can spectral files w/o well #'s in 2nd column
  source("R scripts/tidy_scanspectra.R")  

  
# Read in nutrient chemistry data
  nutrients_may <- read_csv("Data/alldata_compiled_2019-05-17.csv", col_types = cols()) %>%
    mutate(r_timestamp = as.character(r_timestamp))
  nutrients_july <- read_csv("Data/alldata_compiled_2019-07-03.csv", col_types = cols()) %>%
    mutate(r_timestamp = as.character(r_timestamp))
  # august <- read_csv("Data/alldata_compiled_2019-08-14.csv",
  #                    col_types = cols(NO3_UM = "n", NH4_UM = "d",PO4_UM = "d", 
  #                                     NO3_mgNL = "d", NH4_mgNL = "d", PO4_mgPL = "d")) %>%
  #   mutate(r_timestamp = as.character(r_timestamp))
  # Here is a weird f&cking workaround for the issue above w/ August data - angry face!
  nutrients_august <- read_csv("Data/alldata_compiled_2019-08-14.csv", col_types = cols()) %>% 
    mutate(r_timestamp = as.character(r_timestamp),
           NO3_UM = as.numeric(NO3_UM),
           NH4_UM = as.numeric(NH4_UM),
           PO4_UM = as.numeric(PO4_UM), 
           NO3_mgNL = as.numeric(NO3_mgNL), 
           NH4_mgNL = as.numeric(NH4_mgNL), 
           PO4_mgPL = as.numeric(PO4_mgPL))
  nutrients_october <- read_csv("Data/alldata_compiled_2019-10-21.csv", col_types = cols()) %>%
    mutate(r_timestamp = as.character(r_timestamp)) 

  # Bind data together
  nutrients <- bind_rows(nutrients_may, nutrients_july, nutrients_august, nutrients_october) %>% 
    # Format date if not already done
    mutate(timestamp = ymd_hms(r_timestamp)) %>% 
    # Add a month column & make it a factor instead of number
    mutate(month = as.factor(month(r_timestamp))) %>% 
    # Remove all date/time columns except timestamp
    select(-c(DateTimeS, r_timestamp, Date, Time, Date.Time)) %>% 
    # Remove other unnecessary columns
    select(-c(elev, no3_mgL_scan, toc_mgL_scan, doc_mgL_scan, suva254_absm_scan, NO3_UM, NH4_UM, PO4_UM, TDN_UM, TDP_UM, TN_UM, TP_UM))
    
  # Remove objects no longer needed
  rm(nutrients_may, nutrients_july, nutrients_august, nutrients_october)  

    
# Read in turbidity compenstated spectra
  spectra_comp <- read_csv("Data/scan Data/scan_spectra_compfp_compiled_20200128.csv")

  # Tidy up spectra object
  spectra_comp <- tidy_scanspectra(spectra_comp, "comp")

  
# Create calib library: combine spectra and chemistry
  # Convert dfs to data.tables
  setDT(nutrients)
  setDT(spectra_comp)
  # Duplicate spectra timestamp
  spectra_comp[, timestamp_spec := timestamp]
  # Merge chemistry data with spectra data by timestamp (within +/- 1 min)
  comb <- spectra_comp[nutrients, roll = "nearest", on = .(timestamp)]
  cal_lib <- comb %>%
    select(timestamp, timestamp_spec, month, transect:ncol(.), everything()) %>%
    rename(timestamp_chem = timestamp) %>% 
    filter(abs(difftime(timestamp_chem, timestamp_spec, units = "mins")) < 1) %>% 
    filter(!is.na(timestamp_spec))
  rm(comb)      
  
# Write calibration library to CSV  
  cal_lib %>% 
    # Convert any time column to character to avoid issues with time zone conversions when writing CSV
    mutate(timestamp_chem = as.character(timestamp_chem),
           timestamp_spec = as.character(timestamp_spec)) %>% 
    # Write to CSV
    write_csv("Data/scan predicted chem/scan_MissDelta_calibration_library.csv")   
  