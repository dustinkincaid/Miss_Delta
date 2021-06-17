# Read in compiled trace metals data and calculate fractions

# Load packages
library("tidyverse")

# Read in data
metals <- read_csv("Raw data/BREE_traceMetals_STREAM_compiled_2020-01-31.csv")

# Calculate fractions
metals <- metals %>% 
  # Rename columms with spaces for ease of use downstream
  rename(ID_ICPMS = "ID for ICP-MS", runDate_ICPMS = "ICP-MS Run Date", ID_sample = "Sample ID", Date_sample = "Sample Date",
         Time_sample = "Sample Time") %>% 
  # Drop ID_ICPMS because it messes with the pivot_wider step below
  # We'll also drop "Bottle no./ Field ID"
  select(-c(ID_ICPMS, "Bottle no./ Field ID")) %>% 
  # Next two steps create columns for each filter size fraction (e.g., Fe_0.02 vs Fe_0.45)
  pivot_longer(cols = "Al ppb":"Fe ppb",
               names_to = c("analyte", "units"),
               names_sep = " ",
               values_to = "conc") %>% 
  pivot_wider(names_from = c(analyte, "Filter size"),
              values_from = conc) %>% 
  # Calculate the colloidal fractions of Fe and Mn
  mutate(Fe_col = Fe_0.45 - Fe_0.02,
         Mn_col = Mn_0.45 - Mn_0.02) %>% 
  # Drop Al_0.02 and P_0.02 columns
  select(-c(Al_0.02, P_0.02)) %>% 
  # Rearrange columns
  select(runDate_ICPMS:units, Fe_0.45, Fe_0.02, Fe_col, Mn_0.45, Mn_0.02, Mn_col, Al_0.45, Si_0.45, Si_0.02, P_0.45)


# Write file to CSV
metals %>% 
  mutate(Time_sample = as.character(Time_sample)) %>% 
  write_csv("traceMetals_fractions_STREAM_2020-02-01.csv")