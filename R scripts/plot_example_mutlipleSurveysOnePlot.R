# An example of how to plot data from more than one survey on one plot

# Load packages
library("tidyverse")
library("lubridate")

# Read in data
may <- read_csv("Data/alldata_compiled_2019-05-17.csv", col_types = cols())
july <- read_csv("Data/alldata_compiled_2019-07-03.csv", col_types = cols())
aug <- read_csv("Data/alldata_compiled_2019-08-14.csv", col_types = cols())

# Pull out August mainstem / exclude Dead Creek
aug_main <- aug %>% filter(transect == "Main")

# Pull out Dead Creek data here
aug_dc <- aug %>% filter(transect == "Dead Creek")

# Bind mainstem data together
comb <- bind_rows(may, july, aug_main) %>% 
  # Format date if not already done
  mutate(r_timestamp = ymd_hms(r_timestamp)) %>% 
  # Add a month column & make it a factor instead of number
  mutate(month = as.factor(month(r_timestamp)))


# Plot data
# Example of plotting one set of sensor data
comb %>%
  ggplot(aes(x = dist_cum_m, y = no3_mgL_scan, group = month, color = month)) +
  geom_line()

# Example of plotting multiple time series from the sensors
comb %>%
  # Gather all the sensor time series into long format
  gather(key = "var", value = "value", c(temp_c:fdom_qsu, turb_NTU_scan:suva254_absm_scan)) %>% 
  # Filter for the variables we want to plot
  filter(var %in% c("temp_c", "do_per", "ph", "doc_mgL_scan")) %>% 
  # Plot them
  ggplot(aes(x = dist_cum_m, y = value, group = month, color = month)) +
    geom_line() +
    facet_wrap(~var, ncol = 1, scales = "free_y")

# Example of plotting all nutrient data
comb %>%
  # Gather all the nutrient time series into long format
  gather(key = "var", value = "value", c(NO3_mgNL:PO4_mgPL)) %>% 
  # Plot them
  ggplot(aes(x = dist_cum_m, y = value, group = month, color = month)) +
    geom_point() +
    facet_wrap(~var, ncol = 1, scales = "free_y")


# Here I plot both the s::can global calibration for NO3 and the NO3 from the grab samples
# As you can see the s::can underpredicts the actual NO3 concentration
comb %>%
  ggplot(aes(x = dist_cum_m, y = no3_mgL_scan, group = month, color = month)) +
  geom_line() +
  geom_point(aes(y = NO3_mgNL))