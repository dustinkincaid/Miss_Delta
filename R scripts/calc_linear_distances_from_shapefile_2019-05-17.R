# Calculate linear distances from shapefiles
# Need to maintain time with each point


# Make sure you install the lwgeom package if you don't have it installed 
# You don't need to load the package afterwards
# install.packages("lwgeom")


# Load packages
library("tidyverse")
library("lubridate")
library("sf")


# Read in shapefile
# NOTE: for shapefiles, need to have the corresponding .sbx, .dbf, and .prj files in the same directory as the .shp file
md_sf <- st_read("Data/GPS Data/2019-05-17 GPS Data/GPS_Downstream.shp") %>%
  # Convert timestamp
  mutate(r_timestamp = ymd_hms(DateTimeS, tz = "America/New_York")) %>%
  # Filter out rows before beginning of survey (9:24)
  filter(r_timestamp >= ymd_hms("2019-05-17 09:24:00", tz = "America/New_York")) %>%
  # Filter out rows after end of survey (11:36)
  filter(r_timestamp <= ymd_hms("2019-05-17 11:36:00", tz = "America/New_York"))


# Calculate Euclidean distances from the previous point (using lag())
# Could calculate distance to next point by using lead()
# Solution from here: https://github.com/r-spatial/sf/issues/799

  # Create an "empty" coordinate to deal with incompatibility b/w 'sf' and lag()
  empty <- st_as_sfc("POINT(EMPTY)")
  
  # Calculate distance from previous point
  md_sf2 <- md_sf %>%
      mutate(
          dist_from_prev_m = sf::st_distance(
                geometry, 
                lag(geometry, default = empty), 
              by_element = TRUE)
      )

   
# Create a non-shapefile to output
  # Extract coordinates
    md_coords <- do.call(rbind, st_geometry(md_sf2)) %>%
      # Coerce the list to a data frame
      as_tibble() %>% 
      # Name columns
      setNames(c("lon", "lat", "elev"))
    
  # Create a tidy object to ouput
  output <- 
    # Add the extracted coordinates to the shapefile
    bind_cols(md_sf2, md_coords) %>% 
    # Convert the distance column to numeric
    mutate(dist_from_prev_m = as.numeric(dist_from_prev_m)) %>%
    # Make the first distance a 0 instead of NA
    mutate(dist_from_prev_m = replace_na(dist_from_prev_m, 0)) %>% 
    # Create a column with cumulative distance
    mutate(dist_cum_m = cumsum(dist_from_prev_m)) %>% 
    # Convert the r_timestamp to character to avoid issues with timezone conversions in CSV
    mutate(r_timestamp = as.character(r_timestamp)) %>% 
    # Select the columns to keep
    select(DateTimeS, r_timestamp, dist_from_prev_m, dist_cum_m, lat, lon, elev)
  
  # Remove geometry and attributes
  st_geometry(output) <- NULL

      
# Write file as a CSV
  write_csv(output, "Data/GPS Data/survey_distances_2019-05-17.csv")
  
    
  

# Another solution from: https://stackoverflow.com/questions/49853696/distances-of-points-between-rows-with-sf
# But I was having issues with printing the object afterwards
# md_sf2 <- md_sf %>% 
#   mutate(
#     lead = geometry[row_number() + 1],
#     dist = st_distance(geometry, lead, by_element = T)
#   ) 



