# Calculate linear distances from shapefiles
# Need to maintain time with each point

# NOTE: this script will offset the cumulative distance traveled (dist_cum_m) 
# so that 0 meters is the same 0 meters as the first survey on 2019-05-17 for the mainstem transect
# The 8/14 float started with a transect down Dead Run Creek


# Make sure you install the lwgeom package if you don't have it installed 
# You don't need to load the package afterwards
# install.packages("lwgeom")


# Load packages
library("tidyverse")
library("lubridate")
library("sf")


# Read in shapefile
# NOTE: for shapefiles, need to have the corresponding .sbx, .dbf, and .prj files in the same directory as the .shp file
# 2019-05-17 float data
   md_sf_may <- st_read("Data/GPS Data/2019-05-17 GPS Data/GPS_Downstream.shp") %>%
    # Convert timestamp
    mutate(r_timestamp = ymd_hms(DateTimeS, tz = "America/New_York")) %>%
    # Filter out rows before beginning of survey (9:24)
    filter(r_timestamp >= ymd_hms("2019-05-17 09:24:00", tz = "America/New_York")) %>%
    # Filter out rows after end of survey (11:36)
    filter(r_timestamp <= ymd_hms("2019-05-17 11:36:00", tz = "America/New_York"))
# 2019-07-03 float data
  md_sf_jul <- st_read("Data/GPS Data/2019-07-03 GPS Data/Export_5_3_Data.shp") %>% 
    # Convert timestamp
    mutate(r_timestamp = ymd_hms(DateTimeS, tz = "America/New_York")) %>% 
    # Filter out rows before beginning of survey (9:24)
    filter(r_timestamp >= ymd_hms("2019-07-03 10:10:00", tz = "America/New_York")) %>% 
    # Filter out rows after end of survey (11:36)
    filter(r_timestamp <= ymd_hms("2019-07-03 12:20:00", tz = "America/New_York"))
# 2019-08-14 float data
  md_sf_aug <- st_read("Data/GPS Data/2019-08-14 GPS Data/MissDelta_8-14_gps_points.shp") %>% 
    # Convert timestamp
    mutate(r_timestamp = ymd_hms(DateTimeS, tz = "America/New_York")) %>% 
    # Filter readings to only include those during formal survey period
    filter((r_timestamp >= ymd_hms("2019-08-14 09:53:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-08-14 11:10:00", tz = "America/New_York")) |
             (r_timestamp >= ymd_hms("2019-08-14 11:52:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-08-14 14:11:00", tz = "America/New_York"))) %>% 
    # Add a column to indicate which transect the readings belong to: Dead Creek Run vs. mainstem
    mutate(transect = ifelse(r_timestamp >= ymd_hms("2019-08-14 09:53:00", tz = "America/New_York") & r_timestamp <= ymd_hms("2019-08-14 11:10:00", tz = "America/New_York"),
                             "Dead Creek", "Main"))
  
# Split md_sf_aug into the two transects
  md_sf_aug_dcr <- md_sf_aug %>% filter(transect == "Dead Creek")
  md_sf_aug_main <- md_sf_aug %>% filter(transect == "Main")
  
# Mainstem transect ONLY
# What is the linear distance offset b/w the first GPS points from the 2019-05-17 & subsequent floats (e.g., 2019-07-03, 2019-08-14) on the MAINSTEM?
# Calculate the linear distance b/w these two points
  # Pull the first points from both surveys
  first_may <- md_sf_may %>% slice(1) %>% select(r_timestamp, geometry)
  first_july <- md_sf_jul %>% slice(1) %>% select(r_timestamp, geometry)
  first_aug <- md_sf_aug_main %>% slice(1) %>% select(r_timestamp, geometry)
  
  # The sf objects have different crs, so transform one of them
    # Check the crs of both
    st_crs(first_may)
    st_crs(first_july)
    st_crs(first_aug)
    # Transform crs of first_july to crs of first_may (EPSG: 4326)
    first_july2 <- first_july %>% st_transform(4326)
    # Confirm it worked
    st_crs(first_july2)
    # first_aug has the same crs as first_may; no need to transform it
  
  # Calculate the distance between these two points
    # (as below) Create an "empty" coordinate to deal with incompatibility b/w 'sf' and lag()
    empty <- st_as_sfc("POINT(EMPTY)")
    
    dist_offset <- rbind(first_aug, first_may) %>%
        mutate(
            dist_from_prev_m = sf::st_distance(
                  geometry, 
                  lag(geometry, default = empty), 
                by_element = TRUE)
        )

    # The August start point is basically the same as in May, so offset will be 0
    # Link to map of sites: https://batchgeo.com/map/dc72390fe5b6100c086206db7a7f9935

    # Set the offset_value from dist_offset
    # We will use this value to offset the cumulative distance traveled below, so that 0 meters is the
    # same 0 meters as the first survey on 2019-05-17
    # NOTE: Ellie mapped these points out and we know the first point on 7/3 was upstream of the first point on 5/17;
    # so we will SUBTRACT this value below
    offset_value <- 0


# Calculate Euclidean distances from the previous point (using lag())
# Could calculate distance to next point by using lead()
# Solution from here: https://github.com/r-spatial/sf/issues/799

  # Create an "empty" coordinate to deal with incompatibility b/w 'sf' and lag()
  empty <- st_as_sfc("POINT(EMPTY)")
  
  # Calculate distance from previous point
  # Dead Creek Run
  md_sf_aug_dcr2 <- md_sf_aug_dcr %>%
      mutate(
          dist_from_prev_m = sf::st_distance(
                geometry, 
                lag(geometry, default = empty), 
              by_element = TRUE)
      )

  # Mainstem
  md_sf_aug_main2 <- md_sf_aug_main %>%
      mutate(
          dist_from_prev_m = sf::st_distance(
                geometry, 
                lag(geometry, default = empty), 
              by_element = TRUE)
      )  
   
# Create a non-shapefile to output
  # Extract coordinates
  # Dead Creek Run
    md_coords_dcr <- do.call(rbind, st_geometry(md_sf_aug_dcr2)) %>%
      # Coerce the list to a data frame
      as_tibble() %>% 
      # Name columns
      setNames(c("lon", "lat", "elev"))
  # Mainstem
    md_coords_main <- do.call(rbind, st_geometry(md_sf_aug_main2)) %>%
      # Coerce the list to a data frame
      as_tibble() %>% 
      # Name columns
      setNames(c("lon", "lat", "elev"))    
    
  # Create a tidy object to ouput
  # NOTE: here we also offset the cumulative distance with the offset value above
  # Dead Creek Run
  output_dcr <- 
    # Add the extracted coordinates to the shapefile
    bind_cols(md_sf_aug_dcr2, md_coords_dcr) %>% 
    # Convert the distance column to numeric
    mutate(dist_from_prev_m = as.numeric(dist_from_prev_m)) %>%
    # Make the first distance a 0 instead of NA
    mutate(dist_from_prev_m = replace_na(dist_from_prev_m, 0)) %>% 
    # Create a column with cumulative distance
    mutate(dist_cum_m = cumsum(dist_from_prev_m)) %>% 
    # CORRECT THE OFFSET, so that 0 meters is the same 0 meters as the first survey on 2019-05-17
    mutate(dist_cum_m = dist_cum_m - offset_value) %>% 
    # Convert the r_timestamp to character to avoid issues with timezone conversions in CSV
    mutate(r_timestamp = as.character(r_timestamp)) %>% 
    # Select the columns to keep
    select(DateTimeS, r_timestamp, dist_from_prev_m, dist_cum_m, lat, lon, elev)
  # Mainstem
  output_main <- 
    # Add the extracted coordinates to the shapefile
    bind_cols(md_sf_aug_main2, md_coords_main) %>% 
    # Convert the distance column to numeric
    mutate(dist_from_prev_m = as.numeric(dist_from_prev_m)) %>%
    # Make the first distance a 0 instead of NA
    mutate(dist_from_prev_m = replace_na(dist_from_prev_m, 0)) %>% 
    # Create a column with cumulative distance
    mutate(dist_cum_m = cumsum(dist_from_prev_m)) %>% 
    # CORRECT THE OFFSET, so that 0 meters is the same 0 meters as the first survey on 2019-05-17
    mutate(dist_cum_m = dist_cum_m - offset_value) %>% 
    # Convert the r_timestamp to character to avoid issues with timezone conversions in CSV
    mutate(r_timestamp = as.character(r_timestamp)) %>% 
    # Select the columns to keep
    select(DateTimeS, r_timestamp, dist_from_prev_m, dist_cum_m, lat, lon, elev)  
  
  # Remove geometry and attributes
  st_geometry(output_dcr) <- NULL
  st_geometry(output_main) <- NULL
  
  # Bind the DCR and mainstem together
  output <- bind_rows(output_dcr %>% mutate(transect = "Dead Creek"), output_main %>% mutate(transect = "Main")) %>% 
    select(DateTimeS, r_timestamp, transect, everything())

      
# Write file as a CSV
  write_csv(output, "Data/GPS Data/survey_distances_2019-08-14.csv")
  
    
  

# Another solution from: https://stackoverflow.com/questions/49853696/distances-of-points-between-rows-with-sf
# But I was having issues with printing the object afterwards
# md_sf2 <- md_sf %>% 
#   mutate(
#     lead = geometry[row_number() + 1],
#     dist = st_distance(geometry, lead, by_element = T)
#   ) 



