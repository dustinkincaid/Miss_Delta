

# PLSR DOC calibration script for Miss Delta samples 

# Call packages
library(lubridate)
library(pls) # pls package for the PLSR algorithm
library(tidyverse)

# ENTER DIRECTORY AND DATABASE FILE NAME HERE (csv format)*
# For Erin:
setwd("/Users/erinseybold/ownCloud/BREE (2)/Watershed Data/Miss_Delta")   # FOR MAC

calib_filename <- "Data/plsr/scan_MissDelta_calibration_library.csv" # Calibration database
meas_filename <- "Data/scan Data/scan_spectra_compfp_compiled_20200128.csv" # Filename for fingerprint file to apply calibration to
meas_site_name <- "MissDelta"

# Read in the calibration data ----------------------------------
data <- read.csv(calib_filename)
# Convert timestamp dates
data[ , "r_timestamp"] <- as.POSIXct(strptime(data$timestamp_chem,'%Y-%m-%d %H:%M:%S', tz = "Etc/GMT-4"))


### plotting boxplot of samples to see range of samples in calibration library ----------------------------------
MD_doc_dist <- boxplot(subset$NPOC_mgCL~subset$month,horizontal=TRUE,main="Miss Delta cal. library DOC distribution",xlab="DOC mg C/L",ylab="Month")
MD_doc_dist
# based on visual inspection of DOC - in next step, will remove one data point that is abnormally low in May sampling date (0.02 mg/L)

################################################ #################################### ################################################
################################################ RUN THIS SECTION FOR MAY CALIBRATION ################################################
# PULL OUT SUBSET OF DATA WE WANT TO WORK WITH; filter out very low values and rename NPOC to DOC - and removing any samples from October
subset <- data %>% filter(!is.na(NPOC_mgCL)) %>% filter(NPOC_mgCL>0.03) %>% rename(DOC_mgL="NPOC_mgCL") %>% 
  filter(month==8) 
#### CHANGE THIS FOR EACH MONTH

# *CHOOSE WHICH PARAMETER YOU'D LIKE TO PREDICT*
param <- "DOC_mgL"
lab_val <- subset[ , param] # Create a new vector with the lab values
lab_val <- data.matrix(lab_val) # Convert to data matrix
type <- "comp"

# *CHOOSE HOW MANY COMPONENTS TO USE*
# This should change based on the results of your model fit and components. Rule of thumb is to keep # of components under 10% of total samples in calibration 
# May - 1 comp, July - 2 comps, Aug - X comps, Oct - 3 comps
cmpts <- 3

# Generate fp matrix
cols <- paste(type, "_", seq(220, 730, 2.5), sep="") # Create a list of raw/comp column names
raw_or_comp_fp <- subset[cols] # Put data from those columns into a new matrix
raw_or_comp_fp <- data.matrix(raw_or_comp_fp) # Convert to a data matrix

# Perform the fit ---------------------------------------------------------
# Run the PLSR algorithm
fit <- plsr(lab_val ~ raw_or_comp_fp, ncomp = 20, validation = "CV")  #PLSR model to predict parameter with cross validation

# Run model diagnostics and export summary info ---------------------------------

# Here are diagnostics to learn more about the fit
summary(fit)
plot(RMSEP(fit), legendpos = "topright")

# Create a data frame with the results of the model
predicted <- as.data.frame(predict(fit, raw_or_comp_fp, ncomp = cmpts, type = c("response")))
fit_results <- data.frame(
  "r_timestamp" = subset$r_timestamp,
  "num_compts" = cmpts,
  "parameter" = param,
  "lab_val" = lab_val,
  "dummy_col" = predicted )
# dummy_col is because the predicted variable name is weird. Quick work around...
fit_results[ , "predicted"] <- fit_results[ , ncol(fit_results)] # Copy the weird column to a new one called "predicted"
fit_results[ , ncol(fit_results) - 1 ] <- NULL # Delete the old column
# Create one more variable for the residuals
fit_results[ , "residual"] <- fit_results$lab_val - fit_results$predicted

# Perform linear regression on predicted vs. lab values
lm.fit <- lm(fit_results$lab_val ~ fit_results$predicted)
summary(lm.fit) # Summary for linear fit of lab vs. predicted

### Plot the model results --------------------------------------------------------
# Start the plot
p <- ggplot(data = fit_results, mapping = aes(x = lab_val, y = predicted))
# Add point geom
p <- p+ geom_point(mapping = aes(color = r_timestamp), size = 2)
p <- p+ geom_smooth(method = lm, color = "black") + xlim(2.5,5) + ylim(2.5,5)
# Label axes
p <- p+ labs(x = paste("Lab", param, "value"),
             y = paste ("Predicted", param, "value -", type, "fp") )
# Change text to bold, a litter bigger
p <- p+ theme(text = element_text(face = "bold",
                                  size = 20),
              # Set the axis tick label font
              axis.text = element_text(face = "bold",
                                       size = 14,
                                       color = "black"),
              # Move the legend into the graph
              legend.position = c(0.2,0.8),
              # Remove the legend background
              legend.background = element_blank())
# Add R^2 value annotation
xpos <- 0.5 * range(fit_results$lab_val)[2]- range(fit_results$lab_val)[1] # Find a good x position
ypos <- 0.75 * range(fit_results$predicted)[2]- range(fit_results$predicted)[1] # Find a good y position
p <- p+ annotate("text", x = xpos, y = ypos, label = paste("R^2 = ", round(summary(lm.fit)$r.squared, digits = 2)), size = 5, fontface = "bold")
# Display plot
p

#ggsave("Data/plsr/figures/MD_doc_labvspred_may.png")
#ggsave("Data/plsr/figures/MD_doc_labvspred_jul.png")
ggsave("Data/plsr/figures/MD_doc_labvspred_aug.png")
#ggsave("Data/plsr/figures/MD_doc_labvspred_oct.png")


### Bring in and clean up the sensor data file ----------------------------------
#  sensor_data1 <- data %>%  select(timestamp_chem:turb_NTU_scan & comp_)

# Read in the data
sensor_data <- read.csv(meas_filename, skip = 1) # Skip the first row
# Remove useless columns
sensor_data <- sensor_data[ , -2 : -10] # Remove status and lower wavelengths NaNs
sensor_data <- sensor_data[ , -(length(sensor_data) - 7) : -length(sensor_data)] # remove high wavelength NaNs
# Add column names
colnames(sensor_data)[1] <- "timestamp"
colnames(sensor_data)[2 : length(sensor_data)] <- cols # Wavelength values based on comp or raw above.

# Convert timestamp dates
sensor_data[ , "r_timestamp"] <- as.POSIXct(strptime(sensor_data$timestamp, '%Y.%m.%d %H:%M:%S', tz = "Etc/GMT-4"))

# Pull out absorbance data
sensor_abs <- sensor_data[cols] # Pull absorbance data and put into a new data frame
sensor_abs <- data.matrix(sensor_abs) # Convert to a data matrix

 ### Generate time series ----------------------------------------------------
  # Apply model to sensor measurements
  output <- predict(fit, sensor_abs, ncomp=cmpts, type=c("response"))  # Predict parameter values using PLSR model and the spectrometer output
  # Add results to a data frame
  time_series <- data.frame(
    "r_timestamp" = sensor_data$r_timestamp,
    "parameter" = param,
    "dummy_col" = output )
  # dummy_col is because the predicted variable name is weird. Quick work around...
  time_series[ , "predicted_param"] <- time_series[ , ncol(time_series)] # Copy the weird column to a new one called "predicted"
  time_series[ , ncol(time_series) - 1 ] <- NULL # Delete the old column
  time_series$r_timestamp <- as.POSIXct(strptime(time_series$r_timestamp, '%Y-%m-%d %H:%M:%S', tz = "Etc/GMT-4"))
  
  # join attributes to time series dataframe
  cal_lib_attributes <- data %>% select(timestamp_chem:NPOC_mgCL) %>% rename(DOC_mgL="NPOC_mgCL")
  cal_lib_attributes$r_timestamp <- as.POSIXct(strptime(cal_lib_attributes$timestamp_spec, '%Y-%m-%d %H:%M:%S', tz = "Etc/GMT-4"))
  time_series <- left_join(time_series,cal_lib_attributes,by = "r_timestamp")
  
### Plot time series ----------------------------------------------

 ###### plotting predicted DOC values by month

  t <- ggplot(data=time_series) + geom_point(aes(x=r_timestamp,y=predicted_param),color="black")
  t <- t + geom_point(aes(x=r_timestamp,y=DOC_mgL,color="red"))
  # adding lab measured grab samples
   t <- t + facet_wrap( ~ month,ncol=2,scales = "free")
  # adding axis labels
  t <- t+ labs(x = "",y = paste("Predicted", param, "value") )
  # Set the axis tick label font
  t <- t+ theme(text = element_text(face = "bold",size = 16),
                axis.text = element_text(face = "bold", size = 14,color = "black") )
  t
  
  #ggsave("Data/plsr/figures/MD_doc_plsr_output_may_cal.png")
  #ggsave("Data/plsr/figures/MD_doc_plsr_output_jul_cal.png")
  ggsave("Data/plsr/figures/MD_doc_plsr_output_aug_cal.png")
  #ggsave("Data/plsr/figures/MD_doc_plsr_output_oct_cal.png")
  
  #write.csv(time_series, file = "Data/plsr/predicted_time_series/MD19_doc_predicted_time_series_4cmpt_may_cal.csv", row.names = FALSE) # Writes the loadings to a file
  #write.csv(time_series, file = "Data/plsr/predicted_time_series/MD19_doc_predicted_time_series_2cmpt_jul_cal.csv", row.names = FALSE) # Writes the loadings to a file
  write.csv(time_series, file = "Data/plsr/predicted_time_series/MD19_doc_predicted_time_series_3cmpt_aug_cal.csv", row.names = FALSE) # Writes the loadings to a file
  #write.csv(time_series, file = "Data/plsr/predicted_time_series/MD19_doc_predicted_time_series_3cmpt_oct_cal.csv", row.names = FALSE) # Writes the loadings to a file
  
  

  