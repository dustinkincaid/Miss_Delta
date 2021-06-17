# Look at linear regressions for each transect + analyte on the main stem of the Missisquoi River delta
# Created by DWK on 2020-03-18

# Load packages
  library("tidyverse")
  library("lubridate")
  library("broom")
  
# Read in data
  may <- read_csv("Data/alldata_compiled_2019-05-17.csv", col_types = cols()) %>%
    mutate(r_timestamp = as.character(r_timestamp)) %>% 
  # Remove low DOC outlier in May
  mutate(NPOC_mgCL = ifelse(NPOC_mgCL < 1, NA, NPOC_mgCL))
  july <- read_csv("Data/alldata_compiled_2019-07-03.csv", col_types = cols()) %>%
    mutate(r_timestamp = as.character(r_timestamp))
  # august <- read_csv("Data/alldata_compiled_2019-08-14.csv",
  #                    col_types = cols(NO3_UM = "n", NH4_UM = "d",PO4_UM = "d", 
  #                                     NO3_mgNL = "d", NH4_mgNL = "d", PO4_mgPL = "d")) %>%
  #   mutate(r_timestamp = as.character(r_timestamp))
  # Here is a weird f&cking workaround for the issue above w/ August data - angry face!
  august <- read_csv("Data/alldata_compiled_2019-08-14.csv", col_types = cols()) %>% 
    mutate(r_timestamp = as.character(r_timestamp),
           NO3_UM = as.numeric(NO3_UM),
           NH4_UM = as.numeric(NH4_UM),
           PO4_UM = as.numeric(PO4_UM), 
           NO3_mgNL = as.numeric(NO3_mgNL), 
           NH4_mgNL = as.numeric(NH4_mgNL), 
           PO4_mgPL = as.numeric(PO4_mgPL))
  october <- read_csv("Data/alldata_compiled_2019-10-21.csv", col_types = cols()) %>%
    mutate(r_timestamp = as.character(r_timestamp))   
  
  # Bind data together
  comb <- bind_rows(may, july, august, october) %>% 
    # Format date if not already done
    mutate(r_timestamp = ymd_hms(r_timestamp)) %>% 
    # Add a month column & make it a factor instead of number
    mutate(month = as.factor(month(r_timestamp)))
    rm(may, july, august, october)
  
  #Calculate N and P fractions (that were not measured)
  comb <- comb %>% 
    #Calculate Particulate N 
    mutate(PN = TN_mgNL - TDN_mgNL) %>% 
    #Calculate DON
    mutate(DON = TDN_mgNL - NH4_mgNL - NO3_mgNL) %>% 
    #Calculate Particulate P
    mutate(PP = TP_mgPL - TDP_mgPL) %>% 
    #Calculate DOP
    mutate(DOP = TDP_mgPL - PO4_mgPL) %>% 
    #Make negative values 0
    mutate(PN = replace(PN, PN < 0, 0),
           PP = replace(PP, PP < 0, 0)) %>% 
    #Calculate molar ratios
    mutate(TN_TP = TN_UM / TP_UM,
           DIN_SRP = (NO3_UM + NH4_UM) / PO4_UM,
           DOC_TN = NPOC_mgCL * 1000/12.011 / TN_UM,
           DOC_TP = NPOC_mgCL * 1000/12.011 / TP_UM,
           DOC_DIN = NPOC_mgCL * 1000/12.011 / (NO3_UM + NH4_UM),
           DOC_SRP = NPOC_mgCL * 1000/12.011 / PO4_UM)  

  
# Look at pattern for ENTIRE TRANSECT
# Look at linear regressions of each site~analyte combo
# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
# https://r4ds.had.co.nz/many-models.html
  # Fit the linear regression models
  lm_results <- comb %>%
    # Let's just work with data from main stem
    filter(transect == "Main") %>% 
    # To simplify the df, select relevant columns
    select(r_timestamp, Date, transect, dist_cum_m, temp_c:Fe_ppb_0.45, PN:DOC_SRP) %>% 
    # Pivot the measured variables into long format
    pivot_longer(cols = temp_c:DOC_SRP, names_to = "var", values_to = "value") %>% 
    # Group and nest the groupings - one regression for each transect date, reach section, and var
    group_by(Date, var) %>% 
    nest() %>% 
    # Here is where we regress the value of the variable on distance along reach
    mutate(model = map(data, ~lm(value ~ dist_cum_m, data = .x)),
           tidied = map(model, tidy),
           glanced = map(model, glance))
  
  # Get the coefficient estimates and stats
  lm_results_coef <- lm_results %>% 
    # Unnesting 'tidied' will give you a summary of the coefficients
    unnest(tidied) %>% 
    filter(term != "(Intercept)") %>% 
    select(-c(data, model, glanced))
  
  # Get R^2 and other summary stats
  lm_results_r2 <- lm_results %>% 
    # Unnesting 'tidied' will give you a summary of the coefficients
    unnest(glanced) %>% 
    select(-c(data, model, tidied, sigma, statistic, p.value, df, logLik, AIC, BIC, deviance, df.residual))
  
  # Join these together
  lm_results <- full_join(lm_results_coef, lm_results_r2, by = c("Date", "var")) %>% 
    # Round estimate, p-value, and r2
    mutate(estimate = round(estimate, 7),
           std.error = round(std.error, 4),
           p.value = round(p.value, 4),
           r.squared = round(r.squared, 2),
           adj.r.squared = round(adj.r.squared, 2)) %>% 
    # Calculate a rate of change per 1000 m (1 km)
    mutate(change1KM = estimate*1000*1000) %>% 
    # Create column for sig. vs. non-sig. relationships based on p-values < 0.05
    mutate(sig_or_not = ifelse(p.value < 0.05, "sig", "not_sig")) %>% 
    # Re-order columns
    select(Date:estimate,change1KM, p.value, sig_or_not, everything())
  rm(lm_results_coef, lm_results_r2)  
  
 # Only look at results from a subset
  # lm_results %>% 
  #   filter(var %in% c("temp_c", "do_per", "spc_uscm", "ph", 
  #                     "NO3_mgNL", "NH4_mgNL", "PO4_mgPL", "TDN_mgNL",
  #                     "TDP_mgPL", "TN_mgNL", "TP_mgPL", "NPOC_mgCL", "tss_mgL",
  #                     "PN", "DON", "PP", "DOP")) %>% 
  #   arrange(var, Date) %>% 
  #   write_csv("Data/linearRegressionResults_entireTransect.csv")
  
  sub <- lm_results %>%
    filter(var %in% c("tss_mgL")) %>%
    arrange(sig_or_not, desc(abs(estimate)), var, Date)
   
  
  
  
# Look at patterns SEPARATELY for upper transect vs. lower transect
# Look at linear regressions of each site~analyte combo
# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
# https://r4ds.had.co.nz/many-models.html
  # Fit the linear regression models
  lm_results_split <- comb %>%
    # Let's just work with data from main stem
    filter(transect == "Main") %>% 
    # Let's look at regressions from top of main stem transect to river fork near lake (6750 m)
    # and from river fork (6750 m) to lake; to do this create new factor for reach section (i.e., upper and lower)
    mutate(reach = ifelse(dist_cum_m <= 6750, "upper", "lower")) %>% 
    # To simplify the df, select relevant columns
    select(r_timestamp, Date, transect, reach, dist_cum_m, temp_c:Fe_ppb_0.45, PN:DOC_SRP) %>% 
    # Pivot the measured variables into long format
    pivot_longer(cols = temp_c:DOC_SRP, names_to = "var", values_to = "value") %>% 
    # Group and nest the groupings - one regression for each transect date, reach section, and var
    group_by(Date, reach, var) %>% 
    nest() %>% 
    # Here is where we regress the value of the variable on distance along reach
    mutate(model = map(data, ~lm(value ~ dist_cum_m, data = .x)),
           tidied = map(model, tidy),
           glanced = map(model, glance))
  
  # Get the coefficient estimates and stats
  lm_results_coef_2 <- lm_results_split %>% 
    # Unnesting 'tidied' will give you a summary of the coefficients
    unnest(tidied) %>% 
    filter(term != "(Intercept)") %>% 
    select(-c(data, model, glanced))
  
  # Get R^2 and other summary stats
  lm_results_r2_2 <- lm_results_split %>% 
    # Unnesting 'tidied' will give you a summary of the coefficients
    unnest(glanced) %>% 
    select(-c(data, model, tidied, sigma, statistic, p.value, df, logLik, AIC, BIC, deviance, df.residual))
  
  # Join these together
  lm_results_split <- full_join(lm_results_coef_2, lm_results_r2_2, by = c("Date", "reach", "var")) %>% 
    # Round estimate, p-value, and r2
    mutate(estimate = round(estimate, 7),
           std.error = round(std.error, 4),
           p.value = round(p.value, 4),
           r.squared = round(r.squared, 2),
           adj.r.squared = round(adj.r.squared, 2)) %>% 
    # Calculate a rate of change per 1000 m (1 km)
    mutate(change1KM = estimate*1000*1000) %>%     
    # Create column for sig. vs. non-sig. relationships based on p-values < 0.05
    mutate(sig_or_not = ifelse(p.value < 0.05, "sig", "not_sig")) %>% 
    # Re-order columns
    select(Date:estimate,change1KM, p.value, sig_or_not, everything())
  rm(lm_results_coef_2, lm_results_r2_2)
  
# Write results to CSV
  # lm_results_split %>%
  #   write_csv("Data/linearRegressionResults_split.csv")

  sub_split <- lm_results_split %>%
    filter(var %in% c("tss_mgL")) %>%
    arrange(sig_or_not, desc(abs(estimate)), var, Date)  
  
# Graph results
  lm_results_split %>% 
    # Let's make all estimates for non-significant relationships (p >= 0.05) 0 for now
    # Only significant relationships will have a bar
    mutate(estimate = ifelse(p.value >= 0.05, 0, estimate)) %>% 
    ggplot(aes(x = var, y = estimate, fill = reach)) +
    facet_wrap(~Date, scales = "free_y") +
    geom_bar(stat="identity", position=position_dodge()) +
    theme(axis.text.x = element_text(angle = 90))
    