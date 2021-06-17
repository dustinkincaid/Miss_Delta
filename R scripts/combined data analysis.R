# 1/27/2020
#Ellie Sovcik
#This script will compile all 4 compiled datasets (from each month) and analyze the data

library("tidyverse")
library("lubridate")
library("cowplot")

setwd('C:/Users/esovc/ownCloud3/Shared/BREE/Watershed Data/Miss_Delta')
 
#Read in data
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
  # rm(may, july, august, october)

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
  

# Calculate summary stats for each month
month_sum <- comb %>% 
  gather(key = "var", value = "value", c(temp_c, do_mgl, do_per, spc_uscm, ph, NO3_UM:Fe_ppb_0.45, PN:DOC_TP)) %>% 
  group_by(transect, month, var) %>% 
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            CV = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE),
            CV_per = CV*100) %>% 
  filter(transect != "Dam") %>% 
  ungroup()


# Creating labels for plots
  sensor_labels <- c(temp_c = "Temperature (deg. C)", do_mgl = "DO (mg/L)", do_per = "DO (% saturation)",
                     ph = "pH", spc_uscm = "Specific conductivity (uS/cm)")
  month_labels <- c("5" = "May", '7' = "July", '8' = "August", '10' = "October")


# Figure 3 - physicochemical parameters
  # Theme for these plots
  theme1 <-
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 18),
          axis.title.x = element_text(margin=margin(5,0,0,0)),
          axis.title.y = element_text(margin=margin(0,5,0,0)),
          strip.text = element_text(size = 14),
          legend.text = element_text(size = 14))  
  
  # Longitudinal profiles
  fig3_a <- comb %>% 
    filter(transect == "Main") %>% 
    pivot_longer(cols = c(temp_c, do_per, ph, spc_uscm), names_to = "var", values_to = "value") %>% 
    # Re-order the variables so that temperature is plotted on top
    mutate(var = factor(var, levels = c("temp_c", "do_per", "ph", "spc_uscm"),
                             # I provide labels here for the facetted plots that include symbols and then use labeller = label_parsed below to parse them into label text
                             labels = c("Temp.~(degree*C)", "DO~('%'*~saturation)", "pH", "Sp.~cond.~(mu*S~cm^{-1})"))) %>%     
    ggplot(aes(x = dist_cum_m, y = value, group = month, color = month)) +
    facet_wrap(~var, scales = "free_y", ncol = 1, labeller = label_parsed) +
    geom_line(size = 0.8) +
    geom_point(size = 1.5) +
    geom_vline(xintercept = 6750) +  
    xlab("Distance downstream (m)") +
    theme1 +
    theme(legend.position = "none",
          axis.title.y = element_blank())

  # Main w/ DO as % sat. AND y-axis scale is the same
  # Also, I've switched CV from proportion to % here
  fig3_b <- month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("do_per", "ph", "spc_uscm", "temp_c")) %>% 
    # Re-order the variables so that temperature is plotted on top
    mutate(var = factor(var, levels = c("temp_c", "do_per", "ph", "spc_uscm"),
                             # I provide labels here for the facetted plots that include symbols and then use labeller = label_parsed below to parse them into label text
                             labels = c("Temp.~(degree*C)", "DO~('%'*~saturation)", "pH", "Sp.~cond.~(mu*S~cm^{-1})"))) %>% 
    # Provide abbreviated month labels for legend instead of numbers
    mutate(month = factor(month, levels = c("5", "7", "8", "10"),
                                 labels = c("May", "Jul", "Aug", "Oct"))) %>%
    # Note here I plot CV as % using CV_per
    ggplot(aes(x = month, y = CV_per, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, ncol = 1, labeller = label_parsed) +
    labs(x = "Month", y = "CV (%)") +
    theme1 +
    theme(legend.title = element_blank())
  
  # Combine into one plot using cowplot
  fig3 <- plot_grid(fig3_a, fig3_b, ncol = 2, align = "v", axis = "tb", rel_widths = c(2.5, 1.1))
  save_plot("Plots/figure3_physParams.png", fig3, base_width = 12, base_height = 8, dpi = 150)

  
# Figure 4 - DOC
  # Theme for these plots
  theme2 <-
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 17),
          axis.title.x = element_text(margin=margin(5,0,0,0)),
          axis.title.y = element_text(margin=margin(0,5,0,0)),
          strip.text = element_text(size = 13),
          legend.text = element_text(size = 13))    
  
  # Longitudinal profiles
  fig4_a <- comb %>% 
    filter(transect == "Main") %>% 
    filter(!is.na(NPOC_mgCL)) %>% 
    # Remove really low outlier in May
    filter(NPOC_mgCL > 2) %>% 
    ggplot(aes(x = dist_cum_m, y = NPOC_mgCL, group = month, color = month)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.5) +
    geom_vline(xintercept = 6750) +    
    xlab("Distance downstream (m)") +
    ylab(expression(DOC~(mg~C~L^{-1}))) +
    theme2 +
    theme(legend.position = "none")

  # Main w/ DO as % sat. AND y-axis scale is the same
  # Also, I've switched CV from proportion to % here
  fig4_b <- month_sum %>% 
    filter(transect == "Main" & var == "NPOC_mgCL") %>% 
    # Change name of NPOC for figure label
    # Provide abbreviated month labels for legend instead of numbers
    mutate(month = factor(month, levels = c("5", "7", "8", "10"),
                                 labels = c("May", "Jul", "Aug", "Oct"))) %>%
    # Note here I plot CV as % using CV_per
    ggplot(aes(x = month, y = CV_per, fill = month)) +  
    geom_bar(stat = "identity") +
    labs(x = "Month", y = "CV (%)") +
    theme2 +
    theme(legend.title = element_blank())
  
  # Combine into one plot using cowplot
  fig4 <- plot_grid(fig4_a, fig4_b, ncol = 2, align = "v", axis = "tb", rel_widths = c(2.5, 1.25), 
                    labels = "auto", label_size = 20, vjust = 1.2)
  save_plot("Plots/figure4_DOC.png", fig4, base_width = 10, base_height = 3, dpi = 150)  
  
# Figure 5 - N Species
  # Theme for these plots
  theme3 <-
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          axis.title.x = element_text(margin=margin(5,0,0,0)),
          axis.title.y = element_text(margin=margin(0,5,0,0)),
          strip.text = element_text(size = 13),
          legend.text = element_text(size = 13))    
  
  #Stacked Bar graphs of N species
  fig5_a <- month_sum  %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("PN", "DON", "NH4_mgNL", "NO3_mgNL")) %>% 
    # Provide abbreviated month labels for legend instead of numbers
    mutate(month = factor(month, levels = c("5", "7", "8", "10"),
                          labels = c("May", "Jul", "Aug", "Oct")),
           var = factor(var, levels = c("PN", "DON", "NH4_mgNL", "NO3_mgNL"),
                              labels = c("PN", "DON", "Ammonium", "Nitrate"))) %>% 
    ggplot(aes(x = month, y = mean, fill = var)) +
    geom_bar(position = "stack", stat = "identity") +
    xlab("Month") +
    ylab(expression(N~Species~(mg~N~L^{-1}))) +
    theme3
  
  #CV % for N species
  fig5_b <- month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("PN", "DON", "NH4_mgNL", "NO3_mgNL", "TN_mgNL")) %>%
    # Provide abbreviated month labels for legend instead of numbers
    mutate(month = factor(month, levels = c("5", "7", "8", "10"),
                          labels = c("May", "Jul", "Aug", "Oct")),
           var = factor(var, levels = c("TN_mgNL", "PN", "DON", "NH4_mgNL", "NO3_mgNL"),
                        labels = c("PN", "DON", "Ammonium", "Nitrate", "TN"))) %>% 
    ggplot(aes(x = month, y = CV_per, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, ncol = 1) +
    labs(x = "Month", y = "CV %") +
    theme3
  
    
    #Combine into one plot using cow plot
    fig5 <- plot_grid(fig5_a, fig5_b, ncol = 2, align = "v", axis = "tb", rel_widths = c(2.5, 1.25),
                      labels = "auto", label_size = 20, vjust = 1.2) 
    save_plot("Plots/figure5_N_spec_bars.png", fig5, base_width = 12, base_height = 5, dpi = 150) 
    
# Figure 6 - P Species
    #Stacked Bar graphs of P species
    fig6_a <- month_sum  %>% 
      filter(transect == "Main") %>% 
      filter(var %in% c("PP", "DOP", "PO4_mgPL")) %>% 
      # Provide abbreviated month labels for legend instead of numbers
      mutate(month = factor(month, levels = c("5", "7", "8", "10"),
                            labels = c("May", "Jul", "Aug", "Oct")),
             var = factor(var, levels = c("PP", "DOP", "PO4_mgPL"),
                          labels = c("PP", "DOP", "Phosphate"))) %>% 
      ggplot(aes(x = month, y = mean, fill = var)) +
      geom_bar(position = "stack", stat = "identity") +
      xlab("Month") +
      ylab(expression(P~Species~(mg~P~L^{-1}))) +
      theme3
    
    #CV % for N species
    fig6_b <- month_sum %>% 
      filter(transect == "Main") %>% 
      filter(var %in% c("PP", "DOP", "PO4_mgPL", "TP_mgPL")) %>%
      # Provide abbreviated month labels for legend instead of numbers
      mutate(month = factor(month, levels = c("5", "7", "8", "10"),
                            labels = c("May", "Jul", "Aug", "Oct")),
             var = factor(var, levels = c("PP", "DOP", "PO4_mgPL"),
                                       labels = c("PP", "DOP", "Phosphate"))) %>% 
      ggplot(aes(x = month, y = CV_per, fill = month)) +  
      geom_bar(stat = "identity") +
      facet_wrap(~var, ncol = 1) +
      labs(x = "Month", y = "CV %") +
      theme3
    
    
    #Combine into one plot using cow plot
    fig6 <- plot_grid(fig6_a, fig6_b, ncol = 2, align = "v", axis = "tb", rel_widths = c(2.5, 1.25),
                      labels = "auto", label_size = 20, vjust = 1.2) 
    save_plot("Plots/figure6_P_spec_bars.png", fig6, base_width = 12, base_height = 5, dpi = 150) 
    
#Figure 7 - TSS
    # Longitudinal profiles
    fig7_a <- comb %>% 
      filter(transect == "Main") %>% 
      filter(!is.na(tss_mgL)) %>% 
      # Remove really low outlier in May
     # filter(tss_mgL > 2) %>% 
      ggplot(aes(x = dist_cum_m, y = tss_mgL, group = month, color = month)) +
      geom_line(size = 0.8) +
      geom_point(size = 1.5) +
      geom_vline(xintercept = 6750) +    
      xlab("Distance downstream (m)") +
      ylab(expression(TSS~(mg~L^{-1}))) +
      theme2 +
      theme(legend.position = "none")
    
    # CV % for TSS
    fig7_b <- month_sum %>% 
      filter(transect == "Main" & var == "tss_mgL") %>% 
      # Change name of NPOC for figure label
      # Provide abbreviated month labels for legend instead of numbers
      mutate(month = factor(month, levels = c("5", "7", "8", "10"),
                            labels = c("May", "Jul", "Aug", "Oct"))) %>%
      # Note here I plot CV as % using CV_per
      ggplot(aes(x = month, y = CV_per, fill = month)) +  
      geom_bar(stat = "identity") +
      labs(x = "Month", y = "CV (%)") +
      theme2 +
      theme(legend.title = element_blank())
    
    # Combine into one plot using cowplot
    fig7 <- plot_grid(fig7_a, fig7_b, ncol = 2, align = "v", axis = "tb", rel_widths = c(2.5, 1.25), 
                      labels = "auto", label_size = 20, vjust = 1.2)
    save_plot("Plots/figure7_TSS.png", fig7, base_width = 10, base_height = 3, dpi = 150)  
    
       
#Supplementary graphs
    #Figure S1 - Longitudinal N species
    figs1 <- comb %>% 
      gather(key = "N_frac", value = "conc", c(TN_mgNL,  PN, DON, NH4_mgNL, NO3_mgNL)) %>% 
      filter(transect == "Main") %>% 
      filter(!is.na(conc)) %>% 
     # mutate(month = factor(month, levels = c("5", "7", "8", "10"),
        #                    labels = c("May", "Jul", "Aug", "Oct")),
         #    var = factor(var, levels = c("TN_mgNL", "PN", "DON", "NH4_mgNL", "NO3_mgNL"),
          #                labels = c("TN", "PN", "DON", "Ammonium", "Nitrate"))) %>% 
      ggplot(aes(x = dist_cum_m, y = conc, group = N_frac, color = N_frac)) +
      #facet_wrap(~var, scales = "free_y", ncol = 1, labeller = label_parsed) +
      geom_line(size = 0.8) +
      geom_point(size = 1.5) +
      geom_vline(xintercept = 6750) +  
      facet_wrap(~month, ncol = 1) +
      xlab("Distance downstream (m)") +
      theme1 +
      theme(legend.position = "none",
            axis.title.y = element_blank()) +
    print(figs1)
    
#graph changes in all N fractions with distance for each month
# Main
comb %>% 
  gather(key = "N_frac", value = "conc", c(TN_mgNL,  PN, DON, NH4_mgNL, NO3_mgNL)) %>% 
  filter(transect != "Dead Creek") %>% 
  filter(!is.na(conc)) %>% 
  # Re-order the variables so that temperature is plotted on top
  #mutate(conc = factor(conc, levels = c("TN_mgNL", "PN", "DON", "NH4_mgNL", "NO3_mgNL"),
              # I provide labels here for the facetted plots that include symbols and then use labeller = label_parsed below to parse them into label text
                    #  labels = c("PN", "DON", "Ammonium", "Nitrate", "TN"))) %>%    
  ggplot(aes(x = dist_cum_m, y = conc, group = N_frac, color = N_frac)) +
    geom_line() +
    geom_point() + 
    facet_wrap(~month, ncol = 1, labeller = ) +
    geom_vline(xintercept = 6750) +
    labs(x = "Distance (m)", y = NULL) +
    theme_bw() + theme(panel.grid = element_blank())
# Dead Creek
comb %>% 
gather(key = "N_frac", value = "conc", c(TN_mgPL, PN, DON, NH4_mgNL, NO3_mgNL)) %>% 
  filter(transect == "Dead Creek") %>% 
  filter(!is.na(conc)) %>% 
  ggplot(aes(x = dist_cum_m, y = conc, group = N_frac, color = N_frac)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~month, ncol = 1) +
  labs(x = "Distance (m)", y = NULL) +
  theme_bw() + theme(panel.grid = element_blank())

# DO (% saturation) along Main transect
comb %>%
  filter(transect == "Main") %>% 
  ggplot(aes(x = dist_cum_m, y = do_per, group = month, color = month)) +
  geom_path() +
  geom_point()


#graph changes in all P fractions with distance for each month
# Main
comb %>% 
  gather(key = "P_frac", value = "conc", c(TP_mgPL, PP, DOP, PO4_mgPL)) %>% 
  filter(transect != "Dead Creek") %>% 
  filter(!is.na(conc)) %>% 
  ggplot(aes(x = dist_cum_m, y = conc, group = P_frac, color = P_frac)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~month, ncol = 1) +
  geom_vline(xintercept = 6750) +
  labs(x = "Distance (m)", y = NULL) +
  theme_bw() + theme(panel.grid = element_blank())
# Dead Creek 
comb %>% 
gather(key = "P_frac", value = "conc", c(PP, DOP, PO4_mgPL)) %>% 
  filter(transect == "Dead Creek") %>% 
  filter(!is.na(conc)) %>% 
  ggplot(aes(x = dist_cum_m, y = conc, group = P_frac, color = P_frac)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~month, ncol = 1) +
  theme_bw() + theme(panel.grid = element_blank())
# Graph NPOC with distance for each month
comb %>% 
  #Remove outlier
  mutate(NPOC_mgCL = replace(NPOC_mgCL, NPOC_mgCL < .5, NA)) %>% 
  filter(!is.na(NPOC_mgCL)) %>% 
  ggplot(aes(x = dist_cum_m, y = NPOC_mgCL, group = month, color = month)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 6750) +
  labs(x = "Distance (m)", y = "DOC (mg C/L)") +
  theme_bw() + theme(panel.grid = element_blank())

#graph changes in physical variables with distance for each month
#creating labels
sensor_labels <- c(temp_c = "Temperature (deg. C)", do_mgl = "DO (mg/L)", do_per = "DO (% saturation)",
                   ph = "pH", spc_uscm = "Specific conductivity (uS/cm)")
month_labels <- c("5" = "May", '7' = "July", '8' = "August", '10' = "October")
#plot them
comb %>% 
  gather(key = "phys", value = "conc", c(temp_c, do_mgl, ph, spc_uscm)) %>% 
  filter(transect == "Main") %>% 
  filter(!is.na(conc)) %>% 
  ggplot(aes(x = dist_cum_m, y = conc, group = month, color = month)) +
  geom_line() + 
  facet_wrap(~phys, ncol = 1, scales = "free_y", labeller = labeller(phys = sensor_labels)) +
  labs(x = "Distance (m)", y = NULL) +
  theme_bw() + theme(panel.grid = element_blank())
  
  


#Stacked bar graph of N fractions by month
  # Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("PN", "DON", "NH4_mgNL", "NO3_mgNL")) %>% 
    ggplot(aes(x = month, y = mean, fill = var)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Month", y = "N concentration") +
    theme_bw() + theme(panel.grid = element_blank())
  # Dead Creek
  month_sum %>% 
    filter(transect == "Dead Creek") %>% 
    filter(var %in% c("PN", "DON", "NH4_mgNL", "NO3_mgNL")) %>% 
    ggplot(aes(x = month, y = mean, fill = var)) +
    geom_bar(position = "stack", stat = "identity")

#Stacked bar graph of P fractions by month
  # Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("PP", "DOP", "PO4_mgPL")) %>% 
    ggplot(aes(x = month, y = mean, fill = var)) +
    geom_bar(position = "stack", stat = "identity") +
    labs(x = "Month", y = "P concentration") +
    theme_bw() + theme(panel.grid = element_blank())
  # Dead Creek
  month_sum %>% 
    filter(transect == "Dead Creek") %>% 
    filter(var %in% c("PP", "DOP", "PO4_mgPL")) %>% 
    ggplot(aes(x = month, y = mean, fill = var)) +
    geom_bar(position = "stack", stat = "identity")


# Plot mean + error bars for all variables
  #Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    ggplot(aes(x = month, y = mean, fill = month)) +  
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    facet_wrap(~var, scales = "free_y")
  #Dead
  month_sum %>% 
    filter(transect == "Dead Creek") %>% 
    ggplot(aes(x = month, y = mean, fill = month)) + 
    scale_fill_manual(values=c("turquoise3", "mediumorchid1")) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    facet_wrap(~var, scales = "free_y")
#Plot mean + error bars for N species
  #Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("PN", "DON", "NH4_mgNL", "NO3_mgNL")) %>%
    ggplot(aes(x = month, y = mean, fill = month)) +  
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    facet_wrap(~var, scales = "free_y")
  #Dead
  month_sum %>% 
    filter(transect == "Dead Creek") %>% 
    filter(var %in% c("PN", "DON", "NH4_mgNL", "NO3_mgNL")) %>%
    ggplot(aes(x = month, y = mean, fill = month)) +  
    scale_fill_manual(values=c("turquoise3", "mediumorchid1")) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    facet_wrap(~var, scales = "free_y")
#Plot mean + error bars for P species
  #Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("PP", "DOP", "PO4_mgPL")) %>% 
    ggplot(aes(x = month, y = mean, fill = month)) +  
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    facet_wrap(~var, scales = "free_y")
  #Dead
  month_sum %>% 
    filter(transect == "Dead Creek") %>% 
    filter(var %in% c("PP", "DOP", "PO4_mgPL")) %>% 
    ggplot(aes(x = month, y = mean, fill = month)) +  
    scale_fill_manual(values=c("turquoise3", "mediumorchid1")) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    facet_wrap(~var, scales = "free_y")
# Plot mean + error bars for sensor variables
  #Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("do_mgl", "ph", "spc_uscm", "temp_c", "tss_mgL", "NPOC_mgCL")) %>% 
    ggplot(aes(x = month, y = mean, fill = month)) +  
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    facet_wrap(~var, scales = "free_y")
  #Dead
  month_sum %>% 
    filter(transect == "Dead Creek") %>% 
    filter(var %in% c("do_mgl", "ph", "spc_uscm", "temp_c", "tss_mgL", "NPOC_mgCL")) %>% 
    ggplot(aes(x = month, y = mean, fill = month)) +  
    scale_fill_manual(values=c("turquoise3", "mediumorchid1")) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    facet_wrap(~var, scales = "free_y")
# temperature
month_sum %>% 
  filter(transect == "Main") %>% 
  filter(var == "temp_c") %>% 
  ggplot(aes(x = month, y = mean, fill = month)) +  
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
  labs(x = "Distance (m)", y = NULL) +
  theme_bw() + theme(panel.grid = element_blank())
#do_mgL
month_sum %>% 
  filter(transect == "Main") %>% 
  filter(var == "do_mgl") %>% 
  ggplot(aes(x = month, y = mean, fill = month)) +  
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) 
#spc_uscm
month_sum %>% 
  filter(transect == "Main") %>% 
  filter(var == "temp_c") %>% 
  ggplot(aes(x = month, y = mean, fill = month)) +  
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) 

# Plot CV as a bar graph for each month for various variables
  #Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    ggplot(aes(x = month, y = CV, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, scales = "free_y")
  #Dead
  month_sum %>% 
    filter(transect == "Dead Creek") %>% 
    ggplot(aes(x = month, y = CV, fill = month)) + 
    scale_fill_manual(values=c("turquoise3", "mediumorchid1")) +
    geom_bar(stat = "identity") +
    facet_wrap(~var, scales = "free_y") 

#Plot CV bars for N species
  #Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("TN_mgNL", "PN", "DON", "NH4_mgNL", "NO3_mgNL")) %>%
    ggplot(aes(x = month, y = CV, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, ncol = 1) +
    labs(x = "Month", y = "CV %") +
    theme_bw() + theme(panel.grid = element_blank())
  #Dead
  month_sum %>% 
    filter(transect == "Dead Creek") %>% 
    filter(var %in% c("TN_mgNL", "PN", "DON", "NH4_mgNL", "NO3_mgNL")) %>%
    ggplot(aes(x = month, y = CV, fill = month)) +  
    scale_fill_manual(values=c("turquoise3", "mediumorchid1")) +
    geom_bar(stat = "identity") +
    facet_wrap(~var, scales = "free_y")

#Plot CV bars for P species
  #Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("TP_mgPL", "PP", "DOP", "PO4_mgPL")) %>% 
    ggplot(aes(x = month, y = CV, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, ncol = 1) +
    labs(x = "Month", y = "CV %") +
    theme_bw() + theme(panel.grid = element_blank())
  #Dead
  month_sum %>% 
    filter(transect == "Dead Creek") %>% 
    filter(var %in% c("TP_mgPL", "PP", "DOP", "PO4_mgPL")) %>% 
    ggplot(aes(x = month, y = CV, fill = month)) +  
    scale_fill_manual(values=c("turquoise3", "mediumorchid1")) +
    geom_bar(stat = "identity") +
    facet_wrap(~var, scales = "free_y")
  
#Plot CV bars for other physical variables
  #Main w/ DO as conc.
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("do_mgl", "ph", "spc_uscm", "temp_c")) %>% 
    ggplot(aes(x = month, y = CV, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, ncol = 1, scales = "free_y", labeller = labeller(var = sensor_labels)) +
    labs(x = "Distance (m)", y = NULL) +
    theme_bw() + theme(panel.grid = element_blank())
  
  # Main w/ DO as % sat. AND y-axis scale is the same
  # Also, I've switched CV from proportion to % here
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("do_per", "ph", "spc_uscm", "temp_c")) %>% 
    # Re-order the variables so that temperature is plotted on top
    mutate(var = factor(var, levels = c("temp_c", "do_per", "ph", "spc_uscm"),
                             # I provide labels here for the facetted plots that include symbols and then use labeller = label_parsed below to parse them into label text
                             labels = c("Temperature~(degree*C)", "DO~('%'*~saturation)", "pH", "Specific~conductivity~(mu*S~cm^{-1})"))) %>% 
    # Provide abbreviated month labels for legend instead of numbers
    mutate(month = factor(month, levels = c("5", "7", "8", "10"),
                                 labels = c("May", "Jul", "Aug", "Oct"))) %>%
    # Note here I plot CV as % using CV_per
    ggplot(aes(x = month, y = CV_per, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, ncol = 1, labeller = label_parsed) +
    labs(x = "Distance (m)", y = "Coefficient of variation (%)") +
    theme_bw() + 
    theme(panel.grid = element_blank(),
          # Get rid of legend title
          legend.title = element_blank())  
    
  #Dead
  month_sum %>% 
    filter(transect == "Dead Creek") %>% 
    filter(var %in% c("do_mgl", "ph", "spc_uscm", "temp_c", "tss_mgL", "NPOC_mgCL")) %>% 
    ggplot(aes(x = month, y = CV, fill = month)) +  
    scale_fill_manual(values=c("turquoise3", "mediumorchid1")) +
    geom_bar(stat = "identity") +
    facet_wrap(~var, scales = "free_y")
  
#Plot CV bar for DOC
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% "NPOC_mgCL") %>% 
    ggplot(aes(x = month, y = CV, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, ncol = 1) +
    labs(x = "Month", y = "CV %") +
    theme_bw() + theme(panel.grid = element_blank())
  
#Plot CV bar for TSS
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% "tss_mgL") %>% 
    ggplot(aes(x = month, y = CV, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, ncol = 1) +
    labs(x = "Month", y = "CV %") +
    theme_bw() + theme(panel.grid = element_blank())
  
# Nutrient Ratios
  #Across Main transect
  #TN/TP, DIN/SRP, DOC/TN
  comb %>% 
    gather(key = "var", value = "conc", c(TN_TP, DIN_SRP, DOC_TN)) %>% 
    filter(transect == "Main") %>% 
    filter(!is.na(conc)) %>% 
    ggplot(aes(x = dist_cum_m, y = conc, group = month, color = month)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = 6750) +
    facet_wrap(~var, scales = "free_y", ncol = 1) +
    theme_bw() + theme(panel.grid = element_blank())
  #DOC/TP, DOC/DIN, DOC/SRP
  comb %>% 
    gather(key = "var", value = "conc", c(DOC_TP, DOC_DIN, DOC_SRP)) %>% 
    filter(transect == "Main") %>% 
    filter(!is.na(conc)) %>% 
    ggplot(aes(x = dist_cum_m, y = conc, group = month, color = month)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = 6750) +
    facet_wrap(~var, scales = "free_y", ncol = 1) +
    theme_bw() + theme(panel.grid = element_blank())
  #Nutrient ratios Bar Graph
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("TN_TP", "DIN_SRP", "DOC_TN", "DOC_TP", "DOC_DIN", "DOC_SRP")) %>%
    ggplot(aes(x = month, y = mean, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, scales = "free_y", ncol = 2) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .3) 
  #CV's
  #Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("TN_TP", "DIN_SRP", "DOC_TN", "DOC_TP", "DOC_DIN", "DOC_SRP")) %>% 
    ggplot(aes(x = month, y = CV, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, scales = "free_y")
  

# Metals!
  #Main
  comb %>% 
    gather(key = "var", value = "conc", c(Al_ppb_0.45, Si_ppb_0.45, Mn_ppb_0.45, Fe_ppb_0.45)) %>% 
    filter(transect == "Main") %>% 
    filter(!is.na(conc)) %>% 
    ggplot(aes(x = dist_cum_m, y = conc, group = month, color = month)) +
      geom_point() +
      geom_line() +
    geom_vline(xintercept = 6750) +
    facet_wrap(~var, scales = "free_y", ncol = 1) +
    theme_bw() + theme(panel.grid = element_blank())
  
    #Dead
  comb %>% 
    gather(key = "var", value = "conc", c(Al_ppb_0.45, Si_ppb_0.45, Mn_ppb_0.45, Fe_ppb_0.45)) %>% 
    filter(transect == "Dead Creek") %>% 
    filter(!is.na(conc)) %>% 
    ggplot(aes(x = dist_cum_m, y = conc, group = month, color = month)) +
    geom_point() +
    geom_line() +
    facet_wrap(~var, scales = "free_y", ncol = 1) +
    theme_bw() + theme(panel.grid = element_blank())
  #Means
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("Al_ppb_0.45", "Si_ppb_0.45", "Mn_ppb_0.45", "Fe_ppb_0.45")) %>% 
    ggplot(aes(x = month, y = mean, fill = month)) +  
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    facet_wrap(~var, scales = "free_y")
  #CV's
  #Main
  month_sum %>% 
    filter(transect == "Main") %>% 
    filter(var %in% c("Al_ppb_0.45", "Si_ppb_0.45", "Mn_ppb_0.45", "Fe_ppb_0.45")) %>% 
    ggplot(aes(x = month, y = CV, fill = month)) +  
    geom_bar(stat = "identity") +
    facet_wrap(~var, scales = "free_y")
  
