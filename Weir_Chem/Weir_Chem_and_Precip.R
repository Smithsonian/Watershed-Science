## SERC all chem
# packages
library(naniar)
library(ggplot2)
library(tsbox)
library(zoo)
library(cowplot)
library(ggpubr)
library(lubridate)
library(tidyverse)
library(EnvStats)
library(trend)
library(mgcv)
library(ggrepel)
library(factoextra)
library(ggcorrplot)
library(multcompView)
library(vegan)
library(data.table)

# Set working directory for file access
setwd("~/ActiveProjects/StreamProgram/Data/analyses/1_long term trend analysis")

# Load data
precipchem <- read.csv("C:/Users//Downloads/Monthly_ave_precipchem_1981_2022.csv") # Precipitation chemistry
weirchem <- read.csv("C:/Users//Downloads/weirs_chempca_results.csv") # Weir chemistry results

#### Discharge ####
# data by weir
w101 <- subset(weirchem, Sta == "w101") 
w102 <- subset(weirchem, Sta == "w102") 
w103 <- subset(weirchem, Sta == "w103") 
w109 <- subset(weirchem, Sta == "w109") 
w110 <- subset(weirchem, Sta == "w110") 

# list of weirs to loop over
weirs <- c("w101", "w102", "w103", "w109", "w110")

# column headers for table
header <- c("Site", "type", "Parameter", "DataResolution", "units", 
            "Test", "Tau", "Sen's Slope", "X2", "Z (trend)", "period of record")

# empty data frame for storing trend test results
results <- data.frame(matrix(ncol = 11, nrow = 0))
names(results) <- header

# split parameters into categories for looping 
d_params <- c("monthlyflow", "PC1", "PC2", "org_mg_l", "nh4_ug_l",
              "on_ug_l", "tn_ug_l", "po4_ug_l", "tp_ug_l", "no3_ug_N_l_composite", "so4_mg_S_l_composite")
d_units <- c("cfs", "unitless", "unitless", "mg/L", "ug/L",
             "ug/L", "ug/L", "ug/L", "ug/L", "ug/L", "ug/L")
#flux
f_params <- c("monthlynh4_g_ha", "monthlyno3_g_ha", "monthlytn_g_ha", "monthlyon_g_ha",
              "monthlytp_g_ha", "monthlyorg_kg_ha")
f_units <- c("g/ha", "g/ha", "g/ha", "g/ha", "g/ha", "kg/ha")

#precip
p_params <- c("monthlyprecip_cm", "mintempC", "maxtempC")
p_units <- c("cm", "degC", "degC")

# loop over weir sites
for (w in weirs) {
  ind = 0 # for connecting units and parameters
  for (p in d_params) {
    ind <- ind + 1
    kstt <- kendallSeasonalTrendTest(as.formula(paste0(p, "~ month + year")), get(w)) # trend test
    newline <- c(w, "Concentration", p, "monthly", d_units[ind], "Seasonal Kendall",
                 kstt$estimate[[1]], kstt$estimate[[2]], kstt$p.value[[1]], kstt$p.value[[2]], 
                 paste0(min(get(w)$year), "-", max(get(w)$year))) # results
    results <- rbind(results, newline) # append
  }
  
  ind = 0 # reset
  for (p in f_params) {
    ind <- ind + 1
    kstt <- kendallSeasonalTrendTest(as.formula(paste0(p, "~ month + year")), get(w))
    newline <- c(w, "Flux", p, "monthly", f_units[ind], "Seasonal Kendall",
                 kstt$estimate[[1]], kstt$estimate[[2]], kstt$p.value[[1]], kstt$p.value[[2]], 
                 paste0(min(get(w)$year), "-", max(get(w)$year)))
    results <- rbind(results, newline)
  }
  
  # only analyze precipitation ta one site
  if (w == "w101") {
    ind <- 0 #reset
    for (p in p_params) {
      ind <- ind + 1
      kstt <- kendallSeasonalTrendTest(as.formula(paste0(p, "~ month + year")), get(w))
      newline <- c("precip", "?", p, "monthly", p_units[ind], "Seasonal Kendall",
                   kstt$estimate[[1]], kstt$estimate[[2]], kstt$p.value[[1]], kstt$p.value[[2]],
                   paste0(min(get(w)$year), "-", max(get(w)$year)))
      results <- rbind(results, newline)
    }
  }
}

# precip chemistry separate
pc_params <- c("precip_no3_ug_l", "precip_so4_ug_l", "precip_ph")
pc_units <- c("ug/L", "ug/L", "pH")

ind <- 0 #reset
for (p in pc_params) {
  ind <- ind + 1
  kstt <- kendallSeasonalTrendTest(as.formula(paste0(p, "~ month + year")), precipchem)
  newline <- c("precipchem", "Concentration", p, "monthly", pc_units[ind], "Seasonal Kendall",
               kstt$estimate[[1]], kstt$estimate[[2]], kstt$p.value[[1]], kstt$p.value[[2]],
               paste0(min(precipchem$year), "-", max(precipchem$year)))
  results <- rbind(results, newline)
}

# save final results 
fwrite(results, paste0("C:/Users/",Sys.getenv("USERNAME"),"/Downloads/weir_n_precip_chem.csv"))
