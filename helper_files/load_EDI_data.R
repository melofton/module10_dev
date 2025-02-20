# Title: Load EDI data for Module 9
# Author: Mary Lofton
# Date: 20FEB23

# Purpose: Load most recent EDI packages for Module 9 data updates

library(tidyverse)
library(lubridate)

# BVR catwalk ----

# Package ID: edi.725.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Time series of high-frequency sensor data measuring water temperature, dissolved oxygen, conductivity, specific conductance, total dissolved solids, chlorophyll a, phycocyanin, fluorescent dissolved organic matter, and turbidity at discrete depths, and water level in Beaverdam Reservoir, Virginia, USA in 2009-2024.
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Adrienne Breef-Pilz - Virginia Tech 
# Contact:  Cayelan Carey -  Virginia Tech  - cayelan@vt.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "ThermistorTemp_C_1",     
                 "ThermistorTemp_C_2",     
                 "ThermistorTemp_C_3",     
                 "ThermistorTemp_C_4",     
                 "ThermistorTemp_C_5",     
                 "ThermistorTemp_C_6",     
                 "ThermistorTemp_C_7",     
                 "ThermistorTemp_C_8",     
                 "ThermistorTemp_C_9",     
                 "ThermistorTemp_C_10",     
                 "ThermistorTemp_C_11",     
                 "ThermistorTemp_C_12",     
                 "ThermistorTemp_C_13",     
                 "RDO_mgL_6",     
                 "RDOsat_percent_6",     
                 "RDOTemp_C_6",     
                 "RDO_mgL_13",     
                 "RDOsat_percent_13",     
                 "RDOTemp_C_13",     
                 "EXOTemp_C_1.5",     
                 "EXOCond_uScm_1.5",     
                 "EXOSpCond_uScm_1.5",     
                 "EXOTDS_mgL_1.5",     
                 "EXODOsat_percent_1.5",     
                 "EXODO_mgL_1.5",     
                 "EXOChla_RFU_1.5",     
                 "EXOChla_ugL_1.5",     
                 "EXOBGAPC_RFU_1.5",     
                 "EXOBGAPC_ugL_1.5",     
                 "EXOfDOM_RFU_1.5",     
                 "EXOfDOM_QSU_1.5",     
                 "EXOTurbidity_FNU_1.5",     
                 "EXOPressure_psi",     
                 "EXODepth_m",     
                 "EXOBattery_V",     
                 "EXOCablepower_V",     
                 "EXOWiper_V",     
                 "LvlPressure_psi_13",     
                 "LvlDepth_m_13",     
                 "LvlTemp_C_13",     
                 "RECORD",     
                 "CR6Battery_V",     
                 "CR6Panel_Temp_C",     
                 "Flag_ThermistorTemp_C_1",     
                 "Flag_ThermistorTemp_C_2",     
                 "Flag_ThermistorTemp_C_3",     
                 "Flag_ThermistorTemp_C_4",     
                 "Flag_ThermistorTemp_C_5",     
                 "Flag_ThermistorTemp_C_6",     
                 "Flag_ThermistorTemp_C_7",     
                 "Flag_ThermistorTemp_C_8",     
                 "Flag_ThermistorTemp_C_9",     
                 "Flag_ThermistorTemp_C_10",     
                 "Flag_ThermistorTemp_C_11",     
                 "Flag_ThermistorTemp_C_12",     
                 "Flag_ThermistorTemp_C_13",     
                 "Flag_RDO_mgL_6",     
                 "Flag_RDOsat_percent_6",     
                 "Flag_RDOTemp_C_6",     
                 "Flag_RDO_mgL_13",     
                 "Flag_RDOsat_percent_13",     
                 "Flag_RDOTemp_C_13",     
                 "Flag_EXOTemp_C_1.5",     
                 "Flag_EXOCond_uScm_1.5",     
                 "Flag_EXOSpCond_uScm_1.5",     
                 "Flag_EXOTDS_mgL_1.5",     
                 "Flag_EXODOsat_percent_1.5",     
                 "Flag_EXODO_mgL_1.5",     
                 "Flag_EXOChla_RFU_1.5",     
                 "Flag_EXOChla_ugL_1.5",     
                 "Flag_EXOBGAPC_RFU_1.5",     
                 "Flag_EXOBGAPC_ugL_1.5",     
                 "Flag_EXOfDOM_RFU_1.5",     
                 "Flag_EXOfDOM_QSU_1.5",     
                 "Flag_EXOTurbidity_FNU_1.5",     
                 "Flag_EXOPressure_psi",     
                 "Flag_EXODepth_m",     
                 "Flag_EXOBattery_V",     
                 "Flag_EXOCablepower_V",     
                 "Flag_EXOWiper_V",     
                 "Flag_LvlPressure_psi_13",     
                 "Flag_LvlTemp_C_13"    ), check.names=TRUE)

unlink(infile1)

dt2 <- read.csv("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")

dt1 <- bind_rows(dt1, dt2)

mycols <- c("Reservoir",     
            "Site",     
            "DateTime",     
            "ThermistorTemp_C_1",     
            "ThermistorTemp_C_2",     
            "ThermistorTemp_C_3",     
            "ThermistorTemp_C_4",     
            "ThermistorTemp_C_5",     
            "ThermistorTemp_C_6",     
            "ThermistorTemp_C_7",     
            "ThermistorTemp_C_8",     
            "ThermistorTemp_C_9",     
            "ThermistorTemp_C_10",     
            "ThermistorTemp_C_11",     
            "ThermistorTemp_C_12",     
            "ThermistorTemp_C_13", 
            "RDO_mgL_6",     
            "RDO_mgL_13", 
            "EXODO_mgL_1.5",  
            "EXOChla_ugL_1.5",  
            "EXOTurbidity_FNU_1.5",
            "EXOfDOM_QSU_1.5"     
            )

bvr <- dt1 %>%
  #filter(year(DateTime) %in% c(2024)) %>%
  select(any_of(mycols)) %>%
  mutate(Date = date(DateTime)) %>%
  select(-DateTime) %>%
  group_by(Reservoir, Site, Date) %>%
  summarise_all(list(mean), na.rm = TRUE) %>%
  ungroup() %>%
  add_column(site_id = "bvre") %>%
  pivot_longer(ThermistorTemp_C_1:EXOfDOM_QSU_1.5, names_to = "var", values_to = "observation") %>%
  separate(var, into = c("variable","unit","depth_m"), sep = "_") %>%
  rename(datetime = Date) %>%
  mutate(variable = ifelse(grepl("Temp", variable),"Temp_C_mean",
                           ifelse(grepl("fDOM",variable),"fDOM_QSU_mean",
                                  ifelse(grepl("Chla",variable),"Chla_ugL_mean",
                                         ifelse(grepl("Turb",variable),"Turbidity_FNU_mean","DO_mgL_mean")))),
         depth_m = as.numeric(depth_m)) %>%
  select(site_id, datetime, depth_m, variable, observation)

check <- bvr %>%
  filter(variable == "fDOM_QSU_mean")

ggplot(data = bvr, aes(x = datetime, y = observation, group = depth_m,
                       color = depth_m))+
  geom_line()+
  facet_wrap(facets = vars(variable))+
  theme_bw()
  

# FCR catwalk ----

# Package ID: edi.271.9 Cataloging System:https://pasta.edirepository.org.
# Data set title: Time series of high-frequency sensor data measuring water temperature, dissolved oxygen, pressure, conductivity, specific conductance, total dissolved solids, chlorophyll a, phycocyanin, fluorescent dissolved organic matter, and turbidity at discrete depths in Falling Creek Reservoir, Virginia, USA in 2018-2024.
# Data set creator:  Cayelan Carey - Virginia Tech 
# Data set creator:  Adrienne Breef-Pilz - Virginia Tech 
# Contact:  Cayelan Carey -  Virginia Tech  - cayelan@vt.edu
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/9/f23d27b67f71c25cb8e6232af739f986" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "ThermistorTemp_C_surface",     
                 "ThermistorTemp_C_1",     
                 "ThermistorTemp_C_2",     
                 "ThermistorTemp_C_3",     
                 "ThermistorTemp_C_4",     
                 "ThermistorTemp_C_5",     
                 "ThermistorTemp_C_6",     
                 "ThermistorTemp_C_7",     
                 "ThermistorTemp_C_8",     
                 "ThermistorTemp_C_9",     
                 "RDO_mgL_5",     
                 "RDOsat_percent_5",     
                 "RDO_mgL_5_adjusted",     
                 "RDOsat_percent_5_adjusted",     
                 "RDOTemp_C_5",     
                 "RDO_mgL_9",     
                 "RDOsat_percent_9",     
                 "RDO_mgL_9_adjusted",     
                 "RDOsat_percent_9_adjusted",     
                 "RDOTemp_C_9",     
                 "EXOTemp_C_1",     
                 "EXOCond_uScm_1",     
                 "EXOSpCond_uScm_1",     
                 "EXOTDS_mgL_1",     
                 "EXODOsat_percent_1",     
                 "EXODO_mgL_1",     
                 "EXOChla_RFU_1",     
                 "EXOChla_ugL_1",     
                 "EXOBGAPC_RFU_1",     
                 "EXOBGAPC_ugL_1",     
                 "EXOfDOM_RFU_1",     
                 "EXOfDOM_QSU_1",     
                 "EXOTurbidity_FNU_1",     
                 "EXOPressure_psi",     
                 "EXODepth_m",     
                 "EXOBattery_V",     
                 "EXOCablepower_V",     
                 "EXOWiper_V",     
                 "LvlPressure_psi_9",     
                 "LvlTemp_C_9",     
                 "LvlDepth_m_9",     
                 "RECORD",     
                 "CR6Battery_V",     
                 "CR6Panel_Temp_C",     
                 "Flag_ThermistorTemp_C_surface",     
                 "Flag_ThermistorTemp_C_1",     
                 "Flag_ThermistorTemp_C_2",     
                 "Flag_ThermistorTemp_C_3",     
                 "Flag_ThermistorTemp_C_4",     
                 "Flag_ThermistorTemp_C_5",     
                 "Flag_ThermistorTemp_C_6",     
                 "Flag_ThermistorTemp_C_7",     
                 "Flag_ThermistorTemp_C_8",     
                 "Flag_ThermistorTemp_C_9",     
                 "Flag_RDO_mgL_5",     
                 "Flag_RDOsat_percent_5",     
                 "Flag_RDOTemp_C_5",     
                 "Flag_RDO_mgL_9",     
                 "Flag_RDOsat_percent_9",     
                 "Flag_RDOTemp_C_9",     
                 "Flag_EXOTemp_C_1",     
                 "Flag_EXOCond_uScm_1",     
                 "Flag_EXOSpCond_uScm_1",     
                 "Flag_EXOTDS_mgL_1",     
                 "Flag_EXODOsat_percent_1",     
                 "Flag_EXODO_mgL_1",     
                 "Flag_EXOChla_RFU_1",     
                 "Flag_EXOChla_ugL_1",     
                 "Flag_EXOBGAPC_RFU_1",     
                 "Flag_EXOBGAPC_ugL_1",     
                 "Flag_EXOfDOM_RFU_1",     
                 "Flag_EXOfDOM_QSU_1",     
                 "Flag_EXOTurbidity_FNU_1",     
                 "Flag_EXOPressure_psi",     
                 "Flag_EXODepth_m",     
                 "Flag_EXOBattery_V",     
                 "Flag_EXOCablepower_V",     
                 "Flag_EXOWiper_V",     
                 "Flag_LvlPressure_psi_9",     
                 "Flag_LvlTemp_C_9"    ), check.names=TRUE)

unlink(infile1)

dt2 <- read.csv("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

dt1 <- bind_rows(dt1, dt2)

mycols <- c("Reservoir",     
            "Site",     
            "DateTime",     
            "ThermistorTemp_C_surface",     
            "ThermistorTemp_C_1",     
            "ThermistorTemp_C_2",     
            "ThermistorTemp_C_3",     
            "ThermistorTemp_C_4",     
            "ThermistorTemp_C_5",     
            "ThermistorTemp_C_6",     
            "ThermistorTemp_C_7",     
            "ThermistorTemp_C_8",     
            "ThermistorTemp_C_9",     
            "RDO_mgL_5_adjusted",     
            "RDO_mgL_9_adjusted",   
            "EXODO_mgL_1",   
            "EXOChla_ugL_1",
            "EXOTurbidity_FNU_1",
            "EXOfDOM_QSU_1"     
)

fcr <- dt1 %>%
  #filter(year(DateTime) %in% c(2025)) %>%
  select(any_of(mycols)) %>%
  mutate(Date = date(DateTime)) %>%
  select(-DateTime) %>%
  group_by(Reservoir, Site, Date) %>%
  summarise_all(list(mean), na.rm = TRUE) %>%
  ungroup() %>%
  add_column(site_id = "fcre") %>%
  pivot_longer(ThermistorTemp_C_surface:EXOfDOM_QSU_1, names_to = "var", values_to = "observation") %>%
  separate(var, into = c("variable","unit","depth_m","method"), sep = "_") %>%
  rename(datetime = Date) %>%
  mutate(variable = ifelse(grepl("Temp", variable),"Temp_C_mean",
                           ifelse(grepl("fDOM",variable),"fDOM_QSU_mean",
                                  ifelse(grepl("Chla",variable),"Chla_ugL_mean",
                                         ifelse(grepl("Turb",variable),"Turbidity_FNU_mean","DO_mgL_mean")))),
         depth_m = ifelse(depth_m == "surface", 0.1, as.numeric(depth_m))) %>%
  select(site_id, datetime, depth_m, variable, observation)

ggplot(data = fcr, aes(x = datetime, y = observation, group = depth_m,
                       color = depth_m))+
  geom_line()+
  facet_wrap(facets = vars(variable), scales = "free_y")+
  theme_bw()

reservoir_data <- bind_rows(bvr, fcr) %>%
  filter(datetime <= "2025-01-01")

write.csv(reservoir_data, "./data/reservoir_data.csv", row.names = FALSE)

fdom <- reservoir_data %>%
  filter(site_id == "bvre" & variable == "fDOM_QSU_mean" & year(datetime) == 2022) %>%
  ggplot()+
  geom_line(aes(x = datetime, y = observation))+
  ylab("fDOM (QSU)")+
  xlab("Date")+
  theme_classic()
fdom
