# ******************************************************************************
# Program: 			  FE_TFR.R
# Purpose: 		    Code to compute Crude Birth Rate  
# Data inputs: 		IR & PR dataset
# Data outputs:		coded variables, and output on screen and in excel tables
# Author:				  Mahmoud Elkasabi
# Date last modified: June 2024 Ali Roghani
# ******************************************************************************
#   
# -----------------------------------------------------------------------------#
# # Indicators created in this file:
# CBR     "Crude birth rate"
# -----------------------------------------------------------------------------#
#
########################################################################################
# CBR
# TABLE 5.1
library(haven)
library(dplyr)
library(DHS.rates)
library(openxlsx)

IRdata<-read_dta("../Resources/NGIR8BDT2024/NGIR8BFL.dta")
PRdata<-read_dta("../Resources/NGPR8ADT2024/NGPR8AFL.dta")


IRdata_FERT <- (IRdata[, c("v021", "v022","v024", "v025", "v005", "v008","v011", "awfactt", "awfactu", "awfactr", "awfacte", "awfactw",
                           paste("b3_0", 1:9, sep=""), paste("b3_", 10:20, sep=""), "v106", "v190","sstate1")])
# # National and urban/rural ASFR from IR data
# ASFR <- as.data.frame(fert(IRdata_FERT,Indicator="asfr",EverMW = "Yes", AWFact = "awfactt"))
# ASFRur <- as.data.frame(fert(IRdata_FERT,Indicator="asfr", Class = "v025",EverMW = "Yes", AWFact = "awfactu"))

# National and state ASFR from IR data
ASFR <- as.data.frame(fert(IRdata_FERT,Indicator="asfr",EverMW = "Yes", AWFact = "awfactt"))
ASFRstate <- as.data.frame(fert(IRdata_FERT,Indicator="asfr", Class = "sstate1",EverMW = "Yes", AWFact = "awfactu"))




# prepare the data
PRdata <- PRdata %>% 
  filter(hv103 == 1) %>%  # select de facto population
  mutate(wt = hv005/1000000) %>%  # create weight
  mutate( agegroup = cut(hv105, breaks= c(0,15,20,25,30,35,40,45,50,999),right = FALSE)) # create age groups

# De facto population counts (national and by state) from PR data
hh_pop <- PRdata %>% 
  summarise(total = sum(wt, na.rm = TRUE))

hh_popstate <- PRdata %>% 
  group_by(shstate1) %>%
  summarise(total = sum(wt, na.rm = TRUE))
hh_popstate <- as.data.frame(hh_popstate)

# De facto women counts (national and by state) from PR data
# National counts by age groups
women_pop <- PRdata %>% 
  filter(hv105 >= 15 & hv105 <= 49 & hv104 == 2) %>%  # select de facto women 15-49
  group_by(agegroup) %>%
  summarise(total = sum(wt, na.rm = TRUE))

CBR_pop = (women_pop$total/hh_pop$total)*ASFR$ASFR

# state counts by age groups
women_popstate <- PRdata %>% 
  filter(hv105 >= 15 & hv105 <= 49 & hv104 == 2) %>%  # select de facto women 15-49
  group_by(agegroup, shstate1) %>%
  summarise(total = sum(wt, na.rm = TRUE))

women_popstate <- as.data.frame(women_popstate)
women_popstate <- merge(women_popstate, hh_popstate, by = "shstate1")

women_popstate$CBR_popstate <- (women_popstate$total.x/women_popstate$total.y) * ASFRstate$ASFR

CBR_kogi = sum(women_popstate$CBR_popstate[women_popstate$shstate1==3])
CBR_kaduna = sum(women_popstate$CBR_popstate[women_popstate$shstate1==15])
CBR_bayelsa = sum(women_popstate$CBR_popstate[women_popstate$shstate1==16])
CBR_lagos = sum(women_popstate$CBR_popstate[women_popstate$shstate1==33])

CBR_national = sum(women_popstate$CBR_popstate)
CBR  = sum(CBR_pop)

CBRres <- as.data.frame(cbind(CBR,CBR_bayelsa,CBR_kaduna,CBR_kogi,CBR_lagos))

write.xlsx(CBRres, "DHS_CBR_state_national_2024.xlsx", sheetName = "CBR")
