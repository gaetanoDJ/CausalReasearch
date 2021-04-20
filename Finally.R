library(did)
library(haven)
library(dplyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(stats)
library(collapse)
library(xlsx)
ucr_meth <- read_dta("G:/Causal Inference/Data/ucr_meth.dta")
UCR <- zap_formats(ucr_meth)
UCR<- zap_label(UCR)



remove <- c("AL", "AK", "AZ", "AR", "CA", "CT", "DC","DE", "HI", "ME", "MA", "MS", "NH", "NJ", "NY", "OR", "PA", "RI", "VT", "WA", "WY", "KY")
UCR <- UCR[!UCR$stname %in% remove,]
UCR <- UCR[complete.cases(UCR), ]


UCRQuarter <- UCR %>%
  mutate(Quarter = ceiling(as.numeric(UCR$month) / 3))

UCRTEST <- collapse::collap(UCRQuarter, ~ stname + year + Quarter)

write.xlsx(UCRTEST, file = "G:/Causal Inference/Data/CleanUP.xlsx")
CleanUP <- read_excel("G:/Causal Inference/Data/CleanUP.xlsx")
CleanUPWork <- collap(CleanUP, ~ stname + year, custom = list(fmean= 22, fmean= 1, fsum = 4:21, fsum = 23))

CleanUPWork$treated <- ifelse(CleanUPWork$treated>1,1,0)




############################## rape
attrape_pc <- att_gt(yname = "rape_pc", # LHS variable
               tname = "year", # time variable
               idname = "statefip", # id variable
               gname = "First.Treat", # first treatment period variable
               data = CleanUPWork, # data
               xformla = NULL, # no covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE,
               clustervars = "statefip", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional

# Aggregate ATT
agg_effects <- aggte(attrape_pc, type = "group")
summary(agg_effects)

# Group-time ATTs
summary(attrape_pc)

# Plot group-time ATTs
ggdid(attrape_pc)

# Event-study
agg_effects_es <- aggte(attrape_pc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_es)

# Plot event-study coefficients
ggdid(agg_effects_es)




############################## larceny
attlarceny_pc <- att_gt(yname = "larceny_pc", # LHS variable
                  tname = "year", # time variable
                  idname = "statefip", # id variable
                  gname = "First.Treat", # first treatment period variable
                  data = CleanUPWork, # data
                  xformla = NULL, # no covariates
                  #xformla = ~ l_police, # with covariates
                  est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                  control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                  bstrap = TRUE, # if TRUE compute bootstrapped SE
                  biters = 1000, # number of bootstrap iterations
                  print_details = FALSE,
                  clustervars = "statefip", # cluster level
                  panel = TRUE) # whether the data is panel or repeated cross-sectional

agg_effects1 <- aggte(attlarceny_pc, type = "group")
summary(agg_effects1)

# Group-time ATTs you want this 
summary(attlarceny_pc)

# Plot group-time ATTs
ggdid(attlarceny_pc)

# Event-study you want this 
agg_effects_es1 <- aggte(attlarceny_pc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_es1)

# Plot event-study coefficients
ggdid(agg_effects_es1)                

# Create table of estimates, particular outcomes, row = estimated att
# table violent, table table non-violent
# what do they recommend for the group specifc att
# work in terms of _pc

############################## vehicle
attvehicle_pc <- att_gt(yname = "vehicle_pc", # LHS variable
                        tname = "year", # time variable
                        idname = "statefip", # id variable
                        gname = "First.Treat", # first treatment period variable
                        data = CleanUPWork, # data
                        xformla = NULL, # no covariates
                        #xformla = ~ l_police, # with covariates
                        est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                        control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                        bstrap = TRUE, # if TRUE compute bootstrapped SE
                        biters = 1000, # number of bootstrap iterations
                        print_details = FALSE,
                        clustervars = "statefip", # cluster level
                        panel = TRUE) # whether the data is panel or repeated cross-sectional

agg_effects2 <- aggte(attvehicle_pc, type = "group")
summary(agg_effects2)

# Group-time ATTs you want this 
summary(attvehicle_pc)

# Plot group-time ATTs
ggdid(attvehicle_pc)

# Event-study you want this 
agg_effects_es2 <- aggte(attvehicle_pc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_es2)

# Plot event-study coefficients
ggdid(agg_effects_es2)  

############################## burglary
attburglary_pc <- att_gt(yname = "burglary_pc", # LHS variable
                        tname = "year", # time variable
                        idname = "statefip", # id variable
                        gname = "First.Treat", # first treatment period variable
                        data = CleanUPWork, # data
                        xformla = NULL, # no covariates
                        #xformla = ~ l_police, # with covariates
                        est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                        control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                        bstrap = TRUE, # if TRUE compute bootstrapped SE
                        biters = 1000, # number of bootstrap iterations
                        print_details = FALSE,
                        clustervars = "statefip", # cluster level
                        panel = TRUE) # whether the data is panel or repeated cross-sectional

agg_effects3 <- aggte(attburglary_pc, type = "group")
summary(agg_effects3)

# Group-time ATTs you want this 
summary(attburglary_pc)

# Plot group-time ATTs
ggdid(attburglary_pc)

# Event-study you want this 
agg_effects_es3 <- aggte(attburglary_pc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_es3)

# Plot event-study coefficients
ggdid(agg_effects_es3) 

############################## manslaughter
attmanslaughter_pc <- att_gt(yname = "manslaughter_pc", # LHS variable
                         tname = "year", # time variable
                         idname = "statefip", # id variable
                         gname = "First.Treat", # first treatment period variable
                         data = CleanUPWork, # data
                         xformla = NULL, # no covariates
                         #xformla = ~ l_police, # with covariates
                         est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                         control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                         bstrap = TRUE, # if TRUE compute bootstrapped SE
                         biters = 1000, # number of bootstrap iterations
                         print_details = FALSE,
                         clustervars = "statefip", # cluster level
                         panel = TRUE) # whether the data is panel or repeated cross-sectional

agg_effects4 <- aggte(attmanslaughter_pc, type = "group")
summary(agg_effects4)

# Group-time ATTs you want this 
summary(attmanslaughter_pc)

# Plot group-time ATTs
ggdid(attmanslaughter_pc)

# Event-study you want this 
agg_effects_es4 <- aggte(attmanslaughter_pc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_es4)

# Plot event-study coefficients
ggdid(agg_effects_es4)  

############################## robbery
attrobbery_pc <- att_gt(yname = "robbery_pc", # LHS variable
                             tname = "year", # time variable
                             idname = "statefip", # id variable
                             gname = "First.Treat", # first treatment period variable
                             data = CleanUPWork, # data
                             xformla = NULL, # no covariates
                             #xformla = ~ l_police, # with covariates
                             est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                             control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                             bstrap = TRUE, # if TRUE compute bootstrapped SE
                             biters = 1000, # number of bootstrap iterations
                             print_details = FALSE,
                             clustervars = "statefip", # cluster level
                             panel = TRUE) # whether the data is panel or repeated cross-sectional

agg_effects5 <- aggte(attrobbery_pc, type = "group")
summary(agg_effects5)

# Group-time ATTs you want this 
summary(attrobbery_pc)

# Plot group-time ATTs
ggdid(attrobbery_pc)

# Event-study you want this 
agg_effects_es5 <- aggte(attrobbery_pc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_es5)

# Plot event-study coefficients
ggdid(agg_effects_es5) 

############################## murder
attmurder_pc<- att_gt(yname = "murder_pc", # LHS variable
                        tname = "year", # time variable
                        idname = "statefip", # id variable
                        gname = "First.Treat", # first treatment period variable
                        data = CleanUPWork, # data
                        xformla = NULL, # no covariates
                        #xformla = ~ l_police, # with covariates
                        est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                        control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
                        bstrap = TRUE, # if TRUE compute bootstrapped SE
                        biters = 1000, # number of bootstrap iterations
                        print_details = FALSE,
                        clustervars = "statefip", # cluster level
                        panel = TRUE) # whether the data is panel or repeated cross-sectional

agg_effects6 <- aggte(attmurder_pc, type = "group")
summary(agg_effects6)

# Group-time ATTs you want this 
summary(attmurder_pc)

# Plot group-time ATTs
ggdid(attmurder_pc)

# Event-study you want this 
agg_effects_es6 <- aggte(attmurder_pc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_es6)

# Plot event-study coefficients
ggdid(agg_effects_es6) 
