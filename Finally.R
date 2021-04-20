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
CleanUPWork <- collap(CleanUP, ~ stname + year)

CleanUPWork$treated <- ifelse(CleanUPWork$treated>0,1,0)
CleanUPWork$l_rape <- log(CleanUPWork$rape)




atts <- att_gt(yname = "l_rape", # LHS variable
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

agg_effects <- aggte(atts, type = "group", na.rm = TRUE)
summary(agg_effects)
summary(atts)


