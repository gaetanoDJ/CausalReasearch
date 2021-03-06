library(did)
library(haven)
library(dplyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(stats)
library(collapse)
library(xlsx)
library(kableExtra)

# ucr_meth <- read_dta("G:/Causal Inference/Data/ucr_meth.dta")
# ucr_meth <- read_dta("C:/Users/gaeta/Documents/GitHub/CausalReasearch/ucr_meth.dta")
UCR <- zap_formats(ucr_meth)
UCR<- zap_label(UCR)



remove <- c("AL", "AK", "AZ", "AR", "CA", "CT", "DC","DE", "HI", "ME", "MA", "MS", "NH", "NJ", "NY", "OR", "PA", "RI", "VT", "WA", "WY", "KY")
UCR <- UCR[!UCR$stname %in% remove,]
UCR <- UCR[complete.cases(UCR), ]


UCRQuarter <- UCR %>%
  mutate(Quarter = ceiling(as.numeric(UCR$month) / 3))

UCRTEST <- collapse::collap(UCRQuarter, ~ stname + year + Quarter)

write.xlsx(UCRTEST, file = "G:/Causal Inference/Data/CleanUP1.xlsx")


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






# Group-time ATTs
summary(attrape_pc)


GroupRape <- c(attrape_pc$group)
TimeRape <- c(attrape_pc$t)
ATTRape <- c(attrape_pc$att)
Rapese <- c(attrape_pc$se)
CILowerRape <- c((attrape_pc$att - 2.565*(sd(attrape_pc$att)/sqrt(6))))
CIHIgherRape <- c((attrape_pc$att + 2.565* (sd(attrape_pc$att)/sqrt(6))))

GTATERape <- data.frame(GroupRape,TimeRape,ATTRape,Rapese,CILowerRape,CIHIgherRape)

# Plot group-time ATTs

ggdid(attrape_pc, title = "Group-Time ATT for Rape") 
ggsave("attrape_pc.png")

# Event-study
agg_effects_es <- aggte(attrape_pc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_es)


rape <- cbind(c(round(agg_effects_es$overall.att, digits = 3), "(-9.37, 2.67)"))
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
larceny <- cbind(c(round(agg_effects_es1$overall.att, digits = 3), "(14.11, 227.28)*"))
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
vehicle <- cbind(c(round(agg_effects_es2$overall.att, digits = 3), "(-165.03, 354.94)"))
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
burglary <- cbind(c(round(agg_effects_es3$overall.att, digits = 3), "(-37.72, 67.14)"))
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
manslaughter <- cbind(c(round(agg_effects_es4$overall.att, digits = 3), "(-0.024, 0.076)"))
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
robbery <- cbind(c(round(agg_effects_es4$overall.att, digits = 3), "(-4.37, 13.57)"))
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
ggsave("attmurder_pc1.png", height = )

# Event-study you want this 
agg_effects_es6 <- aggte(attmurder_pc, type = "dynamic", na.rm = TRUE)
summary(agg_effects_es6)

murder <- cbind(c(round(agg_effects_es6$overall.att, digits = 3), "(-1.05,0.31)"))

# Plot event-study coefficients
ggdid(agg_effects_es6) 


Description <- rbind("Overall ATT", "95% CI")
Table <- cbind(Description,Murder = c(murder), Manslaughter = c(manslaughter), Rape = c(rape), Vehicle = c(vehicle), Robbery = c(robbery), Larceny = c(larceny), Burglary = c(burglary))




kbl(Table, caption = "Minimum Wage Aggreagted Treatment Effect Estimates", booktabs =T)%>%
add_header_above(c(" ","Violent Crimes"=3,"Non-Violent Crimes"=4))  %>%
  kable_styling(latex_options =c("striped","hold_position")) %>%
  row_spec(0:2,align = "c")

ggdid(attmurder_pc, title = "Average Group Treatment Effect for Murder Treated in")
ggsave("attmurder_pc.png", height = 9)
ggdid(attrobbery_pc, title = "Average Group Treatment Effect for Robbery Treated in")
ggsave("attrobbery_pc.png", height = 9)
ggdid(attmanslaughter_pc, title = "Average Group Treatment Effect for Manslaughter Treated in")
ggsave("attmanslaughter_pc.png", height = 9)
ggdid(attburglary_pc, title = "Average Group Treatment Effect for Burglary Treated in")
ggsave("attburglary_pc.png", height = 9)
ggdid(attvehicle_pc, title = "Average Group Treatment Effect for Vehicle Treated in")
ggsave("attvehicle_pc.png", height = 9)
ggdid(attlarceny_pc, title = "Average Group Treatment Effect for Larceny Treated in")
ggsave("attlarceny_pc.png", height = 9)
ggdid(attrape_pc, title = "Average Group Treatment Effect for Rape Treated in")
ggsave("attrape_pc.png", height = 9)
