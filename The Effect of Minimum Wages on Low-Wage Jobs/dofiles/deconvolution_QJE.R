
#This R file is created by DC on 08/13/2018.
##################################################
###########      Deconvolution            ########
##################################################
rm(list = ls())
.libPaths( c( .libPaths(), "/Volumes/Dube 3TB HD/Data/R_lib_DC/") )
library("deamer")
library("dplyr")
library("haven")
library("foreach")
library("doSNOW")
cl<-makeCluster(6) 
registerDoSNOW(cl)

set.seed(12345)
setwd("/Volumes/Dube 3TB HD/Data/CDLZ_replication/")
data<-as_tibble(read_dta("CPS_nominal_1979onwards.dta"))
for_measurement_error<-as_tibble(read_dta("ML_results_pooled_all_CPS.dta"))
data$group <- data %>% group_by(statenum, quarterdate) %>% group_indices
cfun <- function(a, b) NULL
a<-summary(data$group)[[6]]
foreach(groupno=1:a, .packages = c("dplyr", "deamer", "haven"), .combine="cfun") %dopar% {
  data_temp<-filter(data, group==groupno & data$wage>0)
  year=summary(data_temp$year)[[1]]
  error_var = (for_measurement_error$se_w[for_measurement_error$year==year][[1]])*
    (for_measurement_error$sig_all[for_measurement_error$year==year][[1]])*
    (for_measurement_error$eta[for_measurement_error$year==year][[1]])
    misreport_rate = 1-for_measurement_error$p[for_measurement_error$year==year][[1]]
    measurement_error<-rbinom(10000, size=1, prob=misreport_rate) *rnorm(10000,0,error_var)

    
    i<-mean(data_temp$statenum)[[1]]
    j<-mean(data_temp$quarterdate)[[1]]
    # Since count variable has 2 implied decimals. To increase the speed, we further divide by 5 (so bins of 5 individuals).
    data_temp$count<-round(data_temp$count/200)
    data_temp<-rep(log(data_temp$wage), data_temp$count)
    est <- deamerSE(data_temp, measurement_error, grid.length = 10000)
    temp<-as.data.frame(cbind(as.vector(est$supp), as.vector(est$f[1,]), rep(i, 10000), rep(j, 10000)))
    temp<-rename(temp, logwage=V1, density=V2, statenum=V3, quarterdate=V4)
    assign(paste0("temp_", i,"_", j),temp)
    write_dta(get(paste0("temp_", i,"_", j)), paste0("me_corrected_logwage_statenum_",i,"_quarterdate_",j, ".dta"))

}

stopCluster(cl)
#closeAllConnections()
