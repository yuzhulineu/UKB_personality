library("survival")
library("survminer")
library(plyr)
library(ggplot2)
library(ggpubr)
####-- Dementia --####
#- population restriction -#
as.data.frame(colnames(merge_3))
data <- merge_3
basurv <- Surv(time = data$dementia_days,event = data$dementia_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'Age','Sex_F','RACE','EDU',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR1 <- round(sum$coefficients[1,2],2)
  CI1 <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue1 <- sum$coefficients[1,5]
  HR2 <- round(sum$coefficients[2,2],2) 
  CI2 <- paste0(round(sum$conf.int[2,3:4],2),collapse = "-")
  PValue2 <- sum$coefficients[2,5]
  Unicox <- data.frame ('Characteristic' = x,'zphP_global' = zphP_global,
                        'HR1(95CI)' = paste0(HR1,'(',CI1,')'),'raw PValue1' = PValue1,
                        'HR2(95CI)' = paste0(HR2,'(',CI2,')'),'raw PValue2' = PValue2)
  return(Unicox)
}
UniCox('W_cate')
VarNames <- colnames(data)[c(23:33)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
dementia <- UniVar
write.csv(dementia,"dementia_results_by_survival.csv")


####-- PD --#####
data <- merge_3
basurv <- Surv(time = data$PD_days,event = data$PD_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'Age','Sex_F','RACE','EDU',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR1 <- round(sum$coefficients[1,2],2)
  CI1 <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue1 <- sum$coefficients[1,5]
  HR2 <- round(sum$coefficients[2,2],2) 
  CI2 <- paste0(round(sum$conf.int[2,3:4],2),collapse = "-")
  PValue2 <- sum$coefficients[2,5]
  Unicox <- data.frame ('Characteristic' = x,'zphP_global' = zphP_global,
                        'HR1(95CI)' = paste0(HR1,'(',CI1,')'),'raw PValue1' = PValue1,
                        'HR2(95CI)' = paste0(HR2,'(',CI2,')'),'raw PValue2' = PValue2)
  return(Unicox)
}
UniCox('W_cate')
VarNames <- colnames(data)[c(23:33)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
PD <- UniVar
write.csv(PD,"PD_results_by_survival.csv")

####-- Stroke --####
data <- merge_3
data <- subset(data,Age > 49)
basurv <- Surv(time = data$stroke_days,event = data$stroke_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'Age','Sex_F','RACE','SBP','DBP',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR1 <- round(sum$coefficients[1,2],2)
  CI1 <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue1 <- sum$coefficients[1,5]
  HR2 <- round(sum$coefficients[2,2],2) 
  CI2 <- paste0(round(sum$conf.int[2,3:4],2),collapse = "-")
  PValue2 <- sum$coefficients[2,5]
  Unicox <- data.frame ('Characteristic' = x,'zphP_global' = zphP_global,
                        'HR1(95CI)' = paste0(HR1,'(',CI1,')'),'raw PValue1' = PValue1,
                        'HR2(95CI)' = paste0(HR2,'(',CI2,')'),'raw PValue2' = PValue2)
  return(Unicox)
}
UniCox('W_cate')
VarNames <- colnames(data)[c(23:33)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
stroke <- UniVar
write.csv(stroke,"stroke_results_by_survival.csv")

####-- Schizophrenia --####
data <- merge_3
basurv <- Surv(time = data$schizophrenia_days,event = data$schizophrenia_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'Age','Sex_F','RACE','EDU',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR1 <- round(sum$coefficients[1,2],2)
  CI1 <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue1 <- sum$coefficients[1,5]
  HR2 <- round(sum$coefficients[2,2],2) 
  CI2 <- paste0(round(sum$conf.int[2,3:4],2),collapse = "-")
  PValue2 <- sum$coefficients[2,5]
  Unicox <- data.frame ('Characteristic' = x,'zphP_global' = zphP_global,
                        'HR1(95CI)' = paste0(HR1,'(',CI1,')'),'raw PValue1' = PValue1,
                        'HR2(95CI)' = paste0(HR2,'(',CI2,')'),'raw PValue2' = PValue2)
  return(Unicox)
}
UniCox('D_cate')
VarNames <- colnames(data)[c(23:33)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
schizophrenia <- UniVar
write.csv(schizophrenia,"schizophrenia_results_by_survival_younger.csv")

####-- Bipolar --####
data <- merge_3
basurv <- Surv(time = data$bipolar_days,event = data$bipolar_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'Age','Sex_F','RACE','EDU',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR1 <- round(sum$coefficients[1,2],2)
  CI1 <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue1 <- sum$coefficients[1,5]
  HR2 <- round(sum$coefficients[2,2],2) 
  CI2 <- paste0(round(sum$conf.int[2,3:4],2),collapse = "-")
  PValue2 <- sum$coefficients[2,5]
  Unicox <- data.frame ('Characteristic' = x,'zphP_global' = zphP_global,
                        'HR1(95CI)' = paste0(HR1,'(',CI1,')'),'raw PValue1' = PValue1,
                        'HR2(95CI)' = paste0(HR2,'(',CI2,')'),'raw PValue2' = PValue2)
  return(Unicox)
}
UniCox('W_cate')
VarNames <- colnames(data)[c(23:27,33)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
bipolar <- UniVar
write.csv(bipolar,"bipolar_results_by_survival.csv")

####-- Depression --####
data <- merge_3
basurv <- Surv(time = data$depression_days,event = data$depression_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'Age','Sex_F','RACE','EDU',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR1 <- round(sum$coefficients[1,2],2)
  CI1 <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue1 <- sum$coefficients[1,5]
  HR2 <- round(sum$coefficients[2,2],2) 
  CI2 <- paste0(round(sum$conf.int[2,3:4],2),collapse = "-")
  PValue2 <- sum$coefficients[2,5]
  Unicox <- data.frame ('Characteristic' = x,'zphP_global' = zphP_global,
                        'HR1(95CI)' = paste0(HR1,'(',CI1,')'),'raw PValue1' = PValue1,
                        'HR2(95CI)' = paste0(HR2,'(',CI2,')'),'raw PValue2' = PValue2)
  return(Unicox)
}
UniCox('W_cate')
VarNames <- colnames(data)[c(23:33)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
depression <- UniVar
write.csv(depression,"depression_results_by_survival.csv")

####-- Anxiety --####
data <- merge_3
basurv <- Surv(time = data$anxiety_days,event = data$anxiety_status)
data$basurv <-with(data,basurv)
UniCox <- function(x){
  FML <- as.formula(paste0('basurv~',paste(x,'Age','Sex_F','RACE','EDU',sep = "+")))
  cox<-coxph(FML, data = data)
  zph0 <- cox.zph(cox)
  a <- nrow(zph0[["table"]])
  zphP_global <- round(zph0[["table"]][a,3],2)
  sum<-summary(cox)
  HR1 <- round(sum$coefficients[1,2],2)
  CI1 <- paste0(round(sum$conf.int[1,3:4],2),collapse = "-")
  PValue1 <- sum$coefficients[1,5]
  HR2 <- round(sum$coefficients[2,2],2) 
  CI2 <- paste0(round(sum$conf.int[2,3:4],2),collapse = "-")
  PValue2 <- sum$coefficients[2,5]
  Unicox <- data.frame ('Characteristic' = x,'zphP_global' = zphP_global,
                        'HR1(95CI)' = paste0(HR1,'(',CI1,')'),'raw PValue1' = PValue1,
                        'HR2(95CI)' = paste0(HR2,'(',CI2,')'),'raw PValue2' = PValue2)
  return(Unicox)
}
UniCox('W_cate')
VarNames <- colnames(data)[c(23:33)]
UniVar <- lapply(VarNames,UniCox)
UniVar <- ldply(UniVar,data.frame)
anxiety <- UniVar
write.csv(anxiety,"anxiety_results_by_survival_older.csv")
