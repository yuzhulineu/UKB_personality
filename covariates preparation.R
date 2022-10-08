#load packages
library(data.table)
library(dplyr)
#UKB_delete_w19542_20220222.csv: participants who withdrawn the study
del<-as.data.frame(fread("~/wbs_data/UKB_delete_w19542_20220222.csv"))
#load personality phenotype data
personality<-as.data.frame(fread("/share/inspurStorage/home1/Vinceyang/rqz/personality/Big_five_data/Big_five.csv"))
personality$IID<-personality$eid
names(personality)[1]<-"FID"
personality<-personality[,c(1,7,2:6)]
#delete people withdrawn the study
personality<-subset(personality,!personality$FID %in% del$V1)
write.table(personality,"~/wbs_data/zrq_GWAS/data/zrq_pheno.txt",sep = "\t",row.names = F,quote = F)
#load the covariates used in the cox model and extract age, sex, and SES data
cov<-as.data.frame(fread("/share/inspurStorage/home1/Vinceyang/rqz/personality/cov_com.csv"))
cov<-cov[,c(1,2,3,5)]
cov$IID<-cov$eid
names(cov)[1]<-"FID"
cov<-cov[,c(1,5,2:4)]
#_ukb_sqc_v2.txt: QC file provided by UKB, batch information is included in the 3rd colomn
sqc<-as.data.frame(fread("/home1/UKB_Gene_v3/disk10t_2/batch/_ukb_sqc_v2.txt"))
fam<-as.data.frame(fread("/home1/UKB_Gene_v3/disk10t_2/batch/fam/ukb19542_cal_chr1_v2_s488265.fam"))
array<-cbind(fam[,c(1,2)],sqc[,3])
colnames(array)<-c("FID","IID","array")
#load the PCA40 provided by UKB and extract the first 10 PCA
pca40<-read.csv("/share/inspurStorage/home1/Royce/wbs_data/ukbimaging_prs/data/PCA40.csv",sep = " ",header = F)
pca40<-pca40[,c(1:11)]
names(pca40)[1] <- 'FID'
pca40$IID<-pca40$FID
pca40<-pca40[,c(1,12,2:11)]
#merge the covariates used in the following GWAS analysis
cov<-merge(cov,array,by=c("FID","IID")) %>% merge(pca40,by=c("FID","IID"))
cov<-subset(cov,!cov$FID %in% del$V1)
write.table(cov,"~/wbs_data/zrq_GWAS/data/zrq_cov.txt",sep = "\t",row.names = F,quote = F)


