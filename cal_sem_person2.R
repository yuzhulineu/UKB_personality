library(lavaan)

setwd('/share/inspurStorage/home1/liyz/Project/acc')
sem_table<-read.csv("sem2_person.csv")

cov1<-scale(sem_table$cov_1)
sem_table$cov1<-cov1

HS.model<-"
         WarmthL=~1*personality_1
         PRSL=~1*prs_1
         BloodL=~1*blood_1+blood_3+blood_4
         MetaL=~1*meta_1+meta_3+meta_7+meta_9
         status=~1*disease_1+disease_2+disease_3+disease_6
         
         
         status~WarmthL+PRSL+BloodL+MetaL+cov1+cov_2+cov_3
         WarmthL~PRSL+cov1+cov_2+cov_3
         BloodL~PRSL+WarmthL+cov1+cov_2+cov_3
         MetaL~PRSL+WarmthL+cov1+cov_2+cov_3
         BloodL~~MetaL
        
         "
fit_warm<-sem(HS.model,data=sem_table,ordered=c('disease_1','disease_2','disease_3','disease_6'))
fit_warmsummary<-summary(fit_warm,standardized=T)

fitmeasures(fit_warm,fit.measures='all',baseline.model = NULL)
modindices(fit_warm, sort = TRUE)

##########Diligence
HS.model1<-"
         DiligenceL=~1*personality_2
         PRSL=~1*prs_2
         BloodL=~1*blood_1+blood_3+blood_4
         MetaL=~1*meta_1+meta_3+meta_7+meta_9
         status=~1*disease_1+disease_2+disease_3+disease_6
         
         status~DiligenceL+PRSL+BloodL+MetaL+cov1+cov_2+cov_3
         DiligenceL~PRSL+cov1+cov_2+cov_3
         BloodL~PRSL+DiligenceL+cov1+cov_2+cov_3
         MetaL~PRSL+DiligenceL+cov1+cov_2+cov_3
         BloodL~~MetaL
         
         "
fit_diligence<-sem(HS.model1,data=sem_table,ordered=c('disease_1','disease_2','disease_3','disease_6'))
fit_diligencesummary<-summary(fit_diligence,standardized=T)

fitmeasures(fit_diligence,fit.measures='all',baseline.model = NULL)
modindices(fit_diligence, sort = TRUE)

############## Nervous ##########
HS.model2<-"
         NervousL=~1*personality_3
         PRSL=~1*prs_3
         BloodL=~1*blood_1+blood_3+blood_4
         MetaL=~1*meta_1+meta_3+meta_7+meta_9
         status=~1*disease_1+disease_2+disease_3+disease_6
         
         status~NervousL+PRSL+BloodL+MetaL+cov1+cov_2+cov_3
         NervousL~PRSL+cov1+cov_2+cov_3
         BloodL~PRSL+NervousL+cov1+cov_2+cov_3
         MetaL~PRSL+NervousL+cov1+cov_2+cov_3
         BloodL~~MetaL
         
         "
fit_nervous<-sem(HS.model2,data=sem_table,ordered=c('disease_1','disease_2','disease_3','disease_6'))
fit_nervoussummary<-summary(fit_nervous,standardized=T)

fitmeasures(fit_nervous,fit.measures='all',baseline.model = NULL)
modindices(fit_nervous, sort = TRUE)

############# Social
HS.model3<-"
         SocialL=~1*personality_4
         PRSL=~1*prs_4
         BloodL=~1*blood_1+blood_3+blood_4
         MetaL=~1*meta_1+meta_3+meta_7+meta_9
         status=~1*disease_1+disease_2+disease_3+disease_6
         
         status~SocialL+PRSL+BloodL+MetaL+cov1+cov_2+cov_3
         SocialL~PRSL+cov1+cov_2+cov_3
         BloodL~PRSL+SocialL+cov1+cov_2+cov_3
         MetaL~PRSL+SocialL+cov1+cov_2+cov_3
         BloodL~~MetaL
         "
fit_social<-sem(HS.model3,data=sem_table,ordered=c('disease_1','disease_2','disease_3','disease_6'))
fit_socialsummary<-summary(fit_social,standardized=T)

fitmeasures(fit_social,fit.measures='all',baseline.model = NULL)
modindices(fit_social, sort = TRUE)

############# Curiosity

HS.model4<-"
         CuroisityL=~1*personality_5
         PRSL=~1*prs_5
         BloodL=~1*blood_1+blood_3+blood_4
         MetaL=~1*meta_1+meta_3+meta_7+meta_9
         status=~1*disease_1+disease_2+disease_3+disease_6
         
         status~CuroisityL+PRSL+BloodL+MetaL+cov1+cov_2+cov_3
         CuroisityL~PRSL+cov1+cov_2+cov_3
         BloodL~PRSL+CuroisityL+cov1+cov_2+cov_3
         MetaL~PRSL+CuroisityL+cov1+cov_2+cov_3
         BloodL~~MetaL
         "
fit_curious<-sem(HS.model4,data=sem_table,ordered=c('disease_1','disease_2','disease_3','disease_6'))
fit_curioussummary<-summary(fit_curious,standardized=T)

fitmeasures(fit_curious,fit.measures='all',baseline.model = NULL)
modindices(fit_curious, sort = TRUE)