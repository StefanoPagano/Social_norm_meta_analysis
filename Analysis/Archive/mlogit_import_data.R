library(mlogit)
MC <- dfidx(Bas115 %>% mutate(scenarios=as.factor(scenarios)), 
            subset = treatment_id=="2020Bas115_1a",
            idx = c("subject_id","scenarios"), 
            idpkg = "mlogit")
head(MC,20)

tab_model(mlogit(A~0+KW_Normative, data=MC))
