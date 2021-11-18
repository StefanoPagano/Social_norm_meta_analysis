MC <- dfidx(Bas115, 
            subset = treatment_id=="2020Bas115_1a", 
            idx = c("subject_id", "scenarios"), idpkg = "mlogit")
head(MC)
