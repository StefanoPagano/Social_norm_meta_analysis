#< set wd 
setwd("G:/Mon Drive/Meta-analysis beliefs/Dati paper/2016Kim003")

csv_path_output <- "~/GitHub/Social_norm_meta_analysis/Paper_csv/"

pgg=read.csv("PG_Data_bysubj.txt", sep="\t")
tgs=read.table("TG_Data_byperiod.txt", header = T)
ug=read.table("UG_Data.txt", header = T)
dg=read.csv("DG_Data.csv", sep="\t")
## the next file contains all data except the conditional PG elicitations, which we did later
norms1=read.csv("Norm_Elicitation_Data.csv", sep = ",")

norms1 <- norms1 %>% mutate(ANSW01 = dplyr::recode(answers.1., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW02 = dplyr::recode(answers.2., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW03 = dplyr::recode(answers.3., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW04 = dplyr::recode(answers.4., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW05 = dplyr::recode(answers.5., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW06 = dplyr::recode(answers.6., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW07 = dplyr::recode(answers.7., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW08 = dplyr::recode(answers.8., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW09 = dplyr::recode(answers.9., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW12 = dplyr::recode(answers.12., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW13 = dplyr::recode(answers.13., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW14 = dplyr::recode(answers.14., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW15 = dplyr::recode(answers.15., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW16 = dplyr::recode(answers.16., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW17 = dplyr::recode(answers.17., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW18 = dplyr::recode(answers.18., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW19 = dplyr::recode(answers.19., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW20 = dplyr::recode(answers.20., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW21 = dplyr::recode(answers.21., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW22 = dplyr::recode(answers.22., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW23 = dplyr::recode(answers.23., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW24 = dplyr::recode(answers.24., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW25 = dplyr::recode(answers.25., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW26 = dplyr::recode(answers.26., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW27 = dplyr::recode(answers.27., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW28 = dplyr::recode(answers.28., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW29 = dplyr::recode(answers.29., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW30 = dplyr::recode(answers.30., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW31 = dplyr::recode(answers.31., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW34 = dplyr::recode(answers.34., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW35 = dplyr::recode(answers.35., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW36 = dplyr::recode(answers.36., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW37 = dplyr::recode(answers.37., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW38 = dplyr::recode(answers.38., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW39 = dplyr::recode(answers.39., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW40 = dplyr::recode(answers.40., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW41 = dplyr::recode(answers.41., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1),
       ANSW42 = dplyr::recode(answers.42., `1` = -1, `2` = -1/3, `3` = 1/3, `4` = 1))
#answ:
#1-9 DG
#12-22 PGG
#23-31 UG
#34-42 TG


# meta-information dataset
meta_dataset <- read_xlsx(path = "G:/Mon Drive/Meta-analysis beliefs/Social Norms meta.xlsx", sheet = "ALL") %>% subset.data.frame(subset = PaperID == "2016Kim003", select = c(n_Paper, PaperID, TreatmentCode, TreatmentName_paper, Year, Outlet, Published, FirstTask, between_vs_within, Game_type, Standard_game, Baseline, Group_size, One_Shot_Repeated, Choice_Method, Matching, Rounds, Punishment, Rewards, Monetary_Incentivized_experiment, Environment, Method_elicitation, Separate_sample_beliefs, Belief_repeated, Before_after_main_decisions, KW_Normative, KW_Personal, Bicchieri_Empirical, Bicchieri_Normative, Bicchieri_Personal_Beliefs, Bicchieri_between, Incentives_beliefs, StatusTreatment_Roma)) %>% mutate(TreatmentCode = as.numeric(TreatmentCode))

# DG -----------------
# get information on treatment

# cleaning DG
## exp_id : date and time of experiment
## exp_num : unique number of the experiment
## subj_id : exp_num * 100 + subj
## subj : subject in the session
## gr_id : unique group id in the dataset
## role : 1 proposer; 0 responder
## sent : amount sent; -1 if not applicable
coldg = c("exp_id","exp_num","subj_id","subj", "role", "sent")

# 1. Choice dataframe ----
dg_dta_coop <- dg %>% subset.data.frame(select = coldg, subset = role == 1) %>%
  mutate(endowment = 16, cooperation = sent/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2016Kim003", TreatmentCode = 7)

# 2. Beliefs dataframe ----
## answers[1-9] : kw appropriateness
## actions: send 16/0; 14/2; 12/4; ...; 0/16
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA

label_col = as.character(seq(0,16,2))
dg_columns <- c(1, 3, 4, 135:143)
n_sub_N = norms1 %>% summarise(n_sub_N = sum(!is.na(ANSW01)))


## compute norm 
dg_appropriateness_sum <- norms1 %>% subset.data.frame(select = dg_columns) %>% 
  summarise_at(vars(ANSW01:ANSW09), sum, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- dg_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2016Kim003", TreatmentCode = 7)

positive_appropriateness <- dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(dg_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- dg_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- dg_appropriateness_sum %>% mutate(delta_max = 0)
  
}

## compute variance norm
dg_norms_var <- norms1[, dg_columns] %>% 
  summarise_at(vars(ANSW01:ANSW09), var, na.rm=T) %>%
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

dg_final_norms <- merge.data.frame(dg_appropriateness_sum, dg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2016Kim003", 
    TreatmentCode = 7, 
    Avg_NE = as.integer(donation)/16,
    Var_NE = ..y, Avg_KW_m = Kw_m,
    Sd_Avg_NE = sd(dg_appropriateness_sum$Kw_m),
    Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
    specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
    specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
    max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(dg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(dg_final_norms, all.x=T, by = c("PaperID","TreatmentCode")) %>%
  subset.data.frame(select = -c(n_sub_N, Kw_m))


# TG ---------------------
## only first round, only proposer action

coltg = c("exp_id","exp_num","prop_id","sent","per")

# 1. Choice dataframe ----
tg_dta_coop <- tgs %>% subset.data.frame(select = coltg, subset = per== 1) %>% 
  mutate(endowment = 80, cooperation = sent/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2016Kim003", TreatmentCode = 6)

# 2. Beliefs dataframe ----
## answers[34-42] : kw appropriateness
## actions: send 0;10;20;...;80
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA
label_col = as.character(seq(0,80,10))
tg_columns <- c(1, 3, 4, 164:172)
n_sub_N = norms1 %>% summarise(n_sub_N = sum(!is.na(ANSW34)))

## compute norm 
tg_appropriateness_sum <- norms1 %>% subset.data.frame(select = tg_columns) %>% 
  summarise_at(vars(ANSW34:ANSW42), sum, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- tg_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2016Kim003", TreatmentCode = 6) %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- tg_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(tg_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- tg_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- tg_appropriateness_sum %>% mutate(delta_max = 0)
  
}


## compute variance norm
tg_norms_var <- norms1[, tg_columns] %>% 
  summarise_at(vars(ANSW34:ANSW42), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

tg_final_norms <- merge.data.frame(tg_appropriateness_sum, tg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2016Kim003", 
    TreatmentCode = 6, 
    Avg_NE = as.integer(donation)/80,
    Var_NE = ..y, Avg_KW_m = Kw_m,
    Sd_Avg_NE = sd(tg_appropriateness_sum$Kw_m),
    Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
    specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
    specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
    max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf =  meta_dataset %>% 
  merge.data.frame(tg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(tg_final_norms) %>% 
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)
  

# UG -----------------
# get information on treatment

# cleaning UG
## exp_id : date and time of experiment
## exp_num : unique number of the experiment
## subj_id : exp_num * 100 + subj
## subj : subject in the session
## gr_id : unique group id in the dataset
## role : 1 proposer; 0 responder
## sent : amount sent; -1 if not applicable
colug = c("exp_id","exp_num","subj_id","subj", "role", "sent")

# 1. Choice dataframe ----
ug_dta_coop <- ug %>% subset.data.frame(select = colug, subset = role == 1) %>% 
  mutate(endowment = 16, cooperation = sent/endowment) %>% 
  summarise(Avg_coop = mean(cooperation, na.rm =T),
            Var_coop = var(cooperation, na.rm = T)) %>% 
  mutate(PaperID = "2016Kim003", TreatmentCode = 8)

# 2. Beliefs dataframe ----
## answers[23-31] : kw appropriateness
## actions: send 16/0; 14/2; 12/4; ...; 0/16
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA
label_col = as.character(seq(0,16,2))
ug_columns <- c(1, 3, 4, 155:163)
n_sub_N = norms1 %>% summarise(n_sub_N = sum(!is.na(ANSW23)))

## compute norm 
ug_appropriateness_sum <- norms1 %>% subset.data.frame(select = ug_columns) %>% 
  summarise_at(vars(ANSW23:ANSW31), sum, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- ug_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2016Kim003", TreatmentCode = 8) %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- ug_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(ug_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- ug_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- ug_appropriateness_sum %>% mutate(delta_max = 0)
  
}


## compute variance norm
ug_norms_var <- norms1[, ug_columns] %>% 
  summarise_at(vars(ANSW23:ANSW31), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

ug_final_norms <- merge.data.frame(ug_appropriateness_sum, ug_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2016Kim003",
    TreatmentCode = 8, 
    Avg_NE = as.integer(donation)/16, 
    Var_NE = ..y, Avg_KW_m = Kw_m,
    Sd_Avg_NE = sd(ug_appropriateness_sum$Kw_m),
    Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
    specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
    specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
    max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf =  meta_dataset %>% 
  merge.data.frame(ug_dta_coop, by = c("PaperID","TreatmentCode")) %>%
  merge.data.frame(ug_final_norms) %>% 
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf)


# PGG (1:Sort_PG)-----------------
# get information on treatment

# cleaning PGG
## exp_id : date and time of experiment
## exp_num : unique number of the experiment
## subj_id : exp_num * 100 + subj
## subj : subject in the session
## treat : select the following number to use the correct TreatmentCode
### treat = 1: RF task, sorting into groups, PG 10 periods with "the rule is" instructions
### treat = 2:  RF task, sorting into groups, PG 10 periods with "no rule" instructions
### treat = 4: PG 10 periods, then  RF task with "rule"
### treat = 5:  RF task, then PG not sorted
### treat = 9: PG 10 periods, then  RF task with "no rule"
## gr_id : unique group id in the dataset
## putin1 : amount put in group account by subject in period 1 of PG
#colpgg = c("exp_id","exp_num","subj_id","subj","treat", "putin1")

# 1. Choice dataframe, no choice for this experiment, that matches norm elicitation ----
pgg_dta_coop <- data.frame(Avg_coop = NA, Var_coop = NA) %>% 
  mutate(PaperID = "2016Kim003", TreatmentCode = 9)

# 2. Beliefs dataframe ----
## answers[12-21] : kw appropriateness
## actions: send 16/0; 14/2; 12/4; ...; 0/16
## exp_id
## session
## subject
## KW scale: 1: VI; 2: I; 3: A; 4: VA
label_col = as.character(seq(0,50,5))
pgg_columns <- c(1, 3, 4, 144:154)
n_sub_N = norms1 %>% summarise(n_sub_N = sum(!is.na(ANSW12)))

### compute norm 
pgg_appropriateness_sum <- norms1 %>% subset.data.frame(select = pgg_columns) %>% 
  summarise_at(vars(ANSW12:ANSW22), sum, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col) %>%
  mutate(n_sub_N, Kw_m = ./n_sub_N)

db_appropriateness <- pgg_appropriateness_sum %>% select(donation, Kw_m) %>% mutate(PaperID = "2016Kim003", TreatmentCode = 9) %>%
  rbind.data.frame(db_appropriateness)

positive_appropriateness <- pgg_appropriateness_sum %>% subset.data.frame(subset = Kw_m > 0) %>% 
  mutate(delta_max = max(Kw_m) - Kw_m)

if (min(pgg_appropriateness_sum$Kw_m) < 0){
  
  negative_appropriateness <- pgg_appropriateness_sum %>% subset.data.frame(subset = Kw_m < 0) %>% 
    mutate(abs_Kw_m = abs(Kw_m), delta_max = max(Kw_m) - Kw_m)
  
} else {
  
  negative_appropriateness <- pgg_appropriateness_sum %>% mutate(delta_max = 0)
  
}


### compute variance norm
pgg_norms_var <- norms1[, pgg_columns] %>% 
  summarise_at(vars(ANSW12:ANSW22), var, na.rm=T) %>% 
  t.data.frame() %>% 
  cbind.data.frame(donation=label_col)

pgg_final_norms <- merge.data.frame(pgg_appropriateness_sum, pgg_norms_var, by = "donation") %>% 
  subset.data.frame(subset = ..x == max(..x)) %>% 
  mutate(PaperID = "2016Kim003", 
    TreatmentCode = 9, 
    Avg_NE = as.integer(donation)/50,
    Var_NE = ..y, Avg_KW_m = Kw_m,
    Sd_Avg_NE = sd(pgg_appropriateness_sum$Kw_m),
    Sd_Avg_NE_min_max = max(positive_appropriateness$Kw_m) - min(positive_appropriateness$Kw_m),
    specificity_plus = sum(positive_appropriateness$delta_max)/((length(positive_appropriateness$delta_max)-1)),
    specificity_min = if (length(negative_appropriateness$delta_max)==1) {0} else {sum(negative_appropriateness$delta_max)/((length(negative_appropriateness$delta_max)-1))},
    max_sigma = sd(c(rep(-1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N-1)/2)), rep(1, ifelse(n_sub_N%%2==0, n_sub_N/2, (n_sub_N+1)/2))))) %>%
  subset.data.frame(select = -c(..x, ..y, donation))

# 3. combine dataset ----
finaldf <- meta_dataset %>% 
  merge.data.frame(pgg_dta_coop, by = c("PaperID","TreatmentCode")) %>% 
  merge.data.frame(pgg_final_norms) %>% 
  subset.data.frame(select = -c(n_sub_N, Kw_m)) %>%
  rbind.data.frame(finaldf) %>%
  mutate(Avg_EE = NA, Avg_PNB = NA, Var_EE = NA, Var_PNB = NA)


write.csv(finaldf, file = paste(csv_path_output, paste(finaldf$PaperID[1], "_finaldf.csv", sep = ""), sep = ""), row.names = F)

write.csv(db_appropriateness, file = paste(csv_path_output, paste(db_appropriateness$PaperID[1], "_avg_kw.csv", sep = ""), sep = ""), row.names = F)
