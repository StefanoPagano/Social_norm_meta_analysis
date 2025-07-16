treatment="2018Her061_9"

df_master <- utility_df_treatment_aic_analysis %>% 
  filter(treatment_id==treatment)

social_expectation=clogit(A ~ payoff + mean_app + strata(subject_id), 
                                data=df_master)

inequality_aversion=clogit(A ~ other_payoff_ahead + other_payoff_behind + sqr_other_payoff_ahead + sqr_other_payoff_behind + strata(subject_id), 
                                 data=df_master)

inequality_aversion2=clogit(A ~ other_payoff_ahead + other_payoff_behind + strata(subject_id), iter.max=25 , method="exact",data=df_master)

social_expectation_aug1=clogit(A ~ payoff + mean_app + log_sd_app + strata(subject_id), 
                                data=df_master)

tab_model(social_expectation, inequality_aversion2, social_expectation_aug1, dv.labels = c("Social Exp", "Ineq. Avers.", "Soc. Exp. Aug."), show.aic = T, show.loglik = T, show.se = T, show.ci = F, title = treatment, transform=NULL)

barplot(predict(inequality_aversion, type = "expected")[1:11])


########################################################################
i="2020Bas115_2a"
temp_choice <- df_real %>% 
  filter(treatment_id == i)
data.frame(pre=predict(temp_model[[1]], type = "expected")[1:11]*100, scen=c(0:10))
temp_model <- models[[i]]

p <- ggplot(temp_choice, 
                aes(x=choice, 
                    y=percentage)) + 
  geom_bar(stat = "identity",
           alpha=0.5)

  p + geom_bar(temp_model[[1]],
           aes(x=scenarios_treatment %>% filter(treatment_id==t) %>% select(n_scen),
               y=predict(temp_model[[1]][1:max(scenarios_treatment %>% filter(treatment_id==t) %>% select(n_scen))])),
           stat = "identity") +
  labs(title = i) +
  xlim(min(scenarios_treatment %>% filter(treatment_id==i) %>% select(scenarios))-1,
       max(scenarios_treatment %>% filter(treatment_id==i) %>% select(scenarios))+1) +
  ylim(0,100) +
  xlab("Choice") + 
  ylab("Subjects (%)")
