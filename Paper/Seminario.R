treatment="2019Cha026_1"
df=utility_df_treatment_aic_analysis %>% filter(treatment_id==treatment)
plot(df$scenarios, df$sd_app)

model_se=clogit(A ~ payoff + mean_app + strata(subject_id), 
                data=df)

model_sd=clogit(A ~ payoff + sd_app + strata(subject_id), 
                                data=df)

model_aug=clogit(A ~ payoff + mean_app + sd_app + strata(subject_id), 
               data=df)

tab_model(model_se,model_sd, model_aug, transform = NULL, show.aic = T, show.ci = F, show.se = T,dv.labels = c("Mean Appropriateness", "With Standard Deviation", "Augmented"))


plot(df$mean_app, df$sd_app)

colors <- c("SD" = "blue", "MEAN" = "red")
ggplot(aes(x=scenarios), data=df) + 
  geom_line(aes(y=sd_app, color="SD")) + 
  geom_line(aes(y=mean_app, color="MEAN")) + 
  labs(x="Scenarios", y="Value", color="Legend") +
  scale_color_manual(name='Legend',
                     breaks=c('SD', 'MEAN'),
                     values=colors) + 
  theme_light()
