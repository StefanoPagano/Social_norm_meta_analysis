library(tidyverse)
library(mlogit)

dataset_bas <- Basic_Verrina_2021 %>% 
  select(code, give, role_DG, social, dg_sn_1:dg_sn_11) %>%
  filter(role_DG==1 & social==0) %>%
  mutate(payoff_0 = 10,
         payoff_1 = 9,
         payoff_2 = 8,
         payoff_3 = 7,
         payoff_4 = 6,
         payoff_5 = 5,
         payoff_6 = 4,
         payoff_7 = 3,
         payoff_8 = 2,
         payoff_9 = 1,
         payoff_10 = 0)  %>%
  mutate(dg_sn_1 = dplyr::recode(dg_sn_1, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         dg_sn_2 = dplyr::recode(dg_sn_2, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         dg_sn_3 = dplyr::recode(dg_sn_3, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         dg_sn_4 = dplyr::recode(dg_sn_4, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         dg_sn_5 = dplyr::recode(dg_sn_5, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         dg_sn_6 = dplyr::recode(dg_sn_6, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         dg_sn_7 = dplyr::recode(dg_sn_7, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         dg_sn_8 = dplyr::recode(dg_sn_8, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         dg_sn_9 = dplyr::recode(dg_sn_9, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         dg_sn_10 = dplyr::recode(dg_sn_10, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1),
         dg_sn_11 = dplyr::recode(dg_sn_11, `1` = -1, `2` = -0.6, `3` = -0.3, `4` = 0.3, `5` = 0.6, `6` = 1))

df_long <- reshape(
  dataset_bas,
  direction = "long",
  varying = list(norm_app = 5:15, payoff = 16:26),
  v.names = c("norms", "payoff"),
  timevar = "scenarios"
) %>%
  arrange(id, scenarios) %>%
  mutate(scenarios = scenarios - 1) %>%
  mutate(A = give == scenarios,
         dictator_final_payoff_position = ifelse(scenarios >= 5,"behind", "ahead"),
         payoff_factor = as.factor(payoff))

yyy <- xtabs(~norms + A, data=df_long)
xxx <- xtabs(A ~ norms, data = df_long) #count for each appropriateness level how much subject choice the scenarios
zzz <- xtabs(~ norms + payoff, data = df_long) 
plot(yyy)
plot(xxx)
plot(zzz)


#model
df_long_model <- mlogit.data(df_long,
                             shape = "long",
                             choice = "A",
                             alt.var = "scenarios")

m1_int <- mlogit(A ~ payoff + norms, data=df_long_model)
summary(m1_int)

m1_no_int <- mlogit(A ~ 0 + payoff + norms, data=df_long_model)
summary(m1_no_int)

m2 <- mlogit(A ~ 0 + payoff + norms + dictator_final_payoff_position, data=df_long_model)
summary(m2)

m3 <- mlogit(A ~ 0 + payoff_factor + norms, data=df_long_model)
summary(m3)
###############################################################################



data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice =
                      "mode")

# a formula with to alternative specific variables (price and
# catch) and an intercept
f1 <- mFormula(mode ~ price + catch)
head(model.matrix(f1, Fish), 2)

# same, with an individual specific variable (income)
f2 <- mFormula(mode ~ price + catch | income)
head(model.matrix(f2, Fish), 2)

# same, without an intercept
f3 <- mFormula(mode ~ price + catch | income + 0)
head(model.matrix(f3, Fish), 2)

# same as f2, but now, coefficients of catch are alternative
# specific
f4 <- mFormula(mode ~ price | income | catch)
head(model.matrix(f4, Fish), 2)
