# ============================================================ 
# 	Mini-assignment # 12
# ============================================================ 

# load the data_pat_mlm.csv file and perform the following modeling
library(data.table)
data_pat <- fread(file = "data_pat_mlm.csv") 
colnames(data_pat)
data_pat[, .N]

# (1) plot the property_val variable against cost_future_total_log, give different colors for each state, and then add regression lines for each state to the plot
plot(cost_future_total_log ~ property_val, data = data_pat, pch = 20, cex = .5, col = rainbow(10, alpha=.5)[as.numeric(factor(data_pat$state))])
abline(lm(cost_future_total_log ~ property_val, data = data_pat[state == 'MD']))
abline(lm(cost_future_total_log ~ property_val, data = data_pat[state == 'DE']))
abline(lm(cost_future_total_log ~ property_val, data = data_pat[state == 'IL']))
abline(lm(cost_future_total_log ~ property_val, data = data_pat[state == 'TN']))
abline(lm(cost_future_total_log ~ property_val, data = data_pat[state == 'RI']))
abline(lm(cost_future_total_log ~ property_val, data = data_pat[state == 'WV']))
abline(lm(cost_future_total_log ~ property_val, data = data_pat[state == 'VA']))
abline(lm(cost_future_total_log ~ property_val, data = data_pat[state == 'DC']))
abline(lm(cost_future_total_log ~ property_val, data = data_pat[state == 'MA']))
abline(lm(cost_future_total_log ~ property_val, data = data_pat[state == 'TX']))


# (2) run an MLM regression using property_val as a fixed effect while using state as the grouping factor (i.e., random intercept) [do NOT use any other fixed effects/predictors]
# show/calculate summary, coef, ranef, fixef, r2, aic, and rmse values of the model
library(lme4)
library(lmerTest)
m_mlm_pval1 = lmer(cost_future_total_log ~ property_val + (1|state), data = data_pat)
summary(m_mlm_pval1)
coef(m_mlm_pval1)
fixef(m_mlm_pval1)
ranef(m_mlm_pval1)

library(sjstats)
performance::r2(m_mlm_pval1)
performance::performance_aic(m_mlm_pval1)
performance::performance_rmse(m_mlm_pval1)

# (3) redo #2 analysis (same grouping factor), but add property_val as a random slope and add age and hcc_ct to the fixed effects/predictors (use 'bobyqa' as the optimizer)
# show/calculate summary, coef, ranef, fixef, r2, aic, and rmse values of the model
# did your model improve compared to #2?

m_mlm_pval2 = lmer(cost_future_total_log ~ age + hcc_ct + property_val + (property_val|state), data = data_pat, control=lmerControl(optimizer='bobyqa'))
summary(m_mlm_pval2)
coef(m_mlm_pval2)
fixef(m_mlm_pval2)
ranef(m_mlm_pval2)

performance::r2(m_mlm_pval2)
performance::performance_aic(m_mlm_pval2)
performance::performance_rmse(m_mlm_pval2)

#yes model 2 was improved compared to model 1. It had a higher conditional rsquared, which takes into account both fixed and random effects. It also has a lower aic.
