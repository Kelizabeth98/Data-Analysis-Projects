library(survival) 
library(survminer) #for customizable graphs of survival function
library(ggplot2) #for graphing (actually loaded by survminer)
library(broom) #for tidy output

#Specifying the outcome
Surv(time, event)

#Specifying the outcome for data in stop-start format
Surv(time, time2, event)

##Kaplan-Meier estimation with survfit()
# ~ 1 indicates KM survival function estimate for whole sample
KM_1 <- survfit(Surv(time, status) ~ 1, data = aml)
KM_1

#Save KM survival function as a tibble (modern dataframe)
KM_1_tab <- tidy(KM_1)
KM_1_tab

#Plotting the survival function
plot(KM_1, ylab = "survival probability", xlab = "months")

#Stratify by x variable
KM_strat <- survfit(Surv(time, status) ~ x, data = aml)
KM_strat

tidy(KM_strat)

###Plotting stratified K-M estimates of survival function
#Stratified KM curves with 95% CI, 2 colors
plot(KM_strat, ylab = "survival probability", xlab = "months",
     conf.int = T, col = c("red", "blue"))

##Customizable, Informative survival plots with survminer
ggsurvplot(KM_strat, conf.int = T)

#Adding a risk table
ggsurvplot(KM_strat, conf.int = T, risk.table = T)

#Adding more arguments
ggsurvplot(KM_strat, conf.int = T, risk.table = T,
           palette = "Accent", #argument to scale_color_brewer()
           size = 1, #arguemnt to geom_line()
           ggtheme = theme_minimal() #changing ggtheme
           )

#Using traditional ggplot2 syntax by extracting the ggplot object as ggsurvplot()$plot
g <- ggsurvplot(KM_strat, conf.int = T, risk.table = T)$plot #this is the ggplot object
g + scale_fill_grey() + scale_color_grey()


###Comparing survival functions with survdiff()
##H0: survival curves across 2 or more groups are equivalent
##HA: survival curves across 2 or more groups are not equivalent

#Log rank test, default is rho = 0
survdiff(Surv(time, status) ~ x, data = aml)


#rho = 1 specifies Peto & Peto modification of Gehan-Wicoxon
#More weight put on earlier time points
survdiff(Surv(time, status) ~ x, data = aml, rho = 1)


####Cox Proportional Hazard Model
#Fit cox model and save results
lung_cox <- coxph(Surv(time, status) ~ age + sex + wt.loss, data = lung)
summary(lung_cox)

##Save summarized results as data.frame
#Exponentiate  = T returns hazard ratios
lung_cox_tab <- tidy(lung_cox, exponentiate = T, conf.int = T)
lung_cox_tab

##Storing the Cox model results as a data.frame makes it easy to use ggplot2 to create plots of the hazard ratios and confidence intervals.
#Plot of hazard ratios and 95% CI
ggplot(lung_cox_tab, 
       aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() + #plots center point (x) and range (xmin, xmax)
  geom_vline(xintercept = 1, color = "red") + # vertical line at HR = 1
  labs(x = "hazard ratio", title = "Hazard ratios and 95% CIs") +
  theme_classic()

###Predicting survival after coxph() with survfit()
##If no covariate values are supplied to survfit(), then a survival function will be estimated for a subject with mean values on all model covariates.
#Predict survival function for subject with means on all covariates
surv_at_means <- survfit(lung_cox)

#Table of survival function
tidy(surv_at_means)

###Plotting predicted survival curve
#Plot of predicted survival for subject at means of covariates
plot(surv_at_means, xlab = "days", ylab = "survival probability")


###Predicting survival at specific covariate values
##Create new data for plotting: 1 row for each sex
#and mean age and wt.loss for both rows
plotdata <- data.frame(age = mean(lung$age),
                       sex = 1:2,
                       wt.loss = mean(lung$wt.loss, na.rm = T))
plotdata

#Get the survival function estimates for each sex
surv_by_sex <- survfit(lung_cox, newdata = plotdata) #one function for each sex
#tidy results
tidy(surv_by_sex)

###Plotting multiple predicted survival function
## Plot survival estimates 
plot(surv_by_sex, xlab = "days", ylab = "survival probability",
     conf.int = T, col = c("blue", "red"))

#Using ggsurvplot
#data =, is the same data used in survfit()
#censor = F, removes censoring symbols 
ggsurvplot(surv_by_sex, data = plotdata, censor = F,
           legend.labs = c("Male", "Female"))


###Assessing the propotional hazards assumption
cox.zph(lung_cox) #No strong evidence of violation of proportional hazards for any covariate, though some suggestion that sex may violate.

##Plot of a smoothed curve over the Schoenfeld residuals
plot(cox.zph(lung_cox))

###Handling Proportional Hazards Violation
##Stratifying by the non-PH variable
lung_strat_sex <- coxph(Surv(time,status) ~ age + wt.loss +
                          strata(sex), data = lung)
summary(lung_strat_sex)

##Modeling time-varying coefficients
#notice sex and tt(sex) in the model formula
lung_sex_by_time <- coxph(Surv(time,status) ~ age + wt.loss +
                          sex + tt(sex), 
                          data = lung,
                          tt = function(x, t,...) x*t) #Linear change in effect of sex
summary(lung_sex_by_time)

plot(cox.zph(lung_cox), var = "sex")

###Time-varying covariates
jasa1_cox <- coxph(Surv(start, stop, event) ~ transplant + age +
                     surgery, data = jasa1)
summary(jasa1_cox)

##Check for PH assumptions
cox.zph(jasa1_cox)

#Plot predicted survival by transplant group at mean age and surgery = 0
plotdata <- data.frame(transplant  = 0:1, age = -2.48, surgery = 0)
surv_by_transplant <- survfit(jasa1_cox, newdata = plotdata)
ggsurvplot(surv_by_transplant, data = plotdata) #remember to supply data to ggsurvplot() for predicted survival after coxph()

