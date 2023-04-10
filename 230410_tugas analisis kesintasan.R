library(readxl)
library(survival) 
library(survminer)
library(dplyr)

tugas_analisis_kesintasan <- read_excel("C:/Users/surfa/Downloads/praktik analisis kesintasan 2.xlsx")
View(tugas_analisis_kesintasan)

#Creating object
surv_object <- Surv(time = tugas_analisis_kesintasan$LENFOL, event = tugas_analisis_kesintasan$FSTAT)

#Computes an estimate of a survival curve for censored data using the Aalen-Johansen estimator.

#Kaplan-Meier ctype artinya cumulative hazard type
km <- survfit(surv_object ~ Treatment, 
                conf.int = 0.95,
                stype = 1, # 1 = direct, 2 = exp(cumulative hazard)
                ctype = 1, # 1 = Nelson-Aalen, 2 = Flemming-Harrington
                conf.type = "log", 
                data = tugas_analisis_kesintasan)

fit1 #Simple output
summary(fit1)$table #Mean survival included
summary(fit1, times = c(6,9,12)) #Specific time

#1.1 membuat kategori adult dan elderly
tugas_analisis_kesintasan$cat_age <- ifelse(tugas_analisis_kesintasan$AGE < 65, "adult",
                        ifelse(tugas_analisis_kesintasan$AGE >= 65, "elderly",0))

#1.2 membuat kurva kaplan meier berdasarkan kategori age
km_age <- survfit(surv_object ~ cat_age,
                  data = tugas_analisis_kesintasan)

#1.3 membuat plot kesintasan berdasarkan cat_age
ggsurvplot(km_age)
ggsurvplot(km_age,
           xlab = "Time in days", #Customize X axis label.
           break.time.by = 500, # Break X axis in time intervals by 3.
           pval = TRUE, # Show p-value of log-rank test.
           conf.int = TRUE, # Show confidence intervals for point estimates of survival curves.
           conf.int.style = "step", # Customize style of confidence intervals
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ncensor.plot = FALSE, # Plot the number of censored subjects at time t
           legend.title = "Age Category",
           legend.labs = c("Adult", "Elderly"),
           fun = "pct", # percent An arbitrary function defining a transformation of the survival curve.
           palette = c("#E7B800", "#2E9FDF"))


#1.4 membuat uji log rank berdasarkan cat_age
diff_age <- survdiff(surv_object ~ cat_age, data = tugas_analisis_kesintasan, rho=0)

#2.1 membuat kategori bmi
tugas_analisis_kesintasan$cat_bmi <- ifelse(tugas_analisis_kesintasan$BMI <= 18.4, "Underweight",
                                            ifelse(tugas_analisis_kesintasan$BMI < 25.0, "Normal",
                                                   ifelse(tugas_analisis_kesintasan$BMI <30.0, "Overweight",
                                                          ifelse(tugas_analisis_kesintasan$BMI >= 30.0, "Obese",
                                                                 0))))

#2.2 membuat kurva kaplan meier berdasarkan cat_bmi
km_bmi <- survfit(surv_object ~ cat_bmi,
                  data = tugas_analisis_kesintasan)

#2.3 membuat plot kesintasan berdasarkan cat_bmi
ggsurvplot(km_bmi)
ggsurvplot(km_bmi,
           xlab = "Time in days", #Customize X axis label.
           break.time.by = 500, # Break X axis in time intervals by 3.
           pval = TRUE, # Show p-value of log-rank test.
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ncensor.plot = FALSE, # Plot the number of censored subjects at time t
           legend.title = "BMI Category",
           legend.labs = c("Underweight", "Normal", "Overweight", "Obese"),
           fun = "pct", # percent An arbitrary function defining a transformation of the survival curve.
           palette = c("#018571", "#DC143C", "#0571B0", "#7B68EE"))

#2.4 membuat uji log rank berdasarkan cat_age
diff_bmi <- survdiff(surv_object ~ cat_bmi, data = tugas_analisis_kesintasan, rho=0)

#3.1 membuat kategori cvd
tugas_analisis_kesintasan$cat_cvd <- ifelse(tugas_analisis_kesintasan$CVD < 1, "No",
                                            ifelse(tugas_analisis_kesintasan$CVD >= 1, "Yes",0))
                                                                                                                    

#3.2 membuat kurva kaplan meier berdasarkan cat_cvd
km_cvd <- survfit(surv_object ~ cat_cvd,
                  data = tugas_analisis_kesintasan)

#3.3 membuat plot kesintasan berdasarkan cat_cvd
ggsurvplot(km_cvd)
ggsurvplot(km_cvd,
           xlab = "Time in days", #Customize X axis label.
           break.time.by = 500, # Break X axis in time intervals by 3.
           pval = TRUE, # Show p-value of log-rank test.
           conf.int = TRUE, # Show confidence intervals for point estimates of survival curves.
           conf.int.style = "step", # Customize style of confidence intervals
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ncensor.plot = FALSE, # Plot the number of censored subjects at time t
           legend.title = "Cardiovascular Disease History",
           legend.labs = c("No", "Yes"),
           fun = "pct", # percent An arbitrary function defining a transformation of the survival curve.
           palette = c("#018571", "#E7B800"))

#3.4 membuat uji log rank berdasarkan cat_cvd
diff_cvd <- survdiff(surv_object ~ cat_cvd, data = tugas_analisis_kesintasan, rho=0)

#4.1 membuat kategori cardiogenic shock
tugas_analisis_kesintasan$cat_sho <- ifelse(tugas_analisis_kesintasan$SHO < 1, "No",
                                            ifelse(tugas_analisis_kesintasan$SHO >= 1, "Yes",0))


#4.2 membuat kurva kaplan meier berdasarkan cat_sho
km_sho <- survfit(surv_object ~ cat_sho,
                  data = tugas_analisis_kesintasan)

#4.3 membuat plot kesintasan berdasarkan cat_sho
ggsurvplot(km_sho)
ggsurvplot(km_sho,
           xlab = "Time in days", #Customize X axis label.
           break.time.by = 500, # Break X axis in time intervals by 3.
           pval = TRUE, # Show p-value of log-rank test.
           conf.int = TRUE, # Show confidence intervals for point estimates of survival curves.
           conf.int.style = "step", # Customize style of confidence intervals
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ncensor.plot = FALSE, # Plot the number of censored subjects at time t
           legend.title = "Cardiogenic Shock",
           legend.labs = c("No", "Yes"),
           fun = "pct", # percent An arbitrary function defining a transformation of the survival curve.
           palette = c("#018571", "#E7B800"))

#4.4 membuat uji log rank berdasarkan cat_sho
diff_sho <- survdiff(surv_object ~ cat_sho, data = tugas_analisis_kesintasan, rho=0)

#5.1 membuat kategori complete heart block
tugas_analisis_kesintasan$cat_av3 <- ifelse(tugas_analisis_kesintasan$AV3 < 1, "No",
                                            ifelse(tugas_analisis_kesintasan$AV3 >= 1, "Yes",0))


#5.2 membuat kurva kaplan meier berdasarkan cat_av3
km_av3 <- survfit(surv_object ~ cat_av3,
                  data = tugas_analisis_kesintasan)

#5.3 membuat plot kesintasan berdasarkan cat_av3
ggsurvplot(km_av3)
ggsurvplot(km_av3,
           xlab = "Time in days", #Customize X axis label.
           break.time.by = 500, # Break X axis in time intervals by 3.
           pval = TRUE, # Show p-value of log-rank test.
           conf.int = TRUE, # Show confidence intervals for point estimates of survival curves.
           conf.int.style = "step", # Customize style of confidence intervals
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ncensor.plot = FALSE, # Plot the number of censored subjects at time t
           legend.title = "Complete Heart Block",
           legend.labs = c("No", "Yes"),
           fun = "pct", # percent An arbitrary function defining a transformation of the survival curve.
           palette = c("#018571", "#E7B800"))

#5.4 membuat uji log rank berdasarkan cat_av3
diff_av3 <- survdiff(surv_object ~ cat_av3, data = tugas_analisis_kesintasan, rho=0)

#6.1 membuat kategori myocardial infarction
tugas_analisis_kesintasan$cat_miord <- ifelse(tugas_analisis_kesintasan$MIORD < 1, "No",
                                            ifelse(tugas_analisis_kesintasan$MIORD >= 1, "Yes",0))


#6.2 membuat kurva kaplan meier berdasarkan cat_miord
km_miord <- survfit(surv_object ~ cat_miord,
                  data = tugas_analisis_kesintasan)

#6.3 membuat plot kesintasan berdasarkan cat_miord
ggsurvplot(km_miord)
ggsurvplot(km_miord,
           xlab = "Time in days", #Customize X axis label.
           break.time.by = 500, # Break X axis in time intervals by 3.
           pval = TRUE, # Show p-value of log-rank test.
           conf.int = TRUE, # Show confidence intervals for point estimates of survival curves.
           conf.int.style = "step", # Customize style of confidence intervals
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ncensor.plot = FALSE, # Plot the number of censored subjects at time t
           legend.title = "Myocardial Infarction",
           legend.labs = c("No", "Yes"),
           fun = "pct", # percent An arbitrary function defining a transformation of the survival curve.
           palette = c("#018571", "#E7B800"))

#6.4 membuat uji log rank berdasarkan cat_miord
diff_miord <- survdiff(surv_object ~ cat_miord, data = tugas_analisis_kesintasan, rho=0)