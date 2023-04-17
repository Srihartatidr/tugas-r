#mengaktifkan paket
library(readxl)
library(survival)
library(survminer)
library("PerformanceAnalytics")

#mengambil data
tugas_ch <- read_excel("C:/Users/surfa/Downloads/praktik analisis kesintasan 2.xlsx")
View(tugas_ch)
str(tugas_ch)

my.datatugasch <- tugas_ch[, c(2, 3, 7, 8, 9, 10, 11, 12, 13)]
chart.Correlation(my.datatugasch, histogram = TRUE, pch = 19)

#memberi label dengan membuat variabel yang baru
tugas_ch$gender1 <- factor(tugas_ch$GENDER, 
                      levels=c(0,1),
                      labels=c("Male","Female"))

tugas_ch$cat_bmi <- ifelse(tugas_ch$BMI <= 18.4, "Underweight",
                                            ifelse(tugas_ch$BMI < 25.0, "Normal",
                                                   ifelse(tugas_ch$BMI <30.0, "Overweight",
                                                          ifelse(tugas_ch$BMI >= 30.0, "Obese",
                                                                 0))))

tugas_ch$cvd1 <- factor(tugas_ch$CVD, 
                           levels=c(0,1),
                           labels=c("No","Yes"))

tugas_ch$afb1 <- factor(tugas_ch$AFB, 
                        levels=c(0,1),
                        labels=c("No","Yes"))

tugas_ch$sho1 <- factor(tugas_ch$SHO, 
                        levels=c(0,1),
                        labels=c("No","Yes"))

tugas_ch$chf1 <- factor(tugas_ch$CHF, 
                        levels=c(0,1),
                        labels=c("No","Yes"))

tugas_ch$av31 <- factor(tugas_ch$AV3, 
                        levels=c(0,1),
                        labels=c("No","Yes"))

tugas_ch$miord1 <- factor(tugas_ch$MIORD, 
                        levels=c(0,1),
                        labels=c("No","Yes"))

#1. performing univariable/simple cox PH regression
uni.cox1 <- coxph(Surv(LENFOL, FSTAT) ~ gender1, 
                 data = tugas_ch)
summary(uni.cox1)

#2. performing univariable/simple cox PH regression
uni.cox2 <- coxph(Surv(LENFOL, FSTAT) ~ cat_bmi, 
                 data = tugas_ch)
summary(uni.cox2)

#3. performing univariable/simple cox PH regression
uni.cox3 <- coxph(Surv(LENFOL, FSTAT) ~ AGE, 
                  data = tugas_ch)
summary(uni.cox3)

#4. performing univariable/simple cox PH regression
uni.cox4 <- coxph(Surv(LENFOL, FSTAT) ~ cvd1, 
                  data = tugas_ch)
summary(uni.cox4)

#5. performing univariable/simple cox PH regression
uni.cox5 <- coxph(Surv(LENFOL, FSTAT) ~ afb1, 
                  data = tugas_ch)
summary(uni.cox5)

#6. performing univariable/simple cox PH regression
uni.cox6 <- coxph(Surv(LENFOL, FSTAT) ~ sho1, 
                  data = tugas_ch)
summary(uni.cox6)

#7. performing univariable/simple cox PH regression
uni.cox7 <- coxph(Surv(LENFOL, FSTAT) ~ chf1, 
                  data = tugas_ch)
summary(uni.cox7)

#8. performing univariable/simple cox PH regression
uni.cox8 <- coxph(Surv(LENFOL, FSTAT) ~ av31, 
                  data = tugas_ch)
summary(uni.cox8)

#9. performing univariable/simple cox PH regression
uni.cox9 <- coxph(Surv(LENFOL, FSTAT) ~ miord1, 
                  data = tugas_ch)
summary(uni.cox9)

#10. Performing multivariable Cox PH model regression
multi.cox <- coxph(Surv(LENFOL, FSTAT) ~ AGE + gender1 + cat_bmi,
                   data =  tugas_ch)
summary(multi.cox)

#Schoenfeld residuals to check the proportional hazards assumption
test.ph <- cox.zph(multi.cox)
test.ph

ggcoxzph(test.ph)

#To test influential observations
ggcoxdiagnostics(multi.cox, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#check outliers by visualizing the deviance residuals.
ggcoxdiagnostics(multi.cox, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())

#plotting the Martingale residuals against continuous covariates
ggcoxfunctional(Surv(LENFOL, FSTAT) ~ AGE + log(AGE) + sqrt(AGE), data = tugas_ch)

res.cox <- coxph(Surv(LENFOL, FSTAT) ~ AGE + gender1 + cat_bmi, data =  tugas_ch)
res.cox
