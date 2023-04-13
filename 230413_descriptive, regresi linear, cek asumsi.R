#deskriptip statistik dari 4 variabel (pef,age, height, sex)
#tabel univariat
table(w5_pef_lj_age_sex_height_pef$sex)
summary(w5_pef_lj_age_sex_height_pef)

#Persamaan di R dalam bentuk y-x (dependent-independent)
#bentuk x,y atau y~x

#histogram
hist(w5_pef_lj_age_sex_height_pef$age)

#grafik normalitas
qqnorm(w5_pef_lj_age_sex_height_pef$age); qqline(w5_pef_lj_age_sex_height_pef$age)

#boxplot berdasarkan sex ditambahkan label axis
boxplot(w5_pef_lj_age_sex_height_pef$age~w5_pef_lj_age_sex_height_pef$sex,xlab = "Jenis Kelamin", ylab = "Usia (tahun)",
        col = c("red","blue"), main = "Distribusi Usia berdasarkan Jenis Kelamin")

#uji beda rata-rata (mean)
t.test(w5_pef_lj_age_sex_height_pef$age~w5_pef_lj_age_sex_height_pef$sex)

#tes normalitas
shapiro.test(w5_pef_lj_age_sex_height_pef$age)

#normality test for big sample
library(nortest)
lillie.test(w5_pef_lj_age_sex_height_pef$age)

#scatterplot
plot(w5_pef_lj_age_sex_height_pef$height,w5_pef_lj_age_sex_height_pef$pef)
plot(w5_pef_lj_age_sex_height_pef$pef~w5_pef_lj_age_sex_height_pef$height, xlab = "Tinggi Badan (cm)", ylab = "Peak Expiratory Flow", 
     main = "Sebaran PEF berdasarkan Tinggi Badan")
plot(w5_pef_lj_age_sex_height_pef$pef)

#scatterplot dengan melihat densitas nya
smoothScatter(w5_pef_lj_age_sex_height_pef$pef~w5_pef_lj_age_sex_height_pef$height,
              xlab = "Tinggi Badan (cm)",
              ylab = "Peak Expiratory Flow", 
              main = "Sebaran PEF berdasarkan Tinggi Badan",
             methods = "b" )

#tabel silang
table(w5_pef_lj_age_sex_height_pef$sex,w5_pef_lj_age_sex_height_pef$sc01_14_14)
table(w5_pef_lj_age_sex_height_pef$sc01_14_14,w5_pef_lj_age_sex_height_pef$sex)
stem(w5_pef_lj_age_sex_height_pef$age)
main()

#membuat boxplot dari variabel pef
boxplot(w5_pef_lj_age_sex_height_pef$pef)

#mendeteksi outlier dari variabel pef
boxplot(w5_pef_lj_age_sex_height_pef$pef)

#mendeteksi nilai minimum dari outlier dari variabel pef
min(boxplot(w5_pef_lj_age_sex_height_pef$pef, plot = FALSE)$out)

#membuat qq line
qqnorm(w5_pef_lj_age_sex_height_pef$pef)
qqline(w5_pef_lj_age_sex_height_pef$pef)

#menguji korelasi (default nya pakai pearson)
cor.test(w5_pef_lj_age_sex_height_pef$pef, w5_pef_lj_age_sex_height_pef$height)
cor.test(w5_pef_lj_age_sex_height_pef$pef, w5_pef_lj_age_sex_height_pef$height, alternative="greater")

#membuat linear regresi
lm(y~x,data=ds)
lm(pef~height, data=w5_pef_lj_age_sex_height_pef)
#persamaannya menjadi y=intercept + slope x
#persamaan y=alpha + beta x
#menjadi pef= -840.31 + 7.79 height
#setiap penambahan 1 cm tinggi badan akan menambah pef sebanyak 7.871 L/menit

#membuat objek dari model regresi linear
model1 = lm(pef~height, data=w5_pef_lj_age_sex_height_pef)
model2 = lm(pef~height+age, data=w5_pef_lj_age_sex_height_pef)
lm(pef~height+age, data=w5_pef_lj_age_sex_height_pef)

#diagnostic regression dengan memeriksa linearity, normality, homoscedasticity, independence

#1. memeriksa linearity yaitu apakah variabel prediktor dan outcome dapat dibuat hubungannya secara linear
#periksa apakah perlu menambahkan non-linearity term
#1.1 menguji linearity secara uji statistik
model2 = lm(pef~height+age, data=w5_pef_lj_age_sex_height_pef)
summary(model2)
#interpretasi dari multiple dan adjusted R-squared --> 50% variasi dari pef itu bisa dijelaskan karena ada perbedaan tinggi badan dan usia.
#membandingkan dengan resettest akan memberikan indikasi apakah dengan menggunakan model
#model yang mengandung non-linear term (kuadrat atau kubik) akan meningkatkan power
#non linear term artinya kurva
library(lmtest)
#default menambahkan power nya 2;3
resettest(model2)
resettest(model2, power =2:3)
resettest(model2, power =2:4)
#hasil p-value < 0.05 menunjukkan bahwa penambahan non linear term akan sangat meningkatkan power


#1.2 menguji linearity secara visual
#1.2.1 membuat variable predictive value dan residualnya
library(dplyr)
w5_pef_lj_age_sex_height_pef <-
   w5_pef_lj_age_sex_height_pef %>%
     mutate(yhat = fitted(model2),
            res = residuals(model2))
   
#1.2.2 membuat plot dari residual
library(ggplot2)
ggplot(w5_pef_lj_age_sex_height_pef, aes(yhat, res)) + 
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth()

ggplot(w5_pef_lj_age_sex_height_pef, aes(age, res)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth()

ggplot(w5_pef_lj_age_sex_height_pef, aes(height, res)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth()

#karena ada outlier, bagaimana corrective action nya untuk membuat model yg lebih baik
#1. Add polynomial term (squared, cubic, etc) and or interaction term
#2. Generalized Linear Model
#3. Add instrumental variable

#2. memeriksa normality yaitu memeriksa normality dari residuals persamaan yang telah dibuat
#deviasi terhadap normalitas itu wajar jika jumlah sampelnya sedikit
#2.1 uji statistik
library(nortest)
lillie.test(w5_pef_lj_age_sex_height_pef$res)

#2.2 uji visual
ggplot() +
  geom_qq(aes(sample = rstandard(model2))) +
  geom_abline(color = "red") +
  coord_fixed()

#3. memeriksa homoscedasticity yaitu memperlihatkan variasi (variance) dari residuals diharapkan bisa homogen
#apakah variance nya konstan sepanjang nilai dari variabel nya atau nilai predicted value nya atau tidak
#3.1 uji statistik Bresucsh-Pagan
library(car)
ncvTest(model2)
#kalau p-value <0.005 berarti variance nya non constant
#null hypo nya variance equal constant

#3.2 uji visual
#3.2.1 membuat variabel variance
w5_pef_lj_age_sex_height_pef <-
  w5_pef_lj_age_sex_height_pef %>%
  mutate(res_sqrt = sqrt(abs(rstandard(model2))))

#3.2.2 membuat plot dari variance
ggplot() +
  geom_qq(aes(sample = rstandard(model2))) +
  geom_abline(color = "red") +
  coord_fixed()

ggplot(w5_pef_lj_age_sex_height_pef, aes(yhat, res_sqrt)) +
  geom_point() +
  geom_smooth()

#4. memeriksa independence yaitu observasi dilakukan secara independen.
#dilihat dari bagaimana cara data tersebut dikumpulkan
#apakah subjek dipilih berdasarkan kriteria yang berhubungan
#cek apakah ada repetead measurement in the same subject - temporal related?
#cek apakah ada pengambilan sampel dalam satu cluster yang kemungkinan mempunyai karakteristik yg sama
#misalnya memeriksa subjek yg diperiksa di satu rumah, apakah ada pengaruh dari tinggal di rumah yg sama
#cek apakah observasi berkaitan secara spasial

#5. isu multicollinearity (bukan asumsi)
#memastikan tdk ada collinearity pada variabel prediktor
#misalnya memeriksa variabel height dengan age, dan ada hubungan antara dua variabel tersebut
#maka berpotensi ada multicollinearity
#diperiksa dengan cek variance infation factor (VIF)
library(car)
vif(model2)
#jadi catatan jika nilai vif >4

cor.test(w5_pef_lj_age_sex_height_pef, alternative = "greater")
