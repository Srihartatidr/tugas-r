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
boxplot(w5_pef_lj_age_sex_height_pef$pef, plot = FALSE)$out)

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
#persamaannya --> y=intercept + slope x
#persamaan --> y=alpha + beta x
#persamannya menjadi --> pef= -840.31 + 7.79 height
#setiap penambahan 1 cm tinggi badan akan menambah pef sebanyak 7.871 L/menit

#membuat objek dari model regresi linear
model1 = lm(pef~height, data=w5_pef_lj_age_sex_height_pef)
model2 = lm(pef~height+age, data=w5_pef_lj_age_sex_height_pef)
lm(pef~height+age, data=w5_pef_lj_age_sex_height_pef)
