library(readxl)
library(readr)
library(ggplot2)
tugas_regresi <- w5_pef_lj_age_sex_height_pef

#mengetahui summary dari variabel pef
summary(tugas_regresi$pef)

#nomor 1
#mengidentifikasi outlier variabel pef berdasarkan kriteria boxplot
#website rujukan https://statsandr.com/blog/outliers-detection-in-r/
boxplot(tugas_regresi$pef)
ggplot(tugas_regresi) + 
  aes(x= "", y = pef) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#mengekstraksi nilai outlier berdasarkan kriteria iqr
boxplot.stats(tugas_regresi$pef)$out

#mengetahui baris mana yang mengandung nilai outlier
out <- boxplot.stats(tugas_regresi$pef)$out
out_ind <- which(tugas_regresi$pef %in% c(out))
out_ind

#nomor 2
#menentukan batas atas dan batas bawah outlier
#website https://universeofdatascience.com/how-to-remove-outliers-from-data-in-r/#:~:text=Firstly%2C%20we%20find%20first%20(Q1,()%20function%20to%20remove%20outliers.
quartiles <- quantile(tugas_regresi$pef, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(tugas_regresi$pef)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
#kriteria batas atas outlier jika nilai pef > q3 + 1.5 iqr yaitu >705
#kriteria batas bawah outlier jika nilai pef < q1 - 1.5 iqr yaitu <25

#nomor 3
#membuat dataset yang tidak berisi outlier
data_nooutlier <- subset(tugas_regresi, tugas_regresi$pef > Lower & tugas_regresi$pef < Upper)
dim(data_nooutlier)

#nomor 4
#melakukan tes normalitas pada dataset dengan outlier dan tanpa outlier
library(nortest)
lillie.test(tugas_regresi$pef)
lillie.test(data_nooutlier$pef)

#nomor 5
#membuat grafik qqline untuk membandingkan dataset dengan outlier dan tanpa outlier
qqnorm(tugas_regresi$pef)
qqline(tugas_regresi$pef)
qqnorm(data_nooutlier$pef)
qqline(data_nooutlier$pef)

#nomor 6
#membuat scatterplot antara pef dengan height
#dengan menambahkan garis regresi linear dan garis lengkung (metode loess)
#website http://www.sthda.com/english/wiki/scatter-plots-r-base-graphs
plot(data_nooutlier$height,data_nooutlier$pef)
plot(data_nooutlier$pef~data_nooutlier$height, xlab = "Tinggi Badan (cm)", ylab = "Peak Expiratory Flow", 
     main = "Sebaran PEF berdasarkan Tinggi Badan")
abline(lm(pef ~ height, data = data_nooutlier), col = "red")
lines(lowess(data_nooutlier$height, data_nooutlier$pef), col = "blue")

#nomor 7
#membuat scatterplot antara pef dengan umur
#dengan menambahkan garis regresi linear dan garis lengkung (metode loess)
plot(data_nooutlier$age,data_nooutlier$pef)
plot(data_nooutlier$pef~data_nooutlier$age, xlab = "Umur (tahun)", ylab = "Peak Expiratory Flow", 
     main = "Sebaran PEF berdasarkan umur")
abline(lm(pef ~ age, data = data_nooutlier), col = "red")
lines(lowess(data_nooutlier$age, data_nooutlier$pef), col = "blue")

#tambahan
library(car)
scatterplot(pef ~ height, data = data_nooutlier)
