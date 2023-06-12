library(readxl)
library(readr)
library(ggplot2)
library(dplyr)

#membuat objek tugas
tugas_reglin <- w5_pef_lj_age_sex_height_pef

#mengetahui summary dari variabel pef
View(tugas_reglin)
summary(tugas_reglin$pef)

#nomor 1
#mengidentifikasi outlier variabel PEF berdasarkan visualisasi grafik boxplot
#website rujukan https://statsandr.com/blog/outliers-detection-in-r/
boxplot(tugas_reglin$pef)
ggplot(tugas_reglin) + 
  aes(x= "", y = pef) +
  geom_boxplot(fill = "grey", outlier.color="blue") +
  labs(title="Boxplot of Peak of Eploratory Flow (PEF)", y="PEF value") +
  theme_minimal()

#mengekstraksi nilai outlier berdasarkan kriteria iqr
boxplot.stats(tugas_reglin$pef)$out

#mengetahui baris mana yang mengandung nilai outlier
out <- boxplot.stats(tugas_regresi$pef)$out
out_ind <- which(tugas_regresi$pef %in% c(out))
out_ind

#nomor 2
#menentukan batas atas dan batas bawah outlier dengan IQR
#menghitung quartiles dan IQR
quartiles <- quantile(tugas_reglin$pef, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(tugas_reglin$pef)
View(quartiles)
View(IQR)

#menentukan cutoff outlier berdasarkan nilai quartiles dan IQR
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR
View(Lower)
View(Upper)

#kriteria batas atas outlier jika nilai pef > Q3 + 1.5 IQR yaitu >705
#kriteria batas bawah outlier jika nilai pef < Q1 - 1.5 IQR yaitu <25

#nomor 3
#membuat dataset yang tidak berisi outlier
data_nooutlier <- subset(tugas_reglin, tugas_reglin$pef > Lower & tugas_reglin$pef < Upper)
dim(data_nooutlier)

#memeriksa apakah di dataset yang baru terdapat outlier atau tidak 
#menggunakan boxplot
boxplot(data_nooutlier$pef)
#menggunakan ggplot
ggplot(data_nooutlier) + 
  aes(x= "", y = pef) +
  geom_boxplot(fill = "grey", outlier.color="blue") +
  labs(title="Boxplot of Peak of Eploratory Flow (PEF)", y="PEF value") +
  theme_minimal()


#nomor 4
#melakukan tes normalitas pada dataset dengan outlier dan tanpa outlier
#karena jumlah sampel nya besar jadi digunakan lillie.test
library(nortest)
lillie.test(tugas_reglin$pef)
lillie.test(data_nooutlier$pef)

#nomor 5
#membuat grafik QQ line untuk membandingkan dataset dengan outlier dan tanpa outlier
#dataset dengan outlier
qqnorm(tugas_reglin$pef)
qqline(tugas_reglin$pef, col="red")

#dataset tanpa outlier
qqnorm(data_nooutlier$pef)
qqline(data_nooutlier$pef, col="red")

#nomor 6
#membuat scatterplot antara pef dengan height
#dengan penambahan garis regresi linear dan smoothed dengan loess
#nomor 6a. pada dataset dengan outlier
ggplot(tugas_reglin, aes(height, pef)) + 
  geom_point() +
  geom_smooth(aes(color= "linear"), method = "lm", se = FALSE) +
  geom_smooth(aes(color= "loess"), method = "loess", se = FALSE) +
  ggtitle("Sebaran PEF berdasarkan tinggi") +
  labs(color= "Metode") +
  theme(plot.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.title.x = element_text(size = 13))

#nomor 6b. pada dataset tanpa outlier
ggplot(data_nooutlier, aes(height, pef)) + 
  geom_point() +
  geom_smooth(aes(color= "linear"), method = "lm", se = FALSE) +
  geom_smooth(aes(color= "loess"), method = "loess", se = FALSE) +
  ggtitle("Sebaran PEF berdasarkan tinggi") +
  labs(color= "Metode") +
  theme(plot.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.title.x = element_text(size = 13))

#nomor 7
#membuat scatterplot antara pef dengan umur
#dengan penambahan garis regresi linear dan smoothed dengan loes
#nomor 7a. pada dataset dengan outlier
ggplot(tugas_reglin, aes(age, pef)) + 
  geom_point() +
  geom_smooth(aes(color= "linear"), method = "lm", se = FALSE) +
  geom_smooth(aes(color= "loess"), method = "loess", se = FALSE) +
  ggtitle("Sebaran PEF berdasarkan umur") +
  labs(color= "Metode") +
  theme(plot.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.title.x = element_text(size = 13))

#nomor 7b. pada dataset tanpa outlier
plot(data_nooutlier$pef~data_nooutlier$age, xlab = "Umur (tahun)", ylab = "Peak Expiratory Flow", 
     main = "Sebaran PEF berdasarkan umur")
ggplot(data_nooutlier, aes(age, pef)) + 
  geom_point() +
  geom_smooth(aes(color= "linear"), method = "lm", se = FALSE) +
  geom_smooth(aes(color= "loess"), method = "loess", se = FALSE) +
  ggtitle("Sebaran PEF berdasarkan umur") +
  labs(color= "Metode") +
  theme(plot.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.title.x = element_text(size = 13))
