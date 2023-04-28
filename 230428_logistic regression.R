library(readxl)
library(readr)
library(ggplot2)
library(dplyr)

#regresi logistik
prak_logres <- w5_pef_lj_age_sex_height_pef
names(prak_logres)
table(prak_logres$Asthma)

#memilih dataset dengan informasi "riwayat asthma" yang tersedia
#memilih berdasarkan kriteria kuantitatif tertentu
prak_logres_as = filter(w5_pef_lj_age_sex_height_pef, Asthma == "No-Asthma" | Asthma == "Yes-Asthma")
class(w5$Asthma)
table(prak_logres_as$Asthma)

model_lr = glm(Asthma ~ pef, prak_logres_as, family = binomial)

#R tidak menerima karakter sehingga variabel outcome harus diubah jadi 1 dan 0
#membuat variabel baru
prak_logres_as$as_new = 1
prak_logres_as$as_new[prak_logres_as$Asthma == "No-Asthma"] = 0
table(prak_logres_as$as_new)

#ulangi prosedur glm dengan variabel outcome yang sudah direkoding jadi 1 dan 0
model_lr = glm(as_new ~ pef, prak_logres_as, family = binomial)
summary(model_lr)