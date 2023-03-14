library(readr)
library(dplyr)
library(janitor)

#1. mengambil data dari akun github dr.Yayan
pef <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv")

#2. mengidentifikasi duplikasi observasi di variabel pidlink
get_dupes(pef, pidlink)

pef %>% distinct(pidlink, .keep_all = TRUE)

#3. mengidentifikasi observasi yang unik berdasarkan variabel pidlink
pef_final <- pef %>% distinct(pidlink, .keep_all = TRUE)

#4. menghapus duplikasi observasi berdasarkan variabel pidlink
pef_tugas <- pef[!duplicated(pef$pidlink),]