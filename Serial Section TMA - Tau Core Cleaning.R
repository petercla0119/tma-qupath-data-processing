library(readxl)
library(dplyr)
missing_core_run2_25 <- c("A-1", "A-12", "B-12", "C-12", "D-12", "E-3","E-12", "F-3", "F-12", "G-12", "H-11", "I-10", "J-9", "K-2", "K-8", "L-2", "L-7", "M-2", "M-6", "N-5", "O-4", "P-3", "P-8", "Q-2", "R-1")
glass_placenta_background <- c("Glass", "Placenta", "Background")
########  RUN 2 SLIDE 1   #######
r2s1 <- read_excel("U:/TMA/Custom Algorithm - Tau/R2S1 11_4_2019 RESULTS/AT8-DAB-6_3_19.mrxs.xlsx", sheet = "Sheet1")

#remove D-3, c1, b1,i7
r2s1_missing <- c("D-3", "C-1", "B-1", "I-7")
r2s1 <- r2s1 %>% 
  filter(!`TMA core`%in% missing_core_run2_25) %>%
  filter(!`TMA core`%in% r2s1_missing) %>%
  filter(!Class %in% glass_placenta_background) %>%
  arrange(`TMA core`)
write.csv(r2s1, "R2S1_CustomAlg_Clean.csv")
########  RUN 2 SLIDE 1   #######
r3s3 <- read_excel("U:/TMA/Custom Algorithm - Tau/R3S3 SERIAL.NUM 13 RESULTS/R3S3 Serial.no 13.xlsx", sheet = "Sheet1")
missing_core_run2_25 <- c("A-1", "A-12", "B-12", "C-12", "D-12", "E-3","E-12", "F-3", "F-12", "G-12", "H-11", "I-10", "J-9", "K-2", "K-8", "L-2", "L-7", "M-2", "M-6", "N-5", "O-4", "P-3", "P-8", "Q-2", "R-1")
r3s3_missing <- c("Q-10", "K-12", "B-1", "I-7", "P-5")
r3s3 <- r3s3 %>% 
  filter(!`TMA core`%in% missing_core_run2_25) %>%
  filter(!`TMA core`%in% r3s3_missing) %>%
  filter(!Class %in% glass_placenta_background) %>%
  arrange(`TMA core`)
write.csv(r3s3, "R3S3_CustomAlg_Clean.csv")


          # ########  RUN 2 SLIDE 2   #######
          # r2s2_missing <- c("D-3", "R-8", "R-2", "R-11", "I-7", "E-7")
          # r2s2 <- read_excel("Custom algorithm results.xlsx", sheet = "Sheet2")
          # r2s2 <- r2s2 %>% 
          #   filter(!`TMA core`%in% missing_core_run2_25) %>%
          #   filter(!`TMA core`%in% r2s2_missing) %>%
          #   arrange(`TMA core`)
          # write.csv(r2s2, "R2S2_CustomAlg_Clean2.csv")
          # ########  RUN 2 SLIDE 3   #######
          # r2s3_missing <- c("D-3", "I-7")# possibly k-12, q-10, p4,p5
          # r2s3 <- read_excel("Custom algorithm results.xlsx", sheet = "R2S3")
          # r2s3 <- r2s3 %>% 
          #   filter(!`TMA core`%in% missing_core_run2_25) %>%
          #   filter(!`TMA core`%in% r2s3_missing) %>%
          #   arrange(`TMA core`)
          # write.csv(r2s3, "R2S3_CustomAlg_Clean.csv")


