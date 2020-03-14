library(ggplot2)
library(BlandAltmanLeh)
library(readxl)

# Set the working directory in the appropriate folder using the correct computer path.  
setwd("U:/FTDC-IrwinLab/TMA Take 2/AT8 TMA HALO results")

# Below hypothetical datasets where "control" is the training data including tissue run in SB1 and SB2, whereas "data" is the dataset with tissue from SB2 to transform into something comparable to SB1. Fill in the correct path to open the datasets on your computer.

df_halo <- read.csv("TMA_AT8_HALO_RESULTS.csv")

# Summary statistics of control data: SB1 vs. SB2 

AO<-df_halo[,c(2,4,6,8,10,12,14,16)]

summary(AO)

sd(df_halo$R1.S1..AO, na.rm = TRUE)
sd(df_halo$R1.S2..AO, na.rm = TRUE)
sd(df_halo$R2.S1..AO, na.rm = TRUE)
sd(df_halo$R2.S2..AO, na.rm = TRUE)
sd(df_halo$R2.S3..AO, na.rm = TRUE)
sd(df_halo$R3.S1..AO, na.rm = TRUE)
sd(df_halo$R3.S2..AO, na.rm = TRUE)
sd(df_halo$R3.S3..AO, na.rm = TRUE)

summary(control$WM_SB1)
summary(control$WM_SB2)

sd(control$WM_SB1, na.rm = TRUE)
sd(control$WM_SB2, na.rm = TRUE)

# Bland-Altman analysis comparing SB1 vs. SB2 data (mean diff, one-sample t-test with mu = 0) in control dataset. 

df_halo$R1.S1_R1.S2 <- df_halo$R1.S1..AO - df_halo$R1.S2..AO
df_halo$R1.S1_R1.S2 <- df_halo$R1.S1..AO - df_halo$R2.S1..AO
df_halo$R1.S1_R2.S2 <- df_halo$R1.S1..AO - df_halo$R2.S2..AO
df_halo$R1.S1_R2.S3 <- df_halo$R1.S1..AO - df_halo$R2.S3..AO
df_halo$R1.S1_R3.S1 <- df_halo$R1.S1..AO - df_halo$R3.S1..AO
df_halo$R1.S1_R3.S2 <- df_halo$R1.S1..AO - df_halo$R3.S2..AO
df_halo$R1.S1_R3.S3 <- df_halo$R1.S1..AO - df_halo$R3.S3..AO

df_halo$R1.S2_R2.S1 <- df_halo$R1.S2..AO - df_halo$R2.S1..AO
df_halo$R1.S2_R2.S2 <- df_halo$R1.S2..AO - df_halo$R2.S2..AO
df_halo$R1.S2_R2.S3 <- df_halo$R1.S2..AO - df_halo$R2.S3..AO
df_halo$R1.S2_R3.S1 <- df_halo$R1.S2..AO - df_halo$R3.S1..AO
df_halo$R1.S2_R3.S2 <- df_halo$R1.S2..AO - df_halo$R3.S2..AO
df_halo$R1.S2_R3.S3 <- df_halo$R1.S2..AO - df_halo$R3.S3..AO

df_halo$R2.S1_R2.S2 <- df_halo$R2.S1..AO - df_halo$R2.S2..AO
df_halo$R2.S1_R2.S3 <- df_halo$R2.S1..AO - df_halo$R2.S3..AO
df_halo$R2.S1_R3.S1 <- df_halo$R2.S1..AO - df_halo$R3.S1..AO
df_halo$R2.S1_R3.S2 <- df_halo$R2.S1..AO - df_halo$R3.S2..AO
df_halo$R2.S1_R3.S3 <- df_halo$R2.S1..AO - df_halo$R3.S3..AO

df_halo$R2.S2_R2.S3 <- df_halo$R2.S2..AO - df_halo$R2.S3..AO
df_halo$R2.S2_R3.S1 <- df_halo$R2.S2..AO - df_halo$R3.S1..AO
df_halo$R2.S2_R3.S2 <- df_halo$R2.S2..AO - df_halo$R3.S2..AO
df_halo$R2.S2_R3.S3 <- df_halo$R2.S2..AO - df_halo$R3.S3..AO

df_halo$R2.S3_R2.S1 <- df_halo$R2.S3..AO - df_halo$R3.S1..AO
df_halo$R2.S3_R3.S2 <- df_halo$R2.S3..AO - df_halo$R3.S2..AO
df_halo$R2.S3_R3.S3 <- df_halo$R2.S3..AO - df_halo$R3.S3..AO

df_halo$R3.S1_R3.S1 <- df_halo$R3.S1..AO - df_halo$R3.S1..AO
df_halo$R3.S1_R3.S2 <- df_halo$R3.S1..AO - df_halo$R3.S2..AO
df_halo$R3.S1_R3.S3 <- df_halo$R3.S2..AO - df_halo$R3.S3..AO

all.diff<-rbind(R1.S1_R1.S2, R1.S1_R2.S2, R1.S1_R2.S3,R1.S1_R3.S1, R1.S1_R3.S2, R1.S1_R3.S3, R1.S2_R2.S1, R1.S2_R2.S2, R1.S2_R2.S3, R1.S2_R3.S1, R1.S2_R3.S2, R1.S2_R3.S3, R2.S1_R2.S2, R2.S1_R2.S3, R2.S1_R3.S1, R2.S1_R3.S2, R2.S1_R3.S3, R2.S2_R2.S3, R2.S2_R3.S1, R2.S2_R3.S2, R2.S2_R3.S3, R2.S3_R2.S1, R2.S3_R3.S2, R2.S3_R3.S3, R3.S1_R3.S1, R3.S1_R3.S2, R3.S1_R3.S3)

differences <- rowMeans(all.diff,na.rm = T)
      mean(R1.S1_R1.S2, na.rm = T)   #confirm accurate values

differences <-cbind(rowSds(all.diff, na.rm=T), differences)

t.test(R1.S1_R1.S2, mu = 0)
t.test(R1.S1_R2.S2, mu = 0)
t.test(R1.S1_R2.S3, mu = 0)
t.test(R1.S1_R3.S1, mu = 0)
t.test(R1.S1_R3.S2, mu = 0)
t.test(R1.S1_R3.S3, mu = 0)
t.test(R1.S2_R2.S1, mu = 0)   #*** p>0.05 - p=0.107
t.test(R1.S2_R2.S2, mu = 0)   #*** p>0.05 - p=0.107
t.test(R1.S2_R2.S3, mu = 0)   #*** p>0.05 - p=0.107
t.test(R1.S2_R3.S1, mu = 0)   #*** p>0.05 - p=0.107
t.test(R1.S2_R3.S2, mu = 0)   #*** p>0.05 - p=0.1198
t.test(R1.S2_R3.S3, mu = 0)   #*** p>0.05 - p=0.3516
t.test(R2.S1_R2.S2, mu = 0)   #*** p>0.05 - p=0.987
t.test(R2.S1_R2.S3, mu = 0)
t.test(R2.S1_R3.S1, mu = 0)   #*** p>0.05 - p=0.987
t.test(R2.S1_R3.S2, mu = 0)   #*** p>0.05 - p=0.7565
t.test(R2.S1_R3.S3, mu = 0)   #*** p>0.05 - p=0.07309
t.test(R2.S2_R2.S3, mu = 0)
t.test(R2.S2_R3.S1, mu = 0)   #*** p=NA?
t.test(R2.S2_R3.S2, mu = 0)   #*** p>0.05 - p=0.7256
t.test(R2.S2_R3.S3, mu = 0)   #*** p>0.05 - p=0.09507
t.test(R2.S3_R2.S1, mu = 0)
t.test(R2.S3_R3.S2, mu = 0)   #*** p>0.05 - p=0.111
t.test(R2.S3_R3.S3, mu = 0)   #*** p>0.05 - p=0.9662
t.test(R3.S1_R3.S1, mu = 0)   #*** p=NA
t.test(R3.S1_R3.S2, mu = 0)   #*** p>0.05 - p=0.7256
t.test(R3.S1_R3.S3, mu = 0)   #*** p>0.05 - p=0.06847


# Linear model to obtain linear trends (R squared) and transformation factors for subsequent transformation. Below also the script for a linear plot using ggplot.  

Reg <- lm(df_halo$R1.S2..AO ~ df_halo$R3.S2..AO)

summary(Reg)

ggplot(data = df_halo, aes(x = R2.S1..AO, y = R3.S1..AO)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("SB2 ln %AO") + ylab("SB1 ln %AO") + ggtitle("GM linear plot") + theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16), plot.title = element_text(size = 20, face = "bold"), legend.text = element_text(size = 18), legend.title = element_text(size = 20))
ggsave("ScatterGM.png", width = 6, height = 5)

cor(df_halo$R1.S2..AO, df_halo$R2.S1..AO, use = "complete.obs")

RegWM  <- lm(control$WM_SB1 ~ control$WM_SB2)

summary(RegWM)

ggplot(data = control, aes(x = WM_SB2, y = WM_SB1)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("SB2 ln %AO") + ylab("SB1 ln %AO") + ggtitle("WM linear plot") + theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16), plot.title = element_text(size = 20, face = "bold"), legend.text = element_text(size = 18), legend.title = element_text(size = 20))
ggsave("ScatterWM.png", width = 6, height = 5)

# Apply the transformation on SB2-stained data in the new dataset "data". We obtain a new variable which contains transformed SB2 data (tSB2). 

data$GM_tSB2 <-  coef(summary(RegGM))[2, 1]  * data$GM_SB2 + coef(summary(RegGM))[1, 1]

data$WM_tSB2 <- coef(summary(RegWM))[2, 1] * data$WM_SB2 + coef(summary(RegWM))[1, 1]
