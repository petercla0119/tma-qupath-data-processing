library(dplyr)

#   Create a list for each of the possible missing core groupings
# Run 1 slide 1 TDP
RUN1SLIDE1MISSING <- c("A-9","B-9", "C-9", "G-2","G-3","G-4","G-5","G-6","G-8","G-9","I-5","I-8","J-2","K-1", "K-2","K-3","L-1","L-2","L-3","L-4","M-1")

RUN1SLIDE2MISSING <- c("A-1","A-2", "A-3","A-4","A-9", "B-3","B-4","B-9","C-9","E-1","E-2","E-3","E-5", "F-1","F-2","F-4","G-1","G-2","G-3","G-4", "G-5","G-6","G-7","G-8","G-9","H-1", "I-5", "I-8","L-4","M-1")

RUN2SLIDE1 <- c("A-9","B-9", "C-9", "E-5","E-9","F-4","G-2","G-3","G-4","G-5","G-6","G-8","G-9","J-5","L-5","M-1")
RUN2SLIDE2 <- c("A-9","B-9", "C-9", "E-5","F-4","G-2","G-3","G-4","G-5","G-6","G-8","G-9","I-5","I-9","L-5","M-1")
RUN2SLIDE3 <- c("A-9","B-9", "C-9", "E-5","G-2","G-3","G-4","G-5","G-6","G-8","G-9","I-5","I-8","I-9","L-4","M-1")

RUN3SLIDE1 <- c("A-9","B-9", "C-9", "E-5","F-4","G-2","G-3","G-4","G-5","G-6","G-8","G-9","M-1")

RUN3SLIDE2 <- c("A-9","B-9", "C-9", "E-5","F-4","G-2","G-3","G-4","G-5","G-6","G-8","G-9","I-5","M-1")

RUN3SLIDE3 <- c("A-9","B-9", "C-9", "E-5","F-4","G-2","G-3","G-4","G-5","G-6","G-8","G-9","M-1")
x<-highly.varied %>% group_by(CORES)
# Create a function to remove missing cores specific to the slide/dataset
remove_missing_cores <- function(df, cores_missing){
  df <- df %>% 
    filter(!TMA_CORE %in% cores_missing)
  
  return(df)
}
r1.s1.clean <- read_excel("TDP43-DAB-5_29_19.mrxs.xlsx")  
r1.s2.clean <- read_excel("TDP43-DAB-5_29_19_11.06.2019_11.07.20.mrxs.xlsx") 
r2.s1.clean <- read_excel("TDP43-DAB-6_3_19.mrxs.xlsx")
r2.s2.clean <- read_excel("TDP43-DAB-6_3_19_11.06.2019_11.37.12.mrxs.xlsx")
r2.s3.clean <- read_excel("TDP43-DAB-6_3_19_11.06.2019_11.37.12.mrxs.xlsx")
r3.s1.clean <- read_excel("TDP43-DAB-6_5_19.mrxs.xlsx")
r3.s2.clean <- read_excel("TDP43-DAB-6_5_19_11.06.2019_12.12.10.mrxs.xlsx")
r3.s3.clean <- read_excel("TDP43-DAB-6_5_19_11.06.2019_12.14.41.mrxs.xlsx")

####   AT8 RUN1.SLIDE1    ####
df <-read_excel("TDP43-DAB-6_5_19_11.06.2019_12.14.41.mrxs.xlsx")
r1.s2.clean <- remove_missing_cores(df, RUN3SLIDE3)
write.csv(r1.s2.clean, file = "r3.s.clean.csv")    


####   AT8 RUN1.SLIDE2    ####
df<-read_excel("AT8-DAB-5_29_19_11.06.2019_11.13.13.mrxs.xlsx")
r1.s2.clean <- remove_missing_cores(df, missing_core_run1_25)
r1.s2.clean <- r1.s2.clean %>% filter(!Class %in% c('Q-2','Q-12','Q-10','Q-1','P-8','P-4','P-12','O-5','O-4','N-9','N-1','M-1','F-11','L-8','N-7','N-6','M-5','K-12','I-12','H-12','D-3')) %>%
  select(Class, `Positive % of total ROI area (d=2, s=1, tN=0.085, tP=0.146)`)
write.csv(r1.s2.clean, file = "r1.s2.clean.csv")    

####   AT8 RUN2.SLIDE1    ####
#     AT8-DAB-6_3_19.mrxs.qptma.data
df <-read_excel("AT8-DAB-6_3_19.mrxs.xlsx")
r2.s1.clean <- remove_missing_cores(df, missing_core_run2_25)
r2.s1.clean <- r2.s1.clean %>% filter(!Class %in% c('B-1','C-1','Q-3','D-3','K-12','I-7')) %>%
  select(Class, `Positive % of total ROI area (d=2, s=1, tN=0.085, tP=0.146)`)
write.csv(r2.s1.clean, file = "r2.s1.clean.csv")    

####   AT8 RUN2.SLIDE2    ####
#     AT8-DAB-6_3_19_11.06.2019_11.27.57.mrxs.qptma.data
df <- read_excel("AT8-DAB-6_3_19_11.06.2019_11.27.57.mrxs.xlsx")
r2.s2.clean <- remove_missing_cores(df, missing_core_run2_25)
r2.s2.clean <- r2.s2.clean %>% filter(!Class %in% c('Q-10','J-7','K-12','D-3')) %>%
  select(Class, `Positive % of total ROI area (d=2, s=1, tN=0.085, tP=0.146)`)
write.csv(r2.s2.clean, file = "r2.s2.clean.csv")    

####   AT8 RUN2.SLIDE3    ####
#     AT8-DAB-6_3_19_11.06.2019_11.30.51.mrxs.qptma.data
df <-read_excel("AT8-DAB-6_3_19_11.06.2019_11.30.51.mrxs.xlsx")
r2.s3.clean <- remove_missing_cores(df, missing_core_run2_25)
r2.s3.clean <- r2.s3.clean %>% filter(!Class %in% c('A-9','I-7','B-1','R-8','J-10','K-7','K-12')) %>%
  select(Class, `Positive % of total ROI area (d=2, s=1, tN=0.085, tP=0.146)`)
write.csv(r2.s3.clean, file = "r2.s3.clean.csv")    

####   AT8 RUN3.SLIDE1    ####
#     AT8-DAB-6_5_19.mrxs.qptma.data
df <-read_excel("AT8-DAB-6_5_19.mrxs.xlsx")
r3.s1.clean <- remove_missing_cores(df, missing_core_24)
r3.s1.clean <- r3.s1.clean %>% filter(!Class %in% c('K-7','J-12')) %>%
  select(Class, `Positive % of total ROI area (d=2, s=1, tN=0.085, tP=0.146)`)
write.csv(r3.s1.clean, file = "r3.s1.clean.csv")    

####   AT8 RUN3.SLIDE2    ####
#     AT8-DAB-6_5_19_11.06.2019_12.03.23.mrxs.qptma.data
df <-read_excel("AT8-DAB-6_5_19_11.06.2019_12.03.23.mrxs.xlsx")
r3.s2.clean<- remove_missing_cores(df, missing_core_run3_27)
r3.s2.clean <- r3.s2.clean %>% filter(!Class %in% c('P-2','P-3','P-4','P-5','P-6','P-7','P-8','P-9','P-10','P-11','P-12','I-7')) %>%
  select(Class, `Positive % of total ROI area (d=2, s=1, tN=0.085, tP=0.146)`)
write.csv(r3.s2.clean, file = "r3.s2.clean.csv")    


####   AT8 RUN3.SLIDE3    ####
#     AT8-DAB-6_5_19_11.06.2019_12.06.20.mrxs.qptma.data
df <-read_excel("AT8-DAB-6_5_19_11.06.2019_12.06.20.mrxs.xlsx")
r3.s3.clean<- remove_missing_cores(df, missing_core_run2_25) %>%
  select(Class, `Positive % of total ROI area (d=2, s=1, tN=0.085, tP=0.146)`)
write.csv(r3.s3.clean, file = "r3.s3.clean.csv")    
#### Create data frame of all random outputs for each slide ####
at8.cores <- list(r1.s1.clean, r1.s2.clean, r2.s1.clean, r2.s2.clean, r2.s3.clean, r3.s1.clean, r3.s2.clean, r3.s3.clean)
at8.cores <- unlist(lapply(at8.cores, as.character))
setwd("C:/GitHub/FTDC-IrwinLab/TMA_Validation/AT8 TMA TISSUE DETECTED HALO RESULTS")
write.csv(at8.cores, file = "AT8 Tissue Detected Core AO.csv")     

##############          TDP         ################
####   TDP RUN1.SLIDE1    ####
setwd("U:/FTDC-IrwinLab/TMA Take 2/TDP TMA HALO results/TDP43-DAB-5_29_19.mrxs.qptma.data")
df <-read.delim("TMA results - TDP43-DAB-5_29_19.mrxs.txt")
r1.s1.clean <- remove_missing_cores(df, missing_core_24)
r1.s1.rand <- sample(r1.s1.clean$Name,12)

####   TDP RUN1.SLIDE2    ####
setwd("U:/FTDC-IrwinLab/TMA Take 2/TDP TMA HALO results/TDP43-DAB-5_29_19_11.06.2019_11.07.20.mrxs.qptma.data")
df<-read.delim("TMA results - TDP43-DAB-5_29_19_11.06.2019_11.07.20.mrxs.txt")
r1.s2.clean <- remove_missing_cores(df, missing_core_run1_25)
r1.s2.rand <- sample(r1.s2.clean$Name,12)
####   TDP RUN2.SLIDE1    ####
#     TDP43-DAB-6_3_19.mrxs.qptma.data
setwd("U:/FTDC-IrwinLab/TMA Take 2/TDP TMA HALO results/TDP43-DAB-6_3_19.mrxs.qptma.data")
df <-read.delim("TMA results - TDP43-DAB-6_3_19.mrxs.txt")
r2.s1.clean <- remove_missing_cores(df, missing_core_run2_25)
r2.s1.rand <- sample(r2.s1.clean$Name,12)
####   TDP RUN2.SLIDE2    ####
setwd("U:/FTDC-IrwinLab/TMA Take 2/AT8 TMA HALO results/TDP43-DAB-6_3_19_11.06.2019_11.37.12.mrxs.qptma.data")
df <-read.delim("TMA results - TDP43-DAB-6_3_19_11.06.2019_11.37.12.mrxs.txt")
r2.s2.clean <- remove_missing_cores(df, missing_core_run2_25)
r2.s2.rand <- sample(r2.s2.clean$Name,12)
####   TDP RUN2.SLIDE3    ####
setwd("U:/FTDC-IrwinLab/TMA Take 2/TDP TMA HALO results/TDP43-DAB-6_3_19_11.06.2019_11.40.04.mrxs.qptma.data")
df <-read.delim("TMA results - TDP43-DAB-6_3_19_11.06.2019_11.40.04.mrxs.txt")
r2.s3.clean <- remove_missing_cores(df, missing_core_run2_25)
r2.s3.rand <- sample(r2.s3.clean$Name,12)
####   TDP RUN3.SLIDE1    ####
setwd("U:/FTDC-IrwinLab/TMA Take 2/TDP TMA HALO results/TDP43-DAB-6_5_19.mrxs.qptma.data")
df <-read.delim("TMA results - TDP43-DAB-6_5_19.mrxs.txt")
r3.s1.clean <- remove_missing_cores(df, missing_core_24)
r3.s1.rand <- sample(r3.s1.clean$Name,12)
####   TDP RUN3.SLIDE2    ####
setwd("U:/FTDC-IrwinLab/TMA Take 2/TDP TMA HALO results/TDP43-DAB-6_5_19_11.06.2019_12.12.10.mrxs.qptma.data")
df <-read.delim("TMA results - TDP43-DAB-6_5_19_11.06.2019_12.12.10.mrxs.txt")
r3.s2.clean<- remove_missing_cores(df, missing_core_run3_27)
r3.s2.rand <- sample(r3.s2.clean$Name,12)
####   TDP RUN3.SLIDE3    ####
setwd("U:/FTDC-IrwinLab/TMA Take 2/TDP TMA HALO results/TDP43-DAB-6_5_19_11.06.2019_12.14.41.mrxs.qptma.data")
df <-read.delim("TMA results - TDP43-DAB-6_5_19_11.06.2019_12.14.41.mrxs.txt")
r3.s3.clean <- remove_missing_cores(df, missing_core_run2_25)
r3.s3.rand <- sample(r3.s3.clean$Name,12)
r1.s1.rand<-as.character(r1.s1.rand)
#######################

# Import packages
library(readxl)       # Reads Microsoft Excel documents (e.g. .xlsx)
library(dplyr)        # Data cleaning/manipulation package
library(reshape2)     # Data reshaping
library(ggplot2)      # Data Visualization
library(stringr)
setwd("U:/FTDC-IrwinLab/TMA Take 2/AT8 TMA results/Run 1 Slide 2")   # Set working directory to the path of the file destination
book7<-readxl::read_excel("AT8-DAB-5_29_19_11.06.2019_11.13.13_Permutation 1_AVG.xlsx")   # Import data into environ.
book<- book7[,-c(1,12,18,24,30,36)]    # Removes unnecessary columns
book1<- book7[,c(1)]                   # Creates dataframe with containing Core Names
book<- book7[,seq(12, ncol(book7),6)]  # Creates dataframe only containing rows with %AO at varying ODs
book<-cbind(book1,book)                # Binds column with names


long_book<-reshape2::melt(book, idvars = "Name")
long_book$variable<-as.character(long_book$variable)
long_book$OD<- substr(long_book$variable, 52,58)
long_book$OD<-gsub("[)]","0",long_book$OD)
long_book<- long_book[order(long_book$Name,long_book$OD),]
long_book.final<-long_book[,-c(2)]

# Create plot of %AO by OD Colored by Core
X<- ggplot(long_book.final, aes(y=value, x=OD)) + 
  geom_point(aes(color = Name))
X + theme(axis.text.x = element_text(angle = 45)) + 
  labs(y="%AO")
X + facet_grid(Name ~ .)
write.csv(long_book.final, "longbookfinal.csv")

# Recursively calculate differences of %AO values based on ODs
empty.cell<- NA
differences<-data.frame(abs(diff(long_book.final$value)))
differences<- cbind(data.frame(empty.cell),differences)
differences<-rbind(c(NA),differences)
long_book.final$differences <- differences$abs.diff.long_book.final.value..
long_book.final$OD<-as.numeric(long_book.final$OD)
l9<-subset(long_book.final, Name=="L-9")
l9 %>%
  filter(!OD==0.10)%>% 
  filter(!OD==0.260)

ggplot(l9,aes(x=OD, y = value)) + geom_point() + geom_line()
filter(l9, !OD==0.10)
l9<-subset(l9, !OD==0.260)

