library(dplyr)

#   Create a list for each of the possible missing core groupings
# Use for AT8 Slide 1 in Run 1 AND Run 3
missing_core_24 <- c("A-1", "A-12", "B-12", "C-12", "D-12", "E-12", "F-3", "F-12", "G-12", "H-11", "I-10", "J-9", "K-2", "K-8", "L-2", "L-7", "M-2", "M-6", "N-5", "O-4", "P-3", "P-8", "Q-2", "R-1")
# Use for Slide 2 in Run 1
missing_core_run1_25 <- c("A-1", "A-12", "B-12", "C-12", "D-12", "E-12", "F-3", "F-12", "G-12", "H-11", "I-10", "J-9", "K-2", "K-8", "L-2", "L-7", "M-2", "M-6", "N-5", "O-4", "P-3", "P-5", "P-8", "Q-2", "R-1")
# Use for all slides in Run 2 AND Slide 3 in Run 3
missing_core_run2_25 <- c("A-1", "A-12", "B-12", "C-12", "D-12", "E-3","E-12", "F-3", "F-12", "G-12", "H-11", "I-10", "J-9", "K-2", "K-8", "L-2", "L-7", "M-2", "M-6", "N-5", "O-4", "P-3", "P-8", "Q-2", "R-1")
# Use for all Slide 2 in Run 3
missing_core_run3_27 <- c("A-1", "A-12", "B-12", "C-12", "D-12", "E-3","E-12", "F-3", "F-12", "G-12", "H-11", "I-10", "J-9", "K-2", "K-8", "L-2", "L-7", "M-2", "M-6", "N-5", "O-4", "P-3", "P-8", "P-10", "P-11", "Q-2", "R-1")

# Create a function to remove missing cores specific to the slide/dataset
remove_missing_cores <- function(df, cores_missing){
  df <- df %>% 
    select(Name, Positive...of.total.ROI.area..d.2..s.1..tN.0.085..tP.0.146.) %>%
    filter(!Name %in% cores_missing)

  return(df)
}

####   AT8 RUN1.SLIDE1    ####
setwd("C:/GitHub/FTDC-IrwinLab/TMA FINAL/TMA HALO TAU SMOOTH 1")
df <-read.delim("AT8-DAB-5_29_19.mrxs.txt")
r1.s1.clean <- remove_missing_cores(df, missing_core_24)
r1.s1.clean$Name<-as.character(r1.s1.clean$Name)
r1.s1.clean<-r1.s1.clean[order(r1.s1.clean$Name, decreasing = T),]
write.csv(r1.s1.clean, file = "r1.s1.clean.csv")

####   AT8 RUN1.SLIDE2    ####
df<-read.delim("AT8-DAB-5_29_19_11.06.2019_11.13.13.mrxs.txt")
r1.s2.clean <- remove_missing_cores(df, missing_core_run1_25)
write.csv(r1.s2.clean, file = "r1.s2.clean.csv")
####   AT8 RUN2.SLIDE1    ####
      #     AT8-DAB-6_3_19.mrxs.qptma.data
df <-read.delim("AT8-DAB-6_3_19.mrxs.txt")
r2.s1.clean <- remove_missing_cores(df, missing_core_run2_25)
write.csv(r2.s1.clean, file = "r2.s1.clean.csv")
####   AT8 RUN2.SLIDE2    ####
      #     AT8-DAB-6_3_19_11.06.2019_11.27.57.mrxs.qptma.data
df <-read.delim("AT8-DAB-6_3_19_11.06.2019_11.27.57.mrxs.txt")
r2.s2.clean <- remove_missing_cores(df, missing_core_run2_25)
write.csv(r2.s2.clean, file = "r2.s2.clean.csv")
####   AT8 RUN2.SLIDE3    ####
      #     AT8-DAB-6_3_19_11.06.2019_11.30.51.mrxs.qptma.data
df <-read.delim("AT8-DAB-6_3_19_11.06.2019_11.30.51.mrxs.txt")
r2.s3.clean <- remove_missing_cores(df, missing_core_run2_25)
write.csv(r2.s3.clean, file = "r2.s3.clean.csv")
####   AT8 RUN3.SLIDE1    ####
      #     AT8-DAB-6_5_19.mrxs.qptma.data
df <-read.delim("AT8-DAB-6_3_19_11.06.2019_11.27.57.mrxs.txt")
r3.s1.clean <- remove_missing_cores(df, missing_core_24)
write.csv(r3.s1.clean, file = "r3.s1.clean.csv")
####   AT8 RUN3.SLIDE2    ####
      #     AT8-DAB-6_5_19_11.06.2019_12.03.23.mrxs.qptma.data
df <-read.delim("AT8-DAB-6_5_19_11.06.2019_12.03.23.mrxs.txt")
r3.s2.clean<- remove_missing_cores(df, missing_core_run3_27)
write.csv(r3.s2.clean, file = "r3.s2.clean.csv")
####   AT8 RUN3.SLIDE3    ####
      #     AT8-DAB-6_5_19_11.06.2019_12.06.20.mrxs.qptma.data
df <-read.delim("AT8-DAB-6_5_19_11.06.2019_12.06.20.mrxs.txt")
r3.s3.clean <- remove_missing_cores(df, missing_core_run2_25)
write.csv(r3.s3.clean, file = "r3.s3.clean.csv")     

#### Create data frame of all random outputs for each slide ####
at8.cores <- list(r1.s1.clean, r1.s2.clean, r2.s1.clean, r2.s2.clean, r2.s3.clean, r3.s1.clean, r3.s2.clean, r3.s3.clean)
at8.cores <- unlist(lapply(at8.rand.cores, as.character))
setwd("U:/FTDC-IrwinLab/TMA Take 2/AT8 TMA HALO results")
write.csv(at8.cores, file = "Permutation # AT8 Random Cores.csv")     

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











####      QuPath Autodetect Results      ####
setwd("U:/FTDC-IrwinLab/TMA Take 2/AT8 TMA results/Run 1 Slide 1")   # Set working directory to the path of the file destination
r1.s1<-readxl::read_excel("AT8-DAB-5_29_19.mrxs_[G-8, M-11, C-7, B-6, M-9, N-4, H-1, A-11, I-9, K-7, M-12, H-3]annotations_permutation_1_AVG.xlsx")   # Import data into environ.

r1.s1.a<- r1.s1[,c(1)]                   # Creates dataframe with containing Core Names
book<- r1.s1[,seq(12, ncol(r1.s1),6)]  # Creates dataframe only containing rows with %AO at varying ODs
book<-cbind(r1.s1.a,book)                # Binds column with names

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
