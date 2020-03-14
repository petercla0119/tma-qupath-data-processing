library(dplyr)
library(readxl)
library(reshape2)     # Data reshaping
library(ggplot2)      # Data Visualization

# Script takes QuPath TMA Random Core permutation analysis and compares the OD intervals. All RGB values were calculated from the Avg across 12 cores. Only OD parameter differs 
#######     Comparing OD intervals (0.005 vs. 0.002) in Run 2 Slide 2       ########
   #####  r2=Run 2; s2=Slide 2; small=0.002 OD; large=0.005 OD; P1=permutation 1;   #####  
#         LARGE PERMUTATION DATA IMPORT
setwd("C:/GitHub/FTDC-IrwinLab/TMA_Validation/at8 detection interval/")    # Set working directory
r2.s2.p1.large <-read_excel("R2.S2.P1.expand.xlsx")  # Large Permutation 1
r2.s2.p2.large <-read_excel("R2.S2.P2.expand.xlsx")  # Large Permutation 2
r2.s2.p3.large <-read_excel("R2.S2.P3.expand.xlsx")  # Large Permutation 3
r2.s2.p4.large <-read_excel("R2.S2.P4.expand.xlsx")  # Large Permutation 4
r2.s2.p5.large <-read_excel("R2.S2.P5.expand.xlsx")  # Large Permutation 5

#         SMALL PERMUTATION DATA IMPORT
#r2.s2.p1.small <-read_excel("AT8-DAB-6_3_19_11.06.2019_11.27.57.mrxs_permutation_1_AVG.xlsx")  # Small Permutation 1
#r2.s2.p2.small <-read_excel("AT8-DAB-6_3_19_11.06.2019_11.27.57.mrxs_permutation_2_AVG.xlsx")  # Small Permutation 2
#r2.s2.p3.small <-read_excel("AT8-DAB-6_3_19_11.06.2019_11.27.57.mrxs_permutation_3_AVG.xlsx")  # Small Permutation 3
#r2.s2.p4.small <-read_excel("AT8-DAB-6_3_19_11.06.2019_11.27.57.mrxs_permutation_4_AVG.xlsx")  # Small Permutation 4
#r2.s2.p5.small <-read_excel("AT8-DAB-6_3_19_11.06.2019_11.27.57.mrxs_permutation_5_AVG.xlsx")  # Small Permutation 5


core_permutation_cleaning_large<-function(df){
  # Due to the QuPath Output, the 0.005 OD is not sequential with 0.002, therefore we needed to separate the even and odd ODs... since the smaller interval of 0.002 did not have 0.005 ODs a new function is created to be used on the large interval datasets
  
  cores <- df[,c(1)]                   # Creates dataframe with containing Core Names
  even <- df[,seq(from=12, to=457,30)]  # separate columns with ODs that are even numbers
  odd<-df[,seq(from=444, to=ncol(df), 6)]   # separate columns with ODs that are odd numbers
  df<-cbind(cores,even,odd) # Binds even and odd ODs with names
  df<-reshape2::melt(df, idvars = "Name")
  df$variable<-as.character(df$variable)
  df$OD<- substr(df$variable, 52,58)
  df$OD<-gsub("[)]","0",df$OD)
  df<- df[order(df$Name,df$OD),]
  df<-df[,-c(2)]
  df<- df[,c("Name","OD","value")]
  names(df) <- c("Core", "OD", "AO")
  
  
  return(df)
}


#       Create function to clean TMA core permutation data to yield coreID, OD, and %AO
#core_permutation_cleaning_small<-function(df){
  cores <- df[,c(1)]                   # Creates dataframe with containing Core Names
  df<- df[,seq(12, ncol(df),7)]  # removes unnessary columns - only keeps %AO of ROI
  df<-cbind(cores,df) # Binds column with names
  df<-reshape2::melt(df, idvars = "Name")
  df$variable<-as.character(df$variable)
  df$OD<- substr(df$variable, 52,58)
  df$OD<-gsub("[)]","0",df$OD)
  df<- df[order(df$Name,df$OD),]
  df<-df[,-c(2)]
  df<- df[,c("Name","OD","value")]
  
  
  return(df)

}


r2.s2.p1.large_long<-core_permutation_cleaning_large(r2.s2.p1.large)
r2.s2.p2.large_long<-core_permutation_cleaning_large(r2.s2.p2.large)
r2.s2.p3.large_long<-core_permutation_cleaning_large(r2.s2.p3.large)
r2.s2.p4.large_long<-core_permutation_cleaning_large(r2.s2.p4.large)
r2.s2.p5.large_long<-core_permutation_cleaning_large(r2.s2.p5.large)
#r2.s2.p1.small_long<-core_permutation_cleaning_small(r2.s2.p1.small)
#r2.s2.p2.small_long<-core_permutation_cleaning_small(r2.s2.p2.small)
#r2.s2.p3.small_long<-core_permutation_cleaning_small(r2.s2.p3.small)
#r2.s2.p4.small_long<-core_permutation_cleaning_small(r2.s2.p4.small)
#r2.s2.p5.small_long<-core_permutation_cleaning_small(r2.s2.p5.small)
r2.s2.p1.large_long <- r2.s2.p1.large_long %>% filter(AO<100)

# Compute the relative change in games played
mutate(avg.r2.s2.p1.large_wide, G_delta = (AO.B.10 + lag(AO.B.10))/2 *100)
######################
# Create plot of %AO by OD Colored by Core
r2.s2.lg_long<- ggplot(r2.s2.p1.large_long, aes(y= AO, x = OD, color = Core, group = Core)) + 
  geom_point(aes(color = Core)) +
  geom_line()
r2.s2.lg_long + theme(axis.text.x = element_text(angle = 45)) + 
  labs(y="%AO") + facet_wrap("Core")
bten <- r2.s2.p1.large_long %>% filter(Core=="B-10")

#graph_AO_OD<- function(df){
#  run.slide.p.sz.dir<- ggplot(df, aes(y=value, x=OD, color=Name, group=Name)) + 
#  geom_point(aes(color = Name)) +
#  geom_line()
#run.slide.p.sz.dir + theme(axis.text.x = element_text(angle = 45)) + 
#  labs(y="%AO")
#}

#X + facet_grid(Name ~ .)
        #write.csv(long_book.final, "longbookfinal.csv")
        # Recursively calculate differences of %AO values based on ODs
        #empty.cell<- NA
        #differences<-data.frame(abs(diff(long_book.final$value)))
        #differences<- cbind(data.frame(empty.cell),differences)
        #differences<-rbind(c(NA),differences)
        #long_book.final$differences <- differences$abs.diff.long_book.final.value..
        #long_book.final$OD<-as.numeric(long_book.final$OD)

recursive_AO_differences<-function(df){
  empty.cell<- NA
  differences<-data.frame(abs(diff(df$AO)))
  df$differences <- rbind(c(NA),differences)
  x<-df$differences
  df<-cbind(df,x)
  df<-df[,-c(4)]
  colnames(df)<-c("Core","OD","AO","Diff")
  df<-reshape(df,idvar="OD", timevar = "Core", direction = "wide")
  df<-data.frame(df[-c(1),])
  return(df)
}

mutate(r2.s2.p1.large_long, G_delta = abs(AO - lag(AO)))
# Calculate the average by AO by
r2.s2.p1.large_long %>% 
  group_by(Core) %>%
  summarise_at(vars(AO), funs(mean(., na.rm = TRUE)))


r2.s2.p1.large_wide<-recursive_AO_differences(r2.s2.p1.large_long)
r2.s2.p2.large_wide<-recursive_AO_differences(r2.s2.p2.large_long)
r2.s2.p3.large_wide<-recursive_AO_differences(r2.s2.p3.large_long)
r2.s2.p4.large_wide<-recursive_AO_differences(r2.s2.p4.large_long)
r2.s2.p5.large_wide<-recursive_AO_differences(r2.s2.p5.large_long)
#r2.s2.p1.small_wide<-recursive_AO_differences(r2.s2.p1.small_long)
#r2.s2.p2.small_wide<-recursive_AO_differences(r2.s2.p2.small_long)
#r2.s2.p3.small_wide<-recursive_AO_differences(r2.s2.p3.small_long)
#r2.s2.p4.small_wide<-recursive_AO_differences(r2.s2.p4.small_long)
#r2.s2.p5.small_wide<-recursive_AO_differences(r2.s2.p5.small_long)

averaging_AO_and_differences<-function(wide_df){
AO.Avg<-wide_df %>%
    select(seq(from=2, to=ncol(.), by=2)) %>%
    transmute(AO.Avg =(rowSums(.))/12)
Diff.Avg<-wide_df %>%
  select(seq(from=3, to=ncol(.), by=2)) %>%
  transmute(Diff.Avg =(rowSums(.))/12)
avgs<-cbind(AO.Avg,Diff.Avg)
wide_df_avg<-cbind(wide_df,avgs)


return(wide_df_avg)

}



avg.r2.s2.p1.large_wide<-averaging_AO_and_differences(r2.s2.p1.large_wide)
avg.r2.s2.p1.large_wide.graph <- ggplot(avg.r2.s2.p1.large_wide, aes(x = as.numeric(OD), y = AO.Avg)) + 
  geom_point() + 
  geom_line()
avg.r2.s2.p1.large_wide.graph + 
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(x = "OD", y="%AO") + 
  ggtitle("Average %AO by OD")
#write.csv(w,"r2.s2.p1.large_wide.csv")
#z<-averaging_AO_and_differences(r2.s2.p2.large_wide)
#write.csv(w,"r2.s2.p2.large_wide.csv")
#w<-averaging_AO_and_differences(r2.s2.p3.large_wide)
#write.csv(w,"r2.s2.p3.large_wide.csv")
#w<-averaging_AO_and_differences(r2.s2.p4.large_wide)
#write.csv(w,"r2.s2.p4.large_wide.csv")
#w<-averaging_AO_and_differences(r2.s2.p5.large_wide)
#write.csv(w,"r2.s2.p5.large_wide.csv")
#w<-averaging_AO_and_differences(r2.s2.p1.small_wide)
#write.csv(w,"r2.s2.p1.small_wide.csv")
#w<-averaging_AO_and_differences(r2.s2.p2.small_wide)
#write.csv(w,"r2.s2.p2.small_wide.csv")
#w<-averaging_AO_and_differences(r2.s2.p3.small_wide)
#write.csv(w,"r2.s2.p3.small_wide.csv")
#w<-averaging_AO_and_differences(r2.s2.p4.small_wide)
#write.csv(w,"r2.s2.p4.small_wide.csv")
#w<-averaging_AO_and_differences(r2.s2.p5.small_wide)
#write.csv(w,"r2.s2.p5.small_wide.csv")




r2.s2.p1.large_long$interval<-c("0.005")
r2.s2.p1.large_long$perm<-c("1")
r2.s2.p1.large_long$perm_int<-c("1_0.005")
r2.s2.p2.large_long$interval<-c("0.005")
r2.s2.p2.large_long$perm<-c("2")
r2.s2.p2.large_long$perm_int<-c("2_0.005")
r2.s2.p3.large_long$interval<-c("0.005")
r2.s2.p3.large_long$perm<-c("3")
r2.s2.p3.large_long$perm_int<-c("3_0.005")
r2.s2.p4.large_long$interval<-c("0.005")
r2.s2.p4.large_long$perm<-c("4")
r2.s2.p4.large_long$perm_int<-c("4_0.005")
r2.s2.p5.large_long$interval<-c("0.005")
r2.s2.p5.large_long$perm<-c("5")
r2.s2.p5.large_long$perm_int<-c("5_0.005")
#r2.s2.p1.small_long$interval<-c("0.002")
#r2.s2.p1.small_long$perm<-c("1")
#r2.s2.p1.small_long$perm_int<-c("1_0.002")
#r2.s2.p2.small_long$interval<-c("0.002")
#r2.s2.p2.small_long$perm<-c("2")
#r2.s2.p2.small_long$perm_int<-c("2_0.002")
#r2.s2.p3.small_long$interval<-c("0.002")
#r2.s2.p3.small_long$perm<-c("3")
#r2.s2.p3.small_long$perm_int<-c("3_0.002")
#r2.s2.p4.small_long$interval<-c("0.002")
#r2.s2.p4.small_long$perm<-c("4")
#r2.s2.p4.small_long$perm_int<-c("4_0.002")
#r2.s2.p5.small_long$interval<-c("0.002")
#r2.s2.p5.small_long$perm<-c("5")
#r2.s2.p5.small_long$perm_int<-c("5_0.002")


all.r2.s2<-rbind(r2.s2.p1.large_long,r2.s2.p2.large_long,r2.s2.p3.large_long,r2.s2.p4.large_long,r2.s2.p5.large_long,r2.s2.p1.small_long,r2.s2.p2.small_long,r2.s2.p3.small_long,r2.s2.p4.small_long,r2.s2.p5.small_long)

#sanity check... should match number of rows in r2.s2.p1.large_long
all.r2.s2 %>% 
  filter(perm==1) %>%
  filter(interval==0.005) %>%
  count()

all.r2.s2<-rename(all.r2.s2, Core=Name)
t.all.r2.s2<-all.r2.s2
t.all.r2.s2$Core<-as.factor(t.all.r2.s2$Core)
t.all.r2.s2$perm_int<-as.factor(t.all.r2.s2$perm_int)
t.all.r2.s2$OD<-as.factor(t.all.r2.s2$OD)

            large_interval_graph<- all.r2.s2 %>%
              filter(interval==0.005) %>%
              group_by(perm)  %>%
              ggplot(aes(y=value,x=OD, color=Core, group=Core)) +
              geom_point() +
              geom_line() + 
              theme(axis.text.x = element_text(angle = 90)) + 
              labs(y="%AO") +
              facet_grid(~perm) +
              labs(title="%AO vs. OD", subtitle = "OD interval = 0.005") +
              theme(plot.title =  element_text(hjust = 0.5),
                    plot.subtitle =  element_text(hjust = 0.5))
t.all.r2.s2<-all.r2.s2
empty.cell<- NA
  differences<-data.frame(abs(diff(t.all.r2.s2$value)))
  t.all.r2.s2$differences <- rbind(c(NA),differences)
  x<-t.all.r2.s2$differences
  t.all.r2.s2<-cbind(t.all.r2.s2,x)
  t.all.r2.s2<-t.all.r2.s2[,-c(7)]
  colnames(t.all.r2.s2)<-c("Core","OD","value","interval","perm","perm_int","Diff")
  AO<-t.all.r2.s2[,c(1:6)]
  t.all.r2.s2<-subset(t.all.r2.s2,!t.all.r2.s2$OD=="0.10")

    
t.all.r2.s2$core2<-t.all.r2.s2$Core
t.all.r2.s2$id<-seq.int(nrow(t.all.r2.s2))
  df<-reshape(t.all.r2.s2,idvar=c("OD","interval","perm","perm_int"), timevar = "Core", direction = "wide")
AO.Avg<-df %>%
  select(seq(from=5, to=ncol(df), by=2)) %>%
  transmute(AO.Avg =(rowSums(.,na.rm = T))/12)

Diff.Avg<-df %>%
  select(seq(from=6, to=ncol(df), by=4)) %>%
  transmute(Diff.Avg =(rowSums(.,na.rm = T))/12)
  

 df<- dcast(t.all.r2.s2,OD+interval+perm_int+perm ~ Core, value.var = c("value"))
  

avgs<-cbind(AO.Avg,Diff.Avg)
wide_df_avg<-cbind(df,AO.Avg)

x<-dcast(setDT(t.all.r2.s2), OD+interval+perm_int+perm ~ Core, value.var=c("AO","Diff"))
df$Core<- substr(df$variable, 52,58)


ggplot(w,aes(x=OD, y = Diff.B.9)) + geom_point() + geom_line()

z<-reshape(wide_df_avg, 
             varying = ao,
           direction = "long",
           idvar="perm_int")

ao<-colnames(df[,c(seq(from=4, to=ncol(df), by=3))],prefix = "value.")
dif<-colnames(df[,c(seq(from=5, to=ncol(df), by=3))],prefix = "Diff.")
core2<-colnames(df[,c(seq(from=6, to=ncol(df), by=4))],prefix = "core2")
id<-colnames(df[,c(seq(from=6, to=ncol(df), by=4))],prefix = "core2")


interval<-wide_df_avg$interval

ao<-colnames(small_wide[,c(seq(from=3, to=26), by=2)] ,prefix = "AO.")
dif<-colnames(small_wide[,c(seq(from=4, to=26), by=2)] ,prefix = "Diff.")




small_wide<-read.csv("r2.s2.p5.small_wide.csv")
reshape(small_wide,
        varying = c(ao, dif),
        direction = "long",
        idvar = "X")


a<-dcast(all.r2.s2,OD+Core~perm_int,value.var="value")
AO.Avg<-a %>%
  select(seq(from=4, to=ncol(a), by=21)) %>%
  transmute(AO.Avg =(rowSums(.,na.rm = T))/12)




