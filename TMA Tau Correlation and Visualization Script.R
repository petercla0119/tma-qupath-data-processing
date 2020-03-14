# If this is your first time using R, please install packages with the commented lines below. Note the line will need to be uncommented (delete leading #) in order to run
# install.packages(c("ggplot2", "BlandAltmanLeh", "readxl", "stargazer", "matrixStats"), dependencies = TRUE)
# Load packages into the current R session.
library(ggplot2)  #data visualization
library(BlandAltmanLeh)
library(readxl)   #read excel files
library(stargazer)  #print out clean tables
library(matrixStats)
library(corrplot)
library(gridExtra)
####       HALO TAU TMA RESULTS        ####
# Set the working directory in the appropriate folder using the correct computer path. 
setwd("U:/FTDC-IrwinLab/TMA Take 2/AT8 TMA HALO results/")
list.files()    # Show files within directory. Check that the desired data file is printed
# This is where I like to leave notes to refer back to why this data was selected. Please see example below:
# TMA_AT8_ALL_RESULTS excel file contains data for all TMA cores on all 8 slides, stained in 3 different runs. This data was used to validate both intra-assay and inter-assay batch variation to account for these differences in the future.
tau_df_halo <- read_excel("book7.xlsx")   # Import your dataset
names(tau_df_halo)    # Print the column names, this will be used to create a new variable to represent the columns of %AO values and exclude the core names
names(tau_df_halo) <- c("R1_S1_CORES", "R1_S1", "R1_S2_CORES", "R1_S2", "R2_S1_CORES", "R2_S1", "R2_S2_CORES", "R2_S2", "R2_S3_CORES", "R2_S3",  "R3_S1_CORES", "R3_S1",  "R3_S2_CORES", "R3_S2", "R3_S3_CORES", "R3_S3")   # Renames columns in dataframe... R does not like to work with special characters (e.g. %)
AO<-tau_df_halo[,c(2,4,6,8,10,12,14,16)]      # Create new variable, "AO", which only contains data from %AO for all slides
stargazer(as.data.frame(AO), title = "Summary of TMA Tau %AO",type = "text")  # Print summary of AO (contains Mean, SD, Min/Max, and Quartiles)
tau_sd <- c(sd(tau_df_halo$R1_S1, na.rm = TRUE), sd(tau_df_halo$R1_S2, na.rm = TRUE), sd(tau_df_halo$R2_S1, na.rm = TRUE), sd(tau_df_halo$R2_S2, na.rm = TRUE), sd(tau_df_halo$R2_S3, na.rm = TRUE), sd(tau_df_halo$R3_S1, na.rm = TRUE), sd(tau_df_halo$R3_S2, na.rm = TRUE), sd(tau_df_halo$R3_S3, na.rm = TRUE))   # Create new vector of SDs and name it tau_sd

# Bland-Altman analysis comparing intra- and inter-assay data (mean diff, one-sample t-test with mu = 0) in control dataset (28 total combinations). These columns contain the %AO difference between Run/slide for each core
tau_df_halo$R1.S1_R1.S2 <- tau_df_halo$R1_S1 - tau_df_halo$R1_S2
tau_df_halo$R1.S1_R2.S1 <- tau_df_halo$R1_S1 - tau_df_halo$R2_S1
tau_df_halo$R1.S1_R2.S2 <- tau_df_halo$R1_S1 - tau_df_halo$R2_S2   # Problem row... values identical to column 5
tau_df_halo$R1.S1_R2.S3 <- tau_df_halo$R1_S1 - tau_df_halo$R2_S3
tau_df_halo$R1.S1_R3.S1 <- tau_df_halo$R1_S1 - tau_df_halo$R3_S1   # Problem row... values identical to column 3
tau_df_halo$R1.S1_R3.S2 <- tau_df_halo$R1_S1 - tau_df_halo$R3_S2
tau_df_halo$R1.S1_R3.S3 <- tau_df_halo$R1_S1 - tau_df_halo$R3_S3
tau_df_halo$R1.S2_R2.S1 <- tau_df_halo$R1_S2 - tau_df_halo$R2_S1
tau_df_halo$R1.S2_R2.S2 <- tau_df_halo$R1_S2 - tau_df_halo$R2_S2
tau_df_halo$R1.S2_R2.S3 <- tau_df_halo$R1_S2 - tau_df_halo$R2_S3
tau_df_halo$R1.S2_R3.S1 <- tau_df_halo$R1_S2 - tau_df_halo$R3_S1
tau_df_halo$R1.S2_R3.S2 <- tau_df_halo$R1_S2 - tau_df_halo$R3_S2
tau_df_halo$R1.S2_R3.S3 <- tau_df_halo$R1_S2 - tau_df_halo$R3_S3
tau_df_halo$R2.S1_R2.S2 <- tau_df_halo$R2_S1 - tau_df_halo$R2_S2
tau_df_halo$R2.S1_R2.S3 <- tau_df_halo$R2_S1 - tau_df_halo$R2_S3
tau_df_halo$R2.S1_R3.S1 <- tau_df_halo$R2_S1 - tau_df_halo$R3_S1
tau_df_halo$R2.S1_R3.S2 <- tau_df_halo$R2_S1 - tau_df_halo$R3_S2
tau_df_halo$R2.S1_R3.S3 <- tau_df_halo$R2_S1 - tau_df_halo$R3_S3
tau_df_halo$R2.S2_R2.S3 <- tau_df_halo$R2_S2 - tau_df_halo$R2_S3
tau_df_halo$R2.S2_R3.S1 <- tau_df_halo$R2_S2 - tau_df_halo$R3_S1  # Problem line... resulting in 0 values in column
tau_df_halo$R2.S2_R3.S2 <- tau_df_halo$R2_S2 - tau_df_halo$R3_S2
tau_df_halo$R2.S2_R3.S3 <- tau_df_halo$R2_S2 - tau_df_halo$R3_S3
tau_df_halo$R2.S3_R3.S1 <- tau_df_halo$R2_S3 - tau_df_halo$R3_S1
tau_df_halo$R2.S3_R3.S2 <- tau_df_halo$R2_S3 - tau_df_halo$R3_S2
tau_df_halo$R2.S3_R3.S3 <- tau_df_halo$R2_S3 - tau_df_halo$R3_S3
tau_df_halo$R3.S1_R3.S2 <- tau_df_halo$R3_S1 - tau_df_halo$R3_S2
tau_df_halo$R3.S1_R3.S3 <- tau_df_halo$R3_S1 - tau_df_halo$R3_S3
tau_df_halo$R3.S2_R3.S3 <- tau_df_halo$R3_S2 - tau_df_halo$R3_S3

# Create vectors of intra and inter-assay groupings
# Intra-assay groups
intra1 <- tau_df_halo[,c("R1_S1", "R1_S2")] #, "R1.S1_R1.S2"
intra2 <- tau_df_halo[,c("R2_S1", "R2_S2", "R2_S3")] #, "R2.S1_R2.S2", "R2.S1_R2.S3", "R2.S2_R2.S3"
intra3 <- tau_df_halo[,c("R3_S1", "R3_S2", "R3_S3")] # , "R3.S1_R3.S2", "R3.S1_R3.S3", "R3.S2_R3.S3"
# Inter-assay groups
inter1 <- tau_df_halo[,c("R1_S1", "R2_S1", "R3_S1")] #, "R1.S1_R2.S1", "R1.S1_R3.S1", "R2.S1_R3.S1"  
inter2 <- tau_df_halo[,c("R1_S2", "R2_S2", "R3_S2")] #, "R1.S2_R2.S2", "R1.S2_R3.S2", "R2.S2_R3.S2"
inter3 <- tau_df_halo[,c("R2_S3", "R3_S3")] #,"R2.S3_R3.S3"

# Create vector to store columns of AO differences by all possible combinations
all.diff<-tau_df_halo[,c("R1.S1_R1.S2", "R1.S1_R2.S1", "R1.S1_R2.S2", "R1.S1_R2.S3", "R1.S1_R3.S1", "R1.S1_R3.S2", "R1.S1_R3.S3", "R1.S2_R2.S1", "R1.S2_R2.S2", "R1.S2_R2.S3", "R1.S2_R3.S1", "R1.S2_R3.S2", "R1.S2_R3.S3", "R2.S1_R2.S2", "R2.S1_R2.S3", "R2.S1_R3.S1", "R2.S1_R3.S2", "R2.S1_R3.S3", "R2.S2_R2.S3", "R2.S2_R3.S1", "R2.S2_R3.S2", "R2.S2_R3.S3", "R2.S3_R3.S1", "R2.S3_R3.S2", "R2.S3_R3.S3", "R3.S1_R3.S2", "R3.S1_R3.S3", "R3.S1_R3.S3")]
# Create vector which contains the average of each row for different OD intervals
differences <- rowMeans(all.diff, na.rm = T)    # Differences contains the average difference for each core across all slides and runs

# BlandAltman One-Sample T-Test with Mean=0 #########
t.test(tau_df_halo$R1.S1_R1.S2, mu = 0)
t.test(tau_df_halo$R1.S1_R2.S1, mu = 0)
t.test(tau_df_halo$R1.S1_R2.S2, mu = 0)
t.test(tau_df_halo$R1.S1_R2.S3, mu = 0)
t.test(tau_df_halo$R1.S1_R3.S1, mu = 0)
t.test(tau_df_halo$R1.S1_R3.S2, mu = 0)
t.test(tau_df_halo$R1.S1_R3.S3, mu = 0)
t.test(tau_df_halo$R1.S2_R2.S1, mu = 0)   #*** p>0.05 - p=0.107
t.test(tau_df_halo$R1.S2_R2.S2, mu = 0)   #*** p>0.05 - p=0.1032
t.test(tau_df_halo$R1.S2_R2.S3, mu = 0)   #*** p>0.05 - p=0.3647
t.test(tau_df_halo$R1.S2_R3.S1, mu = 0)   #*** p>0.05 - p=0.1032
t.test(tau_df_halo$R1.S2_R3.S2, mu = 0)   #*** p>0.05 - p=0.1198
t.test(tau_df_halo$R1.S2_R3.S3, mu = 0)   #*** p>0.05 - p=0.3516
t.test(tau_df_halo$R2.S1_R2.S2, mu = 0)   #*** p>0.05 - p=0.987
t.test(tau_df_halo$R2.S1_R2.S3, mu = 0)
t.test(tau_df_halo$R2.S1_R3.S1, mu = 0)   #*** p>0.05 - p=0.987
t.test(tau_df_halo$R2.S1_R3.S2, mu = 0)   #*** p>0.05 - p=0.7565
t.test(tau_df_halo$R2.S1_R3.S3, mu = 0)   #*** p>0.05 - p=0.07309
t.test(tau_df_halo$R2.S2_R2.S3, mu = 0)
t.test(tau_df_halo$R2.S2_R3.S1, mu = 0)   #*** p=NA?
t.test(tau_df_halo$R2.S2_R3.S2, mu = 0)   #*** p>0.05 - p=0.7256
t.test(tau_df_halo$R2.S2_R3.S3, mu = 0)   #*** p>0.05 - p=0.09507
t.test(tau_df_halo$R2.S3_R3.S1, mu = 0)
t.test(tau_df_halo$R2.S3_R3.S2, mu = 0)   #*** p>0.05 - p=0.111
t.test(tau_df_halo$R2.S3_R3.S3, mu = 0)   #*** p>0.05 - p=0.9662
t.test(tau_df_halo$R3.S1_R3.S2, mu = 0)   #*** p>0.05 - p=0.7256
t.test(tau_df_halo$R3.S1_R3.S3, mu = 0)   #*** p>0.05 - p=0.09507
t.test(tau_df_halo$R3.S2_R3.S3, mu = 0)   #*** p>0.05 - p=0.06847


# Create function for calculating correlations and outputting lower triangle correlation matrices 
lower.tri.cor.matrix <- function(df){
  cor.matrix<-round(cor(df, use = "complete.obs"), 3)
  cor.matrix[upper.tri(cor.matrix)]<-""
  cor.matrix<-as.data.frame(cor.matrix)
  cor.matrix
  
  return(cor.matrix)
}
####    Intra-assay Correlation Matrix     #### 
lower.tri.cor.matrix(intra1)    # R2=0.864
lower.tri.cor.matrix(intra2)    # R2>0.97
lower.tri.cor.matrix(intra3)    # R2>0.96
####    Inter-assay Correlation Matrix     #### 
lower.tri.cor.matrix(inter1)    # R2>0.95
lower.tri.cor.matrix(inter2)    # R2>0.93
lower.tri.cor.matrix(inter3)    # R2=0.965

####     Intra-assay Linear Models   ####
# Run 1
run1.1v2 <- lm(tau_df_halo$R1_S1 ~ tau_df_halo$R1_S2)
summary(run1.1v2)
stargazer(run1.1v2, type="text", title = "Run 2 Intra-assay Linear Regression")  # In table, Constant=?? and value below in ()=Std. Error
# Run 2
run2.1v2 <- lm(tau_df_halo$R2_S1 ~ tau_df_halo$R2_S2)
run2.2v3 <- lm(tau_df_halo$R2_S2 ~ tau_df_halo$R2_S3)
run2.1v3 <- lm(tau_df_halo$R2_S1 ~ tau_df_halo$R2_S3)
summary(run2.1v2)
summary(run2.2v3)
summary(run2.1v3)
stargazer(run2.1v2, run2.1v3, run2.2v3, type="text", title = "Run 2 Intra-assay Linear Regression")
# Run 3
run3.1v2 <- lm(tau_df_halo$R3_S1 ~ tau_df_halo$R3_S2)
run3.2v3 <- lm(tau_df_halo$R3_S2 ~ tau_df_halo$R3_S3)
run3.1v3 <- lm(tau_df_halo$R3_S1 ~ tau_df_halo$R3_S3)
summary(run3.1v2)
summary(run3.2v3)
summary(run3.1v3)
stargazer(run3.1v2, run3.1v3, run3.2v3, type="text", title = "Run 3 Intra-assay Linear Regression")

# Combined intra-assay model output
stargazer(run1.1v2, run2.1v2, run2.1v3, run2.2v3, run3.1v2, run3.1v3, run3.2v3, type="text", title = "Intra-assay Linear Regression")

####     Inter-assay Linear Models   ####
# Slide 1
slide1.r1vr2 <- lm(tau_df_halo$R1_S1 ~ tau_df_halo$R2_S1)
summary(slide1.r1vr2)
slide1.r2vr3 <- lm(tau_df_halo$R2_S1 ~ tau_df_halo$R3_S1)
summary(slide1.r2vr3)
slide1.r1vr3 <- lm(tau_df_halo$R1_S1 ~ tau_df_halo$R3_S1)
summary(slide1.r1vr3)
stargazer(slide1.r1vr2,slide1.r2vr3,slide1.r1vr3, type="text", title = "Slide 1 Inter-assay Linear Regression Models")

# Slide 2
slide2.r1vr2 <- lm(tau_df_halo$R1_S2 ~ tau_df_halo$R2_S2)
summary(slide2.r1vr2)
slide2.r2vr3 <- lm(tau_df_halo$R2_S2 ~ tau_df_halo$R3_S2)
summary(slide2.r2vr3)
slide2.r1vr3 <- lm(tau_df_halo$R1_S2 ~ tau_df_halo$R3_S2)
summary(slide2.r1vr3)
stargazer(slide2.r1vr2,slide2.r2vr3,slide2.r1vr3, type="text", title = "Slide 2 Inter-assay Linear Regression Models")

# Slide 3
slide3.r2vr3 <- lm(tau_df_halo$R2_S3 ~ tau_df_halo$R3_S3)
summary(slide3.r2vr3)
stargazer(slide3.r2vr3,  type="text", title = "Slide 3 Inter-assay Linear Regression Models")

# Combined inter-assay model output
stargazer(slide1.r1vr2,slide2.r1vr2,slide1.r1vr3,slide2.r1vr3,slide1.r2vr3,slide2.r2vr3,slide3.r2vr3, type="text", title = "Inter-assay Linear Regression Models")

####    Intra-assay plots     ####
# Run 1 Plot
ggplot(data = tau_df_halo, aes(x = R1_S1, y = R1_S2)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 1 Slide 1 %AO") + ylab("Run 1 Slide 2 %AO") + ggtitle("Run 1 Comparison") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 20, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.864")
ggsave("Run1_comparison.png", width = 6, height = 5)

# Run 2 Plot (1v2)
r2.1v2 <- ggplot(data = tau_df_halo, aes(x = R2_S1, y = R2_S2)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 2 Slide 1 %AO") + ylab("Run 2 Slide 2 %AO") +  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.988")
# Run 2 Plot (2v3)
r2.2v3 <- ggplot(data = tau_df_halo, aes(x = R2_S2, y = R2_S3)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 2 Slide 2 %AO") + ylab("Run 2 Slide 3 %AO") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.98")
# Run 2 Plot (1v3)
r2.1v3 <- ggplot(data = tau_df_halo, aes(x = R2_S1, y = R2_S3)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 2 Slide 1 %AO") + ylab("Run 2 Slide 3 %AO") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.978")

grid.arrange(r2.1v2, r2.2v3, r2.1v3, nrow = 1, top = "Run 2 Comparison") # Arrange 3 plots for Run 2 together

# Run 3 Plot (1v2)
r3.1v2 <- ggplot(data = tau_df_halo, aes(x = R3_S1, y = R3_S2)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 3 Slide 1 %AO") + ylab("Run 3 Slide 2 %AO") +  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.961")
# Run 3 Plot (2v3)
r3.2v3 <- ggplot(data = tau_df_halo, aes(x = R3_S2, y = R3_S3)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 3 Slide 2 %AO") + ylab("Run 3 Slide 3 %AO") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.96")
# Run 3 Plot (1v3)
r3.1v3 <- ggplot(data = tau_df_halo, aes(x = R3_S1, y = R3_S3)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 3 Slide 1 %AO") + ylab("Run 3 Slide 3 %AO") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.966")

grid.arrange(r3.1v2, r3.2v3, r3.1v3, nrow = 1, top = "Run 3 Comparison") # Arrange 3 plots for Run 3 together

####    Inter-assay plots     ####
# Slide 1
inter1.s1.1v2<- ggplot(data = inter1, aes(x = R1_S1, y = R2_S1)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 1 Slide 1 %AO") + ylab("Run 2 Slide 1 %AO") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.953")
inter1.s1.2v3<- ggplot(data = inter1, aes(x = R2_S1, y = R3_S1)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 2 Slide 1 %AO") + ylab("Run 3 Slide 1 %AO") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.988")
inter1.s1.1v3<- ggplot(data = inter1, aes(x = R1_S1, y = R3_S1)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 1 Slide 1 %AO") + ylab("Run 3 Slide 1 %AO") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.956")

grid.arrange(inter1.s1.1v2, inter1.s1.2v3, inter1.s1.1v3, nrow = 1, top = "Inter-Assay Slide 1 Comparison") # Arrange 3 plots for Slide 1 for all staining runs together

# Slide 2
inter2.s2.1v2<- ggplot(data = inter2, aes(x = R1_S2, y = R2_S2)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 1 Slide 2 %AO") + ylab("Run 2 Slide 2 %AO") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.827")
inter2.s2.2v3<- ggplot(data = inter2, aes(x = R2_S2, y = R3_S2)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 2 Slide 2 %AO") + ylab("Run 3 Slide 2 %AO") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.967")
inter2.s2.1v3<- ggplot(data = inter2, aes(x = R1_S2, y = R3_S2)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 1 Slide 2 %AO") + ylab("Run 3 Slide 2 %AO") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.832")

grid.arrange(inter2.s2.1v2, inter2.s2.2v3, inter2.s2.1v3, nrow = 1, top = "Inter-Assay Slide 2 Comparison") # Arrange 3 plots for Slide 1 for all staining runs together

# Slide 3
inter3.s3.2v3<- ggplot(data = inter3, aes(x = R2_S3, y = R3_S3)) + geom_point(size = 2, colour = "blue") + geom_smooth(method = "lm", colour = "red") + xlab("Run 2 Slide 3 %AO") + ylab("Run 3 Slide 3 %AO") +  ggtitle("Slide 3 Comparison") + theme(axis.title = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14), legend.title = element_text(size = 14)) + annotate("text", x=10, y=80, label = "R^2=0.965")
ggsave("Slide 3 inter-assay comparison.png", width = 6, height = 5)

#####     CV VALUES    #####
inter.assay.cv <- function(df, ...){
  grp_vars <- quos(...)
    selected.cols <- df %>%
    select(!!!grp_vars)
  dat.as.matrix <- as.matrix(selected.cols)
  selected.cols$mean <- rowMeans(selected.cols,na.rm = T)
  selected.cols$sd <- rowSds(dat.as.matrix,na.rm = T)
  selected.cols <- selected.cols %>%
    mutate(CV = sd/mean,
           pct.CV = (sd/mean)*100)
  
  return(selected.cols)
}
# Intra-assay CV
# Slide 1 - 4 Combinations 
R1.cv <- inter.assay.cv(intra1, R1_S1, R1_S2)
names(R1.cv) <- c("R1_S1", "R1_S2", "R1_mean", "R1_sd", "R1_CV", "R1_pct.CV")

# Slide 2 - 4 Combinations 
R2.cv <- inter.assay.cv(intra2, R2_S1, R2_S2, R2_S3)
names(R2.cv) <- c("R2_S1", "R2_S2", "R2_S3", "R2_mean", "R2_sd", "R2_CV", "R2_pct.CV")
R2.1v2.cv <- inter.assay.cv(intra2, R2_S1, R2_S2)
names(R2.1v2.cv) <- c("R2_S1", "R2_S2", "R2.1v2_mean", "R2.1v2_sd", "R2.1v2_CV", "R2.1v2_pct.CV")
R2.2v3.cv <- inter.assay.cv(intra2, R2_S2, R2_S3)
names(R2.2v3.cv) <- c("R2_S2", "R2_S3", "R2.2v3_mean", "R2.2v3_sd", "R2.2v3_CV", "R2.2v3_pct.CV")
R2.1v3.cv <- inter.assay.cv(intra2, R2_S1, R2_S3)
names(R2.1v3.cv) <- c("R2_S1", "R2_S3", "R2.1v3_mean", "R2.1v3_sd", "R2.1v3_CV", "R2.1v3_pct.CV")
R2.cv <- cbind(R2.cv, R2.1v2.cv, R2.2v3.cv, R2.1v3.cv)

# Slide 3 - 1 Combination
R3.cv <- inter.assay.cv(intra3, R3_S1, R3_S2, R3_S3)
names(R3.cv) <- c("R3_S1", "R3_S2", "R3_S3", "R3_mean", "R3_sd", "R3_CV", "R3_pct.CV")
R3.1v2.cv <- inter.assay.cv(intra3, R3_S1, R3_S2)
names(R3.1v2.cv) <- c("R3_S1", "R3_S2", "R3.1v2_mean", "R3.1v2_sd", "R3.1v2_CV", "R3.1v2_pct.CV")
R3.2v3.cv <- inter.assay.cv(intra3, R3_S2, R3_S3)
names(R3.2v3.cv) <- c("R3_S2", "R3_S3", "R3.2v3_mean", "R3.2v3_sd", "R3.2v3_CV", "R3.2v3_pct.CV")
R3.1v3.cv <- inter.assay.cv(intra3, R3_S1, R3_S3)
names(R3.1v3.cv) <- c("R3_S1", "R3_S3", "R3.1v3_mean", "R3.1v3_sd", "R3.1v3_CV", "R3.1v3_pct.CV")
R3.cv <- cbind(R3.cv, R3.1v2.cv, R3.2v3.cv, R3.1v3.cv)

# Inter-assay CV
# Slide 1 - 4 Combinations 
R1_R2_R3_S1.cv <- inter.assay.cv(inter1, R1_S1, R2_S1, R3_S1)
names(R1_R2_R3_S1.cv) <- c("R1_S1", "R2_S1", "R3_S1", "R1_R2_R3_S1_mean", "R1_R2_R3_S1_sd", "R1_R2_R3_S1_CV", "R1_R2_R3_S1_pct.CV")
R1_R2_S1.cv <- inter.assay.cv(inter1, R1_S1, R2_S1)
names(R1_R2_S1.cv) <- c("R1_S1", "R2_S1", "R1_R2_S1_mean", "R1_R2_S1_sd", "R1_R2_S1_CV", "R1_R2_S1_pct.CV")
R2_R3_S1.cv <- inter.assay.cv(inter1, R2_S1, R3_S1)
names(R2_R3_S1.cv) <- c("R2_S1", "R3_S1", "R2_R3_S1_mean", "R2_R3_S1_sd", "R2_R3_S1_CV", "R2_R3_S1_pct.CV")
R1_R3_S1.cv <- inter.assay.cv(inter1, R1_S1, R3_S1)
names(R1_R3_S1.cv) <- c("R1_S1", "R3_S1", "R1_R3_S1_mean", "R1_R3_S1_sd", "R1_R3_S1_CV", "R1_R3_S1_pct.CV")

# Slide 2 - 4 Combinations 
R1_R2_R3_S2.cv <- inter.assay.cv(inter2, R1_S2, R2_S2, R3_S2)
names(R1_R2_R3_S2.cv) <- c("R1_S2", "R2_S2", "R3_S2", "R1_R2_R3_S2_mean", "R1_R2_R3_S2_sd", "R1_R2_R3_S2_CV", "R1_R2_R3_S2_pct.CV")
R1_R2_S2.cv <- inter.assay.cv(inter2, R1_S2, R2_S2)
names(R1_R2_S2.cv) <- c("R1_S2", "R2_S2", "R1_R2_S2_mean", "R1_R2_S2_sd", "R1_R2_S2_CV", "R1_R2_S2_pct.CV")
R2_R3_S2.cv <- inter.assay.cv(inter2, R2_S2, R3_S2)
names(R2_R3_S2.cv) <- c("R2_S2", "R3_S2", "R2_R3_S2_mean", "R2_R3_S2_sd", "R2_R3_S2_CV", "R2_R3_S2_pct.CV")
R1_R3_S2.cv <- inter.assay.cv(inter2, R1_S2, R3_S2)
names(R1_R3_S2.cv) <- c("R1_S2", "R3_S2", "R1_R3_S2_mean", "R1_R3_S2_sd", "R1_R3_S2_CV", "R1_R3_S2_pct.CV")

# Slide 3 - 1 Combination
R2_R3_S3.cv<- inter.assay.cv(inter3, R2_S3, R3_S3)
names(R2_R3_S3.cv) <- c("R2_S3", "R3_S3", "R2_R3_S3_mean", "R2_R3_S3_sd", "R2_R3_S3_CV", "R2_R3_S3_pct.CV")

# 
####    Intra-assay BlandAltman Plots ####
# Run 1
plot(tau_df_halo$R1_S1, tau_df_halo$diffs, col=sex, 
     sub=paste("critical difference is", round(ba.stats$critical.diff,4)),
     main="make your own graph easily", ylim=c(-1.5,1.5), pch=18-sex)


#########   Transforming values ####
# transform slide 2 values in run 2 and run 3 to those of run 1
slide2 <- tau_df_halo %>% 
  select(R1_S2, R2_S2, R1.S2_R2.S2) %>%
  mutate(transformed = (R2_S2*0.8858 + 6.78717))
cor(tau_df_halo$R1_S2, tau_df_halo$R2_S2, use='complete.obs')
cor(tau_df_halo$R1_S2, slide2$transformed, use = 'complete.obs')
tau_df_halo$R2_S2*0.8858 + 6.78717
plot(tau_df_halo$R1_S2, tau_df_halo$R2_S2)

####     IDENTIFYING PROBLEM CORES    ####
r1.r2.s1.problem <- tau_df_halo %>% 
  select(R1_S1_CORES,R1.S1_R2.S1) %>% 
  filter(R1.S1_R2.S1>10)
r1.r2.s1<-tau_df_halo%>%
  select(R1_S1_CORES, R1_S1, R2_S1)
