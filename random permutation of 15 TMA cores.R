#import data
df <- read.csv("U:/TMA/Custom Algorithm - Tau/serial slides 12 and 13 clean.csv")
#random sample of slide 12 cores

p2.slide12 <- sample(as.character(df$slide12), size = 15)
p3.slide12 <- sample(as.character(df$slide12), size = 15)
p4.slide12 <- sample(as.character(df$slide12), size = 15)
p5.slide12 <- sample(as.character(df$slide12), size = 15)

p2.slide13 <- sample(as.character(df$slide13), size = 15)
p3.slide13 <- sample(as.character(df$slide13), size = 15)
p4.slide13 <- sample(as.character(df$slide13), size = 15)
p5.slide13 <- sample(as.character(df$slide13), size = 15)

rand.samp <- cbind(p2.slide12, p3.slide12, p4.slide12, p5.slide12, p2.slide13, p3.slide13, p4.slide13, p5.slide13)


p1.slide12 <- sample(as.character(df$slide12), size = 10)
p2.slide12 <- sample(as.character(df$slide12), size = 20)
p3.slide12 <- sample(as.character(df$slide12), size = 30)
p4.slide12 <- sample(as.character(df$slide12), size = 40)
p5.slide12 <- sample(as.character(df$slide12), size = 50)
p6.slide12 <- sample(as.character(df$slide12), size = 60)
p7.slide12 <- sample(as.character(df$slide12), size = 70)
p8.slide12 <- sample(as.character(df$slide12), size = 80)
p9.slide12 <- sample(as.character(df$slide12), size = 90)
p10.slide12 <- sample(as.character(df$slide12), size = 100)

rand = rbind(p1.slide12, p2.slide12, p3.slide12,p4.slide12,p5.slide12,p6.slide12,p7.slide12,p8.slide12,p9.slide12,p10.slide12)
write.csv(p1.slide12, "10 TMA cores.csv", row.names = FALSE)
write.csv(p2.slide12, "20 TMA cores.csv", row.names = FALSE)
write.csv(p3.slide12, "30 TMA cores.csv", row.names = FALSE)
write.csv(p4.slide12, "40 TMA cores.csv", row.names = FALSE)
write.csv(p5.slide12, "50 TMA cores.csv", row.names = FALSE)
write.csv(p6.slide12, "60 TMA cores.csv", row.names = FALSE)
write.csv(p7.slide12, "70 TMA cores.csv", row.names = FALSE)
write.csv(p8.slide12, "80 TMA cores.csv", row.names = FALSE)
write.csv(p9.slide12, "90 TMA cores.csv", row.names = FALSE)
write.csv(p10.slide12, "100 TMA cores.csv", row.names = FALSE)

