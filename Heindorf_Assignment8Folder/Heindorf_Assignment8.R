library(plyr)

d <- read.table("/Users/gheindorf/Documents/Assignment 6 Dataset.txt", 
                header = T, sep = ",")

AVGgradeBYsex <- ddply(d, "Sex", transform, Avg.Grade = mean(Grade))
write.csv(AVGgradeBYsex, "AvgGradeBySex")

dNameWithi <- d[which(grepl("i", d$Name, ignore.case = T)),]
write.csv(dNameWithi, "Subset_NameWithI")
