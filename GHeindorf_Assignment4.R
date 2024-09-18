hospital_data <- data.frame(t(data.frame(c("0.6","103","bad","low","low"),
  c("0.3","87","bad","low","high"),
  c("0.4","32","bad","high","low"),
  c("0.4","42","bad","high","high"),
  c("0.2","59","good","low","low"),
  c("0.6","109","good","low","high"),
  c("0.3","78","good","high","low"),
  c("0.4","205","good","high","high"),
  c("0.9","135","NA","high","high"),
  c("0.2","176","bad","high","high"))))
row.names(hospital_data) <- 1:nrow(hospital_data)
colnames(hospital_data) <- c("Frequency", 
                             "BP", "First", 
                             "Second",
                             "FinalDecision")
hospital_data$Frequency <- as.numeric(hospital_data$Frequency)
hospital_data$BP <- as.numeric(hospital_data$BP)
hospital_data$First[which(hospital_data$First == "NA")] <- NA
boxplot(Frequency ~ First, data = hospital_data)
boxplot(Frequency ~ Second, data = hospital_data)
boxplot(Frequency ~ FinalDecision, data = hospital_data)
boxplot(BP ~ First, data = hospital_data)
boxplot(BP ~ Second, data = hospital_data)
boxplot(BP ~ FinalDecision, data = hospital_data)

hist(hospital_data$Frequency)
hist(hospital_data$BP)
