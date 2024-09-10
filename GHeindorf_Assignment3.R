Name <- c("Jeb", "Donald", "Ted", "Marco", "Carly", "Hillary", "Berine")
ABC_poll <- c(4, 62, 51, 21, 2, 14, 15)
CBS_poll <- c(12, 75, 43, 19, 1, 21, 19)

Comb <- data.frame(Candidates=Name, ABC=ABC_poll, CBS=CBS_poll)

###ABC Poll results: descending order
Comb$Candidates[order(Comb$ABC, decreasing=T)]

###CBS Poll results: descending order
Comb$Candidates[order(Comb$CBS, decreasing =T)]

###Combined Poll results: descending order
Comb$Comb_poll <- (Comb$ABC+Comb$CBS)/2
Comb$Candidates[order(Comb$Comb_poll, decreasing = T)]

###Max votes by poll
Comb$Candidates[which(Comb$ABC == max(Comb$ABC))]
Comb$Candidates[which(Comb$CBS == max(Comb$CBS))]

###Box plot graphs for results
library(ggplot2)
 ###Results for just ABC poll:
ABC_bar <- ggplot(Comb, aes(x=Candidates, y=ABC))+
  stat_summary(fun.data = mean_sdl, geom = "bar")
ABC_bar

###Results for just CBS poll:
CBS_bar <- ggplot(Comb, aes(x=Candidates, y=CBS))+
  stat_summary(fun.data=mean_sdl, geom="bar")
CBS_bar

###Restuls for combined polls:
Comb_bar <- ggplot(Comb, aes(x=Candidates, y=Comb_poll))+
  stat_summary(fun.data=mean_sdl, geom="bar")
Comb_bar

#check range of votes by poll:
range(Comb$ABC)
range(Comb$CBS)

#check means by poll:
mean(Comb$ABC)
mean(Comb$CBS)
