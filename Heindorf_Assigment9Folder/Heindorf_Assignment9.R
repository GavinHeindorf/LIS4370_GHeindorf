library(tidyverse)
library(reshape2)
library(lattice)

d <- read.table("/Users/gheindorf/Documents/US States Production.txt",
                header = T, sep = ",")

#data preparation
d <- d[-1]

dByYear <- d %>%
  group_by(year)%>%
  summarise(Unemp.AVG = mean(unemp), GSP.AVG = mean(gsp))
dByYear$Unemp.AVG <- scale(dByYear$Unemp.AVG)
dByYear$GSP.AVG <- scale(dByYear$GSP.AVG)

dFL <- d[which(d$state == "FLORIDA"),]
dFL_long <- melt(dFL, id.vars = "year", measure.vars = c("hwy", "water", "util"))
dFL_long$variable <- factor(dFL_long$variable, levels = c("hwy", "water", "util"),
                            labels = c("Highways", "Water", "Other Utilities"))




#Visualization using base plot() 
plot(dByYear$year, dByYear$Unemp.AVG, type = "l", col = "Red",
     main = "Trends in Unemployment Rate and GSP from 1970 to 1986",
     xlab = "Year", 
     ylab = "GSP and Unemployment % (z-scored)")
lines(dByYear$year, dByYear$GSP.AVG, col = "Blue")
legend(x = 1970, y = 1.87, legend = c("Unemployment Rate", 
                                     "GSP"), 
       fill = c("Red", "Blue"))

#Visualization using ggplot2
ggplot(dFL_long, aes(x = year, y = value, color = variable))+
  geom_line(linewidth = 1.3)+
  theme_bw()+
  labs(title = "Trends in Investment from 1970 to 1986", 
       color = "Sector",
       x = "Year", y = "Investment ($)")+
  coord_cartesian(expand = T)+
  theme(text = element_text(family = "Times New Roman"),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 10))

#visualization using lattice package
fun_color_range <- colorRampPalette(c("lightblue", "darkblue"))    
my_colors <- fun_color_range(length(unique(d$unemp)))
levelplot(unemp ~ factor(year)*factor(state), data = d,
          main = "Unemployment Rate by State from 1970 to 1986",
          xlab = "Year", ylab = "State", 
          col.regions = my_colors,scales = 
            list(x = list(rot = 45), y = list(cex = 0.45)))
        
