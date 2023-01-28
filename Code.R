library(tidyverse)
library(corrplot)
library(sf)
library(reshape)
library(extrafont)
library(ggplot2)
library(plot3D)
library(corrgram)
library(car)
library(ggpubr)
library(dplyr)

#Read the dataset file and transformation some of the attributes' type

Happiness = read.csv("test.csv")
str(Happiness)
Happiness$ï..Country <- as.factor(Happiness$ï..Country)
Happiness$Continent <- as.factor(Happiness$Continent)
Happiness$Crime.index <- as.numeric(Happiness$Crime.index)

#Draw the box plot between happiness score and continents

ggplot(Happiness , aes(x = Continent, y = Happiness.score)) +
  geom_boxplot(aes(fill=Continent)) + theme_bw() +
  theme(axis.title = element_text(size = (8)))

#Draw average value of happiness factors for different continents

Happiness.Continent <- Happiness[-c(3, 12, 13, 14, 15, 16, 17, 18)] %>%
  group_by(Continent) %>%
  summarise_at(vars(-ï..Country), funs(mean(., na.rm=TRUE)))
Happiness.Continent <- data.frame(Happiness.Continent)
Happiness.Continent.melt <- melt(Happiness.Continent)
  ggplot(Happiness.Continent.melt, aes(y=value, x=Continent, 
  color=Continent, fill=Continent)) + 
  geom_bar(stat="identity") +    
  facet_wrap(~variable) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of happiness variables for different
  continents", y = "Average value")

#Simple regression analysis between happiness score and seven main factors

attach(mtcars)
par(mfrow=c(3,3))
a <- ggplot(Happiness, aes(x = Dystopia..1.83....residual, y =
Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8) 
b <- ggplot(Happiness, aes(x = GDP.per.capita, y = Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8) 
c <- ggplot(Happiness, aes(x = Healthy.life.expectancy, y =
Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8) 
d <- ggplot(Happiness, aes(x = Social.support, y = Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8)
e <- ggplot(Happiness, aes(x = Freedom.to.make.life.choices, y =
Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8)
f <- ggplot(Happiness, aes(x = Generosity, y = Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8)
g <- ggplot(Happiness, aes(x = Perceptions.of.corruption, y =
Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8)
ggarrange(a, b, c, d, e, f, g,
          labels = c("1", "2", "3", "4", "5", "6", "7"),
          ncol = 3, nrow = 3)

#Show the interesting results of the above analysis

attach(mtcars)
par(mfrow=c(2,2))
a <- ggplot(Happiness, aes(x = Social.support, y = Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8) 
b <- ggplot(Happiness, aes(x = GDP.per.capita, y = Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8) 
c <- ggplot(Happiness, aes(x = Healthy.life.expectancy, y =
Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8) 
d <- ggplot(Happiness, aes(x = Generosity, y = Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8) 
ggarrange(a, b, c, d,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

#Show the remaing simple regression analysis

attach(mtcars)
par(mfrow=c(2,2))
a <- ggplot(Happiness, aes(x = Dystopia..1.83....residual, y =
Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8) 
b <- ggplot(Happiness, aes(x = Freedom.to.make.life.choices, y =
Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8) 
c <- ggplot(Happiness, aes(x = Perceptions.of.corruption, y =
Happiness.score)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+   stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.y = 8) 
ggarrange(a, b, c,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

#Draw the relationship between happines rank and some factors

ggplot(subset(Happiness, Happiness$Continent != "Oceania"), aes(x =
Social.support, y = Happiness.score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Relationship between Happiness Ranking and
  Social Support")

ggplot(subset(Happiness, Happiness$Continent != "Oceania"), aes(x =
GDP.per.capita, y = Happiness.score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Relationship between Happiness Ranking and GDP
  per capita")

ggplot(subset(Happiness, Happiness$Continent != "Oceania"), aes(x =
Healthy.life.expectancy, y = Happiness.score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Relationship between Happiness Ranking and
  Healthy Life Expectancy")

ggplot(subset(Happiness, Happiness$Continent != "Oceania"), aes(x =
Generosity, y = Happiness.score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Relationship between Happiness Ranking and
  Generosity")

#Draw the correlation matrix by color and number

Num.cols <- sapply(Happiness[3:11], is.numeric)
Cor.data <- cor(Happiness[3:11][, Num.cols])
corrplot(Cor.data, method = 'color')

Num.cols <- sapply(Happiness[4:11], is.numeric)
Cor.data <- cor(Happiness[4:11][, Num.cols])
corrplot(Cor.data, method = 'number')

#Multiple regression analysis between happiness score and main factors

linear_regression = lm(Happiness.score~Dystopia..1.83....residual+Perceptions.of.corruption+
                         Social.support+Healthy.life.expectancy+Generosity+
                         Freedom.to.make.life.choices, data = Happiness)
summary(linear_regression)
vif_value <- vif(linear_regression)
barplot(vif_value, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)
linear_regression2 = lm(Happiness.score~Dystopia..1.83....residual+Perceptions.of.corruption+
                          Social.support+Healthy.life.expectancy+
                          Freedom.to.make.life.choices, data = Happiness)
summary(linear_regression2)
par(mfrow=c(2,2))
plot(linear_regression2)

#Correlogram of happiness score of top 20 highest and lowest countries
corrgram(Happiness[ , 4:11] %>% filter(Happiness$RANK %in% 1:20),
order=TRUE, upper.panel=panel.cor, main="Happiness Matrix for Top 20 Higest
Countries")
corrgram(Happiness[ , 4:11] %>% filter(Happiness$RANK %in% 127:146),
order=TRUE, upper.panel=panel.cor, main="Happiness Matrix for Top 20 Lowest
Countries")

#Multiple regression between happiness score and COVID-19 cases & deaths
#Draw 3D plot of the relationship

scatter3D(Happiness$COVID.19.cases, Happiness$COVID.19.deaths,
Happiness$Happiness.score, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",
          main = "Happiness Score Relationship", xlab = "COVID-19 cases",
          ylab ="COVID-19 deaths", zlab = "Happiness Score")
linear_regression <- lm(Happiness.score~COVID.19.cases+COVID.19.deaths,
data = Happiness)
summary(linear_regression)

#Simple regression between happiness score and unemployment/inflation rate

linear_regression <- lm(Happiness.score~Unemployment.rate, 
data = Happiness)
summary(linear_regression)
linear_regression <- lm(Happiness.score~Infaltion.rate, data = Happiness)
summary(linear_regression)

#Draw the 3D of the relationship

scatter3D(Happiness$Unemployment.rate, Happiness$Inflation.rate,
Happiness$Happiness.score, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",
          main = "Happiness Score Relationship", xlab = "Unemployment
          Rate", ylab ="Inflation Rate", zlab = "Happiness Score")

#Multiple regression analysis

linear_regression2 <- lm(Happiness.score~Unemployment.rate+Inflation.rate,
data = Happiness)
summary(linear_regression2)
par(mfrow=c(2,2))
plot(linear_regression2)

#Simple regression between happiness score and suicide rate/density
linear_regression <- lm(Happiness.score~Suicide.rate, data = Happiness)
summary(linear_regression)

linear_regression <- lm(Happiness.score~Density, data = Happiness)
summary(linear_regression)
