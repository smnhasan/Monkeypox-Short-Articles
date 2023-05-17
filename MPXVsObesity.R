library(ggplot2)
library(ggrepel)

options(scipen = 999)


#selection of Top-20 countries
#Data Management

setwd('E:\\Aminul\\Monkeypox Short Articles\\Monkeypox Short Articles')
MPX <- read.csv("MPX.csv")

MPX <- subset(MPX, MPX$date == "2023-04-30") #5/29/2021

#Remove World and International information
MPX<-MPX[!(MPX$iso_code=="OWID_WRL"),]

NROW(MPX)

#Creating CFR
MPX$CFR <- (MPX$total_deaths/MPX$total_cases)*100

options(scipen = 999)

GHSI <- read.csv("GHSI.csv")
GHSI[GHSI == 0] <- NA

WGI <- read.csv("WGI.csv")
WGI[WGI == 0] <- NA

Obesity <- read.csv("Obesity.csv")

#Merge all data with GHSI and WGI

finaldt1 <- merge(GHSI, WGI,  by="location")

finaldt2 <- merge(finaldt1, Obesity,  by="location")

MPX2 <- merge(MPX, finaldt2,  by="location")

cor (MPX2$total_cases , MPX2$Obesity_rate, method = c("pearson"), use = "complete.obs")
cor.test(MPX2$total_cases , MPX2$Obesity_rate, method = c("pearson"), use = "complete.obs")

lm <- glm(Obesity_rate ~ total_cases, data=MPX2)

summary(lm)
confint(lm)

with(summary(lm), 1 - deviance/null.deviance)

a <- ggplot(MPX2, aes(x = total_cases, y = Obesity_rate))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="dark green") +
  geom_text_repel(show.legend = F, 
                  size = 3.5, 
                  label=MPX2$location, 
                  hjust = 0.2,
                  vjust= 0.5) +
  xlab("Total Cases of 103 Countries (Worldwide)") + ylab("Obesity Rate (%)")  +
  geom_smooth(method = "lm", se = FALSE,colour="dark green") 
a

#Max Cases
MPXT20cases <- MPX2[order(-MPX2$total_cases),]
MPXT20cases

MPXT20cases <- MPXT20cases[1:10,]
MPXT20cases

cor (MPXT20cases$total_cases , MPXT20cases$Obesity_rate, method = c("pearson"), use = "complete.obs")
cor.test(MPXT20cases$total_cases , MPXT20cases$Obesity_rate, method = c("pearson"), use = "complete.obs")

lm <- glm(Obesity_rate ~ total_cases, data=MPXT20cases)

summary(lm)
confint(lm)

with(summary(lm), 1 - deviance/null.deviance)

b <- ggplot(MPXT20cases, aes(x = total_cases, y = Obesity_rate))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="dark green") +
  geom_text_repel(show.legend = F, 
                  size = 3.5, 
                  label=MPXT20cases$location, 
                  hjust = 0.2,
                  vjust= 0.5) +
  xlab("Total Cases of Top-10 Infected Countries") + ylab("Obesity Rate (%)")  +
  geom_smooth(method = "lm", se = FALSE,colour="dark green") 
b

MPX2 <- MPX2[c(MPX2$total_deaths > 0),]
NROW(MPX2)
cor (MPX2$total_deaths , MPX2$Obesity_rate, method = c("pearson"), use = "complete.obs")
cor.test(MPX2$total_deaths , MPX2$Obesity_rate, method = c("pearson"), use = "complete.obs")

lm <- glm(Obesity_rate ~ total_deaths, data=MPX2)

summary(lm)
confint(lm)

with(summary(lm), 1 - deviance/null.deviance)

c <- ggplot(MPX2, aes(x = total_deaths, y = Obesity_rate))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="red") +
  geom_text_repel(show.legend = F, 
                  size = 3.5, 
                  label=MPX2$location, 
                  hjust = 0.2,
                  vjust= 0.5) +
  xlab("Total Deaths of 18 Countries (Worldwide)") + ylab("Obesity Rate (%)")  +
  geom_smooth(method = "lm", se = FALSE,colour="red") 
c


#Max deaths
MPXT20deaths <- MPX2[order(-MPX2$total_deaths),]
MPXT20deaths

MPXT20deaths <- MPXT20deaths[1:10,]
MPXT20deaths

cor (MPXT20deaths$total_deaths , MPXT20deaths$Obesity_rate, method = c("pearson"), use = "complete.obs")
cor.test(MPXT20deaths$total_deaths , MPXT20deaths$Obesity_rate, method = c("pearson"), use = "complete.obs")


lm <- glm(Obesity_rate ~ total_deaths, data=MPXT20deaths)

summary(lm)
confint(lm)

with(summary(lm), 1 - deviance/null.deviance)

d <- ggplot(MPXT20deaths, aes(x = total_deaths, y = Obesity_rate))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="red") +
  geom_text_repel(show.legend = F, 
                  size = 3.5, 
                  label=MPXT20deaths$location, 
                  hjust = 0.2,
                  vjust= 0.5) +
  xlab("Total Deaths of Top-10 Infected Countries") + ylab("Obesity Rate (%)")  +
  geom_smooth(method = "lm", se = FALSE,colour="red") 
d

tiff("Obesity.tiff", units="in", width=12, height=12, res=300)
gridExtra::grid.arrange(a,b,c,d, ncol = 2, nrow=2)
dev.off()

