library(dplyr)
library(foreign)
library(readxl)
library(stringi)
library(tidyr)
library(plyr)
library(ggpubr)
library(ggplot2)
library(magrittr)
AfricaTemp <- read.csv("AfricaTemperature.csv")
AfricaData <- data.frame(AfricaTemp%>%group_by(AfricaTemp$Country,AfricaTemp$Year,AfricaTemp$ISO3)%>%summarise_each(funs(mean),TemperatureCelsius))
AfricaData<-AfricaData %>% dplyr::rename(Country = AfricaTemp.Country)
AfricaData<-AfricaData %>% dplyr::rename(Year = AfricaTemp.Year)
AfricaData<-AfricaData %>% dplyr::rename(ISO3 = AfricaTemp.ISO3)
AfricaRain<- read.csv("AfricaRainfall.csv")
AfricaData2<- data.frame(AfricaRain%>%group_by(AfricaRain$Country,AfricaRain$Year,AfricaRain$ISO3)%>%summarise_each(funs(mean),RainfallMM))
AfricaData2<-AfricaData2%>% dplyr::rename(Country = AfricaRain.Country)
AfricaData2<-AfricaData2%>% dplyr::rename(Year = AfricaRain.Year)
AfricaData2<-AfricaData2%>% dplyr::rename(ISO3 = AfricaRain.ISO3)
AfricaData3<-data.frame(cbind(AfricaData, AfricaData2))
AfricaData3$Country.1<-NULL
AfricaData3$Year.1<-NULL
AfricaData3$ISO3.1<-NULL
AfricaData3$ISO3 <- trimws(AfricaData3$ISO3 , which = c("left"))
AfricaData3$Country <- trimws(AfricaData3$Country , which = c("left"))
AfricaSCAD <- read_excel("AfricaSCAD.xls")
index <- stri_length(AfricaSCAD$STARTDATE) == 9
AfricaSCAD$STARTDATE[index] <- substring(AfricaSCAD$STARTDATE[index], 8, 9)
AfricaSCAD$STARTDATE[!index] <- substring(AfricaSCAD$STARTDATE[!index], 7, 8)
AfricaEvent <- AfricaSCAD %>% select(8,9) %>% add_count(COUNTRYNAM, STARTDATE, name = "Events") %>% distinct()
AfricaEvent <- AfricaEvent %>% dplyr::rename(Year = STARTDATE)
AfricaEvent <- AfricaEvent %>% dplyr::rename(Country = COUNTRYNAM)
index <- AfricaEvent$Year > 20
AfricaEvent$Year[index] <- paste("19", AfricaEvent$Year[index], sep = "")
AfricaEvent$Year[!index] <- paste("20", AfricaEvent$Year[!index], sep = "")
AfricaData3$Country <- as.character(AfricaData3$Country)
AfricaData3$Year <- as.numeric(AfricaData3$Year)
AfricaEvent$Year <- as.numeric(AfricaEvent$Year)
AfricaData <- full_join(AfricaEvent, AfricaData3, by = c("Country", "Year"))
AfricaData <- AfricaData %>% drop_na(ISO3)
AfricaData <- AfricaData %>% drop_na(Events)
AfricaData <- arrange(AfricaData, Country, Year, group_by = Country)
AfricaData <- dlply(AfricaData, "Country", identity)
### LINE-BAR GRAPH ###
mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", hjust = 0.5, size = (15)), 
                      legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"))
for (i in 1:length(AfricaData)) {
  FileName <- paste0("graphs/linebar/linebar-", AfricaData[[i]][1,]$Country, ".png")
  png(FileName)
  g1 <- ggplot(AfricaData[[i]], aes(x = Year)) + 
    geom_bar(aes(x = AfricaData[[i]]$Year, y = AfricaData[[i]]$Events,  color = "Social Instability"), stat = "identity" ) + 
    geom_line(aes(x = AfricaData[[i]]$Year, y = AfricaData[[i]]$TemperatureCelsius, color = 'Average Temperature (Celsius)'), stat="identity", size = 1) + 
    geom_line(aes(x = AfricaData[[i]]$Year, y = AfricaData[[i]]$RainfallMM, color = "RainFall (mm)"), stat="identity", size = 1) + 
    scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "Temperature and RainFall")) + xlab("Year") + ylab("Instability Factor") + ggtitle(AfricaData[[i]][1,]$Country)
  print(g1 + mynamestheme)
  dev.off()
}
### SCATTER PLOT ###
for (i in 1:length(AfricaData)) {
  FileName <- paste0("graphs/scatter/scatter-", AfricaData[[i]][1,]$Country, ".png")
  png(FileName)
  g1 <- ggplot(AfricaData[[i]], aes(x = Year) ) + geom_point(aes(y = TemperatureCelsius, color = "Average Temperature (Celsius)")) + 
    geom_point(aes(y = RainfallMM, color = "Average Rainfall (mm)") ) + 
    geom_point(aes(y = Events, color = "Social Instability")) +
    xlab("YEAR") + ylab("") + ggtitle(AfricaData[[i]][1,]$Country)
  print(g1 + mynamestheme)
  dev.off()
}
### SUBPLOTS ###
for (i in 1:length(AfricaData)) {
  FileName <- paste0("graphs/subplots/scatter-", AfricaData[[i]][1,]$Country, ".png")
  png(FileName)
  p <- ggplot(AfricaData[[i]], aes(x = Year))
  g1 <- p + geom_point(aes(y = TemperatureCelsius), color = "blue3" ) + xlab("") + ylab("TEMPERATURE")
  g2 <- p + geom_point(aes(y = RainfallMM), color = "red")  + xlab("") + ylab("RAINFALL")
  g3 <- p + geom_point(aes(y = Events ), color = "green4") + xlab( AfricaData[[i]][1,]$Country) + ylab("INSTABILITY")
  figure <- ggarrange(g1, g2, g3, ncol = 1, nrow = 3)
  print(figure)
  dev.off()
}