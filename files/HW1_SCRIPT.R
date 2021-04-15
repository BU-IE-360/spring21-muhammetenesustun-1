library(openxlsx)
library(ggplot2)
library(ggcorrplot)
library(car)



exportdata=read.xlsx("RUS_DATA.xlsx",sheet='EVDS')

ggplot(exportdata, aes(Month, Tourists))+geom_line(col="blue", size=2) + geom_smooth(method = "lm")


ggplot(exportdata, aes(Month, US_TL))+geom_line(col="black", size=2) + geom_smooth(method = "lm")
ggplot(exportdata, aes(Month, PriceIndex))+geom_line(col="red", size=2) + geom_smooth(method = "lm")
ggplot(exportdata, aes(Month, RUB_TL))+geom_line(col="green", size=2) + geom_smooth(method = "lm")



ggplot(exportdata, aes(Month, AvgDailySearch))+geom_line(col="purple", size=2) + geom_smooth(method = "lm")



ggplot(exportdata,aes(Month,Tourists/10000))+geom_line(color="blue", size=2)+geom_line(aes(Month,AvgDailySearch),color="purple",size=2)+ylab("Number of Tourists/10000 and Avg. Daily Search")


scatterplot(Tourists ~ AvgDailySearch, data = exportdata)


correl_info=cor(exportdata)
ggcorrplot(correl_info, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


