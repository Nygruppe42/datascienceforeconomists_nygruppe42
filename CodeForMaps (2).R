library(devtools)
library(vdemdata)
library(WDI)
devtools::install_github("vdeminstitute/vdemdata")
setwd("/Users/mosta/OneDrive/Desktop/Data Science")
library (tidyverse)
library(janitor)
library(ggplot2)
library(caret)
library(readr)
library(parallel)
library(maps)
##some are unused

###cleaning vdem dataset----

vdem <- vdem %>%
  filter(year == 2020) %>%
  select(country_name, country_text_id, year, v2x_polyarchy)

####constructing selected indicators of WDI dataset----

WDIsearch ("GDP.*capita,")
WDIsearch("child mortality")
WDIsearch ("life expectancy")
WDIsearch( "school")
WDIsearch ("trade.*gdp")


WDIselected = WDI(indicator=c('NY.GDP.PCAP.KD', "SP.POP.80UP.MA.5Y","SP.POP.80UP.FE.5Y", "SG.LAW.INDX","SP.POP.DPND.OL", "AG.LND.FRST.ZS") , country="all", start=1960, end=2021)
#####merging-----

WDIselected <- WDIselected%>%
  rename(country_text_id = iso3c)

WDIselectedVdem <- merge(vdem,WDIselected,by=c("year","country_text_id"))


rm(list = setdiff(ls(), "WDIselectedVdem")) 

####adding maps function to the dataset-----
WorldMap <- map_data("world")
WorldMap <- WorldMap%>%
  rename (country_name = region)
##Checking for differences and fixing tgeììhem ##
diff <- setdiff(WDIselectedVdem$country_name, WorldMap$country_name)
view(diff)
c("Republic of Congo", "UK", "Gambia", "Myanmar", "Palestine", "Swaziland", "Trinidad", "USA" )
##Hong Kong is treated as an independent country WDI dataset and as a region of China maps, so renaming##

WorldMap$country_name[WorldMap$subregion=='Hong Kong'] <- 'Hong Kong'
WorldMap <- WorldMap %>%
  mutate(country_name = recode(str_trim(country_name), 
                               "USA" = "United States of America",
                               "UK" = "United Kingdom" ,
                               "Gambia" = "The Gambia",
                               "Trinidad" = "Trinidad and Tobago" ,
                               "Republic of Congo" = "Republic of the Congo" ,
                               "Swaziland"= "Eswatini",
                               "Palestine" = "Palestine/West Bank",
                               "Myanmar" = "Burma/Myanmar" )) 

diff <- setdiff(WDIselectedVdem$country_name, WorldMap$country_name)
view(diff)


MergedMap <- inner_join(WDIselectedVdem, WorldMap, by= "country_name")

p <- ggplot()
p <- p + geom_polygon( data=MergedMap, 
                       aes(x=long, y=lat, group=group, fill = v2x_polyarchy), 
                       color="white", size = 0.1)
p



#Colour coded for different indicators
p <- ggplot()
p <- p + geom_polygon( data=MergedMap, 
                       aes(x=long, y=lat, group=group, fill = v2x_polyarchy), 
                       color="black", size = 0.1)
p



b <- p + scale_fill_gradient("% polyarchy", low = "yellow", high =  "red", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())
b

#2
q <- ggplot()
q <- q + geom_polygon( data=MergedMap, 
                       aes(x=long, y=lat, group=group, fill = NY.GDP.PCAP.KD), 
                       color="black", size = 0.1)
q



plot2 <- q + scale_fill_gradient("% GDP per capita", low = "yellow", high =  "green", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())
plot2

#3
r <- ggplot()
r <- r + geom_polygon( data=MergedMap, 
                       aes(x=long, y=lat, group=group, fill = SG.LAW.INDX), 
                       color="black", size = 0.1)
r



plot3 <- r + scale_fill_gradient("Women Business and the Law Index", low = "blue", high =  "yellow", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())
plot3

#4
s<- ggplot()
s <- s+ geom_polygon( data=MergedMap, 
                       aes(x=long, y=lat, group=group, fill = SP.POP.80UP.MA.5Y), 
                       color="black", size = 0.1)
s



plot4 <- s + scale_fill_gradient("Population ages 80 and above, male (% of male population)", low = "blue", high =  "red", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank())
plot4
