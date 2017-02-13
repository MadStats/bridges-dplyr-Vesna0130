library(tidyverse)
library(plyr)
library(choroplethr)
library(readr)
library(data.table)
library(ggplot2)
library(hexbin)
library(choroplethrMaps)
library(dplyr)
dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
classes = sapply(tmp, class)

states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()

dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)  

M = x16
M = M[,-14]
fun = function(x){ return(which(x>20)) }
(bad =  is.na(M) %>% colSums %>% fun)
M = M[,-bad]

keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "ADT_029" , "YEAR_ADT_030" ,
         "YEAR_BUILT_027" , "HISTORY_037" ,"STRUCTURE_KIND_043A","STRUCTURE_TYPE_043B" )

M = as.tbl(M)
x = select(M, one_of(keep)) 

x = mutate(x, fips = STATE_CODE_001*1000+COUNTY_CODE_003)
min2dec = function(x){
  n = nchar(x)
  as.numeric(substr(x,1,n-6)) + as.numeric(substr(x,n-5,n))/6e+05 %>% return
}
x = mutate(x,lat = min2dec(LAT_016), lon = -min2dec(LONG_017))

pdf("Bridges2.pdf",width=6,height=4,paper='special') 

ggplot(data = x) + geom_hex(mapping = aes(y = lat, x = lon))
x = x %>% filter(lat > 24 , lat < 50, lon > -130, lon < -66)
ggplot(data = x) + geom_hex(mapping = aes(y = lat, x = lon))

x = x %>% filter(YEAR_BUILT_027>1900)

ggplot(data = x) + 
  geom_bar(mapping = aes(x = YEAR_BUILT_027, fill = as.factor(STRUCTURE_KIND_043A)))

ggplot(data = x) +
  geom_point(mapping = aes(x = lon, y = lat, colour = STRUCTURE_KIND_043A))

x %>% group_by(STRUCTURE_KIND_043A) %>% summarise(ADT=mean(ADT_029)) %>%
  ggplot(mapping = aes(x=STRUCTURE_KIND_043A,y=ADT)) +
  geom_point() +
  geom_line() +
  ggtitle("Average Daily Traffic")

a <- c(1:50)
x %>% filter(YEAR_BUILT_027<=1950) %>% group_by(fips) %>%
  summarise(StrcKind=sum(STRUCTURE_KIND_043A==7)/length(STRUCTURE_KIND_043A)) %>%
  transmute(region = fips, value = StrcKind) %>% county_choropleth(state_zoom = tolower(states$Alberta[a])) + ggtitle("#Bridges Built Before 1950")

x %>% filter(YEAR_BUILT_027>1950) %>% group_by(fips) %>%
  summarise(StrcKind=sum(STRUCTURE_KIND_043A==7)/length(STRUCTURE_KIND_043A)) %>%
  transmute(region = fips, value = StrcKind) %>% county_choropleth(state_zoom = tolower(states$Alberta[a])) + ggtitle("#Bridges Built After 1950")

x %>% filter(YEAR_BUILT_027<=1950) %>% group_by(fips) %>%
  summarise(StrcKind=sum(STRUCTURE_KIND_043A==1|STRUCTURE_KIND_043A==3)/length(STRUCTURE_KIND_043A)) %>%
  transmute(region = fips, value = StrcKind) %>% county_choropleth(state_zoom = tolower(states$Alberta[a])) + ggtitle("#Bridges Built Before 1950")

x %>% filter(YEAR_BUILT_027>1950) %>% group_by(fips) %>%
  summarise(StrcKind=sum(STRUCTURE_KIND_043A==1|STRUCTURE_KIND_043A==3)/length(STRUCTURE_KIND_043A)) %>%
  transmute(region = fips, value = StrcKind) %>% county_choropleth(state_zoom = tolower(states$Alberta[a])) + ggtitle("#Bridges Built After 1950")

dev.off()
