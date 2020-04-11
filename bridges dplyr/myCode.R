install.packages("ggplot2")
install.packages("plyr")
install.packages("readr")
install.packages("choroplethr")
install.packages("dplyr")
install.packages("tidyverse")

library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(tidyverse)


##Download data from the website
dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/WI16.txt"
dat = fread(dest) 
dat = as.tbl(dat)
classes = sapply(dat, class) #get the variables of this data

##View the data
dat
is.na(dat) %>% rowSums %>% hist #check num of missing values of each column
is.na(dat) %>% colSums %>% hist(breaks = 100) #check missing entries
fun = function(x){ return(which(x>20)) }#which is more than 20
(bad =  is.na(dat) %>% colSums %>% fun)
dat = dat[,-bad] #get rid of the columns with too many missing values.


colnames(dat)

##Keep part of the variables
keep = c("COUNTY_CODE_003", "LAT_016" ,"LONG_017" ,"OWNER_022", "YEAR_BUILT_027", 
         "TRAFFIC_LANES_ON_028A", "TRAFFIC_LANES_UND_028B")
x = dat[,match(keep, colnames(dat))]

##Lets see when the bridges are built
ggplot(data = x) +
  geom_bar(mapping = aes(x = YEAR_BUILT_027 ))

##I would like to focus on the bridges built during the Roosevelt's New Deal, 
##that is between 1933 to 1938
nd = x %>% filter(YEAR_BUILT_027 > 1932 & YEAR_BUILT_027 < 1939)
ggplot(data = nd) +
  geom_bar(mapping = aes(x = YEAR_BUILT_027))

##Then I would like to see where these bridges are built
ggplot(data = nd) +
  geom_point(mapping = aes(y = LAT_016, x = LONG_017))
#It seems there is an error point at 0 latitude.
nd = nd %>% filter(LAT_016 > 0)
ggplot(data = nd) +
  geom_point(mapping = aes(y = LAT_016, x = LONG_017, color = YEAR_BUILT_027))
#Throught our the period, bridges are built all over Wisconsin.

##Next I would like to see bridges in some particular country have more lanes 
ggplot(data = nd) + 
  geom_point(mapping = aes(x = COUNTY_CODE_003, y = TRAFFIC_LANES_ON_028A))

ggplot(data = nd) + 
  geom_point(mapping = aes(x = COUNTY_CODE_003, y = TRAFFIC_LANES_UND_028B))

##Last let's take a look who owns these bridges
ggplot(data = nd) + 
  geom_bar(mapping = aes(x = OWNER_022)) + 
  coord_polar()
#Most bridges are owned by only four owners.

nd2 = nd %>% filter(OWNER_022 < 10)
ggplot(data = nd2) +
  geom_point(mapping = aes(y = LAT_016, x = LONG_017, color = OWNER_022))
#There is no specific geographic patterns of the ownership of the bridges.

