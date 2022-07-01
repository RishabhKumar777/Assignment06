library(ggplot2)
library(dplyr)
library(tidyverse)
library(bslib)
library(stringr)
# 1
stormy=as.data.frame(read.table("./Downloads/StormEvents.csv",
                                sep=",", header=TRUE))
colnames(stormy)

# 2
stormyWithColumns=stormy[,c("BEGIN_DATE_TIME","END_DATE_TIME","EPISODE_ID",
                            "EVENT_ID","STATE","STATE_FIPS","CZ_NAME",
                            "CZ_TYPE","CZ_FIPS","EVENT_TYPE","SOURCE",
                            "BEGIN_LAT","BEGIN_LON","END_LAT","END_LON")]

# 3
stormyArranged=stormyWithColumns%>%arrange(STATE)

# 4
stormyArranged$STATE = str_to_title(stormyArranged$STATE)

# 5
stormyFiltered=filter(stormyArranged,CZ_TYPE=="C")
stormyFiltered=subset(stormyFiltered,select=-c(CZ_TYPE))

# 6
stormyFiltered$CZ_FIPS=str_pad(stormyFiltered$CZ_FIPS, 3, 
                               side = c("left"), pad = "0")
stormyFiltered$STATE_FIPS=str_pad(stormyFiltered$STATE_FIPS, 2, 
                                  side = c("left"), pad = "0")
stormyUnited=unite(stormyFiltered,col='fips'
                   , c('CZ_FIPS', 'STATE_FIPS') , sep = "", remove = TRUE)

# 7
stormyLower=stormyUnited%>%rename_all(tolower)

#8
data("state")
us_state_info<-data.frame(state=state.name, region=state.region, 
                          area=state.area)
statesInfo=mutate_all(us_state_info,toupper)

# 9
stateCounts=stormyWithColumns %>% count(STATE)%>%rename(c("newstate"="STATE"))
stormyMerged <- merge(x=stateCounts,y=statesInfo,by.x="newstate", by.y="state")
head(stormyMerged)
stormyMerged$area <- sapply(stormyMerged$area, as.numeric)

# 10
library(ggplot2)
stormPlots=ggplot(stormyMerged,aes(x=area,y=n))+geom_point(aes(color=region))+
  labs(x="Land area(Square miles)",y="# storms in 92'")
stormPlots

head(stormyMerged$area,500)


