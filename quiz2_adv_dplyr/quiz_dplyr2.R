
setwd('/Users/dylan/PycharmProjects/FSAN-830')


###### load practice for convenient
library("causact")

head(delivDF) # see head
head(prodLineDF)
###################################

################ delivDF$actualShipDate[1] - delivDF$plannedShipDate[1]
library("lubridate")
# extract variable to a new df
shipDF = delivDF
shipDF$plannedShipDate = mdy(shipDF$plannedShipDate) # convert to date
shipDF$actualShipDate = mdy(shipDF$actualShipDate)

shipDF$actualShipDate[1] - shipDF$plannedShipDate[1]

# get today
thisDay = today()
year(thisDay)
month(thisDay)
day(thisDay)
wday(thisDay)
wday(thisDay, label = TRUE)

mday(thisDay)
yday(thisDay)



################ lateness calculation
head(shipDF)
##### Does service level (measured by on-time shipments) vary across product categories??
#  Using dplyr to compute lateness
library("dplyr")

# extract some variables ...focus on
shipDateDF = shipDF %>% 
  select(shipID, plannedShipDate, actualShipDate) %>%
  distinct() # get unique rows to avoid double-counting

head(shipDateDF)

# add a column(var) for capturing lateness.
shipDateDF = shipDateDF %>%
  mutate( # add a new column
    lateFlag = ifelse(actualShipDate > plannedShipDate, TRUE, FALSE))

head(shipDateDF)
# tip: R treats logical (TRUE/FALSE) values as numbers when used with numeric

# calculating late shipments, 
#the following code collapses the data on 23,339 shipments into two rows: 
#one for on-time shipments (i.e. lateFlag = FALSE) and 
# one for late shipments (i.e. lateFlag = TRUE):

shipDateDF %>% # did not assign to a new name(so only display)
  group_by(lateFlag) %>%  #### group by 
  summarise(countLate = n()) %>%  # creates a new data frame, one row for a group, 
  #countLate is new var in that df (you could create more)
  mutate(proportion = countLate/sum(countLate))  # add a new column




############ Bringing in Product Category Information

head(prodLineDF)
head(shipDF)
# we want to calculate lateness by product category
#the product category information is in prodLineDF 
# the actual/planned shipment data is in shipDF ------> combine these 2

#  left join ---------------- how it works -------------> 

catDF = shipDF %>%
  left_join(prodLineDF) %>%  # "partID"
  filter(!is.na(prodCategory)) # in ship DF, may 

head(catDF)








######## Does service level (measured by on-time shipments) vary across product categories?


catDF %>% 
  select(
    shipID,plannedShipDate, actualShipDate, prodCategory
  ) %>% 
  distinct() %>% 
  mutate(lateFlag = ifelse(actualShipDate > plannedShipDate, TRUE, FALSE)
  ) %>%  # create new column
  group_by(prodCategory, lateFlag) %>% 
  summarize(countLate = n()) %>%  # True =1, False=0... so
  mutate(proportion = countLate/sum(countLate)) %>% 
  arrange(lateFlag, proportion) # orders the rows of a data frame by the values of selected columns.
# ascending default（从小到大）, arrange(desc(mass)) for descend


###  How often do orders include products from more than one product category?
#例如 1个shipment 包含 machine 和 Liquids
# find # of categories included on each shipID
numCatDF = catDF %>%
  select(shipID,
         plannedShipDate,
         actualShipDate,
         prodCategory) %>%
  distinct() %>% # unique rows only
  group_by(shipID) %>%
  summarize(numCategories = n()) %>%
  arrange(desc(numCategories))

# print out summary of numCategories column
numCatDF %>% 
  group_by(numCategories) %>%
  summarize(numShipID = n()) %>%
  mutate(percentOfShipments = 
           numShipID / sum(numShipID))






####### quiz start
#how many product categories are included in each shipID
# how many shipID's (i.e. shipments) included ( 3 种products)
#e.g. shipemnt 100001 有 橘子， 火车， 桌子, count+1

numCatDF = catDF %>%
  select(shipID,
         plannedShipDate,
         actualShipDate,
         prodCategory) %>%
  distinct() %>% # unique rows only
  group_by(shipID) %>%
  summarize(numCategories = n()) %>% # 每个shipID出现了多少次
  arrange(desc(numCategories))

numCatDF %>% 
  group_by(numCategories) %>%
  summarize(numShipID = n())


#
library("hflights")
#which day of the week experienced the largest percentage of delayed flights from Houston in 2011:

hflights %>%
  mutate(delayFlag = ifelse(DepDelay > 0,1,0)) %>%
  group_by(DayOfWeek) %>%
  summarise(pctDelay = mean(delayFlag, na.rm = TRUE),count = n()) %>%
  filter(count > 30000) %>%
  arrange(desc(pctDelay))

# which destination airports have the largest percentage of delayed flights (NA's can be ignored). 
# Only collect data for destinations that had greater than 50 flights from Houston
# Check all of the following destinations that experienced departure delays on more than 75% of the flights to that destination.


hflights  %>%
  group_by(Dest) %>%
  mutate(count_dest = n()) %>%
  filter(count_dest > 50) %>%
  mutate(delayFlag = ifelse(DepDelay > 0,1,0)) %>%
  summarise(pctDelay = mean(delayFlag, na.rm = TRUE), count = n()) %>%
  filter(pctDelay > 0.75)

# smallest
  
hflights  %>%
  group_by(Dest) %>%
  mutate(count_dest = n()) %>%
  filter(count_dest > 50) %>%
  mutate(delayFlag = ifelse(DepDelay > 0,1,0)) %>%
  summarise(pctDelay = mean(delayFlag, na.rm = TRUE), count = n()) %>%
  filter(pctDelay < 0.25)
















######### store #30 sold 77,206 units of item #44.

load(file="train.RData")  ##this loads a dataframe called train which has daily sales from 45 Wal-Mart Stores

newDF = train %>% group_by(store_nbr, item_nbr) %>% summarize(totalSales = sum(units)) %>% ungroup() %>% arrange(desc(totalSales))

View(newDF)  

newDF %>%
  group_by(item_nbr)%>%
  mutate(total_sale = sum(totalSales)) %>%
  
  summarise(item_nbr, total_sale)%>%
  distinct()%>%
  arrange(desc(total_sale))
  












###### vote data

county = read.csv("county_facts.csv")
primary = read.csv("primary_results.csv")
head(county)

primary %>%
  group_by(candidate) %>%
  mutate(total = sum(votes)) %>%
  filter(party == "Democrat") %>%
  summarise(candidate, total) %>%
  distinct() %>%
  arrange(desc(total))


##
votejoin = primary %>% left_join(county) 
head(votejoin)


votejoin %>%
  group_by(countyID, party) %>%
  mutate(county_party_sum = sum(votes)) %>%
  filter(county_party_sum==81931, party=="Republican") %>%
  summarise(countyID, stateAbbrev, county_party_sum)%>%
  distinct()

##
votejoin %>%
  group_by(countyID, party) %>%
  mutate(county_party_sum = sum(votes)) %>%
  filter(county_party_sum==22800, party=="Democrat") %>%
  summarise(countyID, stateAbbrev, county_party_sum)%>%
  distinct()

## 
votejoin  %>%
  group_by(countyID)  %>%
  filter(party == "Republican")  %>%
  mutate(county_rep_sum = sum(votes))  %>%
  summarise(areaName, county_rep_sum)  %>%
  distinct() %>%
  arrange(desc(county_rep_sum))

## 
county %>%
  filter(stateAbbrev=="IA") %>%
  summarise(areaName, medianHouseholdIncome)%>%
  arrange(desc(medianHouseholdIncome))


## 



votejoin %>%
  group_by(countyID) %>%
  mutate(pct = votes/sum(votes)) %>% # ungroup 相当于取消countyID # 这个sum限制在group中
  filter(candidate=="Hillary Clinton") %>%
  summarise(pct, candidate, areaName) %>%
  arrange(desc(pct))
  


## NH
votejoin %>%
  filter(stateAbbrev == "NH", party == "Republican", medianHouseholdIncome>50000) %>%
  mutate(all_vote = sum(votes)) %>%
  group_by(candidate) %>%
  mutate(pct = sum(votes)/all_vote) %>%
  filter(candidate == "Donald Trump") %>%
  summarise(pct, candidate) %>%
  distinct()
  

votejoin %>%
  group_by(countyID) %>%
  mutate(pct = votes/sum(votes)) %>% # ungroup 相当于取消countyID # 这个sum限制在group中
  filter(candidate=="Ted Cruz") %>%
  summarise(pct, candidate, areaName) %>%
  arrange(desc(pct))



votejoin %>%
  filter(party == "Republican", stateAbbrev=="IA", medianHouseholdIncome>50000) %>%
  mutate(all_vote = sum(votes)) %>%
  group_by(candidate) %>%
  mutate(pct = sum(votes)/all_vote) %>%
  summarise(pct, candidate) %>%
  distinct()
  







