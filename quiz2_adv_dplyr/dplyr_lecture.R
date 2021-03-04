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



