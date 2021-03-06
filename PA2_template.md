# The Most Expensive and Deadliest Weather Events in the United States, 1950-2011
apyle@github.com  
2015-09-27  

<br />
<hr />

## Synopsis

Weather events can be dangerous and costly in the United States. The United States 
National Oceanic and Atmospheric Administration (NOAA) tracks these events and 
makes it publically available in a storm database. By analyzing this database we 
can find the weather events that are the most dangerous and most expensive. The 
data covers over fifty years of events, some of which is not relevant to these 
questions. After cleaning the database it becomes evident that tornados, thunderstorm 
wind, excessive heat, flooding, and lightning are the most harmful events for 
population health. Floods, hurricanes & typhons, tornados, storm tides, and hail 
cause the most economic damage. These results are important for driving public 
policy, particularly for tornados and flooding which appear in the top five of 
both lists.

<br />
<hr />

## Data

A few things to note about the NOAA data. 

  * Using actual dollar amounts; these have not been adjusted for inflation;
  * We're assuming there are no duplicate records. 
  
<br />
<hr />

## Data Processing

Note: all code is maintained in my code repository on [Github][1].

This analysis was performed using RStudio, including tools knitr, data.table, and 
dplyr.


```r
# keep the chatter down so the write-up stays clean
options(warn = -1)

# make sure we have access to the knitr library for setup and housekeeping
# suppressWarnings should not be necessary since we have used the warn option
# but using it to reinforce the idea that we want to keep the write-up clean.
suppressWarnings(library(knitr))

# use data.table for ease of storage
suppressWarnings(library(data.table))

# use dplyr for data manipulation
suppressWarnings(library(dplyr))
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:data.table':
## 
##     between, last
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# we'll use the ggplot2 system to create and save the plots 
suppressWarnings(library(ggplot2))

# set knitr default option values, specifically echo, turning on the cache and
# setting its location, and specifying the figure directory
opts_chunk$set(echo = TRUE, 
               cache = TRUE, 
               cache.path = "cache/", 
               fig.path = "figure/")
```

<br />
<hr />

The NOAA data file was downloaded from the class website at https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2. 
Much of the data in the is not relevant to the current questions so the most fields
where not loaded into RStudio and most records were discarded as they do not have 
entries for fatalities, injuries, property damage, or crop damage. 


```r
# We only need the injury and fatality counts to answer the first question and the 
# property and crop damage for the second question. This will allow us to not load 
# the values we don't need, saving memory and processing time
col_classes <- c("NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", 
                 "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", 
                 "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", 
                 "numeric", "numeric", "numeric", "factor", "numeric", "factor", 
                 "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", 
                 "numeric")
storm.file <- bzfile("repdata_data_StormData.csv.bz2")
storm.data <- data.table(read.csv(storm.file, colClasses = col_classes))

# most event entries do not have any reported fatalities, injuries, or damages. Filter those out
storm.subset <- storm.data[!(FATALITIES == 0 & INJURIES == 0 & PROPDMG == 0 & CROPDMG == 0), ]
```

There are 48 weather events categorized by NOAA. The input data has 985. In order 
to analyze the data and find the most dangerous and expensive events we must
clean up the `EVTYPE` field in the input file. This effort applies to answering both 
questions.


```r
# these are NOAA's weather events. Use this list to clean up the values found in the input file
event.list <- c(
              "Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", 
                "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", 
                "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", 
                "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Freezing Fog",
                "Frost/Freeze", "Funnel Cloud", "Hail", "Heat", 
                "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", 
                "Hurricane/Typhoon", "Ice Storm", "Lakeshore Flood", "Lake-Effect Snow",
                "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind",
                "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", 
                "Storm Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", 
                "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", 
                "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")
event.list <- data.table(event.list)
event.list <- mutate(event.list, search = toupper(event.list))
setkey(event.list, search)

# add a variable in our data set to store the cleaned up version of the event type
storm.subset <- mutate(storm.subset, search = toupper(EVTYPE))
setkey(storm.subset, search)

# Match the input data with the NOAA list
storm.set1 <- left_join(storm.subset, event.list, by = c("search" = "search"))

# how many are matched with good EVTYPE values?
records.total <- count(storm.set1)
records.unmatched <- sum(is.na(storm.set1$event.list))
records.matched <- sum(!is.na(storm.set1$event.list))
records.ratio <- round(records.matched / records.total, 3)
```

There are a total of 254633 records. Of these records, there are 172897 
which correctly coded as NOAA's recognized weather events. This is a ratio of 67.9% 
that were well coded. The remaining unmatched records represent a significant number of fatalities, 
injuries, and damage that could change the results of the most dangerous and expensive weather events. 
In order to keep this miscoding from under-reporting the results, we'll clean up as much of the data 
as we can.

Some of the data records include multiple weather events such as Thunderstorm Wind and Lightning or 
Heavy Snow and High Wind. In these cases we will categorize the record under the first weather event. 
In these two examples, the data record would be recorded as Thunderstorm Wind and Heavy Snow 
respectively and the data would not appear as Lightning or Heavy Snow. This will prevent fatalities, 
injuries, and damages from appearing multiple times which would skew the total. The second, or third, 
events listed will be under reported in these cases. Another approach would be to divide the damages 
evenly between the weather events. However, this proves to be very time consuming for a marginal 
refinement of the results.


```r
# most of the records were coded with the correct EVTYPE, i.e., the ones in the NOAA weather event list.
# Now we need to clean up as many of the incorrectly coded EVTYPEs

# there are a large number of Thunderstorm Winds that are not coded correctly. 
is.tstm <- substr(storm.set1$search, 1, 9) == "TSTM WIND"
storm.set1$event.list[is.tstm] <- c("Thunderstorm Wind")
# some are spelled out but also include plurals or suffixes
is.tstm <- substr(storm.set1$search, 1, 17) == "THUNDERSTORM WIND"
storm.set1$event.list[is.tstm] <- c("Thunderstorm Wind")
# some are spelled out with extra plurals
is.tstm <- substr(storm.set1$search, 1, 19) == "THUNDERSTORMS WINDS"
storm.set1$event.list[is.tstm] <- c("Thunderstorm Wind")

# Marine Thunderstorm Winds are also coded as TSTM
is.marine <- grep("MARINE TSTM", storm.set1$search)
storm.set1$event.list[is.marine] <- c("Marine Thunderstorm Wind")

# Flooding is categorized in many ways
is.flood <- grep("URBAN/SML STREAM FLD", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")
is.flood <- grep("FLOOD/FLASH FLOOD", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")
is.flood <- grep("RIVER FLOOD", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")
is.flood <- grep("URBAN FLOODING", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")
is.flood <- grep("URBAN FLOOD", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")
is.flood <- grep("FLOODING", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")
is.flood <- grep("COASTAL FLOODING", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")
is.flood <- grep("RIVER FLOODING", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")
is.flood <- grep("FLOOD/RAIN/WINDS", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")
is.flood <- grep("RIVER FLOODING", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")
is.flood <- grep("FLOODS", storm.set1$search)
storm.set1$event.list[is.flood] <- c("Flood")

# there are a large number of Flash Floods that are not coded correctly. 
is.tstm <- substr(storm.set1$search, 1, 11) == "FLASH FLOOD"
storm.set1$event.list[is.tstm] <- c("Flash Flood")

# there are a large number of Hail events that are not coded correctly. 
is.tstm <- substr(storm.set1$search, 1, 4) == "HAIL"
storm.set1$event.list[is.tstm] <- c("Hail")

# Prefer Excessive Heat when there is a descriptor
is.heat <- substr(storm.set1$search, 1, 9) == "HEAT WAVE"
storm.set1$event.list[is.heat] <- c("Excessive Heat")
is.heat <- substr(storm.set1$search, 1, 12) == "EXTREME HEAT"
storm.set1$event.list[is.heat] <- c("Excessive Heat")

# Heat often has descriptors that make it hard to code correctly
is.cold <- substr(storm.set1$search, 1, 12) == "EXTREME COLD"
storm.set1$event.list[is.cold] <- c("Extreme Cold/Wind Chill")

# several items are listed as plural while the standard is singular
is.plural <- grep("HIGH WINDS", storm.set1$search)
storm.set1$event.list[is.plural] <- c("High Wind")

is.plural <- grep("RIP CURRENTS", storm.set1$search)
storm.set1$event.list[is.plural] <- c("Rip Current")


# Hurricanes & Typhons are very damaging. Make sure we include these rare but 
# important events in the results
# These grep statements will include the entries already matched but does not 
# cause a problem
is.ht <- grep("HURRICANE", storm.set1$search)
storm.set1$event.list[is.ht] <- c("Hurricane/Typhoon")
is.ht <- grep("TYPHOON", storm.set1$search)
storm.set1$event.list[is.ht] <- c("Hurricane/Typhoon")

# Tropical Storms are too
is.ts <- grep("TROPICAL STORM", storm.set1$search)
storm.set1$event.list[is.ts] <- c("Tropical Storm")

# clean up some Tornados
is.tornado <- substr(storm.set1$search, 1, 7) == "TORNADO"
storm.set1$event.list[is.tornado] <- c("Tornado")

# Storm Tide comes across as Storm Surge
is.st <- substr(storm.set1$search, 1, 11) == "STORM SURGE"
storm.set1$event.list[is.st] <- c("Storm Tide")

#  Heavy rain has additional terms
is.rain <- substr(storm.set1$search, 1, 10) == "HEAVY RAIN"
storm.set1$event.list[is.rain] <- c("Heavy Rain")

#  Fog can be categorized as Dense Fog
is.fog <- substr(storm.set1$search, 1, 3) == "FOG"
storm.set1$event.list[is.rain] <- c("Dense Fog")

# after our clean-up how many are matched with good EVTYPE values?
records.total <- count(storm.set1)
records.unmatched <- sum(is.na(storm.set1$event.list))
records.matched <- sum(!is.na(storm.set1$event.list))
records.ratio <- round(records.matched / records.total, 3)
```

After cleaning up the data there are still a total of 254633 records 
of which we now have matched 252430 entries with the 48 NOAA 
categories. This is a success rate of 99.1% records matched.

While it would be preferable to account for 100% of the records, there are still 
entries that have damages or injuries but fall outside of NOAA's list of weather 
events. These are items such as Wild Fires. The remaining unmatched items do not 
change the results of this analysis for finding the weather events with the biggest 
impact on population safety and economic well being.

The input data records store property and crop damages in two fields. One is for 
the numerical amount (typically three significant digits) and another for the 
magnitude of the damages. This is recorded as K for thousands, M for millions, and 
B for billions of dollars. In order to properly analyze the economic impact of the 
various events these figures need to be normalized. 


```r
# the input data records property and crop damages in two fields, one for a numerical amount 
# and another for the magnitude of damages. We'll need to get the actual number for our analysis

# adjust property damage costs based on the PROPGMGEXP
storm.set1 <- storm.set1 %>%
        mutate(prop.total = PROPDMG) %>%
        mutate(prop.total = ifelse(PROPDMGEXP == "K", prop.total * 1000, prop.total)) %>%
        mutate(prop.total = ifelse(PROPDMGEXP == "k", prop.total * 1000, prop.total)) %>%
        mutate(prop.total = ifelse(PROPDMGEXP == "M", prop.total * 1000000, prop.total)) %>%
        mutate(prop.total = ifelse(PROPDMGEXP == "m", prop.total * 1000000, prop.total)) %>%
        mutate(prop.total = ifelse(PROPDMGEXP == "B", prop.total * 1000000000, prop.total))

# and do the same for crop damage costs
storm.set1 <- storm.set1 %>%
        mutate(crop.total = CROPDMG) %>%
        mutate(crop.total = ifelse(CROPDMGEXP == "K", crop.total * 1000, crop.total)) %>%
        mutate(crop.total = ifelse(CROPDMGEXP == "k", crop.total * 1000, crop.total)) %>%
        mutate(crop.total = ifelse(CROPDMGEXP == "M", crop.total * 1000000, crop.total)) %>%
        mutate(crop.total = ifelse(CROPDMGEXP == "m", crop.total * 1000000, crop.total)) %>%
        mutate(crop.total = ifelse(CROPDMGEXP == "B", crop.total * 1000000000, crop.total))
```

## Results

Answer these two questions:

1. Across the United States, which types of events (as indicated in the `EVTYPE` variable) are most harmful with respect to population health?



```r
# assemble the reporting data structure from the cleaned up data

# first, assemble the reporting data for dangerous weather events
by_event <- group_by(storm.set1, event.list)
dangerous <- summarize(by_event, sum(FATALITIES), sum(INJURIES))
setnames(dangerous, 1:3, c("WeatherEvent", "Deaths", "Injuries"))
dangerous <- dangerous %>% 
        na.omit() %>% 
        mutate(Total = Deaths + Injuries) %>% 
        mutate(rev.sort = 100000 - Total) %>%
        mutate(WeatherEvent = as.factor(WeatherEvent))

setkey(dangerous, rev.sort)

g <- ggplot(dangerous, aes(x = WeatherEvent, y = Total)) +
        geom_bar(stat = "identity") +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("Weather Event") +
        ylab("Total Injuries and Fatalities") +
        ggtitle("The Most Dangerous Weather Events in the United States")

dev.set(2)
```

```
## pdf 
##   2
```

```r
png(file="figure/dangerous.png", width=1672)
print(g)
dev.off()
```

```
## pdf 
##   2
```

```r
dangerous[1:5, ]
```

```
##         WeatherEvent Deaths Injuries Total rev.sort
## 1:           Tornado   5658    91364 97022     2978
## 2: Thunderstorm Wind    709     9459 10168    89832
## 3:    Excessive Heat   2180     7074  9254    90746
## 4:             Flood    530     6894  7424    92576
## 5:         Lightning    816     5230  6046    93954
```

As can be seen from the above table and the chart below, Tornados are by far the 
most dangerous weather event in the United States. It is an order of magnitude more 
dangerous than the second most dangerous event, Thunderstorm Wind.


![](figure/dangerous.png)

<br />
<hr />

2. Across the United States, which types of events have the greatest economic consequences?


```r
# assemble the reporting data for weather event costs
by_event <- group_by(storm.set1, event.list)
costs <- summarize(by_event, sum(prop.total), sum(crop.total))
setnames(costs, 1:3, c("WeatherEvent", "Property", "Crop"))
costs <- costs %>% 
        na.omit() %>% 
        mutate(Total = Property + Crop) %>% 
        mutate(rev.sort = 100000000 - Total)


setkey(costs, rev.sort)

g <- ggplot(costs, aes(x = WeatherEvent, y = Total)) +
        geom_bar(stat = "identity") +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("Weather Event") +
        ylab("Total Costs in Dollars") +
        ggtitle("The Most Expensive Weather Events in the United States")

dev.set(2)
```

```
## pdf 
##   2
```

```r
png(file="figure/cost.png", width=1672)
print(g)
dev.off()
```

```
## pdf 
##   2
```

```r
costs[1:5, ]
```

```
##         WeatherEvent     Property        Crop        Total      rev.sort
## 1:             Flood 150442587594 10951026050 161393613644 -161293613644
## 2: Hurricane/Typhoon  85356410010  5516117800  90872527810  -90772527810
## 3:           Tornado  58541931979   417461470  58959393449  -58859393449
## 4:        Storm Tide  47964724000      855000  47965579000  -47865579000
## 5:              Hail  15974469548  3026094623  19000564171  -18900564171
```

As can be seen from the above table and the chart below, Floods are the most 
expensive weather event in the United States. However, Hurricanes, Tornadoes, and 
Storm Tide have also caused tens of billions of dollars in damages during the 
research period. Flooding is also the only weather event that causes more than ten 
billion dollars in damages for both property and agriculture.

![](figure/cost.png)

[1]: https://github.com/apyle/RepData_PeerAssessment2


