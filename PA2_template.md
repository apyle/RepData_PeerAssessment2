# The Most Expensive and Deadliest Weather Events in the United States, 1950-2011
apyle@github.com  
2015-09-27  

<br />
<hr />

## Synopsis

Weather events can be dangerous and costly in the United States. The United States 
National Oceanic and Atmospheric Administration's (NOAA) tracks these events and 
makes it publically available in a storm database. By analyzing this database we 
can find the weather events that are the most dangerous and most expensive. The 
data covers over fifty years of events, some of which is not relevant to these 
estions. After cleaning the database it becomes evident that tornados, thunderstorm 
wind, excessive heat, flooding, and lightning are the most harmful events for 
population health. Floods, hurricanes & typhons, tornados, storm tides, and hail 
cause the most economic damage. These results are important for driving public 
policy, particularly for tornados and flooding which appear in the top five of 
both lists.

<br />
<hr />

## Data

Actual dollars, not adjusted for inflation

Assumes no duplicate records
<br />
<hr />

## Data Processing


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
# set knitr default option values, specifically echo, turning on the cache and
# setting its location, and specifying the figure directory
opts_chunk$set(echo = TRUE, 
               cache = TRUE, 
               cache.path = "cache/", 
               fig.path = "figure/")
```

<br />
<hr />


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

## Just for sanity checks. Not really needed for analysis
#my.factor <- as.factor(storm.data$EVTYPE)
#summary(my.factor)
#levels(my.factor)
```

There are 48 weather events categorized by NOAA. The input data has 985. In order 
to analyze the data and find the most dangerous and expensive events we much 
clean up the `EVTYPE` field in the input file. This applies to answering both 
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


# Hurricanes & Typhons are very damaging. Make sure we include these rare but important events in the results
# These grep statements will include the entries already matched but does not cause a problem
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

# after our clean-up how many are matched with good EVTYPE values?
records.total <- count(storm.set1)
records.unmatched <- sum(is.na(storm.set1$event.list))
records.matched <- sum(!is.na(storm.set1$event.list))
records.ratio <- round(records.matched / records.total, 3)
```

After cleaning up the data there are still a total of 254633 records of which we now have matched 252430 for a total of 99.1%.


```r
# the input data records property and crop damages in two fields, one for a numerical amount 
# and another for the magnitude of damages. We'll need to get the actual number for our analysis

# adjust property damage costs based on the PROPGMGEXP
storm.set1 <- storm.set1 %>%
        mutate(prop.total = PROPDMG) %>%
        mutate(prop.total = ifelse(PROPDMGEXP == "K", prop.total * 1000, prop.total)) %>%
        mutate(prop.total = ifelse(PROPDMGEXP == "M", prop.total * 1000000, prop.total)) %>%
        mutate(prop.total = ifelse(PROPDMGEXP == "B", prop.total * 1000000000, prop.total))

# and do the same for crop damage costs
storm.set1 <- storm.set1 %>%
        mutate(crop.total = CROPDMG) %>%
        mutate(crop.total = ifelse(CROPDMGEXP == "K", crop.total * 1000, crop.total)) %>%
        mutate(crop.total = ifelse(CROPDMGEXP == "M", crop.total * 1000000, crop.total)) %>%
        mutate(crop.total = ifelse(CROPDMGEXP == "B", crop.total * 1000000000, crop.total))
```

## Results

Answer these two questions:

1. Across the United States, which types of events (as indicated in the `EVTYPE` variable) are most harmful with respect to population health?
1. Across the United States, which types of events have the greatest economic consequences?



```r
# assemble the reporting data structure from the cleaned up data

# first, assemble the reporting data for dangerous weather events
by_event <- group_by(storm.set1, event.list)
dangerous <- summarize(by_event, sum(FATALITIES), sum(INJURIES))
setnames(dangerous, 1:3, c("Weather Event", "Deaths", "Injuries"))
dangerous <- dangerous %>% 
        #na.omit() %>% 
        mutate(Total = Deaths + Injuries) %>% 
        mutate(rev.sort = 100000 - Total)

setkey(dangerous, rev.sort)

dangerous[1:47, ]
```

```
##                Weather Event Deaths Injuries Total rev.sort
##  1:                  Tornado   5658    91364 97022     2978
##  2:        Thunderstorm Wind    709     9459 10168    89832
##  3:           Excessive Heat   2180     7074  9254    90746
##  4:                    Flood    530     6894  7424    92576
##  5:                Lightning    816     5230  6046    93954
##  6:                       NA    532     2865  3397    96603
##  7:                     Heat    937     2100  3037    96963
##  8:              Flash Flood   1018     1785  2803    97197
##  9:                Ice Storm     89     1975  2064    97936
## 10:                High Wind    289     1500  1789    98211
## 11:             Winter Storm    206     1321  1527    98473
## 12:        Hurricane/Typhoon    135     1333  1468    98532
## 13:                     Hail     15     1361  1376    98624
## 14:               Heavy Snow    127     1021  1148    98852
## 15:              Rip Current    577      529  1106    98894
## 16:                 Wildfire     75      911   986    99014
## 17:                 Blizzard    101      805   906    99094
## 18:  Extreme Cold/Wind Chill    287      255   542    99458
## 19:               Dust Storm     22      440   462    99538
## 20:           Tropical Storm     66      383   449    99551
## 21:           Winter Weather     33      398   431    99569
## 22:                Avalanche    224      170   394    99606
## 23:              Strong Wind    103      280   383    99617
## 24:                Dense Fog     18      342   360    99640
## 25:               Heavy Rain     98      255   353    99647
## 26:                High Surf    104      156   260    99740
## 27:                  Tsunami     33      129   162    99838
## 28:          Cold/Wind Chill     95       12   107    99893
## 29:               Storm Tide     24       43    67    99933
## 30: Marine Thunderstorm Wind     19       34    53    99947
## 31:               Dust Devil      2       43    45    99955
## 32:       Marine Strong Wind     14       22    36    99964
## 33:               Waterspout      3       29    32    99968
## 34:            Coastal Flood      3        2     5    99995
## 35:                  Drought      0        4     4    99996
## 36:             Funnel Cloud      0        3     3    99997
## 37:         Marine High Wind      1        1     2    99998
## 38:                    Sleet      2        0     2    99998
## 39:    Astronomical Low Tide      0        0     0   100000
## 40:              Dense Smoke      0        0     0   100000
## 41:             Freezing Fog      0        0     0   100000
## 42:             Frost/Freeze      0        0     0   100000
## 43:         Lake-Effect Snow      0        0     0   100000
## 44:          Lakeshore Flood      0        0     0   100000
## 45:              Marine Hail      0        0     0   100000
## 46:                   Seiche      0        0     0   100000
## 47:      Tropical Depression      0        0     0   100000
##                Weather Event Deaths Injuries Total rev.sort
```

```r
#summarize(dangerous$Total)
```
<br />
<hr />


```r
# assemble the reporting data for weather event costs
by_event <- group_by(storm.set1, event.list)
costs <- summarize(by_event, sum(prop.total), sum(crop.total))
setnames(costs, 1:3, c("Weather Event", "Property", "Crop"))
costs <- costs %>% 
        #na.omit() %>% 
        mutate(Total = Property + Crop) %>% 
        mutate(rev.sort = 100000000 - Total)


setkey(costs, rev.sort)

#na.omit(costs[1:47, ])
costs[1:50, ]
```

```
##                Weather Event     Property        Crop        Total
##  1:                    Flood 150442587594 10951026050 161393613644
##  2:        Hurricane/Typhoon  85336410030  5506117810  90842527840
##  3:                  Tornado  58530431990   417461470  58947893460
##  4:               Storm Tide  47964724000      855000  47965579000
##  5:                     Hail  15969569553  3025678040  18995247593
##  6:              Flash Flood  16732819178  1437153160  18169972338
##  7:                  Drought   1046106000 13972566000  15018672000
##  8:        Thunderstorm Wind   9750415531  1224394992  10974810523
##  9:                Ice Storm   3944927860  5022113500   8967041360
## 10:           Tropical Storm   7714390550   694896000   8409286550
## 11:                       NA   5689751450  1377301915   7067053365
## 12:                High Wind   6063225043   691821900   6755046943
## 13:             Winter Storm   6688497251    26944000   6715441251
## 14:                 Wildfire   4765114000   295472800   5060586800
## 15:               Heavy Rain   3230998140   795752800   4026750940
## 16:  Extreme Cold/Wind Chill     76385400  1313023000   1389408400
## 17:             Frost/Freeze     10480000  1094186000   1104666000
## 18:               Heavy Snow    932589142   134653100   1067242242
## 19:                Lightning    928659447    12092090    940751537
## 20:                 Blizzard    659213950   112060000    771273950
## 21:           Excessive Heat     18528750   503002000    521530750
## 22:                     Heat      1797000   401461500    403258500
## 23:            Coastal Flood    259570560           0    259570560
## 24:              Strong Wind    175259450    64953500    240212950
## 25:                  Tsunami    144062000       20000    144082000
## 26:                High Surf     89955000           0     89955000
## 27:         Lake-Effect Snow     40115000           0     40115000
## 28:           Winter Weather     20866000    15000000     35866000
## 29:                Dense Fog      9674000           0      9674000
## 30:               Waterspout      9353700           0      9353700
## 31:               Dust Storm      5549000     3100000      8649000
## 32:          Lakeshore Flood      7540000           0      7540000
## 33: Marine Thunderstorm Wind      5857400       50000      5907400
## 34:                Avalanche      3721800           0      3721800
## 35:          Cold/Wind Chill      1990000      600000      2590000
## 36:             Freezing Fog      2182000           0      2182000
## 37:      Tropical Depression      1737000           0      1737000
## 38:         Marine High Wind      1297010           0      1297010
## 39:                   Seiche       980000           0       980000
## 40:               Dust Devil       718630           0       718630
## 41:             Volcanic Ash       500000           0       500000
## 42:       Marine Strong Wind       418330           0       418330
## 43:    Astronomical Low Tide       320000           0       320000
## 44:             Funnel Cloud       194600           0       194600
## 45:              Rip Current       163000           0       163000
## 46:              Dense Smoke       100000           0       100000
## 47:              Marine Hail         4000           0         4000
## 48:                    Sleet            0           0            0
## 49:                       NA           NA          NA           NA
## 50:                       NA           NA          NA           NA
##                Weather Event     Property        Crop        Total
##          rev.sort
##  1: -161293613644
##  2:  -90742527840
##  3:  -58847893460
##  4:  -47865579000
##  5:  -18895247593
##  6:  -18069972338
##  7:  -14918672000
##  8:  -10874810523
##  9:   -8867041360
## 10:   -8309286550
## 11:   -6967053365
## 12:   -6655046943
## 13:   -6615441251
## 14:   -4960586800
## 15:   -3926750940
## 16:   -1289408400
## 17:   -1004666000
## 18:    -967242242
## 19:    -840751537
## 20:    -671273950
## 21:    -421530750
## 22:    -303258500
## 23:    -159570560
## 24:    -140212950
## 25:     -44082000
## 26:      10045000
## 27:      59885000
## 28:      64134000
## 29:      90326000
## 30:      90646300
## 31:      91351000
## 32:      92460000
## 33:      94092600
## 34:      96278200
## 35:      97410000
## 36:      97818000
## 37:      98263000
## 38:      98702990
## 39:      99020000
## 40:      99281370
## 41:      99500000
## 42:      99581670
## 43:      99680000
## 44:      99805400
## 45:      99837000
## 46:      99900000
## 47:      99996000
## 48:     100000000
## 49:            NA
## 50:            NA
##          rev.sort
```

## Conclusion

