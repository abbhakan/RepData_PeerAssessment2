# Reproducible Research: Peer Assessment 2


## Loading and preprocessing the data
Load the repdata-data-StormData.csv.bz2 file into workspace.

```r
library("R.utils")
```

```
## Warning: package 'R.utils' was built under R version 3.1.2
```

```
## Loading required package: R.oo
```

```
## Warning: package 'R.oo' was built under R version 3.1.2
```

```
## Loading required package: R.methodsS3
```

```
## Warning: package 'R.methodsS3' was built under R version 3.1.2
```

```
## R.methodsS3 v1.6.1 (2014-01-04) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.18.0 (2014-02-22) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## 
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## 
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## 
## R.utils v1.34.0 (2014-10-07) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## 
## The following object is masked from 'package:utils':
## 
##     timestamp
## 
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```

```r
if (!file.exists("./repdata-data-StormData.csv")) {
        bunzip2("./repdata-data-StormData.csv.bz2")
}
```
        
# Read data
Read the repdata-data-Stormdata.csv into data frame.

```r
data <- read.table("./repdata-data-StormData.csv", header = TRUE, sep = ",", na.strings = "", comment.char = "#")
cnames <-readLines("./repdata-data-StormData.csv", 1)
cnames <- strsplit(cnames, ",", fixed = TRUE)
cnames <- make.names(cnames[[1]])
cnames<-gsub("(.*)\\.", "\\1", cnames)
cnames<-gsub("_", "", cnames)
names(data) <- cnames
```
## Which types of events are most harmful with respect to population health?
Convert to data table and summarise injuries and fatalities then group by event type. Order by sum and display the top five items.

```r
library("data.table")
dt <- data.table(data)
res <- dt[, list(sum=sum(X.FATALITIES, X.INJURIES)), by = X.EVTYPE]
res <- res[rev(order(res$sum))]
head(res)
```

```
##          X.EVTYPE   sum
## 1:        TORNADO 96979
## 2: EXCESSIVE HEAT  8428
## 3:      TSTM WIND  7461
## 4:          FLOOD  7259
## 5:      LIGHTNING  6046
## 6:           HEAT  3037
```
We can see that Tornado is causing the highest total sum of fatalities and injuries.

## Which types of events have the greatest economic consequences?
Convert to data table and summarise injuries and fatalities then group by event type. Order by sum and display the top five items.

```r
library("data.table")
dt <- data.table(data)
res <- dt[, list(sum=sum(X.FATALITIES, X.INJURIES)), by = X.EVTYPE]
res[rev(order(res$sum))]
```

```
##                          X.EVTYPE   sum
##   1:                      TORNADO 96979
##   2:               EXCESSIVE HEAT  8428
##   3:                    TSTM WIND  7461
##   4:                        FLOOD  7259
##   5:                    LIGHTNING  6046
##  ---                                   
## 981:                     LIGHTING     0
## 982: THUNDERSTORM WINDS LIGHTNING     0
## 983:                   TORNADO F0     0
## 984:            THUNDERSTORM WINS     0
## 985:                     SNOW/ICE     0
```
We can see that Tornados is causing the highest total sum of fatalities and injuries.
