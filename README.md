# Week-2
Finished Exercise 2 commit
---
title: "Exercise 2 FINAL"
author: "Steele Postlewaite"
date: '2022-05-04'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Exercise 1
## Starting task 1 - Import the data

```{r echo=T, results='hide',warning = FALSE, message = FALSE}
# First we need to Load packages

library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(lubridate)
library(zoo)
library(scales)
library(tidyr)

```



```{r echo=T, results='hide',warning = FALSE, message = FALSE}

# Then importing our wild boar data

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") 

# I must have moved the dataset while working on this
# Ended up using the import dataset option in the environment and manually opening the csv file

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

```


# Now onto Task 2


```{r echo=T, results='hide',warning = FALSE, message = FALSE}
# We need to Calculate the time difference between subsequent rows (as per demo)

wildschwein_BE <- group_by(wildschwein_BE,TierID) # group by TierID

wildschwein_BE$timelag  <- as.numeric(difftime(lead(wildschwein_BE$DatetimeUTC), wildschwein_BE$DatetimeUTC),units = "secs")

# converting timelag to integer

wildschwein_BE$timelag = as.integer(wildschwein_BE$timelag)


wildschwein_BE %>% 
  group_by(TierID) %>%
  summarise(
    mean_timelag=mean(timelag, na.rm=T)
  )
```

# How many individuals were tracked?

```{r}
unique(wildschwein_BE$TierName)
unique(wildschwein_BE$TierID)

# This data set contains three individuals, 002A (Sabi), 016A(Rosa), and 018A(Ruth)
```

# How long were they tracked, and are there any gaps?

```{r}
Max_a<-  max(wildschwein_BE$DatetimeUTC)
Max_a
Min_a <- min(wildschwein_BE$DatetimeUTC)
difftime(Max_a,Min_a,units = "days")

#They were tracked for 339 days. No apparent gaps

```

# Were all individuals tracked concurrently or sequentially?

```{r}

ggplot(data=wildschwein_BE)+
  geom_line(mapping=aes(x=wildschwein_BE$DatetimeUTC,y=wildschwein_BE$timelag,colour=wildschwein_BE$TierID))

# They appear to be tracking concurrently
## Note: Look up adjusting group aesthetic

```


# What is the temporal sampling interval between the locations?

```{r}

# Time sampling interval = time lag
# Median of ~900 seconds, about 15 minutes
```

# Task 3: Deriving movement parameters I: Speed

```{r}

# Calculating the Euclidean distance

euc.dist = function(x,y) (sqrt((lead(x)-x)^2+(lead(y)-y)^2))

wildschwein_BE$steplength= euc.dist(wildschwein_BE$E, wildschwein_BE$N)

# Calculating speed between consecutive locations 

wildschwein_BE$Speed = wildschwein_BE$steplength/wildschwein_BE$timelag

#Units in degrees per second 





```

# Task 4: Cross-scale movement analysis

```{r}

# importing new dataset "caro"

caro <- read_delim("caro60.csv",",")
caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)

# trying to reduce granularity of sampling interval
# select every 3rd, 6th and 9th row 

caro_3 = caro[seq(from = 1, to = 200, by = 3),] 
caro_6 = caro[seq(from = 1, to = 200, by = 6),]
caro_9 = caro[seq(from = 1, to = 200, by = 9),]

nrow(caro)
nrow(caro_3)
nrow(caro_6)
nrow(caro_9)

#timelag, length, and speed time, as with previous dataset, got a great deal of help

#1st
caro$timelag  <- as.numeric(difftime(lead(caro$DatetimeUTC), caro$DatetimeUTC),units = "secs")
caro$steplength= euc.dist(caro$E, caro$N)
caro$Speed = caro$steplength/caro$timelag

#3rd
caro_3$timelag  <- as.numeric(difftime(lead(caro_3$DatetimeUTC), caro_3$DatetimeUTC),units = "secs")
caro_3$steplength= euc.dist(caro_3$E, caro_3$N)
caro_3$Speed = caro_3$steplength/caro_3$timelag

#6th
caro_6$timelag  <- as.numeric(difftime(lead(caro_6$DatetimeUTC), caro_6$DatetimeUTC),units = "secs")
caro_6$steplength= euc.dist(caro_6$E, caro_6$N)
caro_6$Speed = caro_6$steplength/caro_6$timelag

#9th
caro_9$timelag  <- as.numeric(difftime(lead(caro_9$DatetimeUTC), caro_9$DatetimeUTC),units = "secs")
caro_9$steplength= euc.dist(caro_9$E, caro_9$N)
caro_9$Speed = caro_9$steplength/caro_9$timelag

# creating a single data frame
caro_13=rbind(caro,caro_3)
caro_13$Trajectory=c(replicate(200, "1 minute"), replicate(67,"3 minutes"))

caro_16=rbind(caro,caro_6)
caro_16$Trajectory=c(replicate(200, "1 minute"), replicate(34,"6 minutes"))

caro_19=rbind(caro,caro_9)
caro_19$Trajectory=c(replicate(200, "1 minute"), replicate(23,"9 minutes"))

caro_all=rbind(caro, caro_3 , caro_6, caro_9)
caro_all$Trajectory=c(replicate(200, "1 minute"), replicate(67,"3 minutes"), replicate(34,"6 minutes"),replicate(23,"9 minutes")  )

# comparisons
#ploting 3rd compared to base caro
ggplot(caro_13, aes(y=N, x=E, col=Trajectory) ) +
  geom_path()+ geom_point()

#ploting 6th compared to base caro
ggplot(caro_16, aes(y=N, x=E, col=Trajectory) ) +
  geom_path()+ geom_point()

#ploting 9th compared to caro
ggplot(caro_19, aes(y=N, x=E, col=Trajectory) ) +
  geom_path()+ geom_point()

#ploting all caro datasets with varying granularities
ggplot(caro_all, aes(y=N, x=E, col=Trajectory) ) +
  geom_path()+ geom_point()

#comparing derived speed at various sampling intervals

caro_all$Time <- format(as.POSIXlt(caro_all$DatetimeUTC, "%Y-%m-%d %H:%M:%S", tz = "UTC"), format = "%H:%M")
str(caro)

ggplot(caro_all, aes(Time,Speed, col = Trajectory, group = 1)) + geom_line()



ggplot(caro_all, aes(Time,Speed, col = Trajectory, group = 1)) + geom_line() +
  scale_x_datetime(breaks = date_breaks("1 hour"), labels=date_format("%H:%M:%S"))+
  theme_minimal() + ggtitle("Compare derived speed to various sampling intervals")

#Bashing head against desk


```


# Task 5: Deriving movement parameters II: Rolling window functions

```{r}
example <- rnorm(10)
rollmean(example,k = 3,fill = NA,align = "left")
##  [1]  0.93634335  0.31709038  0.02370048  0.67869801  0.73369105  0.50401344
##  [7] -0.56144365 -0.56902598          NA          NA
rollmean(example,k = 4,fill = NA,align = "left")
##  [1]  0.6775521  0.2045005  0.5848215  0.5255629  0.3446928  0.1459635
##  [7] -0.4102301         NA         NA         NA


# Now to apply to caro
caro_long <- gather(caro, key = k_value , value = Speed, Speed:Speedk5, factor_key = TRUE) 

caro_long$Time <- format(as.POSIXlt(caro_long$DatetimeUTC, "%Y-%m-%d %H:%M:%S", tz = "UTC"), format = "%H:%M")

# processing
caro_long$k_value=as.factor(caro_long$k_value)
caro_long$Time = strptime(caro_long$Time, format = "%H:%M" )
caro_long$Time = as.POSIXct(caro_long$Time, format = "%H:%M")

# attempting to visualize
ggplot(caro_long, aes(Time, Speed,col=k_value, group = 1)) + geom_line()+
  scale_x_datetime(breaks = date_breaks("1 hour"), labels=date_format("%H:%M:%S"))+
  theme_minimal() + ggtitle("Comparing speed @ varying window sizes")


#test

```
