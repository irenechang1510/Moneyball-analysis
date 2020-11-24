library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggmap)
library(viridis)
library(plotly)

### Sneak peak into the dataset

mydata <- read.csv("911.csv")

dim(mydata)

str(mydata)

summary(mydata)


### Cleaning the dataset

mydata$zip <- factor(mydata$zip)

mydata <- mydata[,-9]

mydata$timeStamp <- as.POSIXct(mydata$timeStamp)

mydata$Date <- as.Date(mydata$timeStamp)

mydata <- separate(mydata, col = title, into = c("Type", "SubType"), sep = ":")

mydata$SubType <- gsub(" -", "", mydata$SubType)


### Creating new variables

mydata$Year <- year(mydata$timeStamp)
mydata$Month <- month(mydata$timeStamp)
mydata$Day <- day(mydata$timeStamp)
mydata$Hour <- hour(mydata$timeStamp)
mydata$Weekday <- weekdays(mydata$timeStamp, TRUE)

mydata$Year <- factor(mydata$Year)
mydata$Month <- factor(mydata$Month)
mydata$Day <- factor(mydata$Day)
mydata$Hour <- factor(mydata$Hour)
mydata$Weekday <- factor(mydata$Weekday)

mydata <- mydata[,-7]

### Number of calls over the period of time and by type?

by_date <- mydata %>% group_by(Date) %>% summarise(Total = n())

head(by_date)

ggplot(by_date, aes(Date, Total)) + geom_line(color = "blue", size = 1)

by_date_type <- mydata %>% group_by(Date, Type) %>% summarise(Total = n())

by_date_type$Type <- factor(by_date_type$Type)

ggplot(by_date_type, aes(Date, Total)) + geom_line( aes(color = Type), size = 0.6)

ggplot(by_date_type, aes(Date, Total)) + geom_line( aes(color = Type), size = 0.6) + facet_wrap(~Type) + theme(legend.position="none")

### How many calls hourly, monthly and yearly?

table(mydata$Year)

table(mydata[mydata$Year==2016,]$Month)

table(mydata[mydata$Year==2016,]$Month)

ggplot(mydata[mydata$Year==2016,], aes(Month, fill = Month)) + geom_bar() + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 - Monthly ")

ggplot(mydata, aes(Hour, fill = Hour)) + geom_bar() + theme(legend.position = "none") + ggtitle("911 Emergency Calls - Hourly")

ggplot(mydata, aes(Weekday, fill = Weekday)) + geom_bar() + theme(legend.position = "none") + ggtitle("911 Emergency Calls - Weekday")


### How many calls based on type?

table(mydata$Type)

ggplot(mydata, aes(Type, fill = Type)) + geom_bar() + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 - Type")

prop.table(table(mydata$Type))

ggplot(as.data.frame(prop.table(table(mydata$Type))), aes(Var1, Freq, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 - Type Percentage") + xlab("Type") + ylab("Percentage of Calls")


### How many calls from each subtypes in overall dataset and also subtypes within each type?

top_subtypes <- as.data.frame(table(mydata$SubType))
top_subtypes <- top_subtypes[order(-top_subtypes$Freq),]
top10_subtypes <- top_subtypes[1:10,]
top10_subtypes$Perc <- top10_subtypes$Freq/sum(top_subtypes$Freq) * 100
top10_subtypes

ggplot(top10_subtypes, aes(reorder(Var1, Freq), Freq, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls") + xlab("Subtype") + ylab("Number of 911 Calls")  + coord_flip()

ggplot(top10_subtypes, aes(reorder(Var1, Perc), Perc, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls") + xlab("Subtype") + ylab("Percentage of 911 Calls")  + coord_flip()

gettop10subtypes <- function(type) {
	mytype <- subset(mydata, Type == type)
	mytype$SubType <- factor(mytype$SubType)
	mytype_subtypes <- as.data.frame(table(mytype$SubType))
	mytype_subtypes <- mytype_subtypes[order(-mytype_subtypes$Freq),]
	top10_types_substype <- mytype_subtypes[1:10,]
	top10_types_substype$Perc <- top10_types_substype$Freq/sum(mytype_subtypes$Freq) * 100
	return(top10_types_substype)
}

gettop10subtypes("EMS")

ggplot(gettop10subtypes("EMS"), aes(reorder(Var1, Freq), Freq, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016") + xlab("Subtype") + ylab("Number of EMS Calls")  + coord_flip()

ggplot(gettop10subtypes("EMS"), aes(reorder(Var1, Perc), Perc, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016") + xlab("Subtype") + ylab("Percentage of EMS Calls")  + coord_flip()


gettop10subtypes("Fire")

ggplot(gettop10subtypes("Fire"), aes(reorder(Var1, Freq), Freq, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 ") + xlab("Subtype") + ylab("Number of Fire Calls")  + coord_flip()

ggplot(gettop10subtypes("Fire"), aes(reorder(Var1, Perc), Perc, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 ") + xlab("Subtype") + ylab("Percentage of Fire Calls")  + coord_flip()

gettop10subtypes("Traffic")[1:7,]

ggplot(gettop10subtypes("Traffic")[1:7,], aes(reorder(Var1, Freq), Freq, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016") + xlab("Subtype") + ylab("Number of Traffic Calls")  + coord_flip()

ggplot(gettop10subtypes("Traffic")[1:7,], aes(reorder(Var1, Perc), Perc, fill = Var1)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 ") + xlab("Subtype") + ylab("Percentage of Traffic Calls")  + coord_flip()


	### Which zip codes have the highest number of calls?

top_zip <- as.data.frame(table(mydata$zip))
top_zip <- top_zip[order(-top_zip$Freq),]
top10_zip <- top_zip[1:10,]

names(top10_zip) <- c("Zip", "Total")
top10_zip$Perc <- top10_zip$Total/sum(top_zip$Freq) * 100
top10_zip$Zip <- factor(top10_zip$Zip)

top10_zip

ggplot(top10_zip, aes(reorder(Zip, -Total), Total, fill = Zip)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 - Top 10 Zip ") + xlab("Zip codes with most number of calls")

ggplot(top10_zip, aes(reorder(Zip, -Perc), Perc, fill = Zip)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 - Top 10 Zip ") + xlab("Zip codes  Percentage of calls")

### Which township contributes to majority of the calls?

length(unique(mydata$twp))

top_twp <- as.data.frame(table(mydata$twp))
top_twp <- top_twp[order(-top_twp$Freq),]
top10_twp <- top_twp[1:10,]

names(top10_twp) <- c("Twp", "Total")
top10_twp$Perc <- top10_twp$Total/sum(top_twp$Freq) * 100
top10_twp$Twp <- factor(top10_twp$Twp)

top10_twp

ggplot(top10_twp, aes(reorder(Twp, -Total), Total, fill = Twp)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 - Top 10 Townships ") + xlab("Townships with most number of calls")  + theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5)) 

ggplot(top10_twp, aes(reorder(Twp, -Perc), Perc, fill = Twp)) + geom_bar(stat = "identity") + theme(legend.position = "none") + ggtitle("911 Emergency Calls for 2016 - Top 10 Townships ") + xlab("Townships Percentage of calls")  + theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5)) 

### HeatMaps

## Get the count of the calls by Month and Hour
day_hour <- mydata[mydata$Year == "2016", c("Day", "Hour")] %>% group_by(Day, Hour) %>% summarise(Count = n())
day_hour <- as.data.frame(day_hour)

## Change the type of the variables
day_hour$Day <- as.factor(day_hour$Day)
day_hour$Hour <- as.factor(day_hour$Hour)

## Building heatmap using ggplot2
ggplot(day_hour, aes(Day, Hour, fill = Count)) + geom_tile(color = "white", size = 0.1) + scale_fill_viridis(name="#Calls") + coord_equal() + labs(title="911 Calls by Day and Hour") 

## Get the count of the calls by Month and Day
month_day <- mydata[mydata$Year == "2016", c("Month", "Day")] %>% group_by(Month, Day) %>% summarise(Count = n())
month_day <- as.data.frame(month_day)

## Change the type of the variables
month_day$Month <- as.factor(month_day$Month)
month_day$Day <- as.factor(month_day$Day)

## Change the levels of the Month 
levels(month_day$Month) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

## Building heatmap using ggplot2
ggplot(month_day, aes(Day, Month, fill = Count)) + geom_tile(color = "white", size = 0.1) + scale_fill_viridis(name="#Calls") + coord_equal() + labs(title="911 Calls by Month and Day") 


### How does the type vary among top 10 township?

ggplot(mydata[mydata$twp %in% top10_twp$Twp, ], aes(twp, fill = Type )) + geom_bar(position = "dodge")  + theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))  + xlab("Township") + ggtitle("Type of calls among Top 10 Townships")

ggplot(mydata[mydata$twp %in% top10_twp$Twp, ], aes(twp, fill = Type)) + geom_bar()  + theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))  + xlab("Township") + ggtitle("Type of calls among Top 10 Townships") + facet_wrap(~Type) + theme(legend.position="none")


	### How does the Top 5 subtypes vary among top 10 twp?

top3 <- top10_subtypes[1:5,]$Var1

sample <- mydata[mydata$twp %in% top10_twp$Twp,]
sample <- sample[sample$SubType %in% top3, ]
dim(sample)

ggplot(sample, aes(twp, fill = SubType )) + geom_bar(position = "dodge")  + theme(axis.text.x=element_text(angle=45,hjust=0.5,vjust=0.5))  + xlab("Township") + ggtitle("Subtype distribution among Top 10 Townships")


### Vehicle accidents by hour, day and type

vehicle <- mydata[mydata$SubType ==" VEHICLE ACCIDENT", ]

table(vehicle$Type)

ggplot(vehicle, aes(Hour, fill = Type)) + geom_bar(position = "dodge") + ggtitle("Vehicle Accidents by Hours")

day_hour <- vehicle[vehicle$Year == "2016", c("Day", "Hour")] %>% group_by(Day, Hour) %>% dplyr::summarise(Count = n())
day_hour <- as.data.frame(day_hour)

day_hour$Day <- as.factor(day_hour$Day)
day_hour$Hour <- as.factor(day_hour$Hour)

ggplot(day_hour, aes(Day, Hour, fill = Count)) + geom_tile(color = "white", size = 0.1) + scale_fill_viridis(name="#Calls") + coord_equal() + labs(title="Vehicle Accident Calls by Day and Hour - 2016")


### CARDIAC EMERGENCY by hour and day

cardiac <- mydata[mydata$SubType ==" CARDIAC EMERGENCY", ]

day_hour <- cardiac[cardiac$Year == "2016", c("Day", "Hour")] %>% group_by(Day, Hour) %>% dplyr::summarise(Count = n())
day_hour <- as.data.frame(day_hour)

day_hour$Day <- as.factor(day_hour$Day)
day_hour$Hour <- as.factor(day_hour$Hour)

ggplot(day_hour, aes(Day, Hour, fill = Count)) + geom_tile(color = "white", size = 0.1) + scale_fill_viridis(name="#Calls") + coord_equal() + labs(title="CARDIAC EMERGENCY Calls by Day and Hour - 2016") 

