library("data.table")
library("dplyr")
library("ggplot2")
library("stringr")

setwd("~/IDS2016/trigger")
x <- rbindlist( lapply(list.files(pattern = "trigger-2016[0-1][0-9].csv"), fread))

setnames(x,c("Date","Hostname","Description"))
x$Date <- as.Date(x$Date,format="%Y-%m-%d %H:%M:%S")
class(x)
head(x,10)
summary(x$Hostname)

x$Description <- str_replace(x$Description, " on .* is", " is")
x$Description <- str_replace(x$Description, " on .*$", "")
x$Description <- str_replace(x$Description, " .* has ", " has ")
x$Description <- str_replace(x$Description, "available memory.*$", "available memory")
x$Description <- str_replace(x$Description, "VPN UP .*", "VPN UP")
x$Description <- str_replace(x$Description, "VPN DOWN .*", "VPN DOWN")
x$Description <- str_replace(x$Description, "VPN tun[0-9]* ", "VPN ")
x$Description <- str_replace(x$Description, "VPN tap[0-9]* ", "VPN ")
x$Description <- str_replace(x$Description, "Too high temp.*", "Too high temp")
x$Description <- str_replace(x$Description, "Too low fan speed.*", "Too low fan speed")

daily <- as.data.table(x %>% group_by(Date) %>% summarize(n = n()))
g <- ggplot ( daily, aes (x = Date, y = n))
g <- g + geom_line()
plot(g)

daily %>% filter(n>500)

daily_desc<- as.data.table(x %>% group_by(Date,Description) %>% summarize(n = n()))
g <- ggplot ( daily_desc, aes (x = Date, y = n, fill = Description))
g <- g + geom_bar(stat="identity")
plot(g)

daily_desc %>% filter(Date == "2016-11-09") %>% arrange(-n) %>% head
daily_desc %>% filter(Date == "2016-03-30") %>% arrange(-n) %>% head
daily_desc %>% filter(Date == "2016-08-16") %>% arrange(-n) %>% head

zabbix <- daily_desc %>% filter(Description %like% "Zabbix")
g <- ggplot ( zabbix, aes (x = Date, y = n))
g <- g + geom_bar(stat = "identity")
plot(g)

clock <- daily_desc %>% filter(Description %like% "Clock")
g <- ggplot ( clock, aes (x = Date, y = n))
g <- g + geom_bar(stat = "identity")
plot(g)

massive <- daily_desc %>% filter(n>50)
g <- ggplot ( massive, aes (x = Date, y = n, fill = Description))
g <- g + geom_bar(stat = "identity")
plot(g)
massive

date.table <- table(x$Date)
date.dt <- as.data.table(date.table)
setnames(date.dt,c("Date","Counter"))
library("xts")
date.xts <- xts(date.dt$Counter,as.POSIXct(date.dt$Date,format="%Y-%m-%d"))
plot(date.xts)


#desc.dt <- as.data.table(table(x$Description))
#setnames(desc.dt,c("Desc","Counter"))
#setorder(desc.dt,Counter)

y <- x %>% group_by(Date,Description) %>% summarize(n = n())
y.dt <- as.data.table(y)
y.dt <- y.dt %>% filter(n > 10)
g <- ggplot ( y.dt, aes (x = Date, y= n, fill=Description))
g <- g + geom_bar(stat="identity")
plot(g)

#z <- x %>% group_by(Date,Hostname) %>% summarize(n = n())
#head(z)
#class(z)
#z.dt <- as.data.table(z)
#head(z.dt)
#class(z.dt)
#g <- ggplot ( z.dt, aes (x = Date, y= n))
#g <- g + geom_bar(stat="identity")
#plot(g)

t <- fread("troubles.csv")
t$Time <- as.POSIXct(t$Time,format="%Y/%m/%d %H:%M")
t$Type <- as.factor(t$Type)
t$Trigger <- as.factor(t$Trigger)
t$Replace <- as.factor(t$Replace)
t$BootCount <- as.numeric(t$BootCount)

z <- full_join(x,t,by="Hostname")
trouble <- z %>% filter(!is.na(SID))
notrouble <- z %>% filter(is.na(SID))
trouble.dt <- as.data.table(trouble %>% group_by(Hostname) %>% summarize(n=n()))
summary(trouble.dt)
notrouble.dt <- as.data.table(notrouble %>% group_by(Hostname) %>% summarize(n=n()))
summary(notrouble.dt)

h <- ggplot(notrouble.dt, aes(x=n))
h <- h + geom_histogram()
plot(h)

notrouble.type.dt <- as.data.table(notrouble %>% group_by(Description.x) %>% summarize(n=n()))
setorder(notrouble.type.dt, -n)
g <- ggplot(notrouble.type.dt, aes(x= reorder(Description.x,-n),y=n))
g <- g + geom_bar(stat="Identity")
g <- g + coord_flip()
plot(g)

trouble.type.dt <- as.data.table(trouble %>% group_by(Description.x) %>% summarize(n=n()))
setorder(trouble.type.dt, -n)
g <- ggplot(trouble.type.dt, aes(x= reorder(Description.x,-n),y=n))
g <- g + geom_bar(stat="Identity")
g <- g + coord_flip()
#g <- g + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plot(g)

ntrs <-  nrow(notrouble.dt)
trs <-  nrow(trouble.dt)

notrouble.dt <- notrouble.dt %>% mutate(Trouble=FALSE) %>% mutate(N=n/ntrs)
trouble.dt <- trouble.dt %>% mutate(Trouble=TRUE) %>% mutate(N=n/trs)
total <- rbind(trouble.dt, notrouble.dt)

#h <- ggplot(total, aes(x=N,fill=Trouble))
h <- ggplot(total, aes(x=n,y=..density..,fill=Trouble))
h <- h + geom_histogram(alpha = 0.5, position = "identity")
plot(h)

# without Low free disk space
z2 <- z %>% filter(!(Description.x %like% "disk"))
z2 <- z2 %>% mutate(Trouble=ifelse(is.na(SID),FALSE,TRUE))
z2.dt <- as.data.table(z2)
z2.dt[, SID:=NULL]
total2 <- as.data.table(z2 %>% group_by(Hostname,Trouble) %>% summarize(n=n()))
z2.dt <- inner_join(z2.dt,total2,by="Hostname")
z2.dt %>% filter(n>15) %>% filter(Trouble==FALSE) %>% group_by(Description.x) %>% summarize(n=n()) %>% arrange(-n)
z2.dt %>% filter(n>15) %>% filter(Trouble==FALSE) %>% group_by(Hostname,Description.x) %>% summarize(n=n()) %>% arrange(-n)

# and without VPN...
z3 <- z2 %>% filter(!(Description.x %like% "VPN"))
z3 <- z3 %>% mutate(Trouble=ifelse(is.na(SID),FALSE,TRUE))
z3.dt <- as.data.table(z3)
z3.dt[, SID:=NULL]
total3 <- as.data.table(z3 %>% group_by(Hostname,Trouble) %>% summarize(n=n()))
z3.dt <- inner_join(z3.dt,total3,by="Hostname")
z3.dt %>% filter(n>15) %>% filter(Trouble==FALSE) %>% group_by(Description.x) %>% summarize(n=n()) %>% arrange(-n)
z3.dt %>% filter(n>15) %>% filter(Trouble==FALSE) %>% group_by(Hostname,Description.x) %>% summarize(n=n()) %>% arrange(-n)
h <- ggplot(total3, aes(x=n,y=..density..,fill=Trouble))
h <- h + geom_histogram(alpha = 0.5, position = "identity")
plot(h)
