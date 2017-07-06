library("data.table")
library("dplyr")
library("ggplot2")
library("stringr")

setwd("~/IDS2016/trigger")
x <- rbindlist( lapply(list.files(pattern = "trigger-2016[0-1][0-9].csv"), fread))

setnames(x,c("Date","Hostname","Description"))
x$Date <- as.Date(x$Date,format="%Y-%m-%d %H:%M:%S")
x$Description <- str_replace(x$Description, " on .* is", " is")
x$Description <- str_replace(x$Description, " on .*$", "")
x$Description <- str_replace(x$Description, " .* has ", " has ")
x$Description <- str_replace(x$Description, ".* has just been restarted", "restarted")
x$Description <- str_replace(x$Description, "available memory.*$", "available memory")
x$Description <- str_replace(x$Description, "VPN UP .*", "VPN UP")
x$Description <- str_replace(x$Description, "VPN DOWN .*", "VPN DOWN")
x$Description <- str_replace(x$Description, "VPN tun[0-9]* ", "VPN ")
x$Description <- str_replace(x$Description, "VPN tap[0-9]* ", "VPN ")
x$Description <- str_replace(x$Description, "Too high temp.*", "Too high temp")
x$Description <- str_replace(x$Description, "Too low fan speed.*", "Too low fan speed")

t <- fread("troubles.csv")
t$Time <- as.POSIXct(t$Time,format="%Y/%m/%d %H:%M")
t$Type <- as.factor(t$Type)
t$Trigger <- as.factor(t$Trigger)
t$Replace <- as.factor(t$Replace)
t$BootCount <- as.numeric(t$BootCount)

# Merge x and t by Hostname
z <- as.data.table(full_join(x,t,by="Hostname"))
# Add "Trouble" column
z <- z %>% mutate(Trouble = ifelse(is.na(SID),FALSE,TRUE))

# Trigger per host
hostname <- as.data.table(z %>% group_by(Hostname,Trouble) %>% summarize(n=n()))

h <- ggplot(hostname %>% filter(Trouble == TRUE), aes(x=n))
h <- h + geom_histogram(position = "identity")
plot(h)

h <- ggplot(hostname %>% filter(Trouble == FALSE), aes(x=n))
h <- h + geom_histogram(position = "identity")
plot(h)

h <- ggplot(hostname, aes(x=n,fill=Trouble))
h <- h + geom_histogram(alpha = 0.5, position = "identity")
plot(h)

h <- ggplot(hostname, aes(x=n,y=..density..,fill=Trouble))
h <- h + geom_histogram(alpha = 0.5, position = "identity")
plot(h)

# Add "n" column
z <- as.data.table(full_join(z,hostname,by="Hostname"))
z[, "Trouble.y":=NULL]
z <- z %>% rename(Trouble = Trouble.x)

# per Description.x histogram
description <- as.data.table(z %>% filter(n>20) %>% group_by(Description.x,Trouble) %>% summarize(N=n()))

g <- ggplot(description %>% filter(Trouble == TRUE) %>% head(20), aes(x=reorder(Description.x,N),y=N))
g <- g + geom_bar(stat = "identity")
g <- g + coord_flip()
plot(g)

g <- ggplot(description %>% filter(Trouble == FALSE) %>% head(20), aes(x=reorder(Description.x,N),y=N))
g <- g + geom_bar(stat = "identity")
g <- g + coord_flip()
plot(g)
