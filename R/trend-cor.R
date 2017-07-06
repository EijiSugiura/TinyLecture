library("data.table")
library("dplyr")
library("ggplot2")
library("stringr")
library("arules")
library("arulesViz")
library("kernlab")
library("neuralnet")

## Load trigger
setwd("~/IDS2016/trigger")
x <- fread("trigger.csv")
# Count up "Failover" + "Reboot", Failover includes Reboot
reboot <- x %>% group_by(Hostname,Description) %>% filter(Description %like% "Failover") %>% summarize(n=n())
reboot <- reboot %>% group_by(Hostname) %>% summarize(Reboot=sum(n))
# Too many processes running 
toomany <- x %>% group_by(Hostname,Description) %>% filter(Description %like% "Too many conn") %>% summarize(n=n())
toomany <- toomany %>% group_by(Hostname) %>% summarize(TooMany=sum(n))
# snmpd 
snmpd <- x %>% group_by(Hostname,Description) %>% filter(Description %like% "snmpd") %>% summarize(n=n())
snmpd <- snmpd %>% group_by(Hostname) %>% summarize(Snmpd=sum(n))

setwd("~/IDS2016/trend")
## Load vsrlist
vsrlist <- fread("vsrlist.csv")
vsrlist <- vsrlist %>% select(-NAPT) %>% select(-Model) %>% select(-OS)
## vsrreboot = vsrlist + reboot
vsrreboot <- as.data.table(full_join(vsrlist,reboot,by="Hostname"))
vsrreboot <- as.data.table(full_join(vsrreboot,toomany,by="Hostname"))
vsrreboot <- as.data.table(full_join(vsrreboot,snmpd,by="Hostname"))
# cleaning up
vsrreboot <- vsrreboot %>% filter(!is.na(Hostname))
vsrreboot <- vsrreboot %>% filter(!is.na(BGP))
# Fix Reboot counter
vsrreboot$Reboot <- ifelse(is.na(vsrreboot$Reboot),0,vsrreboot$Reboot)
vsrreboot$TooMany <- ifelse(is.na(vsrreboot$TooMany),0,vsrreboot$TooMany)
vsrreboot$Snmpd <- ifelse(is.na(vsrreboot$Snmpd),0,vsrreboot$Snmpd)

## Load trend
trend <- fread("trend-1201.csv")
## z = vsrreboot + trend
z <- as.data.table(full_join(vsrreboot,trend,by="Hostname"))
z <- z %>% filter(!is.na(BGP))
# omit min/max/std
z <- z %>% select(-(which(names(z) %like% "_min")))
z <- z %>% select(-(which(names(z) %like% "_max")))
z <- z %>% select(-(which(names(z) %like% "_std")))

temp <- z %>% select(Hostname,which(names(z) %like% "temp"))
temp$temp <- rowMeans(temp[,2:ncol(temp)],na.rm=T)
temp <- temp %>% select(Hostname,temp)
z <- z %>% select(-(which(names(z) %like% "temp")))
z <- as.data.table(full_join(z,temp,by="Hostname"))

fan <- z %>% select(Hostname,which(names(z) %like% "fan"))
fan$fan <- rowMeans(fan[,2:ncol(fan)],na.rm=T)
fan <- fan %>% select(Hostname,fan)
z <- z %>% select(-(which(names(z) %like% "fan")))
z <- as.data.table(full_join(z,fan,by="Hostname"))

power <- z %>% select(Hostname,which(names(z) %like% "power"))
power$power <- rowMeans(power[,2:ncol(power)],na.rm=T)
power <- power %>% select(Hostname,power)
z <- z %>% select(-(which(names(z) %like% "power")))
z <- as.data.table(full_join(z,power,by="Hostname"))

packets <- z %>% select(Hostname,which(names(z) %like% "packets"))
packets$packets <- rowMeans(packets[,2:ncol(packets)],na.rm=T)
packets <- packets %>% select(Hostname,packets)
z <- z %>% select(-(which(names(z) %like% "packets")))
z <- as.data.table(full_join(z,packets,by="Hostname"))

bytes <- z %>% select(Hostname,which(names(z) %like% "bytes"))
bytes$bytes <- rowMeans(bytes[,2:ncol(bytes)],na.rm=T)
bytes <- bytes %>% select(Hostname,bytes)
z <- z %>% select(-(which(names(z) %like% "bytes")))
z <- as.data.table(full_join(z,bytes,by="Hostname"))

memory <- z %>% select(Hostname,which(names(z) %like% "vm.memory"))
memory <- memory %>% mutate(memfree = ifelse(is.na(`vm.memory.size[free]`),NA,`vm.memory.size[free]`/`vm.memory.size[total]`))
memory <- memory %>% select(Hostname,memfree)
z <- z %>% select(-(which(names(z) %like% "vm.memory")))
z <- as.data.table(full_join(z,memory,by="Hostname"))

## Correlation
library("GGally")
ggpairs(z[,c("Reboot","temp","power","bytes")])
ggpairs(z[,c("Reboot","temp","power","TooMany")])
ggpairs(z[,c("Reboot","Snmpd","power","temp","TooMany")])
cor(z %>% select(-Hostname))
cor(z %>% filter(!is.na(vsr.ip.conntracks)) %>% filter(!is.na(temp)) %>% filter(!is.na(bytes)) %>% select(-Hostname))[,"Reboot"]

