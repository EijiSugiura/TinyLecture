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
# Count up "Too"
too <- x %>% group_by(Hostname,Description) %>% filter(Description %like% "Too many processes run") %>% summarize(n=n())
too <- too %>% group_by(Hostname) %>% summarize(Too=sum(n))
# Count up "restarted"
restarted <- x %>% group_by(Hostname,Description) %>% filter(Description %like% "snmpd") %>% summarize(n=n())
restarted <- restarted %>% group_by(Hostname) %>% summarize(Restarted=sum(n))

setwd("~/IDS2016/trend")
## Load vsrlist
vsrlist <- fread("vsrlist.csv")
vsrlist <- vsrlist %>% select(-NAPT) %>% select(-Model) %>% select(-OS)
## vsrstatus = vsrlist + reboot + too
vsrstatus <- as.data.table(full_join(vsrlist,reboot,by="Hostname"))
vsrstatus <- as.data.table(full_join(vsrstatus,too,by="Hostname"))
vsrstatus <- as.data.table(full_join(vsrstatus,restarted,by="Hostname"))
# cleaning up
vsrstatus <- vsrstatus %>% filter(!is.na(Hostname))
vsrstatus <- vsrstatus %>% filter(!is.na(BGP))

## Load trend data
trend <- fread("trend-1201.csv")
## z = vsrstatus + trend
z <- as.data.table(full_join(vsrstatus,trend,by="Hostname"))
# cleaning up
z <- z %>% filter(!is.na(BGP))
# omit min/max/std
z <- z %>% select(-(which(names(z) %like% "_min")))
z <- z %>% select(-(which(names(z) %like% "_max")))

std <- z %>% select(Hostname,which(names(z) %like% "_std"))
z <- z %>% select(-(which(names(z) %like% "_std")))

temp_std <- std %>% select(Hostname,which(names(std) %like% "temp"))
temp_std$temp_std <- rowMeans(temp_std[,2:ncol(temp_std)],na.rm=T)
temp_std <- temp_std %>% select(Hostname,temp_std)

temp <- z %>% select(Hostname,which(names(z) %like% "temp"))
temp$temp <- rowMeans(temp[,2:ncol(temp)],na.rm=T)
temp <- temp %>% select(Hostname,temp)
z <- z %>% select(-(which(names(z) %like% "temp")))
z <- as.data.table(full_join(z,temp,by="Hostname"))
z <- as.data.table(full_join(z,temp_std,by="Hostname"))

fan_std <- std %>% select(Hostname,which(names(std) %like% "fan"))
fan_std$fan_std <- rowMeans(fan_std[,2:ncol(fan_std)],na.rm=T)
fan_std <- fan_std %>% select(Hostname,fan_std)

fan <- z %>% select(Hostname,which(names(z) %like% "fan"))
fan$fan <- rowMeans(fan[,2:ncol(fan)],na.rm=T)
fan <- fan %>% select(Hostname,fan)
z <- z %>% select(-(which(names(z) %like% "fan")))
z <- as.data.table(full_join(z,fan,by="Hostname"))
z <- as.data.table(full_join(z,fan_std,by="Hostname"))

power_std <- std %>% select(Hostname,which(names(std) %like% "power"))
power_std$power_std <- rowMeans(power_std[,2:ncol(power_std)],na.rm=T)
power_std <- power_std %>% select(Hostname,power_std)

power <- z %>% select(Hostname,which(names(z) %like% "power"))
power$power <- rowMeans(power[,2:ncol(power)],na.rm=T)
power <- power %>% select(Hostname,power)
z <- z %>% select(-(which(names(z) %like% "power")))
z <- as.data.table(full_join(z,power,by="Hostname"))
z <- as.data.table(full_join(z,power_std,by="Hostname"))

packets_std <- std %>% select(Hostname,which(names(std) %like% "packets"))
packets_std$packets_std <- rowMeans(packets_std[,2:ncol(packets_std)],na.rm=T)
packets_std <- packets_std %>% select(Hostname,packets_std)

packets <- z %>% select(Hostname,which(names(z) %like% "packets"))
packets$packets <- rowMeans(packets[,2:ncol(packets)],na.rm=T)
packets <- packets %>% select(Hostname,packets)
z <- z %>% select(-(which(names(z) %like% "packets")))
z <- as.data.table(full_join(z,packets,by="Hostname"))
z <- as.data.table(full_join(z,packets_std,by="Hostname"))

bytes_std <- std %>% select(Hostname,which(names(std) %like% "bytes"))
bytes_std$bytes_std <- rowMeans(bytes_std[,2:ncol(bytes_std)],na.rm=T)
bytes_std <- bytes_std %>% select(Hostname,bytes_std)

bytes <- z %>% select(Hostname,which(names(z) %like% "bytes"))
bytes$bytes <- rowMeans(bytes[,2:ncol(bytes)],na.rm=T)
bytes <- bytes %>% select(Hostname,bytes)
z <- z %>% select(-(which(names(z) %like% "bytes")))
z <- as.data.table(full_join(z,bytes,by="Hostname"))
z <- as.data.table(full_join(z,bytes_std,by="Hostname"))

memory <- z %>% select(Hostname,which(names(z) %like% "vm.memory"))
memory <- memory %>% mutate(memfree = ifelse(is.na(`vm.memory.size[free]`),NA,`vm.memory.size[free]`/`vm.memory.size[total]`))
memory <- memory %>% select(Hostname,memfree)
z <- z %>% select(-(which(names(z) %like% "vm.memory")))
z <- as.data.table(full_join(z,memory,by="Hostname"))

# convert to transactions
# Reboot counter
z$Reboot <- as.numeric(ifelse(is.na(z$Reboot),0,1))
# Too counter
z$Too <- as.numeric(ifelse(is.na(z$Too),0,1))
# Restarted counter
z$Restarted <- as.numeric(ifelse(is.na(z$Restarted),0,1))
# vsr.ip.conntracks
z <- z %>% filter(!is.na(vsr.ip.conntracks))
z$vsr.ip.conntracks <- as.numeric(ifelse(z$vsr.ip.conntracks > 10000,1,0))
# cpu.load.avg15
z <- z %>% filter(!is.na(cpu.load.avg15))
z$cpu.load.avg15 <- as.numeric(ifelse(z$cpu.load.avg15 > 1.0,1,0))
z <- z %>% select(-cpu.load.avg1) %>% select(-cpu.load.avg5)
# cpu.idle
z <- z %>% filter(!is.na(cpu.idle))
z$cpu.idle <- as.numeric(ifelse(z$cpu.idle < 0.20,1,0))
z <- z %>% select(-cpu.iowait) %>% select(-cpu.nice) %>% select(-cpu.user) %>% select(-cpu.system) %>% select(-cpu.softirq)
# temp
z <- z %>% filter(!is.na(temp))
z$temp <- as.numeric(ifelse(z$temp > 50.0,1,0))
z <- z %>% filter(!is.na(temp_std))
z$temp_std <- as.numeric(ifelse(z$temp_std > 2.0,1,0))
# power
z <- z %>% filter(!is.na(power))
z$power <- as.numeric(ifelse(z$power > 1.8,1,0))
z <- z %>% filter(!is.na(power_std))
z$power_std <- as.numeric(ifelse(z$power_std > 0.0,1,0))
# packets
z <- z %>% filter(!is.na(packets))
z$packets <- as.numeric(ifelse(z$packets > 1000,1,0))
z <- z %>% filter(!is.na(packets_std))
z$packets_std <- as.numeric(ifelse(z$packets_std > 1000,1,0))
# bytes
z <- z %>% filter(!is.na(bytes))
z$bytes <- as.numeric(ifelse(z$bytes > 3000000,1,0))
z <- z %>% filter(!is.na(bytes_std))
z$bytes_std <- as.numeric(ifelse(z$bytes_std > 3000000,1,0))
# memfree
z <- z %>% filter(!is.na(memfree))
z$memfree <- as.numeric(ifelse(z$memfree < 0.30,1,0))

z[is.na(z)] <- 0
z.trans <- as(as.matrix(z[,4:ncol(z)]),"transactions")

z.rules <- apriori(z.trans,parameter=list(support=0.01,maxlen=100))
inspect(head(sort(z.rules,by="lift"),10))
itemFrequencyPlot(z.trans, topN=20, type="absolute")
plot(z.rules,method="graph",interactive = F)
plot(head(sort(z.rules,by="lift"),10),method="graph",interactive = F)

reboot.rules <- subset(z.rules,subset = rhs %in% "Reboot")
inspect(reboot.rules)
inspect(sort(reboot.rules,by="lift"))
plot(reboot.rules,method="graph",interactive = F)
plot(head(sort(reboot.rules,by="lift"),10),method="graph",interactive = F)

restarted.rules <- subset(z.rules,subset = rhs %in% "Restarted")
inspect(restarted.rules)
plot(restarted.rules,method="graph",interactive = F)
plot(head(sort(restarted.rules,by="lift"),10),method="graph",interactive = F)

too.rules <- subset(z.rules,subset = lhs %in% "Too")
inspect(too.rules)
