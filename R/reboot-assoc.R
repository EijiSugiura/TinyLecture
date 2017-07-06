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

## Load vsrlist
setwd("~/IDS2016/trend")
vsrlist <- fread("vsrlist.csv")
vsrlist <- vsrlist %>% select(-NAPT) %>% select(-Model) %>% select(-OS)

## vsrreboot = vsrlist + reboot
vsrreboot <- as.data.table(full_join(vsrlist,reboot,by="Hostname"))
# cleaning up
vsrreboot <- vsrreboot %>% filter(!is.na(Hostname))
vsrreboot <- vsrreboot %>% filter(!is.na(BGP))

# convert to transactions
vsrreboot$Reboot <- as.numeric(ifelse(is.na(vsrreboot$Reboot),0,1))
vsrreboot.trans <- as(as.matrix(vsrreboot[,4:ncol(vsrreboot)]),"transactions")

vsrreboot.rules <- apriori(vsrreboot.trans,parameter=list(support=0.01,maxlen=5))
inspect(head(sort(vsrreboot.rules,by="lift"),10))
itemFrequencyPlot(vsrreboot.trans, topN=20, type="absolute")
plot(vsrreboot.rules,method="graph",interactive = F)
plot(head(sort(vsrreboot.rules,by="lift"),10),method="graph",interactive = F)

reboot.rules <- subset(vsrreboot.rules,subset = rhs %in% "Reboot")
inspect(reboot.rules)
plot(reboot.rules,method="graph",interactive = F)
