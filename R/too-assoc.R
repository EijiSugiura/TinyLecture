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
# Count up "Too"
too <- x %>% group_by(Hostname,Description) %>% filter(Description %like% "Too") %>% summarize(n=n())
too <- too %>% group_by(Hostname) %>% summarize(Too=sum(n))

## Load vsrlist
setwd("~/IDS2016/trend")
vsrlist <- fread("vsrlist.csv")
vsrlist <- vsrlist %>% select(-NAPT) %>% select(-Model) %>% select(-OS)

## vsrtoo = vsrlist + too
vsrtoo <- as.data.table(full_join(vsrlist,too,by="Hostname"))
# cleaning up
vsrtoo <- vsrtoo %>% filter(!is.na(Hostname))
vsrtoo <- vsrtoo %>% filter(!is.na(BGP))

# convert to transactions
vsrtoo$Too <- as.numeric(ifelse(is.na(vsrtoo$Too),0,1))
vsrtoo.trans <- as(as.matrix(vsrtoo[,4:ncol(vsrtoo)]),"transactions")

vsrtoo.rules <- apriori(vsrtoo.trans,parameter=list(support=0.01,maxlen=5))
inspect(head(sort(vsrtoo.rules,by="lift"),10))
itemFrequencyPlot(vsrtoo.trans, topN=20, type="absolute")
plot(vsrtoo.rules,method="graph",interactive = F)
plot(head(sort(vsrtoo.rules,by="lift"),10),method="graph",interactive = F)

too.rules <- subset(vsrtoo.rules,subset = rhs %in% "Too")
inspect(too.rules)
plot(too.rules,method="graph",interactive = F)
