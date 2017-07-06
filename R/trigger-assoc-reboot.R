library("data.table")
library("dplyr")
library("ggplot2")
library("stringr")
library("arules")
library("arulesViz")

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

# Omit "Zabbix" triggers
z <- as.data.table(x %>% select(Hostname,Description) %>% filter(!(Description %like% "Zabbix")))

# Avoid duplicates
z <- unique(z)

# Convert to transactions
write.csv(z,"trans.csv",quote=FALSE,row.names=FALSE)
z.trans <- read.transactions("trans.csv",format="single",cols=c(1,2),sep=",")

LIST(z.trans[1:5])
itemInfo(z.trans)

# Exec!
z.rules <- apriori(z.trans,parameter=list(support=0.1,maxlen=20))

inspect(head(sort(z.rules,by="lift"),10))

itemFrequencyPlot(z.trans, topN=20, type="absolute")

plot(z.rules,method="graph",interactive = F)

