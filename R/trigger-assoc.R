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

t <- fread("troubles.csv")
t$Time <- as.POSIXct(t$Time,format="%Y/%m/%d %H:%M")
t$Type <- as.factor(t$Type)
t$Trigger <- as.factor(t$Trigger)
t$Replace <- as.factor(t$Replace)
t$BootCount <- as.numeric(t$BootCount)

# Add "Trouble" column
z <- as.data.table(full_join(x,t,by="Hostname"))
z <- z %>% mutate(Trouble = ifelse(is.na(SID),FALSE,TRUE))

# Omit "Zabbix" triggers
z.notrouble <- as.data.table(z %>% filter(Trouble == FALSE) %>% select(Hostname,Description.x) %>% filter(!(Description.x %like% "Zabbix")))
z.trouble <- as.data.table(z %>% filter(Trouble == TRUE) %>% select(Hostname,Description.x) %>% filter(!(Description.x %like% "Zabbix")))

# Avoid duplicates
z.notrouble <- unique(z.notrouble)
z.trouble <- unique(z.trouble)

# Convert to transactions
write.csv(z.notrouble,"notrouble.trans.csv",quote=FALSE,row.names=FALSE)
write.csv(z.trouble,"trouble.trans.csv",quote=FALSE,row.names=FALSE)
notrouble.trans <- read.transactions("notrouble.trans.csv",format="single",cols=c(1,2),sep=",")
trouble.trans <- read.transactions("trouble.trans.csv",format="single",cols=c(1,2),sep=",")

LIST(notrouble.trans[1:5])
LIST(trouble.trans[1:5])
itemInfo(notrouble.trans)
itemInfo(trouble.trans)

# Exec!
notrouble.rules <- apriori(notrouble.trans,parameter=list(support=0.2,maxlen=20))
trouble.rules <- apriori(trouble.trans,parameter=list(support=0.1,maxlen=20))

inspect(head(sort(notrouble.rules,by="lift"),10))
inspect(head(sort(trouble.rules,by="lift"),10))

itemFrequencyPlot(notrouble.trans, topN=20, type="absolute")
itemFrequencyPlot(trouble.trans, topN=20, type="absolute")

plot(notrouble.rules,method="graph",interactive = F)
plot(trouble.rules,method="graph",interactive = F)
plot(head(sort(notrouble.rules,by="lift"),10),method="graph",interactive = F)
plot(head(sort(trouble.rules,by="lift"),10),method="graph",interactive = F)
