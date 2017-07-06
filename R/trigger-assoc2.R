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

t[,Description:=NULL]
t <- t %>% rename(Description = Repair)
# Add "troubles" description
t <- t %>% select(Hostname,Description)
x <- x %>% select(Hostname,Description)
z <- rbind(x,t)

# Avoid duplicates
z <- unique(z)

# Convert to transactions
write.csv(z,"z.trans.csv",quote=FALSE,row.names=FALSE)
z.trans <- read.transactions("z.trans.csv",format="single",cols=c(1,2),sep=",")

LIST(z.trans[1:5])
itemInfo(z.trans)

# Exec!
z.rules <- apriori(z.trans,parameter=list(support=0.2,maxlen=20))

inspect(head(sort(z.rules,by="lift"),10))

itemFrequencyPlot(z.trans, topN=20, type="absolute")

plot(z.rules,method="graph",interactive = F)
inspect(subset(z.rules,subset=rhs %in% "single"))

#z.rules <- apriori(z.trans,parameter=list(support=0.09,maxlen=250),
#                   appearance = list(rhs = c("single","HA片系"),
#                                     default = "lhs"))
setwd("~/IDS2016/trend")
vsrlist <- fread("vsrlist.csv")
#trend <- rbindlist( lapply(list.files(pattern = "*-20160401.csv"), fread(fill=T)))
trend <- data.table()
for(f in list.files(pattern = "*-20160401.csv")){
  test <- fread(f);
  trend <- rbind(trend,test,fill=T)
}
