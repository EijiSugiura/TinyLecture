library("data.table")
library("dplyr")
library("ggplot2")
library("stringr")
library("arules")
library("arulesViz")
library("kernlab")
library("neuralnet")

setwd("~/IDS2016/trigger")

#trouble <- fread("troubles.csv")
#trouble$Time <- as.POSIXct(trouble$Time,format="%Y/%m/%d %H:%M")
#trouble$Type <- as.factor(trouble$Type)
#trouble$Trigger <- as.factor(trouble$Trigger)
#trouble$Replace <- as.factor(trouble$Replace)
#trouble$BootCount <- as.numeric(trouble$BootCount)

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

# Count up "Failover" + "Reboot", Failover includes Reboot
reboot <- x %>% group_by(Hostname,Description) %>% filter(Description %like% "Failover") %>% summarize(n=n())
# Count up "Reboot" only
# reboot <- x %>% group_by(Hostname,Description) %>% filter(Description %like% "Reboot") %>% summarize(n=n())
reboot <- reboot %>% group_by(Hostname) %>% summarize(Reboot=sum(n))

setwd("~/IDS2016/trend")
vsrlist <- fread("vsrlist.csv")
vsrlist <- vsrlist[,NAPT:=NULL]
vsrlist$Model <- as.factor(vsrlist$Model)
vsrlist$OS <- as.factor(vsrlist$OS)

vsrreboot <- as.data.table(full_join(vsrlist,reboot,by="Hostname"))
# cleaning up
vsrreboot <- vsrreboot %>% filter(!is.na(Hostname))
vsrreboot <- vsrreboot %>% filter(!is.na(BGP))
vsrreboot$Reboot <- as.numeric(ifelse(is.na(vsrreboot$Reboot),0,1))

vsrreboot.trans <- as(as.matrix(vsrreboot[,4:26]),"transactions")
vsrreboot.rules <- apriori(vsrreboot.trans,parameter=list(support=0.01,maxlen=5))
inspect(head(sort(vsrreboot.rules,by="lift"),10))
itemFrequencyPlot(vsrreboot.trans, topN=20, type="absolute")
plot(vsrreboot.rules,method="graph",interactive = F)
plot(head(sort(vsrreboot.rules,by="lift"),10),method="graph",interactive = F)

reboot.rules <- subset(vsrreboot.rules,subset = rhs %in% "Reboot")
inspect(reboot.rules)

## SVM
vsrreboot <- as.data.table(full_join(vsrlist,reboot,by="Hostname"))
# cleaning up
vsrreboot <- vsrreboot %>% filter(!is.na(Hostname))
vsrreboot <- vsrreboot %>% filter(!is.na(BGP))
# Fix Reboot counter
vsrreboot$Reboot <- ifelse(is.na(vsrreboot$Reboot),0,vsrreboot$Reboot)

v <- vsrreboot[,2:26]
index <- sample(nrow(v),300)
v.test <- v[index,]
v.train <- v[-index,]
#v.classifier <- ksvm(Reboot ~ ., data = v.train, kernel = "vanilladot")
v.classifier <- ksvm(Reboot ~ ., data = v.train,
                     scale = T,
                     kernel = "rbfdot", type="C-bsvc")
v.classifier

v.pred <- predict(v.classifier, v.test)
v.table <- table(v.pred, v.test$Reboot)
v.table
1-sum(diag(v.table))/sum(v.table)

## Neuralnet
#v.classifier <- neuralnet(Reboot ~ BGP+BR+NAT+PARP+VPN+HA+MH+LB+IDS+ADS+SSLVPN+PPTP+L2TP+VC+VCWEB+CF+UF+NY+L7+LBSSL+LANMON+AWSVPN,
#                          data=v.train,rep=10,stepmax=100000)
#plot(v.classifier)
##v.result <- compute(v.classifier,v.test)
##v.pred <- v.result$net.result
##cor(v.pred,v.sample$Model)

#x %>% group_by(Description) %>% filter(Description %like% "Reboot") %>% summarize(n=n())
#x %>% group_by(Description) %>% summarize(n=n()) %>% arrange(Description)

trend <- rbindlist( lapply(list.files(pattern = "*-20160401.csv"), fread), fill=T)
for(i in 0:10){
  for(s in c("bytes","packets")){
    for(d in c("in","out")){
      o <- sprintf("net.if.%s[eth%d_%s]",d,i,s)
      r <- sprintf("eth%d_%s_%s",i,d,s)
      names(trend)[names(trend) == o] <- r
      o <- sprintf("net.if.%s[eth%d_%s]_std",d,i,s)
      r <- sprintf("eth%d_%s_%s_std",i,d,s)
      names(trend)[names(trend) == o] <- r
      o <- sprintf("net.if.%s[eth%d_%s]_min",d,i,s)
      r <- sprintf("eth%d_%s_%s_min",i,d,s)
      names(trend)[names(trend) == o] <- r
      o <- sprintf("net.if.%s[eth%d_%s]_max",d,i,s)
      r <- sprintf("eth%d_%s_%s_max",i,d,s)
      names(trend)[names(trend) == o] <- r
    }
  }
}

trend <- trend %>% rename(power_CPU = `sensor.power[CPU core]`)
trend <- trend %>% rename(power_VCore = `sensor.power[VCore]`)
trend <- trend %>% rename(power_VCore1 = `sensor.power[VCore 1]`)
trend <- trend %>% rename(temp_CPU = `sensor.temp[CPU Temp]`)
trend <- trend %>% rename(temp_SYS = `sensor.temp[Sys Temp]`)
trend <- trend %>% rename(temp_AUX = `sensor.temp[AUX Temp]`)
trend <- trend %>% rename(temp_MB = `sensor.temp[M/B Temp]`)
trend <- trend %>% rename(temp_Core0 = `sensor.temp[Core 0]`)
trend <- trend %>% rename(temp_Core1 = `sensor.temp[Core 1]`)
trend <- trend %>% rename(temp_1 = `sensor.temp[temp1]`)
trend <- trend %>% rename(temp_2 = `sensor.temp[temp2]`)
trend <- trend %>% rename(temp_3 = `sensor.temp[temp3]`)
trend <- trend %>% rename(fan_AUX = `sensor.fan[Aux Fan]`)
trend <- trend %>% rename(fan_Case = `sensor.fan[Case Fan]`)
trend <- trend %>% rename(fan_CPU = `sensor.fan[CPU Fan]`)
trend <- trend %>% rename(fan_PS = `sensor.fan[P/S Fan]`)
trend <- trend %>% rename(fan_1 = `sensor.fan[fan1]`)
trend <- trend %>% rename(fan_2 = `sensor.fan[fan2]`)
trend <- trend %>% rename(fan_3 = `sensor.fan[fan3]`)
trend <- trend %>% rename(fan_4 = `sensor.fan[fan4]`)

trend <- trend %>% rename(power_CPU_std = `sensor.power[CPU core]_std`)
trend <- trend %>% rename(power_VCore_std = `sensor.power[VCore]_std`)
trend <- trend %>% rename(power_VCore1_std = `sensor.power[VCore 1]_std`)
trend <- trend %>% rename(temp_CPU_std = `sensor.temp[CPU Temp]_std`)
trend <- trend %>% rename(temp_SYS_std = `sensor.temp[Sys Temp]_std`)
trend <- trend %>% rename(temp_AUX_std = `sensor.temp[AUX Temp]_std`)
trend <- trend %>% rename(temp_MB_std = `sensor.temp[M/B Temp]_std`)
trend <- trend %>% rename(temp_Core0_std = `sensor.temp[Core 0]_std`)
trend <- trend %>% rename(temp_Core1_std = `sensor.temp[Core 1]_std`)
trend <- trend %>% rename(temp_1_std = `sensor.temp[temp1]_std`)
trend <- trend %>% rename(temp_2_std = `sensor.temp[temp2]_std`)
trend <- trend %>% rename(temp_3_std = `sensor.temp[temp3]_std`)
trend <- trend %>% rename(fan_AUX_std = `sensor.fan[Aux Fan]_std`)
trend <- trend %>% rename(fan_Case_std = `sensor.fan[Case Fan]_std`)
trend <- trend %>% rename(fan_CPU_std = `sensor.fan[CPU Fan]_std`)
trend <- trend %>% rename(fan_PS_std = `sensor.fan[P/S Fan]_std`)
trend <- trend %>% rename(fan_1_std = `sensor.fan[fan1]_std`)
trend <- trend %>% rename(fan_2_std = `sensor.fan[fan2]_std`)
trend <- trend %>% rename(fan_3_std = `sensor.fan[fan3]_std`)
trend <- trend %>% rename(fan_4_std = `sensor.fan[fan4]_std`)

trend <- trend %>% rename(power_CPU_min = `sensor.power[CPU core]_min`)
trend <- trend %>% rename(power_VCore_min = `sensor.power[VCore]_min`)
trend <- trend %>% rename(power_VCore1_min = `sensor.power[VCore 1]_min`)
trend <- trend %>% rename(temp_CPU_min = `sensor.temp[CPU Temp]_min`)
trend <- trend %>% rename(temp_SYS_min = `sensor.temp[Sys Temp]_min`)
trend <- trend %>% rename(temp_AUX_min = `sensor.temp[AUX Temp]_min`)
trend <- trend %>% rename(temp_MB_min = `sensor.temp[M/B Temp]_min`)
trend <- trend %>% rename(temp_Core0_min = `sensor.temp[Core 0]_min`)
trend <- trend %>% rename(temp_Core1_min = `sensor.temp[Core 1]_min`)
trend <- trend %>% rename(temp_1_min = `sensor.temp[temp1]_min`)
trend <- trend %>% rename(temp_2_min = `sensor.temp[temp2]_min`)
trend <- trend %>% rename(temp_3_min = `sensor.temp[temp3]_min`)
trend <- trend %>% rename(fan_AUX_min = `sensor.fan[Aux Fan]_min`)
trend <- trend %>% rename(fan_Case_min = `sensor.fan[Case Fan]_min`)
trend <- trend %>% rename(fan_CPU_min = `sensor.fan[CPU Fan]_min`)
trend <- trend %>% rename(fan_PS_min = `sensor.fan[P/S Fan]_min`)
trend <- trend %>% rename(fan_1_min = `sensor.fan[fan1]_min`)
trend <- trend %>% rename(fan_2_min = `sensor.fan[fan2]_min`)
trend <- trend %>% rename(fan_3_min = `sensor.fan[fan3]_min`)
trend <- trend %>% rename(fan_4_min = `sensor.fan[fan4]_min`)

trend <- trend %>% rename(power_CPU_max = `sensor.power[CPU core]_max`)
trend <- trend %>% rename(power_VCore_max = `sensor.power[VCore]_max`)
trend <- trend %>% rename(power_VCore1_max = `sensor.power[VCore 1]_max`)
trend <- trend %>% rename(temp_CPU_max = `sensor.temp[CPU Temp]_max`)
trend <- trend %>% rename(temp_SYS_max = `sensor.temp[Sys Temp]_max`)
trend <- trend %>% rename(temp_AUX_max = `sensor.temp[AUX Temp]_max`)
trend <- trend %>% rename(temp_MB_max = `sensor.temp[M/B Temp]_max`)
trend <- trend %>% rename(temp_Core0_max = `sensor.temp[Core 0]_max`)
trend <- trend %>% rename(temp_Core1_max = `sensor.temp[Core 1]_max`)
trend <- trend %>% rename(temp_1_max = `sensor.temp[temp1]_max`)
trend <- trend %>% rename(temp_2_max = `sensor.temp[temp2]_max`)
trend <- trend %>% rename(temp_3_max = `sensor.temp[temp3]_max`)
trend <- trend %>% rename(fan_AUX_max = `sensor.fan[Aux Fan]_max`)
trend <- trend %>% rename(fan_Case_max = `sensor.fan[Case Fan]_max`)
trend <- trend %>% rename(fan_CPU_max = `sensor.fan[CPU Fan]_max`)
trend <- trend %>% rename(fan_PS_max = `sensor.fan[P/S Fan]_max`)
trend <- trend %>% rename(fan_1_max = `sensor.fan[fan1]_max`)
trend <- trend %>% rename(fan_2_max = `sensor.fan[fan2]_max`)
trend <- trend %>% rename(fan_3_max = `sensor.fan[fan3]_max`)
trend <- trend %>% rename(fan_4_max = `sensor.fan[fan4]_max`)

## SVM
vsrreboot <- as.data.table(full_join(vsrlist,reboot,by="Hostname"))
# cleaning up
vsrreboot <- vsrreboot %>% filter(!is.na(Hostname))
vsrreboot <- vsrreboot %>% filter(!is.na(BGP))
# Fix Reboot counter
vsrreboot$Reboot <- ifelse(is.na(vsrreboot$Reboot),0,vsrreboot$Reboot)

z <- as.data.table(full_join(vsrreboot,trend,by="Hostname"))
z <- as.data.table(z %>% filter(!is.na(BGP)))
z <- z[,Model:=NULL]
z <- z[,OS:=NULL]
#z[is.na(z)] <- 0
z <- z %>% select(-(which(names(z) %like% "Date")))
z <- z %>% select(-(which(names(z) %like% "eth8")))
z <- z %>% select(-(which(names(z) %like% "eth9")))
z <- z %>% select(-(which(names(z) %like% "eth10")))

y <- as.data.table(z)
y[,Hostname:=NULL]
y[is.na(y)] <- 0
index <- sample(nrow(y),300)
y.test <- y[index,]
y.train <- y[-index,]
y.classifier <- ksvm(Reboot ~ ., data = y.train,
                     scale = T,
                     kernel = "rbfdot", type = "C-bsvc")
y.classifier
y.pred <- predict(y.classifier, y.test)
y.table <- table(y.pred, y.test$Reboot)
y.table
1-sum(diag(y.table))/sum(y.table)

z <- z %>% select(-(which(names(z) %like% "_min")))
z <- z %>% select(-(which(names(z) %like% "_max")))
z <- z %>% select(-(which(names(z) %like% "_std")))

y <- as.data.table(z)
y[,Hostname:=NULL]
y[is.na(y)] <- 0
index <- sample(nrow(y),300)
y.test <- y[index,]
y.train <- y[-index,]
y.classifier <- ksvm(Reboot ~ ., data = y.train,
                     scale = T,
                     kernel = "rbfdot", type = "C-bsvc")
y.classifier
y.pred <- predict(y.classifier, y.test)
y.table <- table(y.pred, y.test$Reboot)
y.table
1-sum(diag(y.table))/sum(y.table)

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


y <- as.data.table(z)
y[,Hostname:=NULL]
y[is.na(y)] <- 0
index <- sample(nrow(y),300)
y.test <- y[index,]
y.train <- y[-index,]
y.classifier <- ksvm(Reboot ~ ., data = y.train,
                     scale = T,
                     kernel = "rbfdot", type = "C-bsvc")
y.classifier
y.pred <- predict(y.classifier, y.test)
y.table <- table(y.pred, y.test$Reboot)
y.table
1-sum(diag(y.table))/sum(y.table)

#colNameMean <- function(x, string){
#    y <- x %>% select(Hostname,which(names(x) %like% string))
#    y[[string]] <- rowMeans(y[,2:ncol(y)],na.rm=T)
#    y <- y %>% select(Hostname, which(names(x) == string))
#    x <- x %>% select(-(which(names(x) %like% string)))
#    x <- as.data.table(full_join(x,y,by="Hostname"))
#    return(x)
#}
#string <- "packets_std"
#y <- colNameMean(z, string)
#string <- "bytes_std"
#y <- colNameMean(z, string)
