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

x <- fread("trigger.csv")
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
v.classifier <- ksvm(Reboot ~ ., data = v.train,
                     scale = T, C = 5,
                     kernel = "rbfdot", type="C-bsvc")
v.classifier

v.pred <- predict(v.classifier, v.test)
v.table <- table(v.pred, v.test$Reboot)
v.table
# error rate
1-sum(diag(v.table))/sum(v.table)

## Neuralnet
v.classifier <- neuralnet(Reboot ~ BGP+BR+NAT+PARP+VPN+HA+MH+LB+IDS+ADS+SSLVPN+PPTP+L2TP+VC+VCWEB+CF+UF+NY+L7+LBSSL+LANMON+AWSVPN,
                          data=v.train,stepmax=100000)
plot(v.classifier)
v.classifier
v.result <- compute(v.classifier,v.test)
v.pred <- v.result$net.result
cor(v.pred,v.test$Reboot)
