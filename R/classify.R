library("data.table")
library("dplyr")
library("ggplot2")
library("stringr")
library("kernlab")

setwd("~/IDS2016/trend")
vsrlist <- fread("vsrlist.csv")

v <- vsrlist %>% select(Model,HA,MH,LB,IDS,ADS,VC,UF,L7)
index <- sample(nrow(v),300)
v.test <- v[index,]
v.train <- v[-index,]
v.classifier <- ksvm(Model ~ ., data = v.train, kernel = "vanilladot")
v.pred <- predict(v.classifier, v.test)
table(v.pred, v.test$Model)

library("neuralnet")
v.classifier <- neuralnet(Model ~ HA+MH+LB+IDS+ADS+VC+UF+L7,
                          data=v.train)
plot(v.classifier)
#v.result <- compute(v.classifier,v.test)
#v.pred <- v.result$net.result
#cor(v.pred,v.sample$Model)

v.classifier <- neuralnet(Model ~ HA+MH+LB+IDS+ADS+VC+UF+L7,
                          data=v.train, hidden=3)
plot(v.classifier)
#v.sample <- v.test[1:10,c("HA","MH","LB","IDS","ADS","VC","UF","L7")]
#v.result <- compute(v.classifier,v.sample)
#v.pred <- v.result$net.result
#cor(v.pred,v.sample$Model)
