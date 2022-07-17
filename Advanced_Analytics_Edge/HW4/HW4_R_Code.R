library(tidyverse)
library(Metrics)
library(zoo)



his = read.csv("/Users/bennetthellman/Desktop/OneDrive - Massachusetts Institute of Technology/AE/HWs/HW4/Dartboard_historical-1.csv");
fut = read.csv("/Users/bennetthellman/Desktop/OneDrive - Massachusetts Institute of Technology/AE/HWs/HW4/Dartboard_future.csv.crdownload");
dc = read.csv("/Users/bennetthellman/Desktop/OneDrive - Massachusetts Institute of Technology/AE/HWs/HW4/Dartboard_dcs.csv");

#dc<-dc%>%mutate(County.Name = Location)

#mdf = merge(his, dc, by.x ="State.Name", by.y = "Location", all.x = TRUE)

his<-his%>%mutate(lsale = log(Sales/Population),saleper = Sales/Population, 
                  linc = log(Income), bseason = as.factor(Season))
train <- his%>%filter(Year <=2013)
test<-his%>%filter(Year ==2014)

mod <- lm(lsale~linc+Week_Num+bseason, data = train)
summary(mod)
summary(mod)$r.squared

pred <- predict(mod, newdata=test)
#OSR^2
1 - sum((pred - test$lsale)^2) / sum((mean(train$lsale) - test$lsale)^2)
#OMAE
mae(test$lsale, pred)
#ORMSE
rmse(test$lsale, pred)

fut<-fut%>%mutate(linc = log(Income), bseason = as.factor(Season))
fut$forecast <- exp(predict(mod, newdata=fut))*fut$Population

#b
inv_df <- fut %>% group_by(Week_Num) %>% summarize(tot_d= sum(forecast)) %>% 
  mutate(inv = (rollapply(data = tot_d, FUN=sum, width=8, align="left", fill=NA)/1000))
#Claire Sailard helped me with this function
current_cap<- dc%>%summarise(tot_pallets_cap = sum(5*Current_Size/(13.5)))

ggplot(main = "Inventory vs. Capacity Over Time")+geom_line(aes(x = Week_Num, y = inv), data = inv_df) + geom_hline(aes(yintercept=1222222, color = "red")) +
  geom_text(aes(200,1222222,label = "Current Capacity", vjust = -1, color = "red")) + geom_text(aes(230,2322222,label = "Inventory", vjust = -1))+
 labs(x ="Week #", y="Pallets", title="Inventory vs. Capacity Over Time")+ theme(legend.position = "none")+xlim(183,306)


#bi
percent_cap_short <- (inv_df[123,3]-current_cap)/(current_cap)*100
percent_cap_short

#exportation

fut$d_pallets = fut$forecast/1000
write.csv(fut,'/Users/bennetthellman/Desktop/OneDrive - Massachusetts Institute of Technology/AE/HWs/HW4/pred_d.csv')


