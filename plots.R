library(tidyverse)
library(oilabs)
library(gsheet)
library(GGally)
library(gridExtra)
titanic <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Cz3wnwugHApFXO0ogMLtBe5CzD_Uu1q8YBPpZPS2R3U/edit?usp=sharing')
titanic$passenger_id <- as.factor(titanic$passenger_id)
titanic$gender <- as.factor(titanic$gender)
titanic$age <- as.factor(titanic$age)
titanic$passenger_class <- as.factor(titanic$passenger_class)
titanic$embarked_from <- as.factor(titanic$embarked_from)
summary(titanic)

titanic %>% 
  select(-passenger_id) %>% 
  ggpairs(lower = list(continuous=wrap("points", position="jitter", size=0.5, alpha=0.8)))


p1<-ggplot(data=gender.1, aes(x=age,y=Freq))+
  geom_bar(stat = "identity")+
  facet_grid(~gender)+
  xlab("age")+
  ylab("percentage")

p2<-ggplot(data=gender.2, aes(x=passenger_class,y=Freq))+
  geom_bar(stat = "identity")+
  facet_grid(~gender)+
  xlab("passenger class")+
  ylab("percentage")

p3<-ggplot(data=gender.3, aes(x=number_of_siblings_and_spouses,y=Freq))+
  geom_bar(stat = "identity")+
  facet_grid(~gender)+
  xlab("number of siblings and spouses")+
  ylab("percentage")


p4<-ggplot(data=gender.4, aes(x=embarked_from,y=Freq))+
  geom_bar(stat = "identity")+
  facet_grid(~gender)+
  xlab("embarked from")+
  ylab("percentage")

gender.1<-as.data.frame(mosaic::tally(age~gender, data=titanic, format="percent"))
gender.2<-as.data.frame(mosaic::tally(passenger_class~gender, data=titanic, format="percent"))
gender.3<-as.data.frame(mosaic::tally(number_of_siblings_and_spouses ~gender, data=titanic, format="percent"))
gender.4<-as.data.frame(mosaic::tally(embarked_from~gender, data=titanic, format="percent"))

p1<-ggplot(gender.1, aes(x=gender, y=Freq, fill=age))+
  geom_bar(stat = "identity")+
  scale_fill_brewer("")+
  #ggtitle("Age")+
  xlab("")+
  ylab("Percentage")

p2<-ggplot(gender.2, aes(x=gender, y=Freq, fill=passenger_class))+
  geom_bar(stat = "identity")+
  scale_fill_brewer("")+
  #ggtitle("Passenger Class")+
  xlab("")+
  ylab("Percentage")

p3<-ggplot(gender.3, aes(x=gender, y=Freq, fill=number_of_siblings_and_spouses))+
  geom_bar(stat = "identity")+
  scale_fill_brewer("")+
  #ggtitle("Number of Siblings and Spouses")+
  xlab("")+
  ylab("Percentage")

p4<-ggplot(gender.4, aes(x=gender, y=Freq, fill=embarked_from))+
  geom_bar(stat = "identity")+
  scale_fill_brewer("", labels=c("Cherbourg", "Queenstown", "Southampton"))+
  #ggtitle("Embarked From")+
  xlab("")+
  ylab("Percentage")


grid.arrange(p1,p2,p3,p4)

pclass<-as.data.frame(mosaic::tally(passenger_class~embarked_from, data=titanic, format="percent"))

p5<-ggplot(pclass, aes(x=embarked_from, y=Freq, fill=passenger_class))+
  geom_bar(stat = "identity")+
  scale_fill_brewer("")+
  #ggtitle("Embarked From")+
  xlab("")+
  ylab("Percentage")+
  scale_x_discrete(labels=c("Cherbourg", "Queenstown", "Southampton"))

names<-c("Has survived", "Gender", "Age", "No. of Siblings & Spouses", "No. of Parents & Children", "Passenger Class", "Fare", "Embarked From")

titanic %>% 
  select(-passenger_id) %>% 
  ggpairs(lower = list(continuous=wrap("points", position="jitter", size=0.5, alpha=0.8)), columnLabels = names)
