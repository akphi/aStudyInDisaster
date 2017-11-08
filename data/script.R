library(dplyr)
titanic <- read_csv("... /aStudyInDisaster/data/titanic3.csv")
titanic <- titanic %>% select(-boat, -name, -home.dest, -cabin, -body, -ticket)
summary(titanic)
for (name in c("pclass", "survived", "sex", "sibsp", "parch", "fare", "embarked")) {
  titanic <- titanic[c(!is.na(titanic[name])),]
}
titanic$isFemale = as.integer(titanic$sex == "female")
titanic$sex = NULL
# C = Cherbourg, Q = Queenstown, S = Southampton
titanic$embarkedFromCherbourg <- as.integer(titanic$embarked == "C")
titanic$embarkedFromQueenstown <- as.integer(titanic$embarked == "Q")
titanic$embarkedFromSouthampton <- as.integer(titanic$embarked == "S")
titanic$embarked = NULL
# Process AGE?????
# Rename column
colnames(titanic) <- c("passengerClass", "hasSurvived", "age", "numberOfSiblingAndSpouses", "numberOfParentAndChildren", "fare", "isFemale", "embarkedFromCherbourg", "embarkedFromQueenstown", "embarkedFromSouthampton")
titanic$passengerId <- as.integer(row.names(titanic))
titanic <- titanic[,c(11,2,7,3,4,5,1,6,8,9,10)]
write_csv(titanic, "... /aStudyInDisaster/data/titanic3.csv", na="NA")