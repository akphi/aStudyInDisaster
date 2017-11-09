library(dplyr)
titanic <- read_csv("<directory>/aStudyInDisaster/data/titanic3.csv")
titanic <- titanic %>% select(-boat, -name, -home.dest, -cabin, -body, -ticket)
summary(titanic)
for (name in c("pclass", "survived", "sex", "sibsp", "parch", "fare", "embarked")) {
  titanic <- titanic[c(!is.na(titanic[name])),]
}
titanic$isFemale = as.integer(titanic$sex == "female")
titanic$sex = NULL

# Process Ports of Embarkation: C = Cherbourg, Q = Queenstown, S = Southampton
titanic$embarkedFromCherbourg <- as.integer(titanic$embarked == "C")
titanic$embarkedFromQueenstown <- as.integer(titanic$embarked == "Q")
titanic$embarkedFromSouthampton <- as.integer(titanic$embarked == "S")
titanic$embarked = NULL

# Due to the large number of missing data for age, turn it into categorical
titanic$age <- round(titanic$age)
titanic$age_missing = as.integer(is.na(titanic$age))
titanic$age_between_0_and_5 = as.integer(!is.na(titanic$age) & 0 <= titanic$age & titanic$age < 6)
titanic$age_between_6_and_18 = as.integer(!is.na(titanic$age) & 6 <= titanic$age & titanic$age < 19)
titanic$age_between_19_and_55 = as.integer(!is.na(titanic$age) & 19 <= titanic$age & titanic$age < 56)
titanic$age_between_56_and_above = as.integer(!is.na(titanic$age) & 56 <= titanic$age)
titanic$age = NULL

# Format decimals
titanic$fare <- as.numeric(format(round(titanic$fare, 2), nsmall = 2))

# Rename columns
colnames(titanic) <- c("passenger_class", "has_survived", "number_of_siblings_and_spouses", "number_of_parents_and_children", "fare", "is_female", "embarked_from_cherbourg", "embarked_from_queenstown", "embarked_from_southampton", "age_missing", "age_between_0_and_5", "age_between_6_and_18", "age_between_19_and_55", "age_between_56_and_above")
titanic$passenger_id <- as.integer(row.names(titanic))
titanic <- titanic[,c(15,2,6,10,11,12,13,14,3,4,1,5,7,8,9)]
write_csv(titanic, "<directory>/aStudyInDisaster/data/titanic.csv", na="NA")