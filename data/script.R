library(dplyr)
DIR <- "<dir>"

# Import
titanic <- read.csv(paste(DIR, "aStudyInDisaster/data/titanic3.csv", sep = "/"))
titanic <- titanic %>% select(-boat, -name, -home.dest, -cabin, -body, -ticket)
summary(titanic)

# Remove uneccesary variables
for (name in c("pclass", "survived", "sex", "sibsp", "parch", "fare", "embarked")) {
  titanic <- titanic[c(!is.na(titanic[name])),]
}
titanic <- titanic[titanic["embarked"] != "",]

# Due to the large number of missing data for age, turn it into categorical
titanic$age <- round(titanic$age)
titanic$age[is.na(titanic$age)] = "missing"
titanic$age[0 <= as.numeric(titanic$age) & as.numeric(titanic$age) < 6] = "[0,5]"
titanic$age[6 <= as.numeric(titanic$age) & as.numeric(titanic$age) < 19] = "[6,18]"
titanic$age[19 <= as.numeric(titanic$age) & as.numeric(titanic$age) < 56] = "[19,55]"
titanic$age[56 <= as.numeric(titanic$age)] = "[56, above)"

# Process passenger class
titanic$pclass[titanic$pclass == 1] = "first"
titanic$pclass[titanic$pclass == 2] = "second"
titanic$pclass[titanic$pclass == 3] = "third"

# Format decimals
titanic$fare <- as.numeric(format(round(titanic$fare, 2), nsmall = 2))

# Rename columns
colnames(titanic) <- c("passenger_class", "has_survived", "gender" , "age" , "number_of_siblings_and_spouses", "number_of_parents_and_children", "fare", "embarked_from")

# Add ID column
titanic$passenger_id <- as.integer(row.names(titanic))

# Rearragne columns
titanic <- titanic[,c(9,2,3,4,5,6,1,7,8)]

# Export
write.csv(titanic, paste(DIR, "aStudyInDisaster/data/titanic.csv", sep = "/"), na="NA")