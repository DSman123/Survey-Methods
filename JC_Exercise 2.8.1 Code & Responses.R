# Joel Cabrera
# Survey Methods
# Professor McCabe
# April 4, 2019

#Exercise 2.8.1 - Efficacy of Small Class Size in Early Education

# Preliminaries
read.csv("STAR.csv")
star <- read.csv("STAR.csv")
dim(star)
summary(star)
table(star)

# Problem 1
star$kinder <- NA
star$kinder[star$classtype == "1"] <- "small"
star$kinder[star$classtype == "2"] <- "regular"
star$kinder[star$classtype == "3"] <- "regular with aid"
# ^^^all = considered character variable
table(star$kinder)
class(star$kinder)
# below = changes character into factor variable
star$kinder <- as.factor(star$kinder)
table(star$kinder)
levels(star$kinder)
star$kinder
star$race <- NA
star$race[star$race == "1"] <- "white"
star$race[star$race == "2"] <- "black"
star$race[star$race == "4"] <- "Hispanic"
star$race[star$race == "3" & star$race == "5"] <- "others"
star$race <- as.factor(star$race)
table(star$race)
class(star$race)
levels(star$race) #why receiving character(0) output?
#race but with different name
star$trace <- NA
star$trace[star$race == "1"] <- "white"
star$trace[star$race == "2"] <- "black"
star$trace[star$race == "4"] <- "Hispanic"
star$trace[star$race == "3" & star$race == "5"] <- "others"
star$trace <- as.factor(star$trace)
levels(star$trace) #same output as above...
# kinder and - barely - race variables have been created and overwritten, respectively.
#Problem 2
tapply(star$kinder, star$g4reading,mean)
star$kinder <- c(star$kinder, NA)
mean(star$g4reading, na.rm = TRUE)
star <-c(star$g4reading, NA)
star <-c(star$g4math, NA)
mean(star, na.rm = TRUE)
tapply(star$kinder, star$g4reading, mean)
mean(star$g4math[star$kinder[1]], na.rm = TRUE) # why is output same for below? (1 = small, 2 = regular)
mean(star$g4math[star$kinder[2]], na.rm = TRUE) #output = 706
sd(star$g4math, na.rm = TRUE)
sd(star$g4reading, na.rm = TRUE)
sd(star$g4math[star$kinder[1]], na.rm = TRUE)
sd(data$weight[data$feed=="casein"]) 
# Students in the smaller classes, on average, perform about the same. The mean for 4th-grade reading and 
# math test scores for students in both small and regular classes is 706. The standard deviations for both kinds
# of test scores, rounded to two digits, are 52.43 and 43.09, respectively.
# Problem 3
summary(star$g4reading[star$kinder[1]]
        quantile(star$g4reading[star$kinder[1]]), probs = seq(from = 0, to = 1, by = 0.1))
quantile(star$g4reading[star$kinder[2]]), probs = seq(from = 0, to = 1, by = 0.1))
# Quantile analysis can add to the analysis of the mean of test scores. Quantiles indicate 
# what percentage of students received a certain score on either the reading or math test. *Quantil command not working...*
# Problem 4 (cont. table: row, column)
table(kinder = star$kinder, yearssmall = star$yearssmall)
yearssmall.kinder.tab <- table(kinder = star$kinder, yearssmall = star$yearssmall)
addmargins(yearssmall.kinder.tab) #Using this for interpretation
prop.table(yearssmall.kinder.tab, margin = 1)*100
prop.table(yearssmall.kinder.tab, margin = 2)*100
addmargins(prop.table(yearssmall.kinder.tab, margin = 2)*100)
round(prop.table(yearssmall.kinder.tab), digits = 2))*100 #why not working?
round(prop.table(yearssmall.kinder.tab), digits = 3)*100
addmargins(round(prop.table(yearssmall.kinder.tab), digits = 3)*100) #use this table; what makes it different from other prop. tables?
# 857 students in small classes stayed for 4 years. 576 students in small classes stayed for 1 year. *How to interpret those who only 
# stayed for 1 year and moved to regular or regular with aid for the remaining years?
mean(star$kinder[g4reading])
mean(star$kinderp[g4math]) # Invalid inputs; how to find average test scores across students who spent different years in small classes?
# Problem 5
# ***Due to failure of recoding race variable, answers will be based on original race variable...***
star$race[star$g4reading, na.rm = TRUE]
star$race[star$g4math, na.rm = TRUE]
mean(star$race[star$g4math, na.rm = TRUE])
mean(star$race[star$g4math, na.rm = TRUE])
# ***How to obtain mean reading and math test scores between white and minority students, in regular and small classes?
# Problem 6
table(kinder = star$kinder, hsgrad = star$hsgrad) # 2 variables (2-way cont. table)
kinder.hsgrad.tab <-table(kinder = star$kinder, hsgrad = star$hsgrad)
addmargins(kinder.hsgrad.tab)
prop.table(kinder.hsgrad.tab, margin = 1)*100
addmargins(round(prop.table(kinder.hsgrad.tab), digits = 3)*100)
table(kinder = star$kinder, hsgrad = star$hsgrad,
      yearssmall = star$yearssmall) # 3 variables (3-way cont. table, depends on number of yearss spent in school)
kinder.hsgrad.yearssmall.tab <- table(kinder = star$kinder, hsgrad = star$hsgrad,
                                      yearssmall = star$yearssmall)
prop.table(kinder.hsgrad.yearssmall.tab, margin = 1)*100
table(race = star$race, hsgrad = star$hsgrad)
star$race[1]
star$race[2, 3, 4, 5, 6] # How to extract white and minority categories from race? (minorities = black, asian, native american, and hispanic)
race.hsgrad.tab <- table(race = star$race, hsgrad = star$hsgrad)
addmargins(race.hsgrad.tab)
prop.table(race.hsgrad.tab, margin = 1)*100 # Based on these results, however, whites have much higher graduation rates than any other race.
# Graduation rates do differ depending upon the number of years spent in small classes.
# The higher the number of years, the higher percentage of students in small classes graduate in high school.