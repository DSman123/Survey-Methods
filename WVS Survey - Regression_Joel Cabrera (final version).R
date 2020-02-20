# Joel Cabrera
# Survey Methods
# Professor McCabe
#April 24, 2019

#Load & Clean/Subset
WVS <- readRDS("F00007944-WV5_Data_R_v20180912.rds", refhook = NULL) #using 05-09 WVS dataset 
WVS <- subset(WVS, COW == 135) #subset to peru only
nrow(WVS)
WVS$V148[WVS$V148 < 0] <- NA #belief in strong leader
WVS$V136[WVS$V136 < 0] <- NA #confidence in police
WVS$V157[WVS$V157 < 0] <- NA #civil rights for liberty
WVS$V163[WVS$V163 < 0] <- NA # perception of quality of democracy in Peru
#WVS$V151[WVS$V151 < 0] <- NA
#Check
table(WVS$V148)
WVS[, "V148"]
table(WVS$V148)
table(WVS$V136)
table(WVS$V157)
#table(WVS$V151)
table(WVS$V163) #Using V163 instead of V151, but why have "-2"? EDIT: fixed
# Bivariate Regression
bireg <- lm(V163 ~ V148, data = WVS)
summary(bireg)
# Multivariate regression
mulreg <- lm(V163 ~ V148 + V136 + V157, data = WVS)
summary(mulreg)
#Plot
plot(x = WVS$V136, #x-values
     y = WVS$V163, # y-values
     main = "Response Relationship between Consider in Police & Consideration of Peru as Democracy", # label for main title
     ylab ="Consideration of Peru as Democracy", #y-axis label
     xlab = "Confidence in Police", #x-axis label
     pch = 1) # point type
abline(bireg, col = "red") #adds reg. line
#Binary logistic regression 
table(WVS$V163)
WVS$V163[WVS$V163 < 6] <- 0 # 0 = not democratic 
WVS$V163[WVS$V163 > 5] <- 1 #had 10 value labels; 1= democratic


table(WVS$V163) #Q: Possible to convert IVs as binary ones, too?

table(WVS$V148)


fit <- glm(V163 ~ V148, data = WVS, 
           family = binomial(link = "logit"))
summary(fit)
## Getting predicted probabilities 
## Predict values of y for every possible value of x 
## Use the newdata = data.frame() argument to tell R the values of x 
## type = "response" puts it on the probability scale 
pprob <- predict(fit, newdata = data.frame(V148 = c(1,2,3,4)), 
                 type = "response") 
pprob # Predicted probability Y=1 for each value of X; got different values, because using V163
## Plot the results. Note: Unlike linear regression, the line is no longer perfectly linear. This is because it has to stay between 0 and 1. 
plot(pprob, type = "b", 
     ylim = c(0, 1), #NOTE: ylim = c(.6, 1) does not display fitted line; have to use c(0, 1) instead
     main = "Predicted Probability V163 = 1", 
     xlab = "Values of V148", 
     ylab = "Predicted Probability V163 = 1")
#1a
# (based on regression results/coefs from summary(fit)) A 1 unit increase in the respondent's tentative belief in having a strong leader is associated with 
# a  0.04758 decrease in the log-odds a respondent considers Peru as a democracy.
#1b.
# (based on plot) A 1 unit increase in the respondent's tentative belief in having a strong leader is associated with 
# a 1 percentage point increase in the probability of the respondent considering Peru as a democracy.
#1c. 
# (see below for rebaled plot)
plot(pprob, type = "b", 
     ylim = c(0, 1), #NOTE: ylim = c(.6, 1) does not display fitted line; have to use c(0, 1) instead
     main = "Is a Strong Leader Associated with Public Perception of Peru as a Democracy?", 
     xlab = "Is Having a Strong Leader Good or Bad?", 
     ylab = "How Democratic is Peru?")

#Further editing
WVS$V148[WVS$V148 < 3] <- 1 # good
WVS$V148[WVS$V148 > 2] <- 0 # bad

fit <- glm(V163 ~ V148, data = WVS, 
           family = binomial(link = "logit"))
summary(fit)
## Getting predicted probabilities 
## Predict values of y for every possible value of x 
## Use the newdata = data.frame() argument to tell R the values of x 
## type = "response" puts it on the probability scale 
table(WVS$V148)
pprob <- predict(fit, newdata = data.frame(V148 = c(0,1)), 
            type = "response") 
pprob 



fit <- glm(V163 ~ V148 + V157 + V136, data = WVS, 
           family = binomial(link = "logit"))
summary(fit)

pprob1 <- predict(fit, newdata = data.frame(V136 = c(1,2,3,4),
                                           V148 = 0,
                                          V157 = mean(WVS$V157, na.rm = T)), 
                 type = "response") 
pprob1
pprob2 <- predict(fit, newdata = data.frame(V136 = c(1,2,3,4),
                                           V148 = 1,
                                           V157 = mean(WVS$V157, na.rm = T)), 
                 type = "response") 
pprob2

pprob3 <- predict(fit, newdata = data.frame(V136 = mean(WVS$V136, na.rm = T),
                                            V148 = c(0, 1),
                                            V157 = mean(WVS$V157, na.rm = T)), 
                  type = "response") 

pprob3
pprob4 <- predict(fit, newdata = data.frame(
                                            V136 = mean(WVS$V136, na.rm = T),
                                            V148 = 0,
                                            V157 = c(1,2,3,4,5,6,7,8,9,10)), 
                  type = "response") 

plot(pprob4, type = "b", 
     ylim = c(0, 1), #NOTE: ylim = c(.6, 1) does not display fitted line; have to use c(0, 1) instead
     main = "Predicted Probability V163 = 1", 
     xlab = "Values of V157, holding constant V148 and V136", 
     ylab = "Predicted Probability V163 = 1")

# Finishing Touch (5/1/19)
# Binary IV for V157
WVS$V148[WVS$V148 < 6] <- 1 # Not essential characteristic of democracy
WVS$V148[WVS$V148 > 5] <- 0 # essential characteristic of democracy
table(WVS$V148)



################################## Finalized Code ################################################
###Load & Clean/Subset data
WVS <- readRDS("F00007944-WV5_Data_R_v20180912.rds", refhook = NULL) #using 05-09 WVS dataset 
WVS <- subset(WVS, COW == 135) #subset to peru only
nrow(WVS)
#IVs
WVS$V148[WVS$V148 < 0] <- NA #belief in strong leader
WVS$V136[WVS$V136 < 0] <- NA #confidence in police
WVS$V157[WVS$V157 < 0] <- NA #civil rights for liberty
#DV
WVS$V163[WVS$V163 < 0] <- NA # perception of quality of democracy in Peru
###Conversion into Binary Variables
#IVs
WVS$V148[WVS$V148 < 3] <- 1 # good way of running country
WVS$V148[WVS$V148 > 2] <- 0 # bad way of running country

WVS$V157[WVS$V157 < 6] <- 0 # Not essential characteristic of democracy
WVS$V157[WVS$V157 > 5] <- 1 # essential characteristic of democracy

WVS$V136[WVS$V136 < 3] <- 1 # confidence in police
WVS$V136[WVS$V136 > 2] <- 0 # no confidence in police

#DV
WVS$V163[WVS$V163 < 6] <- 0 #  Peru is not democratically governed
WVS$V163[WVS$V163 > 5] <- 1 #  Peru is democratically governed
##Check
table(WVS$V136)
table(WVS$V148)
table(WVS$V157)
table(WVS$V163)
###Binary Logistic regression 
blogit <- glm(V163 ~ V148 + V157 + V136, data = WVS, #All IVs
           family = binomial(link = "logit"))
summary(blogit)
##Getting more intuitive interpretations
pprob <- predict(blogit, newdata = data.frame(V148 = c(0,1)), #only V148; below include all other IVs
                 type = "response") 
pprob 

pprob1 <- predict(blogit, newdata = data.frame(V136 = c(0,1)),
                                            V148 = 0,
                                            V157 = mean(WVS$V157, na.rm = T)), #when V148 = 0
                  type = "response") 
pprob1

pprob2 <- predict(blogit, newdata = data.frame(V136 = c(0, 1)),
                                            V148 = 1,
                                            V157 = mean(WVS$V157, na.rm = T)), #when V148 = 1
                  type = "response") 
pprob2

pprob3 <- predict(blogit, newdata = data.frame(V136 = mean(WVS$V136, na.rm = T),
                                            V148 = c(0, 1),
                                            V157 = 1, #NEW CODE; changed V157 
                  type = "response") 
pprob3

pprob4 <- predict(fit, newdata = data.frame(V136 = mean(WVS$V136, na.rm = T),
                                            V148 = c(0, 1),
                                            V157 = 0, #NEW CODE; changed V157 
                  type = "response") 
pprob4

##Plotting logit
plot(pprob4, type = "b", 
     ylim = c(0, 1), #NOTE: ylim = c(.6, 1) does not display fitted line; have to use c(0, 1) instead
     main = "Predicted Probability V163 = 1", 
     xlab = "Values of V157, holding constant V148 and V136", 
     ylab = "Predicted Probability V163 = 1")