# Question: Can you make a model that predicts a players value?
# Use the model to answer whether a team overpayed for a player or if a player under performed his contract. 
# I want to use 2016-2017 NBA data to predict 2017-2018 salary
# Which players underperformed?
# Which team overpayed/underpaid their players?

# Libraries
library(readr)

# Load Data
#2017
trad17 <- read.csv("trad17.csv",stringsAsFactors = F)
misc17 <- read.csv("misc17.csv", stringsAsFactors = F)
def17 <- read.csv("def17.csv", stringsAsFactors = F)
track17 <- read.csv("track17.csv", stringsAsFactors = F)
Adv17 <- read.csv("Adv17.csv", stringsAsFactors = F)

# 2018
trad18 <- read.csv("trad18.csv", stringsAsFactors = F)
misc18 <- read.csv("misc18.csv", stringsAsFactors = F)
def18 <- read.csv("def18.csv", stringsAsFactors = F)
track18 <- read.csv("track18.csv", stringsAsFactors = F)
Adv18 <- read.csv("Adv18.csv", stringsAsFactors = F)

# Salary for 2017-2018 season
Salary2017_18 <- read.csv("salary2017_18.csv",stringsAsFactors = F)

#explore the data 
names(Adv18)
View(Adv18)

# Fix an error in Adv18 dataset. Observation #534 has an error in PIE (-400 should be -4)
Adv18$PIE[534] <- -4 

# merge 2017 data
library(dplyr)
nba2017_1 <- left_join(trad17, misc17, by = c("Year", "Player", "Team", "Age", "GP", "W", "L", "Min", "BLK", "PF"))
names(nba2017_1)

nba2017_2 <- left_join(nba2017_1, def17, by = c("Year", "Player", "Team", "Age", "GP", "W", "L", "Min", "BLK", "STL", "DREB"))
names(nba2017_2)

nba2017_3 <- left_join(nba2017_2, track17, by = c("Year", "Player", "Team"))
names(nba2017_3)

nba2017_4 <- left_join(nba2017_3, Adv17, by = c("Year", "Player", "Team","Age"))
names(nba2017_4)

# Clean the data by removing repeated columns
nba2017_4 <- nba2017_4[ -c(2,32,43,54:58, 66:69)]


# merge 2018 data
nba2018_1 <- left_join(trad18, misc18, by = c("Year", "Player", "Team", "Age", "GP", "W", "L", "Min", "BLK", "PF"))
names(nba2018_1)

nba2018_2 <- left_join(nba2018_1, def18, by = c("Year", "Player", "Team", "Age", "GP", "W", "L", "Min", "BLK", "STL", "DREB"))
names(nba2018_2)

nba2018_3 <- left_join(nba2018_2, track18, by = c("Year", "Player", "Team"))
names(nba2018_3)

nba2018_4 <- left_join(nba2018_3, Adv18, by = c("Year", "Player", "Age", "Team"))
names(nba2018_4)

# Clean the data by removing repeated columns
nba2018_4 <- nba2018_4[ -c(2,32,43,54:58, 66:69)]
names(nba2018_4)

# Relabel for clearlity
nba2017 <- nba2017_4
nba2018 <- nba2018_4

# Merge with salary
names(Salary2017_18)
nba2017 <- left_join(nba2017, Salary2017_18, by = "Player")
names(nba2017)

# Clean data
nba2017 <- nba2017[-73]

# Explore data
str(nba2017)

# Identify NAs in the salary17_18 column 
which(is.na(nba2017$salary2017_18))

# Fill in missing data by looking up salaries online
nba2017$salary2017_18[130] <- 3903900
nba2017$salary2017_18[241]<- 1471382
nba2017$salary2017_18[251]<- 1471382
nba2017$salary2017_18[277]<-2093040
nba2017$salary2017_18[309]<-2422560


# Function to remove rows if specific column that still have NAs in the salary column 
completeFun <- function(data, desiredCols) {
        completeVec <- complete.cases(data[, desiredCols])
        return(data[completeVec, ])
}

# Remove NAs from players with missing salary. The majority of remaining empty salary17_18 is due to the player not getting a deal. 
nba2017 <- completeFun(nba2017, "salary2017_18")

# Check to see if it worked 
which(is.na(nba2017$salary2017_18))
str(nba2017)

####################### Prepare the data for modeling

# Remove variables that are not going to be used to generate the model.
names(nba2017)
nba2017_mod <- nba2017[c(4:73)]

# Partition the data using stratified sampling to ensure that train and test have similar ranges of salary
library(caret)
set.seed(123)
ind <- createDataPartition(nba2017_mod$salary2017_18, p=0.70, list = F)
train <- nba2017_mod[ind,]
test <- nba2017_mod[-ind,]

mean(train$salary2017_18) # 7132338
mean(test$salary2017_18) #  6988651

####################### Modeling

# Make model 
# Try Ridge, Lasso, Elastic Net , and MAR(s) regression models
# Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

# Get the abreviation for the model type (eg. "lm" = linear model)
names(getModelInfo())

# Linear Model
# Tuning parameter: intercept
set.seed(123)
lm <- train(salary2017_18 ~., 
            train, 
            method = "lm", 
            trControl = custom,
            metric = "Rsquared")

# Results
lm 
#RMSE     Rsquared   MAE    
#5078933  0.5809521  3994940
lmRsq = 0.5809521

#Rsquared  =  more than 58.09% of variablity in salary2017_18 is because of the model. 
summary(lm)

lm$results # Look at P value of variables

# Visualizing predicted vs actual
plot(lm$finalModel) # Plotting of residues 
# 1st plot  = if the linearity assumption is meet you should see no pattern here and the line should be flat, you should also see not pattern if the variation is constant
# 2nd plot = (Q-Q) if the errors are normally distributed the dots should fall on the diagonal line. 
# 3rd and 4th - help identify non-linearity and non-constant variance as well as troublesome observations.
# there is quite a bit of variation at the extremes but not so much in the middle or bulk of salaries. 
residuals <- resid(lm)
predictedValues <- predict(lm)
plot(train$salary2017_18, residuals)
abline()
plot(train$salary2017_18, predictedValues)

plot(varImp(lm)) # Importance of vaiables.
# Surprisingly BLKA > Opp 2nd chance pts > GP > Opp pts in the paint > W were the top 5 correlated variables.  

##################################################################################
# Ridge regression
# Ridge shrinks the coeficients but keeps all variables
# 2 Advantages over linear regression 1) It penalizes the estimates; it chooses the feature's estimate to penalize in such a way that less infuluential features undergo more penalization.  
#                                     2) When features are highly correlated with each other, then the rank of matrix X will be less than P+1 (where P is number of regressors).

# Tuning parameter: lambda
# Determine the best lambda using K-folds to select the best lambda
# Set the seq of lambda to test

lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))
# Best lambda is 0.01

set.seed(1234)
ridge <- train(salary2017_18~., data = train,
               method='ridge',
               trControl = custom,
               #tuneGrid = lambdaGrid,
               preProcess=c('center', 'scale')
)

ridge
#lambda    RMSE     Rsquared    MAE    
#1e-01   4761807  0.6307733  3720376
ridgeRsq = 0.6307733

ridge$modelInfo
plot(ridge)
plot(varImp(ridge, scale = F))  # Importance of vaiables.
# FP(fantasy pts) > min > Pts > FGM > Dist_miles were the top 5 correlated variables


# There is another way to run ridge using glmnet and although obtains a similar Rsq value it uses different variables 
# try using the glmnet and set the alpha to 0 to run ridge.

library(glmnet)
set.seed(12345)
glmnet_ridge <- train(salary2017_18 ~.,
               train, 
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = seq(0.0001, 1, length=10)),
               trControl = custom)

# It determined that the best value for lambda = 1. 

glmnet_ridge
#lambda  RMSE     Rsquared   MAE    
#1.0000  4750549  0.6325503  3699844
glmnetRidgeRsq = 0.6325503

plot(varImp(glmnet_ridge, scale = F))  # Importance of vaiables.
# DEF_win shares > BLKA > Dist_miles_def > avg_speed_off > 3pt made were the top 5 correlated variables

# While both ridge and glmnet produce similar Rsquared it looks like ridge is more resonable in terms of variables used.

#########################################
# Lasso Regression
set.seed(123456)
lasso <- train(salary2017_18 ~., 
               train, 
               method = 'lasso',
               preProc = c('scale', 'center'),
               #tuneGrid = expand.grid(alpha=1,         # Use if you are using glmnet method you want to set alpha to 1 (alpha = 1)
                #                      lambda = 10^seq(4, -1, length = 5)),
               trControl = custom)

lasso
#fraction  RMSE     Rsquared   MAE    
#0.1       4735988  0.6286117  3680057
lassoRsq = 0.6286117

plot(varImp(lasso, scale = F))

# Similarly to Ridge FP(fantasy pts) > min > Pts > FGM > Dist_miles were the top 5 correlated variables


#############################################
# Elastic Net Regression
set.seed(4321)
en <- train(salary2017_18 ~., data = train,
            method = 'glmnet',
            tuneGrid = expand.grid(alpha = seq(0,1, length = 10),
                                   lambda = seq(0.0001, 0.2, length = 5)),
            trControl = custom)

en
#alpha      lambda    RMSE     Rsquared   MAE    
#0.0000000  0.000100  4720643  0.6341874  3698572
enRsq = 0.6341874
plot(en)
plot(varImp(en, scale = F))
# Just like glmnet ridge DEF_win shares > BLKA > Dist_miles_def > avg_speed_off > 3pt made were the top 5 correlated variables

#############################################
# Multivariate Adapative Regression Spline
# MARS allows you to fit non linear correlations; MARS is a good model to use for feature selection because it sets all the unused variables to 0.
# tuning parameters: nprune(#Terms), degree (Product Degree)
library(earth)

grid = expand.grid(degree = 2, nprune = 7) # Play around with different nprune and see which works best (went from 40 down to 7; gave the best Rsq)
set.seed(54321)
MARS1 <- train(salary2017_18 ~., data = train,
              method = 'earth',
              tuneGrid = grid,
              trControl = custom)
MARS1 
# RMSE     Rsquared   MAE    
#5120492  0.5925366  3763366
MARS1Rsq = 0.5925366

MARS1$finalModel
plot(varImp(MARS1, scale = F))

# Min, Age, FP, BLKA, TD3 (triple doubles) were the only variables used. 

# Multivariate Adapative Regression Splines
# tuning parameter degree (Product Degree)
set.seed(654321)
MARS <- train(salary2017_18 ~., data = train,
            method = 'gcvEarth',
            tuneLength = 5,
            trControl = custom)

MARS 
#RMSE     Rsquared  MAE    
#5004682  0.5956883  3747718
MARSRsq = 0.5956883

MARS$finalModel
plot(varImp(MARS, scale = F))
# FP, Dist_Miles_def, Age, TD3, BLKA, STL, OPP_FBPS, BLK, DEF_WS were the only variables used
# MARS looks like it does the best at incorporating the best mix of offensive and defensive stats.

# Compare models
model_list <- list(LinearModel = lm, Ridge = ridge, GlmnetRidge = glmnet_ridge, Lasso = lasso, ElasticNet = en, MARS = MARS1, MARSs = MARS)
res <- resamples(model_list)
summary(res)

# Based on mean Rsquared it looks like the best model was en (Elastic Net Regression)
# However lets use all the models to predict the test dataset and give it the old eyeball test


# Remove observation 223 from nba2018.  It has some data missing.  
names(nba2018)
names(nba2017)
nba2018 <- nba2018[-223,]
str(nba2018)

# Predict the salary using all the models.
nba2018$PV_EN <- predict(en, nba2018)
nba2018$PV_lm <- predict(lm, nba2018)
nba2018$PV_ridge <- predict(ridge, nba2018)
nba2018$PV_lasso <- predict(lasso, nba2018)
nba2018$PV_MARS1 <- predict(MARS1, nba2018)
nba2018$PV_MARS <- predict(MARS, nba2018)

# Merge actual salary with nba2018 dataset and compute the difference between actual and predicted
names(nba2018)
nba2018_salaryFinal <- left_join(nba2018, Salary2017_18, by = "Player")
names(nba2018_salaryFinal)

# compute difference

nba2018_salary <- nba2018_salaryFinal %>%
                filter(salary2017_18 >= 0) %>%
                select(Year:Team.x, PV_EN, PV_lm, PV_ridge, PV_lasso, PV_MARS, PV_MARS1, salary2017_18) %>%
                mutate(Diff_EN = PV_EN - salary2017_18, Diff_lm = PV_lm - salary2017_18, Diff_ridge = PV_ridge - salary2017_18, Diff_lasso = PV_lasso - salary2017_18, 
                       Diff_MARS = PV_MARS - salary2017_18, Diff_MARS1 = PV_MARS1 - salary2017_18) 

# Write the final data set to file so that you have a copy
write.csv(nba2018_salary, "nba2018_salary.csv")


# If you wanted to save the final model as a function that can be later recalled do the following:
saveRDS(en, "final_model.rds")
fm <- readRDS("final_model.rds")
print(fm)

# Use the model for prediction
predict1 <- predict(fm, train)
sqrt(mean((train$salary2017_18-predict1)^2))

predict2 <- predict(en, test)
sqrt(mean((test$salary2017_18-predict2)^2))


# Determine the team with the biggest difference between predicted and actual salary.  Basically which team is over/underpaying the most?
# First remove duplicate observations (these occur because players play for more than one team in a season)
duplicated(nba2018_salary$Player)
nba2018_salary_minDup <- nba2018_salary[!duplicated(nba2018_salary$Player),]
duplicated(nba2018_salary_minDup$Player)


names(nba2018_salary_minDup)
nba2018_salary_minDup <- tbl_df(nba2018_salary_minDup)

team_summary <- nba2018_salary_minDup %>%
                group_by(Team.x) %>%
                summarise(count = n(),
                          Sum_MARS1 = sum(Diff_MARS1, na.rm = T))
                
write.csv(team_summary, "team_summary.csv")


# Which team has the most undervalued starts
team_player_sum <- nba2018_salary_minDup %>%
                        filter(Diff_MARS1>0) %>%
                        group_by(Team.x) %>%
                        summarise(count = n())


write.csv(team_player_sum, "team_player_sum.csv")
