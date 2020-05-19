
library(tidyverse)
require(tidyverse)
California <- read.csv("https://personal.utdallas.edu/~sxg180154/housing.csv")
View(California)
summary(California)
dim(California)

# eliminated all null values in rows
#finding the percentage of null values in each columns
for(i in 1:ncol(California)) {
  colName <- colnames(California[i])
  pctNull <- sum(is.na(California[,i]))/length(California[,i])
    print(paste("Column ", colName, " has ", round(pctNull*100, 3), "% of nulls"))
}

cali_clean <- na.exclude(California)
dim(cali_clean)
 
# features having highest correlation among the predictors
cor(cali_clean$total_rooms, cali_clean$households)
cor(cali_clean$population,cali_clean$total_rooms)
cor(cali_clean$population,cali_clean$households)

# correlation between the output value(median_house_value) and all other predictors
cor(cali_clean$median_income,cali_clean$median_house_value)
cor(cali_clean$longitude,cali_clean$median_house_value)
cor(cali_clean$latitude,cali_clean$median_house_value)
cor(cali_clean$housing_median_age,cali_clean$median_house_value)
cor(cali_clean$total_rooms,cali_clean$median_house_value)
cor(cali_clean$total_bedrooms,cali_clean$median_house_value)
cor(cali_clean$population,cali_clean$median_house_value)
cor(cali_clean$households,cali_clean$median_house_value)

# getting a correlation plot for visuaization
library(corrplot)
require(corrplot)
Mat <- cor(cali_clean[sapply(cali_clean, is.numeric)])
corrplot(Mat)
# From the graph we can estimate that the correlation betweeen 
# Total_rooms - Households
# Population - Total_rooms
# Population - Households
# Median_Income - Median_House_value are highly correlated


# plotting the housing_value based on the latitude, longitude and population
# the region with the highest population has high housing value
ggplot(data = cali_clean,mapping = aes(cali_clean$latitude,y=cali_clean$longitude, color = cali_clean$median_house_value, size = cali_clean$population/100))+geom_point()

#plotting the histogram of all predictors vs the housing value
ggplot(data = cali_clean,mapping= aes(cali_clean$longitude))+geom_histogram(fill = "light green", color="blue", binwidth = 1)
ggplot(data = cali_clean,mapping= aes(cali_clean$latitude))+geom_histogram(fill = "red", color="blue", binwidth = 1)
ggplot(data = cali_clean,mapping= aes(cali_clean$housing_median_age))+geom_histogram(fill = "light pink", color="blue", binwidth = 1)
ggplot(data = cali_clean,mapping= aes(cali_clean$total_rooms))+geom_histogram(fill = "grey", color="blue", binwidth = 1)
ggplot(data = cali_clean,mapping= aes(cali_clean$total_bedrooms))+geom_histogram(fill = "cyan", color="blue", binwidth = 1)
ggplot(data = cali_clean,mapping= aes(cali_clean$population))+geom_histogram(fill = "purple", color="blue", binwidth = 1)
ggplot(data = cali_clean,mapping= aes(cali_clean$households))+geom_histogram(fill = "dark red", color="blue", binwidth = 1)
ggplot(data = cali_clean,mapping= aes(cali_clean$median_income))+geom_histogram(fill = "light yellow", color="blue", binwidth = 1)
ggplot(data = cali_clean,mapping= aes(cali_clean$median_house_value))+geom_histogram(fill = "pink", color="blue", binwidth = 1)

# finding the concentration of house values based on the ocean proximity
ggplot(data = cali_clean,mapping = aes(x = cali_clean$ocean_proximity, y= cali_clean$median_house_value))+geom_boxplot()

# number of houses in each category of ocean proximity
group_ocean = cali_clean %>% group_by(ocean_proximity) %>% summarise(Number = n()) %>% arrange(desc(Number))
group_ocean

#top 10 costliest houses in the california housing data
cost = cali_clean %>% filter(!is.na(median_house_value)) %>% arrange(desc(median_house_value)) %>% head(10)
cost 

# splitting rooms_per_household, bedrooms_per_household, population_per_household
rooms = trunc(cali_clean$total_rooms/cali_clean$households)
bedrooms = trunc(cali_clean$total_bedrooms/cali_clean$total_rooms)
popu_per_house = trunc(cali_clean$population/cali_clean$households)

# adding the additional columns(rooms_per_household, bedrooms_per_room, population_per_husehold) in the cali_full dataframe
cali_full = cali_clean %>% add_column(rooms = rooms,bedrooms = bedrooms, pop_per_house = popu_per_house) 

# again finding the correlation between the predcitors and the newly added columns
matrix <- cor(cali_full[sapply(cali_full, is.numeric)])
corrplot(matrix)
cor(matrix)

# plotting the graph between the median_house_value, median_income , total_rooms and housing _median_age
plot(cali_full[,c("median_house_value","median_income","total_rooms","housing_median_age")])

#the quantile range of median_house_value using box_plot
boxplot(cali_full$median_house_value)

#Histogram for median_house_value
hist(cali_full$median_house_value,col = "dark green", border = 2)

ggplot(cali_full, aes(x = rooms)) + geom_histogram(fill = "yellow", color = "black", binwidth = 1) 
ggplot(cali_full, aes(bedrooms)) + geom_histogram(color = "black", fill = "navy blue", binwidth = 1)
ggplot(cali_full, aes(x = pop_per_house)) + geom_histogram(fill = " orange", color = "black", binwidth = 100)

# creating model for each features of the dataset to find out the f stat and R^2 value
# to find out the importance of each feature
fit1 <- lm(median_house_value ~ longitude,cali_clean)
summary(fit1)
fit2 <- lm(median_house_value ~ latitude , cali_clean)
summary(fit2)
fit3 <- lm(median_house_value ~ housing_median_age, cali_clean)
summary(fit3)
fit4 <- lm(median_house_value ~ total_rooms, cali_clean)
summary(fit4)
fit5 <- lm(median_house_value ~ total_bedrooms, cali_clean)
summary(fit5)
fit6 <- lm(median_house_value ~ population, cali_clean)
summary(fit6)
fit7 <- lm(median_house_value ~ households, cali_clean)
summary(fit7)
fit8 <- lm(median_house_value ~ median_income, cali_clean)
summary(fit8)

# since the correlation between median_income and mdeian_house_value is higher we consider it for the model
model1 <- lm(median_house_value ~ median_income, cali_full)
summary(model1)
# standard_error in this model is 83740 but the r^2 value and the f statistic value are higher which  suits for a best model


# categorical data
# since there is only one categorical data and it is correlated with the median_house_value 
# we have to consider the ocean proximity by converting it into quantitative variable
#converting the categorical variable into integer values which eases to find the correlation between the predictor and the output value
cali_full$ocean_proximity =factor(cali_full$ocean_proximity, level = c("<1H OCEAN","INLAND","ISLAND","NEAR BAY","NEAR OCEAN"), labels = c(1,2,3,4,5))


# model with all the features of cali_full dataset
model2 <- lm(median_house_value ~ ., cali_full)
summary(model2)
# This model concludes with F-statistic - 2499 and R^2 - 0.6473 values and relatively with a lesser Standard error of 68580. 
#Hence this model is comparitively a good fit than other models.This model includes the categorical variables and the derived features.


#this model includes just the logitude and latitude festure of the data set
model3 <- lm(median_house_value ~ longitude+latitude, cali_full)
summary(model3)

#this model includes all other features except the logitude and latitude feature of the data set
model4 <- lm(median_house_value ~ housing_median_age+households+total_rooms+population+total_bedrooms, cali_full)
summary(model4)

# including all the features except the features we splitted - rooms_per_house, bedrooms_per_room, population_per_house
mod.fit <- lm(median_house_value ~ longitude+latitude+housing_median_age+total_rooms+population+median_income+ocean_proximity, cali_full)
summary(mod.fit)

# when compared the R^2 ans F-statistic we can conclude that the both is higher if we consider all the features including the feaures we extracted from the existing  predictors
#i.e. model2 has comparitively higher F-statistic and R^2 value and lesser Std error

# Extracting features for forward and backward selection
long <- cali_full$longitude
lat <- cali_full$latitude
age <- cali_full$housing_median_age
rooms <- cali_full$total_rooms
bed <- cali_full$total_bedrooms
pop <- cali_full$population
house <- cali_full$households
inc <- cali_full$median_income
ocean <- cali_full$ocean_proximity


# Forward selection based on AIC.
fit.forward <- step(lm(cali_full$median_house_value ~ 1),
                     scope = list(upper = ~ long + lat + age + rooms + bed + pop + house +inc+ocean),direction = "forward")

summary(fit.forward)

# Backward elimination based on AIC.
fit.backward <-step(lm(cali_full$median_house_value  ~ long + lat + age + rooms + bed + pop + house +inc+ocean),
                     scope = list(lower = ~1),direction = "backward")

summary(fit.backward)

# Both forward/backward.
fit.both <-step(lm(cali_full$median_house_value ~ 1),
                 scope = list(lower = ~1,
                              upper = ~ long + lat + age + rooms + bed + pop + house +inc+ocean),
                 direction = "both")
summary(fit.both)


# since model2 is comparitively the best model we can the Residual vs Fitted, Normal QQ plot, Scale-Location, Residuals vs leverage
plot(fit.both)
plot(model2)
#<Return>


#predicting (testing) the model with one of the value of the trained data
df <- as.data.frame(cali_full[1,])
df
df[,"median_house_value"] <- NULL
df
predict.lm(model2,df )
