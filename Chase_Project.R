### Load in Data
data = read.csv("Chase_data.csv") #Load in data
library(ggplot2)

### Filter for just sales

just_sales = data[data['Type'] == "Sale", ]

### Initial Plots

ggplot(just_sales, aes(x = Amount)) + geom_histogram() + 
  labs(title = "Histogram of Purchases")
# Might be some outliers based on the histogram

boxplot(Amount~Type, data = just_sales) #Definitely some outliers

#Plotting Amount spent across dates
ggplot(just_sales, aes(x = Transaction.Date, y = Amount)) + 
  geom_point() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) 
#Almost no relationship across date

just_sales$Transaction.Month = as.factor(just_sales$Transaction.Month)
just_sales$Transaction.Year = as.factor(just_sales$Transaction.Year)
#Convert to categorical for analysis across years and months

# Boxplot by Month
boxplot(Amount~Transaction.Month, data = just_sales,
        main = "Boxplot of Amount by Month")
#Outliers are present in every month, but I think they're worth keeping as some could actual purchasing behavior.
# There doesn't seem to be any drastic changes across months

#Regression by Month
month_model = lm(Amount~Transaction.Month, data = just_sales)
summary(month_model)
# It says I spend $46 on average purchases in january. 
# April, November and December are the most significant affectors, with the most decrease in spending. 
# January is the highest increase with 46.826, it's also very significant.

ggplot(just_sales, aes(x = Transaction.Month, y = Amount)) + geom_point() +
  labs(x = "Month", title = "Amount Spent Across Months of 2022 - 2024")

### Identifying outliers
threshold = 4 / dim(just_sales)[1] #Establish threshold for cooks distance
cooks.distance(month_model)[cooks.distance(month_model) > threshold]

cd = cooks.distance(month_model)

plot(cd, main = "Cooks Distance Plot by Monthly Model")
abline(h = threshold, col = "red") #plots threshold line

index_to_remove <- as.numeric(names(cd)[(cd > (4/dim(just_sales)[1]))])
without_outliers = just_sales[-index_to_remove,]

month_model_1 = lm(Amount~Transaction.Month, data = without_outliers)
summary(month_model_1)
# Now January, April, November and December are significant at the 0.05 alpha level. March and May are also close to significant
# The R squared is quite bad, meaning our predictors don't explain the variability of my spending very well. 
# This model without outliers actually seems worse than the original.
plot(month_model_1)
#There's some solid linearity, but there's lack of independence across the variable. The error is also not normalized
ggplot(data = without_outliers, aes(month_model_1$residuals)) + geom_histogram()
#The histogram shows a skewed distribution and breaks between error values, this is also a violation of assumptions.

### Now try analysis with Years
ggplot(just_sales, aes(x = Transaction.Year, y = Amount)) + geom_point() + 
  labs(x = "Year", title = "Spending across 2022 - 2024")

boxplot(Amount~Transaction.Year, data = just_sales)
# Outliers are present again, 2022 has a much fatter quartile section than the other 2 years interestingly. 

year_model = lm(Amount~Transaction.Year, data = just_sales)
summary(year_model)
# We see that 2022 and 2024 are significant at the 0.05 level, very much so for 2022. 
# But, we do see a very low R squared value, meaning again the variability isn't strongly explained year-by-year
# The F-statistic is much better than the month models tho, meaning there is a relationship across the variables.

#Let's again look for outliers with cooks
sum(cooks.distance(year_model) > 1)
#There aren't any values above one, which is good, let's use our threshold again
cooks.distance(year_model)[cooks.distance(year_model) > threshold]
cd_year = cooks.distance(year_model)

#Now we can compare the cooks distance plots and see any similarities
par(mfrow = c(1,2))
plot(cd_year, main = "Yearly model")
abline(h = threshold, col = "red") #plots threshold line
plot(cd, main = "Monthly Model")
abline(h = threshold, col = "red") #plots threshold line

# The monthly model had points with higher distractions from model performance.

dev.off()
#Let's remove the outliers and see the effect on the model again
index_to_remove <- as.numeric(names(cd_year)[(cd_year > (4/dim(just_sales)[1]))])
without_outliers_yearly = just_sales[-index_to_remove,]

yearly_model_2 = lm(Amount~Transaction.Year, data = without_outliers_yearly)
summary(yearly_model_2)
# Removing the cooks distance outliers made the significant years more likely to be significant. It also improved the R 
# squared slightly. F value was also increased, indicating more significance across the predictors. 
plot(yearly_model_2)
#We see the same issues with our assumptions in the yearly model as well.

### Now let's try combining months and years

month_year_model = lm(Amount~Transaction.Year+Transaction.Month, data = just_sales)
summary(month_year_model)


#Predicting is kind of useless though because it's attempting to predict a single purchase, rather than spending for a month
new_data = data.frame(Transaction.Month = as.factor(3), Transaction.Year = as.factor(2023))
predict(month_year_model, new_data)


## We should then try grouping by year and month then making a model with that
library(dplyr)
grouped_dates = just_sales %>% group_by(Transaction.Month, Transaction.Year) %>% summarise(total = sum(Amount))

colnames(grouped_dates) = c("Month", "Year", "Total_Spent")

ggplot(grouped_dates, aes(x = Month, y = Total_Spent, fill = Year)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Spending by Month, colored by Year")
#This gives a better indication of the spending, 2024 is much lower than 2023 and 2022

boxplot(Total_Spent~Year, data = grouped_dates, main = "Total Spent by Month Across 2022 - 2024")

model = lm(Total_Spent~Year+Month, data = grouped_dates)
summary(model)
plot(model) #Residuals aren't linearized around zero.
ggplot(data = grouped_dates, aes(model$residuals)) + geom_histogram() + 
  labs(title = "Histogram of Residual Errors", x = "Residuals")
# Residuals don'y follow normalized pattern, but that is to be expected with
# how little our sample is

### Comparison of Models with Anova
library(car)
month_model = lm(Total_Spent~Month, data = grouped_dates)
year_model = lm(Total_Spent~Year, data = grouped_dates)
anova(month_model, model)
anova(year_model, model)

#Although the addition of months seemingly does very little
#it has a F score of .5463, the addition of years seems to really improve overall efficiency
summary(month_model) #F value p statistic of .8335, adjusted r squared is negative, meaning we shouldnt use it for prediction
summary(year_model) #F value p statistic of .07391, adjusted r squared is .1391, not great but we could use it!
summary(model) #F value p statistic of .644, adjusted r squared is -.1134, shouldn't predict

### Category spending analysis
## Now we could try predicting based on category of spending
## I think we shouldn't group because it could show what individual purchases are like in each category
grouped_categories = just_sales %>% group_by(Category) %>% 
  summarize(total_spent = sum(Amount))

ggplot(grouped_categories, aes(x = reorder(Category, -total_spent), y = total_spent)) +
  geom_bar(stat = "identity") + coord_flip() + labs(x = "Category",
                                                    y = "Total Spent", 
                                                    title = "Total Spent by Category")

# We see that shopping, food and drink and entertainment are the categories I
# spend the most in
category_model = lm(Amount~Category, data = just_sales)
summary(category_model)
# Although we have a pretty good F value, the R squared is not decent for performance
# The base case is automative, meaning we spend 185 on average for automative purchases
# The only higher purchase is Home 
# It seems that categories with a large amount of purchases are the most significant,
# Whereas categories with lower counts of purchases are less significant.

plot(category_model)


#The QQ plot is terrible, maybe we can try a log transformation, lets use
# a boxcox plot to see which to use.
library(MASS)
boxcox(category_model)
#It hovers around 0, which means a log transformation should work

category_model_log = lm(log(Amount)~Category, data = just_sales)
summary(category_model_log)
# The R squared is better, the F statistic is better, overall it seems better
plot(category_model_log)
# The QQ plot is much better, as well as the residuals vs fitted plot

## Now let's try building models with every variable and performing anovas 

year_model = lm(log(Amount)~Transaction.Year+Category, data = just_sales)
month_model = lm(log(Amount)~Transaction.Month+Category, data = just_sales)
all_variables_model = lm(log(Amount)~Transaction.Year+Transaction.Month+Category,
                         data = just_sales)

year_sum_r2 = summary(year_model)$adj.r.squared
month_sum_r2 = summary(month_model)$adj.r.squared
all_variables_r2 = summary(all_variables_model)$adj.r.squared
category_r2 = summary(category_model_log)$adj.r.squared

data.frame("Model" = c("Year Model", "Monthly Model", "Category Model",
                       "All Variables Model"), "Adjusted R Squared" = c(year_sum_r2,
                                                                        month_sum_r2, 
                                                                        category_r2,
                                                                        all_variables_r2))
#There's not much of a difference in terms of adjusted r squared
anova(category_model_log, all_variables_model)
anova(year_model, all_variables_model)
anova(month_model, all_variables_model)
#Seems that adding all variables makes the model much better
summary(all_variables_model)
#we expect to see that adding months doesn't do much compared to years