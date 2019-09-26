#-->remove all objects
rm(list = ls())
#-->Set working directory
setwd("C:/Users/vgonsalv/Desktop/DataScience/project")
#-->Confirm whether working directory is set or not
getwd()
#-install required packages
install.packages(c("gplots", "psych","ggplot2","gridExtra","corrgram","usdm","DMwR","randomForest"))
library("usdm")
library("class")
library("ggplot2")
library("gridExtra")
library("scales")
library("psych")
library("gplots")
library("corrgram")
library("rpart")
library("DMwR")
library("randomForest")
#-->Load day.csv file in Bike_renting object.MArk 1st row as header and also mark Null columns as NA
Bike_renting = read.csv("day.csv",
                        header = T,
                        na.strings = c("", " ", "NA"))
#-->view the loaded data to check if properly loaded or not
View(Bike_renting)
#-->Check for any missing values...none in this data set
missing_val = data.frame(apply(Bike_renting, 2, function(x) {
  sum(is.na(x))
}))
#-->Check the datatype of all variables
str(Bike_renting)#-->every variable is either int or numeric.We need to convert the below variables to factor as they have factor/categorical data
#-->Converting to factor variables season,yr,mnth,holiday,weekday,workingday,weathersit
for (value in c(3:9))
{
  Bike_renting[, value] = as.factor(Bike_renting[, value])
}
#-->Check if conversion is done
str(Bike_renting)
#-->Create a vector numeri index with value true for numeric variables
numeric_index = sapply(Bike_renting, is.numeric)
#-->create a dataframe having only numeric data and extract the column names having only numeric data
numeric_data = Bike_renting[, numeric_index]
cnames = colnames(numeric_data)
#-->Plot boxplot for all numeric values and assign names to boxplot as gn_column_name
for (i in 1:length(cnames))
{
  assign(
    paste0("gn", i),
    ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(Bike_renting)) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      geom_boxplot(
        outlier.colour = "red",
        fill = "grey" ,
        outlier.shape = 18,
        outlier.size = 1,
        notch = FALSE
      ) +
      theme(legend.position = "bottom") +
      labs(y = cnames[i], x = "cnt") +
      ggtitle(paste("Box plot of cnt for", cnames[i]))
  )
}
#-->See the plotted boxplot and for conclusions
gridExtra::grid.arrange(gn1, gn2, gn3, gn4, gn5, gn6, gn7, gn8, ncol =
                          4)
#-->'instant' is record index and hence outlier analysis will not be done for it.Simillarly 'casual' and 'registered' and 'cnt'(casual+registered)
#-->  are our target variables  hence   outlier analysis will not be done for it
gridExtra::grid.arrange(gn2, gn3, gn4, gn5, ncol = 4)
#-->As can be seen we have some outliers in 'hum' and 'windspeed' column which we will remove
for (i in cnames[2:5]) {
  print(i)
  val = Bike_renting[, i][Bike_renting[, i] %in% boxplot.stats(Bike_renting[, i])$out]
  print(length(val))
  Bike_renting = Bike_renting[which(!Bike_renting[, i] %in% val), ]
}
#-->Check for co-relation in numeric variables using pearson method
corrgram(
  Bike_renting[, numeric_index],
  order = F,
  upper.panel = panel.pie,
  text.panel = panel.txt,
  main = "Corelation plot"
)
corr_Bike_Renting <-
  data.frame(
    Bike_renting$cnt,
    Bike_renting$temp,
    Bike_renting$atemp,
    Bike_renting$windspeed,
    Bike_renting$hum
  )
cor(corr_Bike_Renting)
#-->It can be seen that 'temp and 'atemp' col are extremely corelated wrt cnt column so we will kepe only 1 of them i.e. temp
factor_index = sapply(Bike_renting, is.factor)
#-->Simillar to numeric index make factor index...i.e. index of factor variables in our data
factor_data = Bike_renting[, factor_index]
#-->create a dataframe having only factor data
for (i in 1:8) {
  print(i)
  print(names(factor_data[i]))
  anova_one_way <-
    aov(Bike_renting$cnt ~ factor_data[, i], data = factor_data)
  print(summary(anova_one_way))
}
#-->Apply anova test to determine which categorical variables to exclude.As can be seen from summary we can exclude dteday,weekday
Bike_renting = subset(Bike_renting, select = -c(instant, dteday, weekday, atemp))
#-->As can be seen from corelation,cnt is almost same as registered.So we will predict registered and casual and not count
corr_Bike_Renting <-
  data.frame(Bike_renting$cnt,
             Bike_renting$registered,
             Bike_renting$casual)
cor(corr_Bike_Renting)
Bike_renting_cnt = subset(Bike_renting, select = -c(registered, casual))
#-->creating train and test datasets #-->Divide data into train and test.Since target variable is continous hence go for Simple Random Sampling
train_cnt_index = sample(1:nrow(Bike_renting_cnt),
                         0.8 * nrow(Bike_renting_cnt))
train_cnt = Bike_renting_cnt[train_cnt_index, ]
test_cnt = Bike_renting_cnt[-train_cnt_index, ]
#-->using decision tree algo
fit = rpart(c(cnt) ~ ., data = train_cnt, method = "anova")
predictions_DT = predict(fit, test_cnt[, -c(10)])
MAPE = function(y, yhat) {
  mean(abs((y - yhat) / y)*100)
}
MAPE(test_cnt[, c(10)], predictions_DT)
#-->only cnt-->24.4% error
#---------------------------------------------------------------------------------------------------------------------------------------------
#-->using random forest
RF_Model <- randomForest(cnt~.,train_cnt,ntree = 900, importance = TRUE)
varImpPlot(RF_Model)
imp <- importance(RF_Model)
RF_Predictions <- predict(RF_Model,test_cnt[, -c(10)])
MAPE(test_cnt[, c(10)], RF_Predictions)
plot(RF_Predictions)
#-->only cnt-->16.74 % error after increasing trees to 700
#---------------------------------------------------------------------------------------------------------------------------------------------
#-->using linear  regression
vifcor(Bike_renting_cnt[,c(7:9)],th=0.9)
#--->No collinearity problem and hence we can go for linear regression
lm_model=lm(cnt~.,data=train_cnt)
summary(lm_model)
predictions_LR=predict(lm_model,test_cnt[, -c(10)])
MAPE(test_cnt[, c(10)], predictions_LR)
plot(lm_model)
#-->only cnt-->16.78% error
#---------------------------------------------------------------------------------------------------------------------------------------------
#As can be seen above best accuracy is for Random forest and hence we take that as our final model
#---------------------------------------------------------------------------------------------------------------------------------------------
results_R <- data.frame(test_cnt, pred_cnt = RF_Predictions)

write.csv(results_R, file = 'Random_forest_output_R.csv', row.names = FALSE, quote=FALSE)
#Write results of Ranfom forest in csv






