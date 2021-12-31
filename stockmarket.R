library(rvest)
library(tidyverse)
library(dplyr)
library(nse2r)


nse_index_list()

nse_index_quote()

nse_index_quote(clean_names = FALSE)

x <- nse_stock_top_gainers()
x[5,3]

write.csv(x[5,], "A19BDS0022.csv")
nse_fo_top_gainers()
nse_infy()


Stock <- read_html("https://finance.yahoo.com/quote/MRF.NS?p=MRF.NS&.tsrc-fin-srch") 
htmlText <- Stock %>%html_nodes ("span") %>%html_text()



directory <- getwd()
directory

#INOFSYS - NSE
Stock <- read_html("https://finance.yahoo.com/quote/INFY.NS?p=INFY.NS&.tsrc=fin-srch")
htmlText <- Stock %>%html_nodes ("span") %>%html_text()
df <- htmlText
write.csv(df, "19BDS00227.csv")

glimpse(df)

#2- TECH.MAhi - 
# Stock <- read_html("https://finance.yahoo.com/quote/TECHM.NS?p=TECHM.NS&.tsrc=fin-srch")
# htmlText <- Stock %>%html_nodes ("span") %>%html_text()
# 
# 
# write.csv(df, "C:\\Users\\aryan\\Desktop\\FALL- 3RD YR\\Programming for data science\\Lab\\Stock market\\19BDS0022.csv.xlsx")

dff <- read.csv("https://raw.githubusercontent.com/anthoniraj/dsr_datasets_final/main/19BDS0022.csv")
dff

library(caTools)
library(caret)      # several functions that attempt to 
# streamline the model building and 
# evaluation process, as well as feature
# selection and other techniques.            

library(e1071)      # For  Naive Bayes Model             


# TEST DATA WITH REGULAR SAMPLE
train_normal_sample <- dff[1:90, ]
test_normal_sample <- dff[81:160, ]


# SPLITTING DATASET INTO TRAIN AND TEST DATA

split <- sample.split(dff, SplitRatio = 0.67)


train_data <- subset(dff, split == TRUE)

test_data <- subset(dff, split == FALSE)




#observed_data <- test_data$open

# FIR=T THE TRAIN DATA INTO MODEL
nb_model <- naiveBayes(open ~., data = train_data)
nb_model



# PREDICTING ON TEST DATA
y_pred <- predict(nb_model, newdata=test_data)
y_pred 

# WE ARE CALC THE ACCURACY OF THE CLASSIFIER

observed_data <- test_data$open
predicted_data <- y_pred
mean(observed_data == predicted_data)

# MEAN OF LOGICAL VALUES
logic <- (observed_data == predicted_data)
num <- as.numeric(logic)
mean(num)

# Model evaluation
library(caret)
library(Metrics)

summary(dff)
 
# Multivariate Linear regression

mmodel <- lm(valueChange ~price+open, data=dff )

#PREDICT SALARY FOR GIVEN EXPERIENCE AND KNOWLEDGE LEVEL
pred <-predict(mmodel, data.frame(price= 1797, open=  1796))

#Sum of Squared Error (SSE)
# Most simple for evaluating a regression model.
sum_squared_error <- sse(test_data$valueChange,pred )
sum_squared_error

#Mean Squared Error (MSE)
#Use the mse command to calculate MSE in R.
mean_squared_error <- mse(test_data$valueChange,pred )
mean_squared_error

# Relative Squared Error
# Here we use rse function to compute relative squared error.

relative_squared_error <- rse(test_data$valueChange,pred)
relative_squared_error

# Mean Absolute Error
# For Mean absolute Error calculation, we use mae function.
# Represents the difference between the original and predicted values extracted by averaged the absolute difference over the data set.

mean_abs_error <- mae(test_data$valueChange,pred)
mean_abs_error

# Relative Absolute Error
# We use rae function to calculate relative absolute error.
relative_abs_error <- rae(test_data$valueChange,pred)
relative_abs_error

# Coefficient of Determination (R2)
# Among the most commonly used measure for evaluating regression models
Y_test<- test_data$valueChange
error <- Y_test - pred
R2=1-sum(error^2)/sum((Y_test- mean(Y_test))^2)
R2

# # MAE (Mean absolute error) represents the difference between the original and predicted values extracted by averaged the absolute difference over the data set.
# mae(test_data$valueChange,pred)
# 
# # MSE (Mean Squared Error) represents the difference between the original and predicted values extracted by squared the average difference over the data set.
# # MSE(predicted, original)
# 
# # RMSE (Root Mean Squared Error) is the error rate by the square root of MSE.
# RMSE(predicted_data, observed_data)
# 
# # R-squared (Coefficient of determination) represents the coefficient of how well the values fit compared to the original values. The value from 0 to 1 interpreted as percentages. The higher the value is, the better the model is.
# R2(predicted_data, observed_data, form = "traditional")

library(graphics) 
library(ggplot2)
# 
# open_vector <- as.vector()
# 
# barplot(dff, main = " STOCK MARKET INFOSYS ANALYSIS",
#             xlab ="open",
#             ylab = "close",
#               horiz = TRUE)

# ?barplot
?boxplot

boxplot ( valueChange ~ percentageChange, data = dff, xlab= "VALUE_CHANGE",
          ylab= "PERCENTAGECHANGE", main= "STOCK MARKET ANALYSIS")

boxplot ( open ~ close, data = dff, xlab= "VALUE_CHANGE",
          ylab= "PERCENTAGECHANGE", main= "STOCK MARKET ANALYSIS")

u <- ggplot(data=dff, aes(x=open, y=close,
                             colour= price))
u +geom_boxplot(size=1.2) +geom_point()

#SCATTERPLOT
w <-ggplot(data=dff, aes(x=open,
                            y=close,
                            colour=Date))
w +geom_point(size=3)

#geometries, how will we add geometries over geometries

#----------plotting with layers
p <-ggplot(data=dff, aes(x=open,
                         y=close,
                         colour=Date))
p+geom_point()

#lines
p + geom_line()

#mulitiple layers
p+ geom_point() + geom_line()
p+ geom_line()+ geom_point()
p+ geom_abline() +geom_boxplot()
p+ geom_abline() + geom_point()
p+ geom_point()+ geom_boxplot()



t<- ggplot(data=dff)
t +geom_histogram(binwidth=10,
                  aes(x=price),
                  fill="White", colour="Blue")

t <- ggplot()
t
