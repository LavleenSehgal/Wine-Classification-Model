#' Loading Libraries 
#' 
## ------------------------------------------------------------------------
## Loading libraries
library(ggplot2)
library(dplyr)
library(ggpubr)
library(GGally)
library(tidyverse)
library(repr) 
library(scales)
library(memisc)
library(MASS)
library(klaR)
library(caret)


#' Load Dataset
#' 
## ------------------------------------------------------------------------
#Load dataset
wine = read.csv("wine.data.txt")
colnames(wine) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                    'Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')
head(wine)

#' 
#' Is there any NA value we should clean before we start analyzing data?
#' 
## ------------------------------------------------------------------------
# Get summary stats
summary(wine)


#' 
#' Type
#' 
#' Since it's the target attribute. Let us first understand its distribution through a bar chart.
#' 
#' 
#' 
## ------------------------------------------------------------------------
## Changing plot size to 4x3 
options(repr.plot.width=4, repr.plot.height=3)
## Plotting the histogram
table(wine$Type)
ggplot(aes(x=Type), data = wine) + 
  geom_bar(fill = 'steelblue', colour='darkgrey', alpha= 0.8)

#' From the above chart, we can notice that class 2 observations are higher than the other 2 class.
#' 
#' We are creating a function to plot histogram, boxplot and scatter plot together to analysis variables.
#' 
## ------------------------------------------------------------------------
wine_attribute <- function(attribute, varName = '', bins = 30) {
  ## Building the histogram:
  histogram <- ggplot(data = wine) +
    geom_histogram(aes(x=attribute), bins = bins,
                   fill = 'steelblue', colour='darkgrey', alpha= 0.8) +
    labs(x = varName)
  ## Histogram scaling y_log10:
  histYlog <- histogram + scale_y_log10() +
    labs(y = 'log10(count)', x= varName)
  
  ## Histogram scaling x_log10:
  histXlog <- histogram + scale_x_log10() + 
    labs(x = paste('log10(', varName,')'))
  
  ## Building the boxplot highlighting the outliers:
  outliers <- ggplot(wine, aes(x = 1, y = attribute)) + 
    geom_jitter(alpha = 0.1 ) +
    geom_boxplot(alpha = 0.2, color = 'red') + 
    labs(x ='distance from mean', y= varName)
  
  ## Arranging all the plots:
  histPlots <- ggarrange(histogram, histXlog, histYlog, ncol=1, nrow=3)
  ggarrange(outliers, histPlots,ncol=2, widths = c(1,1.5))
}

#' Alcohol
#' 
#' This attribute refers to the percent alcohol content of the wine (% of volume).
#' 
## ------------------------------------------------------------------------
## plot size to 7.5 x 3 
options(repr.plot.width=7.5, repr.plot.height=3)
## How the "alcohol" attribute is distributed?
wine_attribute(wine$Alcohol, varName = 'Alcohol (% of vol)')

#' 
#' We can notice from the boxplot the data points are highly distributed and From the histogram, we can see the distribution of the data points for each class in the "Alcohol" attribute.
#' 
#' Malic Acid
#' 
#' 
## ------------------------------------------------------------------------
## plot size to 7.5 x 3 
options(repr.plot.width=7.5, repr.plot.height=3)
## How the "alcohol" attribute is distributed?
wine_attribute(wine$Malic, varName = 'Malic (mg/L)')

#' 
#' From the boxplot, this attribute is skewed left side and there are few outliers in this attribute.
#' 
#' 
#' 
#' Ash
#' 
#' 
#' 
## ------------------------------------------------------------------------
## plot size to 7.5 x 3 
options(repr.plot.width=7.5, repr.plot.height=3)
## How the "alcohol" attribute is distributed?
wine_attribute(wine$Ash, varName = 'Ash')

#' 
#' From the boxplot, we can notice the data points are near to mean and there are few outliers in this attribute.
#' 
#' Alcalinity
#' 
## ------------------------------------------------------------------------
## plot size to 7.5 x 3 
options(repr.plot.width=7.5, repr.plot.height=3)
## How the "alcohol" attribute is distributed?
wine_attribute(wine$Alcalinity, varName = 'Alcalinity')
2+3

#' 
#' We can notice from boxplot there are few outliers in this attribute. From the histogram, we can see the distribution of the data points for each class in the "Alcalinity" attribute.
#' 
#' Bivariate Analysis
#' 
## ------------------------------------------------------------------------
## plot size to 6 x 4 
options(repr.plot.width=6, repr.plot.height=4)  
ggcorr(wine[,1:14], geom = "blank", label = TRUE, 
       hjust = 0.9, layout.exp = 2) +
  geom_point(size = 8, aes(color = coefficient > 0, 
                           alpha = abs(coefficient) > 0.35)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

#' 
#' From the correlation matrix we notice the only relevant attributes to " Type" 
#' #are Malic, Alcalinity, Phenols, Flavanoids, Nonflavanoids, Proanthocyanins, 
#' #Hue, Dilution and Proline. We can notice most of the variables contribute on
#' # target variable and There are some relevant correlation among other 
#' #variables as well. It will be insteresting to take a look on them trying 
#' #to find out some new information or insight.
#' 
#' we can take only relevant attributes to the target variable for further analysis.
#' We create some pairplots relating all these chosen attributes
#' 
## ------------------------------------------------------------------------
options(repr.plot.width=12, repr.plot.height=12)  #Setting the plot size

theme_set(theme_minimal(10))
## Defining the variables we will explore:
variables <- c('Type', 'Malic', 'Alcalinity', 'Phenols', 'Flavanoids', 'Nonflavanoids', 'Proanthocyanins', 'Hue', 'Dilution','Proline')

## Plotting ggpairs for the selected attributed:
ggpairs(wine[variables], aes(alpha=0.3))

#' 
#' The pair plots above summarize most of the information.
#' # We can notice how much two attributes correlated each other.
#' 
#' 0.514% positively Alcalinity attribute is correlated with target attribute
#' 
#' Alcalinity Vs Type
#' 
## ------------------------------------------------------------------------
#plot size
options(repr.plot.width=6, repr.plot.height=4)  #Setting the plot size
ggplot(aes(x= factor(Type), y= Alcalinity), data = wine) +
  geom_jitter( alpha = .3) +
  geom_boxplot( alpha = .5,color = 'blue')+
  stat_summary(fun.y = "mean", geom = "point", color = "darkblue", 
               shape = 4, size = 4) +
  labs(x= 'Type',
       y= 'Alcalinity',
       title= 'Alcalinity Vs Type')


#' We can notice that if the alcalinity level is below 16, which kind of wine might belongs to Type 1 or Type 2 and some range of Alcalinity is overlapping each other type and few Alcalinity values are very high which is belongs to Type we can say.
#' 
#' Malic Vs Type
#' 
#' 
## ------------------------------------------------------------------------
#plot size
options(repr.plot.width=6, repr.plot.height=4)  #Setting the plot size
ggplot(aes(x= factor(Type), y= Malic), data = wine) +
  geom_jitter( alpha = .3) +
  geom_boxplot( alpha = .5,color = 'blue')+
  stat_summary(fun.y = "mean", geom = "point", color = "darkblue", 
               shape = 4, size = 4) +
  labs(x= 'Type',
       y= 'Malic',
       title= 'Malic Vs Type')

#' We can notice that the malic values of Type 1 and Type 2 are overlapping and it might correlate with other attributes and Type 3 contains malic acid value higher than others. Anyway, we cannot decide the type of wine based on Malic acid value easily. 
#' 
#' Nonflavanoids Vs Type
#' 
## ------------------------------------------------------------------------
#plot size
options(repr.plot.width=6, repr.plot.height=4)  #Setting the plot size
ggplot(aes(x= factor(Type), y= Nonflavanoids), data = wine) +
  geom_jitter( alpha = .3) +
  geom_boxplot( alpha = .5,color = 'blue')+
  stat_summary(fun.y = "mean", geom = "point", color = "darkblue", 
               shape = 4, size = 4) +
  labs(x= 'Type',
       y= 'Nonflavanoids',
       title= 'Nonflavanoids Vs. Type')

#' We can notice that the Nonflavanoids level of Type 1 is low. We might say low Nonflavanoids value wine belongs to Type 1 while high Nonflavanoids value wine belongs to Type 3. 
#' 
#' Flavanoids Vs Type
#' 
## ------------------------------------------------------------------------
options(repr.plot.width=6, repr.plot.height=4)  #Setting the plot size
ggplot(aes(x= factor(Type), y= Flavanoids), data = wine) +
  geom_jitter( alpha = .3) +
  geom_boxplot( alpha = .5,color = 'blue')+
  stat_summary(fun.y = "mean", geom = "point", color = "darkblue", 
               shape = 4, size = 4) +
  labs(x= 'Type',
       y= 'Flavanoids',
       title= 'Flavanoids Vs Type')

#' 
#' We can notice that the flavanoids level of Type 1 is high while the flavanoids value of Type 3 is low. We might classify three types of wine based on the Flavanoids value of the wine. 
#' 
#'  
#' We will check some attribute is correlated to each other even if they do not have a direct impact on target. 
#'  
#' Flavanoids Vs Dilution
## ------------------------------------------------------------------------
ggplot(aes(y = Flavanoids, x = Dilution), data = wine) + 
  geom_point(alpha = 0.35, pch = 1) + 
  geom_smooth(method = loess) + 
  labs(y= 'Flavanoids',
       x= 'Dilution',
       title= 'Relationship of Flavanoids Vs Dilution')

#' 
#' These two attributes are correlated highly. When the value of Flavanoids attribute is low, the value of Dilution attribute is also low while the value of Flavanoids attribute might be high, the value of Dilution attribute is also high.
#' 
#' 
#' We will check how these tow attribute correlation effects on wine type.
#' 
## ------------------------------------------------------------------------
ggplot(data=wine, aes(x=Dilution, y=Flavanoids, col=Type)) + geom_point() +labs(
  title= 'Relationship of Dilution Vs. Flavanoids accordingly to Type')


#' 
#' We can notice how two attributes are correlated with wine types. 
#' Value of both attribute is high in a wine, it might belongs to wine type 1. 
#' 
#' Based on EDA, We found important variable to classify wine types. 
#' we might say 'Malic', 'Alcalinity', 'Phenols', 'Flavanoids', 'Nonflavanoids', and  'Dilution' are important to classify wien types.
#' 
#' We will use LDA to classify wine types.
#' 
#' 
#' Model Development
#' 
#' Data type of target variable is converted to factor type.
## ------------------------------------------------------------------------
wine$Type <- as.factor(wine$Type)

#' 
## ------------------------------------------------------------------------
#Preparing the Data
#sample(c(TRUE, FALSE), 10, replace = T, prob = c(0.7,0.3))
set.seed(1234)
A = sample(c(TRUE, FALSE), nrow(wine), replace = T,prob = c(0.8,0.2))
sum(A)
table(A)
143/177
training_sample <- sample(c(TRUE, FALSE), nrow(wine), replace = T, prob = c(0.8,0.2))

#K = c(2,3,4,5)
#K[c(T,T,F,F)]
#logic= c(T,T,F,F)

K[!logic]
train <- wine[training_sample, ]
test <- wine[!training_sample, ]

#' 
## ------------------------------------------------------------------------
#Applying LDA
lda.wine <- lda(Type ~ ., wine)
lda.wine #show results



#' 
#' Prior probabilities of groups:
#' 
#' 32.76% of training observations are wine which is belongs to class1.
#' 40.11 of training observations are wine which is belongs to class2.
#' 27.1% of training observations are wine which is belongs to class3.
#' 
#' Group means:
#' 
#' These are the average of each predictor within each class, and are used by LDA as estimates of mean. These suggest that Alcohol in class 2 is 1% lower than the other 2 class and Malic in class 3 is 2% higher than the other 2 class. Nonflavanoids and Proanthocyanins are more or less equall to all class.
#' 
#' Coefficients of linear discriminants:
#' 
#' LD1 is a linear combination of 13 predictor and LD2 is an another linear combination of 13 predictor
#' 
#' Proportion of trace
#' 
#' LD1 shows 68.73% variance among wine types and LD2 shows 31.27 among wine types
#' 
#' Visualize LD1 and LD2
## ------------------------------------------------------------------------
#plot type
plot(lda.wine, col = as.integer(wine$Type))

#' We can notice how wine types are classified by LDA.
#' 
#' We will see training and testing of model
#' 
#' Train the model
#' 
## ------------------------------------------------------------------------
wine_train <- train(Type ~ ., method = "lda", data = train)


#' Training Accuracy
## ------------------------------------------------------------------------
confusionMatrix(train$Type, predict(wine_train, train))

#' 
#' Predictions on test data
#' 
## ------------------------------------------------------------------------
predict_values <- predict(wine_train,test)
predict_values


#' 
#' Testing Accuracy
#' 
## ------------------------------------------------------------------------
confusionMatrix(test$Type, predict(wine_train, test))