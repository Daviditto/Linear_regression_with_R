df <- read.csv('student-mat.csv', sep = ';')
head(df)
any(is.na(df))
str(df)
str(df)
library('ggplot2')
library('dplyr')

num.cols <- sapply(df, is.numeric) # get the numeric columns, bool type
head(df[, num.cols]) 
corr.data <- cor(df[, num.cols]) # this gets the real data, use the cor function
corr.data

install.packages('corrgram')
install.packages('corrplot')
library(corrplot)
library(corrgram)

print(corrplot(corr.data, method='color')) # require filtering out the numeric columns
corrgram(corr.data) # pass the dataframe directly

??corrgram

#Examples:
corrgram(df, order=TRUE,
         main="Auto data (PC order)", # this is the title,
         lower.panel=corrgram::panel.ellipse,
         upper.panel=panel.bar, diag.panel=panel.minmax,
         col.regions=colorRampPalette(c("darkgoldenrod4", "burlywood1",
                                        "darkkhaki", "darkgreen")))
corrgram(df, order=TRUE,
         main="Baseball data PC2/PC1 order", # this is the title,
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)


# check the G3 colomn in the dataset
ggplot(df, aes(x=G3)) +geom_histogram(fill='blue',color='black',bins=20, alpha=0.5) + theme_bw()

# split the dataset into train and test set
install.packages('caTools')
library(caTools)
set.seed(101)

# split up the sample
sample <- sample.split(df$G3, SplitRatio = 0.7)
# 70% of df is training data
train <- subset(df, sample==TRUE)
# 30% of df is testing data
test <- subset(df, sample==FALSE)

# build the linear model
lm.model <- lm(G3~., train)
print(summary(lm.model))

# plot the residuals
res <- residuals(lm.model) # get the residuals
res <- residuals(lm.model) # another way to get the residuals
res <- as.data.frame(res)
head(res)
ggplot(res, aes(res)) +geom_histogram(color ='black', fill='blue', alpha=0.5)

plot(lm.model)

# predict the data
G3.prediction <- predict(lm.model, test)
result <- cbind(G3.prediction,test$G3)
colnames(result) <- c('prediction', 'test.set')
result <- as.data.frame(result)
head(result)

# take care of the negative prediction, which do not make sense
# first of all, let's define a function
push.to.zero <- function(x) {
  if(x<0){
    return(0)
  } else{
    return(x)
  }
}

# apply the function to our result data
result$prediction <- sapply(result$prediction, push.to.zero)

# mse
mse <- mean((result$test.set - result$prediction)**2)

# rmse
rmse <- mse^0.5

print('mse')
mse
print('rmse')
rmse

# SSE
SSE <- sum((result$test.set - result$prediction)**2)
SST <- sum((mean(df$G3) - result$test.set)^2)

R2 <- 1-SSE/SST
print('R2')
R2 # that's not bad!!!!








