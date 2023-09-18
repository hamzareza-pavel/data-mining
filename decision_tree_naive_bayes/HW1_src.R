# author Hamza Reza Pavel
# student id: 1001041797

# Group specific values
# seed: 06271993
# K-value for folding: 6
# Train/test split: 70/30
library(dplyr)
library(caTools)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(e1071)
library(klaR)
library(psych)
library(mlbench)
library(MLeval)
library(cvTools)
#read the csv

df=read.csv("census-adult.txt", header=FALSE, sep=",", strip.white = TRUE)
#print(dim(df))

#preprocessing steps

df[df == "?"] <- NA
df[df == "Holand-Netherlands"] <- NA
cleaned_df = na.omit(df)
#print(dim(cleaned_df))

#str(cleaned_df)

nm1 <- c("V2", "V4", "V6", "V7", "V8", "V9", "V14") 
cleaned_df[nm1] <- lapply(cleaned_df[nm1], gsub, pattern = "-", replacement = "_")
cleaned_df["V15"] <- lapply(cleaned_df["V15"], gsub, pattern = "<=50K", replacement = "LTE50K")
cleaned_df["V15"] <- lapply(cleaned_df["V15"], gsub, pattern = ">50K", replacement = "UT50K")

## The conversion
cleaned_df[sapply(cleaned_df, is.character)] <- lapply(cleaned_df[sapply(cleaned_df, is.character)], 
                                       as.factor)

#str(cleaned_df)
#split the dataset into training and testing set

set.seed(06271993)
split_sample = sample.split(cleaned_df, SplitRatio = .70) #doing the 70-30 split
train=subset(cleaned_df, sample == TRUE)
test= subset(cleaned_df, sample == FALSE)

#print(dim(train))

#print(dim(test))


#decision tree using gini index



gini_decisionTree <- rpart(
  V15 ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14, 
  data = train, 
  method = "class",
  parms = list(split = 'gini'))

#print(gini_decisionTree)

#testoutput <- predict(gini_decisionTree, newdata = test, type = "class")

#print(testoutput)


#cv for gini based tree
tcontrol <- trainControl(method = "cv",
                              number = 10,
                              classProbs=TRUE,
                              savePredictions = TRUE
                              )

rpart.cv <- train(V15 ~ ., 
                  data = cleaned_df,
                  method = "rpart",
                  trControl = tcontrol,
                  tuneLength = 10,
                  parms = list(split = 'gini'))

#testoutputgini <- predict(rpart.cv, newdata = test, type = "raw")

#res_gini = confusionMatrix(testoutputgini, test$V15)
#print(res_gini)
#importance <- varImp(rpart.cv, scale=FALSE)
#print(importance)
#plot(importance)
print(rpart.cv)

ig_decisionTree <- train(V15 ~ ., 
                         data = cleaned_df,
                         method = "rpart",
                         trControl = tcontrol,
                         tuneLength = 10,
                         parms = list(split = 'information'))


#print(ig_decisionTree)

#get individual level count of factors
#res = cleaned_df %>%group_by(V14) %>% summarise(Count = n())

#print(res)
#naive bayes


x = cleaned_df[,-14]
y = cleaned_df$V15
nvb.cv = train(x, y, method='nb', trControl=tcontrol)

#testoutputnb <- predict(nvb.cv, newdata = test, type = "raw")
#res_nb = confusionMatrix(testoutputnb, test$V15)
#print(res_nb)
print(nvb.cv)


#Plotting ROC Curve
#res <- evalm(list(rpart.cv,ig_decisionTree,nvb.cv),gnames=c('gini','ig','nb'), plots = 'r', rlinethick = 0.08, fsize=8)

#print(res)

pairs.panels(cleaned_df, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)



#All plotting goes here
ib50k = (cleaned_df$V15 == "LTE50K")
xlimit = c (min (cleaned_df$V1), max (cleaned_df$V1))
ylimit = c (0, 3000)

incomegraph1 = qplot (V1, data = cleaned_df[ib50k,], margins = TRUE,
               binwidth = 3, xlim = xlimit, ylim = ylimit,fill=I("red"),  col=I("black"), colour = V15)

incomegraph2 = qplot (V1, data = cleaned_df[!ib50k,], margins = TRUE,
               binwidth = 3, xlim = xlimit, ylim = ylimit, fill=I("green"),  col=I("black"), colour = V15)


edu = qplot (V15, data = cleaned_df, fill = V4) + facet_grid (. ~ V4)

genderplot = qplot (V15, data = cleaned_df, fill = V10) + facet_grid (. ~ V10)

raceplot = qplot (V15, data = cleaned_df, fill = V9) + facet_grid (. ~ V9)

workplot = qplot (V15, data = cleaned_df, fill = V7) + facet_grid (. ~ V7)


workclass = qplot (V15, data = cleaned_df, fill = V2) + facet_grid (. ~ V2)





#cv for gini based tree
tcontrol <- trainControl(method = "cv",
                              number = 6,
                              classProbs=TRUE,
                              savePredictions = TRUE
                              )

gini_tree <- train(V15 ~ ., 
                  data = train,
                  method = "rpart",
                  trControl = tcontrol,
                  tuneLength = 6,
                  parms = list(split = 'gini'))

testoutputnb <- predict(gini_tree, newdata = test, type = "raw")
res_gini = confusionMatrix(testoutputnb, test$V15, positive='LTE50K')

precision <- posPredValue(testoutputnb, test$V15, positive="LTE50K")
recall <- sensitivity(testoutputnb, test$V15, positive="LTE50K")

F1 <- (2 * precision * recall) / (precision + recall)

print(gini_tree)
print(precision)
print(recall)
print(F1)
print(res_gini)



x = test[,-14]
y = test$V15
nbc = train(x, y, method='nb', trControl=tcontrol)

testoutputnb <- predict(nbc, newdata = test, type = "raw")
res_nb = confusionMatrix(testoutputnb, test$V15, positive='LTE50K')

precision <- posPredValue(testoutputnb, test$V15, positive="LTE50K")
recall <- sensitivity(testoutputnb, test$V15, positive="LTE50K")

F1 <- (2 * precision * recall) / (precision + recall)

print(nbc)
print(precision)
print(recall)
print(F1)
print(res_gini)



