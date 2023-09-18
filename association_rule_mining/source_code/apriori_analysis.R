library(arules)
library(tidyverse)
library(ggplot2)
library(knitr)
library(RColorBrewer)
library(sets)
library(scatterplot3d)
library(arulesViz)



imdb_transactions <- read.transactions('processed_itemlist.csv', format = 'basket', sep=',', rm.duplicates = TRUE)


summary(imdb_transactions)


rules <- apriori(imdb_transactions, parameter = list(supp=0.003, conf=0.05, maxlen=10, minlen = 1, target = 'frequent'))

rules <- apriori(imdb_transactions, parameter = list(supp=0.003, conf=0.05, minlen = 2, maxlen=10))

#data collection
rules <- apriori(imdb_transactions, parameter = list(supp=0.003, conf=0.08, maxlen=10, target = 'frequent'))
rules <- apriori(imdb_transactions, parameter = list(supp=0.007, conf=0.08, maxlen=10, target = 'frequent'))
rules <- apriori(imdb_transactions, parameter = list(supp=0.012, conf=0.05, minlen = 2, maxlen=2, target = 'frequent'))

l1 <- as(items(rules), "list")
s1 = as.set(l1)
s2 = as.set(l1)
s3 = s1 * s2

#data collection 2
rules <- apriori(imdb_transactions, parameter = list(supp=0.012, conf=0.08, minlen = 2, maxlen=10))
#rules <- apriori(imdb_transactions, parameter = list(supp=0.007, conf=0.08, maxlen=10))
#rules <- apriori(imdb_transactions, parameter = list(supp=0.012, conf=0.05, maxlen=10))




length(rules)


#inspect(rules)


#plotting
itemFrequencyPlot(imdb_transactions, topN=30,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

minsups <- c(0.003, 0.003, 0.003, 0.007, 0.007, 0.007, 0.012, 0.012, 0.012)
confs <- c(0.05, 0.07, 0.08,0.05, 0.07, 0.08,0.05, 0.07, 0.08)
rulescount <- c(386, 379, 367, 163, 161, 154, 99, 98, 97)

dfiv <- data.frame(minsups, confs, rulescount)
names(dfiv) <- c('Min_sup', 'Confidence', 'RuleCount')

scatterplot3d(x = minsups, y=confs, z=rulescount, main="Rules Count for Different min_sup and Conf values", xlab = "Min_sup", ylab = "Confidence)", zlab = "Rules Count")


rules <- apriori(imdb_transactions, parameter = list(supp=0.003, conf=0.05, minlen = 2, maxlen=10))
subRules1<-rules[quality(rules)$lift>1]
subRules1<-head(subRules1, n = 5, by = "support")
inspect(subRules1)

rules <- apriori(imdb_transactions, parameter = list(supp=0.003, conf=0.05, minlen = 2, maxlen=10))
subRules2<-rules[quality(rules)$lift<1]
subRules2<-head(subRules2, n = 5, by = "support")
inspect(subRules2)

rules <- apriori(imdb_transactions, parameter = list(supp=0.003, conf=0.05, minlen = 2, maxlen=10))
subRules3<-rules[quality(rules)$lift==1]
#subRules3<-head(subRules3, n = 5, by = "support")
inspect(subRules3)


rules <- apriori(imdb_transactions, parameter = list(supp=0.003, conf=0.05, minlen = 2, maxlen=10))

subRules <-head(rules, n = 100, by = "support")
inspect(subRules)
plot(subRules)


plot(subRules2, method = "graph",  engine = "htmlwidget")

