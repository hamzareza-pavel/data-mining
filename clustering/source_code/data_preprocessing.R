# author: Hamza Reza Pavel
# ID: 1001741797
# Group: 2
# Seed Value: 06271993
# Data subset: year-> 2006, 2008, 2009. Months: February, August

#imports go here
library(dplyr)
library(VIM)
library(magrittr)
library(clusteval)


set.seed(06271993)

y2006 <- read.table("hourly_2006.g", sep = "" , header = F, stringsAsFactors= T)
y2008 <- read.table("hourly_2008.g", sep = "" , header = F, stringsAsFactors= T)
y2009 <- read.table("hourly_2009.g", sep = "" , header = F, stringsAsFactors= T)

#read and merge all 3 dataset into a single dataframe
df<-bind_rows(y2006, y2008, y2009)

#handle missing values
df[df == 999.9] <- NA
df[df == 9999.9] <- NA


df$V1 <- as.factor(df$V1)
df$V2 <- as.factor(df$V2)
#summary(df)
#parse and convert date str column to DateTime
df$ParsedTime =  df$V3 %>% strptime(., format="%Y%m%d_%H")

df <- subset(df, select=-c(V2, V3, V6, V8, V10, V12, V14, V15, V17, V19))


#y2006
y2006_Feb=df[df$ParsedTime >= "2006-02-1" & df$ParsedTime < "2006-03-01", ]
y2006_Aug=df[df$ParsedTime >= "2006-08-1" & df$ParsedTime < "2006-09-01", ]
 
#y2008
y2008_Feb=df[df$ParsedTime >= "2008-02-1" & df$ParsedTime < "2008-03-01", ]
y2008_Aug=df[df$ParsedTime >= "2008-08-1" & df$ParsedTime < "2008-09-01", ]

#y2009
y2009_Feb=df[df$ParsedTime >= "2009-02-1" & df$ParsedTime < "2009-03-01", ]
y2009_Aug=df[df$ParsedTime >= "2009-08-1" & df$ParsedTime < "2009-09-01", ]

#define function to aggregate mean of temp, dewP, stp, wsdp for each station

processDatasetByMonthYear <- function(fdf) {
  #remove everything from dataframe except station ID, temp, dewP, stp, wsdp column
  fdf <- subset(fdf, select=-c(V7, V11, V16, V18, ParsedTime))
  oldnames = c("V1","V4","V5","V9","V13")
  newnames =  c("STN","TEMP","DEWP","STP","WSDP")
  fdf <- fdf %>% rename_at(vars(oldnames), ~ newnames)
  grpbydf<- group_by(fdf, STN)
  tempmean <- summarize(grpbydf, mean(TEMP, na.rm = TRUE))
  dewpmean <- summarize(grpbydf, mean(DEWP, na.rm = TRUE))
  stpmean <- summarize(grpbydf, mean(STP, na.rm = TRUE))
  wsdpmean <- summarize(grpbydf, mean(WSDP, na.rm = TRUE))
  ndf<- merge(tempmean,dewpmean,by="STN")
  ndf<- merge(ndf,stpmean,by="STN")
  ndf<- merge(ndf,wsdpmean,by="STN")
  ndf[ndf == NaN] <- NA
  ndf <- na.omit(ndf)
  return(ndf)
}


y2006_Feb <- processDatasetByMonthYear(y2006_Feb)
y2006_Aug <- processDatasetByMonthYear(y2006_Aug)

y2008_Feb <- processDatasetByMonthYear(y2008_Feb)
y2008_Aug <- processDatasetByMonthYear(y2008_Aug)

y2009_Feb <- processDatasetByMonthYear(y2009_Feb)
y2009_Aug <- processDatasetByMonthYear(y2009_Aug)


write.csv(y2006_Feb, "y2006_Feb.csv", row.names = FALSE)
write.csv(y2006_Aug, "y2006_Aug.csv", row.names = FALSE)

write.csv(y2008_Feb, "y2008_Feb.csv", row.names = FALSE)
write.csv(y2008_Aug, "y2008_Aug.csv", row.names = FALSE)

write.csv(y2009_Feb, "y2009_Feb.csv", row.names = FALSE)
write.csv(y2009_Aug, "y2009_Aug.csv", row.names = FALSE)

#km.res1 <- kmeans(y2006_Feb, 4)
#km.res2 <- kmeans(y2006_Aug, 4)

#res = cluster_similarity(km.res1, km.res2, similarity = c("jaccard"))

#temp, dew, stp, wsdp
