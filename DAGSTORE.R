---
title: "Project Gstore"
author: "Priyanka and Anugraha"
date: "April 6, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
read.csv("/home/arjun/Documents/sample.csv") -> train.df
#rm(train)
#table(train.df$transactionRevenue)
```

##Loading libraries
```{r,message=FALSE}
library(tidyr)
library(dplyr)
library(jsonlite)
library(scales)
library(lubridate)
library(repr)
library(ggrepel)
library(gridExtra)
#library(highcharter)
library(countrycode)
#library(plotly)
#library(tidyverse)
#library(jsonlite)
#library(scales)
#library(lubridate)
##library(repr)
#library(ggrepel)
#library(gridExtra)
library(xgboost)
library(tibble)
```

```{r}
train.df$date <- ymd(train.df$date)
str(train.df$date)
train.df$visitStartTime <- as.POSIXct(train.df$visitStartTime, tz="UTC", origin='1970-01-01')
glimpse(train.df)
```
```{r}
options(repr.plot.height=4)
NAcol <- which(colSums(is.na(train.df)) > 0)
NAcount <- sort(colSums(sapply(train.df[NAcol], is.na)), decreasing = TRUE)
NADF <- data.frame(variable=names(NAcount), missing=NAcount)
NADF$PctMissing <- round(((NADF$missing/nrow(train.df))*100),1)
NADF %>%
    ggplot(aes(x=reorder(variable, PctMissing), y=PctMissing)) +
    geom_bar(stat='identity', fill='blue') + coord_flip(y=c(0,110)) +
    labs(x="", y="Percent missing") +
    geom_text(aes(label=paste0(NADF$PctMissing, "%"), hjust=-0.1))
str(train.df$transactionRevenue)
train.df$transactionRevenue <- as.integer(train.df$transactionRevenue)
#table(train.df$transactionRevenu
#y <- as.integer(train.df$transactionRevenue)
#table(y)
```

```{r}
#setting missing values to zero
train.df$transactionRevenue[is.na(train.df$transactionRevenue)] <- 0
y <- train.df$transactionRevenue #saving original values in a vector
train.df$transactionRevenue <- train.df$transactionRevenue/1000000
#table(train.df$transactionRevenue)
```

```{r}
train.df %>% filter(train.df$transactionRevenue >0) %>% summarize('number of transactions'=n(), 'total revenues train set'=sum(train.df$transactionRevenue))

```


```{r}
range(train.df %>% select(transactionRevenue) %>% filter(transactionRevenue !=0))
train.df %>% filter(transactionRevenue>=1000) %>% summarize('number of transactions with at least 1000 USD revenues'=n(), 'sum revenues of transactions with at least 1000 USD revenues'=sum(transactionRevenue))

```

```{r}
options(repr.plot.height=4)
train.df %>% filter(transactionRevenue >0 & transactionRevenue<1000) %>%
ggplot(aes(x=transactionRevenue)) +
        geom_histogram(fill="blue", binwidth=10) +
        scale_x_continuous(breaks= seq(0, 1000, by=100), labels = comma)


```
range(train.df$date)

```{r}
options(repr.plot.height=6)
d1 <- train.df %>% group_by(date) %>% summarise(dailySessions = n()) %>%
    ggplot(aes(x=date, y=dailySessions)) + geom_line(col='blue') +
    scale_y_continuous(labels=comma) + geom_smooth(col='red') +
    labs(x="", y="Sessions per Day") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
d2 <- train.df %>% group_by(date) %>% summarise(dailyRevenue = sum(transactionRevenue)) %>%
    ggplot(aes(x=date, y=dailyRevenue)) + geom_line(col='blue') +
    scale_y_continuous(labels=comma) + geom_smooth(col='red') +
    labs(x="", y="Daily Revenues (USD)") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
grid.arrange(d1,d2)
range(train.df$date)

```

```{r}
train.df$weekday <- wday(train.df$date, label = TRUE)
str(train.df$weekday)
```

```{r}
plotSessions <- function(dataframe, factorVariable, topN=10) {
    var_col <- enquo(factorVariable)
    dataframe %>% count(!!var_col) %>% top_n(topN, wt=n) %>%
    ggplot(aes_(x=var_col, y=~n, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="number of sessions")+
    theme(legend.position="none")
    }

#also creating a function to plot transactionRevenue for a factorvariable
plotRevenue <- function(dataframe, factorVariable, topN=10) {
    var_col <- enquo(factorVariable)
    dataframe %>% group_by(!!var_col) %>% summarize(rev=sum(transactionRevenue)) %>% filter(rev>0) %>% top_n(topN, wt=rev) %>% ungroup() %>%
    ggplot(aes_(x=var_col, y=~rev, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="Revenues (USD)")+
    theme(legend.position="none")
    }

```

```{r}
options(repr.plot.height=4)
w1 <- plotSessions(train.df, weekday)
w2 <- plotRevenue(train.df, weekday)
grid.arrange(w1, w2)
```

```{r}
options(repr.plot.height=4)

train.df$month <- month(train.df$date, label=TRUE)
#test$month <- month(test$date, label=TRUE)


m1 <- plotSessions(train.df, month, 12)
m2 <- plotRevenue(train.df, month, 12)
grid.arrange(m1, m2)

```
```{r}
options(repr.plot.height=6)

#adding reordering of x manually
sessionOrder <- train.df %>% count(channelGrouping) %>% top_n(10, wt=n) %>% arrange(desc(n))
sessionOrder <- sessionOrder$channelGrouping

c1 <- plotSessions(train.df, channelGrouping) + scale_x_discrete(limits=sessionOrder)
c2 <- plotRevenue(train.df, channelGrouping) + scale_x_discrete(limits=sessionOrder)
grid.arrange(c1, c2)

```
```{r}
#works with the normal aes() but does seem to work with aes_q()

plotSessionsFlip <- function(dataframe, factorVariable, topN=10) {
    var_col <- enquo(factorVariable)
    x <- dataframe %>% count(!!var_col) %>% top_n(topN, wt=n) %>% arrange(n)
    y <- x[[1]]
    x %>% ggplot(aes_(x=var_col, y=~n)) + coord_flip() +
    geom_bar(stat='identity', fill="orange")+
    scale_y_continuous(labels=comma)+
    labs(x="", y="number of sessions")+
    theme(legend.position="none") +
    scale_x_discrete(limits=y)
    }

plotRevenueFlip <- function(dataframe, factorVariable, topN=10) {
    var_col <- enquo(factorVariable)
    x <- dataframe %>% group_by(!!var_col) %>% summarize(rev=sum(transactionRevenue)) %>% filter(rev>0) %>% top_n(topN, wt=rev) %>% arrange(rev) %>% ungroup()
    y <- x[[1]]
    x %>% ggplot(aes_(x=var_col, y=~rev)) + coord_flip() +
    geom_bar(stat='identity', fill="orange")+
    scale_y_continuous(labels=comma)+
    labs(x="", y="Revenues (USD)")+
    theme(legend.position="none") +
    scale_x_discrete(limits=y)
    }

```

```{r}
train.df$sourceMedium <- paste(train.df$source, train.df$medium, sep="/")
s1 <- plotSessionsFlip(train.df, sourceMedium, 20) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
s2 <- plotRevenueFlip(train.df, sourceMedium, 20) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(s1, s2, nrow=1)

```

```{r}
options(repr.plot.height=5)
d1 <- plotSessions(train.df, deviceCategory)
d2 <- plotRevenue(train.df, deviceCategory)
grid.arrange(d1, d2)


```

```{r}
options(repr.plot.height=5)

sessionOrder <- train.df %>% filter(operatingSystem != "(not set)") %>% count(operatingSystem) %>% top_n(7, wt=n) %>% arrange(desc(n))
sessionOrder <- sessionOrder$operatingSystem

o1 <- plotSessions(train.df %>% filter(operatingSystem != "(not set)"), operatingSystem, 7) + scale_x_discrete(limits=sessionOrder)
o2 <- plotRevenue(train.df, operatingSystem) + scale_x_discrete(limits=sessionOrder)
grid.arrange(o1, o2)


```

```{r}

options(repr.plot.height=5)

d1 <- plotSessionsFlip(train.df, browser)
d2 <- plotRevenueFlip(train.df, browser)
grid.arrange(d1, d2)

```


```{r}
options(repr.plot.height=4)
#sessions with more than 28 pageviews all have frequencies of less than 1,000. Since these are hardly visible, I am excluding them.
#excluding 100 pageview NAs

p1 <- train.df %>% filter(!is.na(train.df$pageviews) & pageviews <=28) %>% 
ggplot(aes(x=pageviews)) +
    geom_histogram(fill='blue', binwidth=1) +
    scale_y_continuous(breaks=seq(0, 500000, by=100000), label=comma) +
    scale_x_continuous(breaks=seq(0, 28, by=5)) +
    coord_cartesian(x=c(0,28))

p2 <- train.df %>% filter(!is.na(train.df$pageviews) & pageviews <=28) %>% group_by(pageviews) %>%
    ggplot(aes(x=pageviews, y=transactionRevenue)) +
    geom_bar(stat='summary', fun.y = "sum", fill='blue') +
    scale_x_continuous(breaks=seq(0, 28, by=5)) +
    coord_cartesian(x=c(0,28)) + labs(y="sum of revenues")
grid.arrange(p1, p2)


```

```{r}
p1 <- train.df %>% filter(!is.na(train.df$pageviews) & pageviews <=28) %>% group_by(pageviews) %>%
    ggplot(aes(x=pageviews, y=transactionRevenue)) +
    geom_bar(stat='summary', fun.y = "mean", fill='blue') +
    scale_x_continuous(breaks=seq(0, 28, by=5)) +
    coord_cartesian(x=c(0,28)) + labs(y="mean of revenues")
p2 <- train.df %>% filter(!is.na(train.df$pageviews) & pageviews <=28) %>% group_by(pageviews) %>%
    ggplot(aes(x=pageviews, y=transactionRevenue)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue') +
    scale_x_continuous(breaks=seq(0, 28, by=5)) +
    coord_cartesian(x=c(0,28)) + labs(y="median of revenues")
grid.arrange(p1, p2)

```

```{r}
train.df %>% filter(!is.na(train.df$pageviews) & pageviews <=28 & transactionRevenue>0) %>% 
ggplot(aes(x=pageviews)) +
    geom_histogram(fill='blue', binwidth=1) +
    scale_x_continuous(breaks=seq(0, 28, by=5)) +
    coord_cartesian(x=c(0,28)) +
    labs(y='number of session with transaction revenue')

```


```{r}
p1 <- train.df %>% filter(!is.na(train.df$pageviews) & pageviews <=28 & transactionRevenue>0) %>% group_by(pageviews) %>%
    ggplot(aes(x=pageviews, y=transactionRevenue)) +
    geom_bar(stat='summary', fun.y = "mean", fill='blue') +
    scale_x_continuous(breaks=seq(0, 28, by=5)) +
    coord_cartesian(x=c(0,28)) + labs(y="mean of revenues") +
    geom_label(stat = "count", aes(label = ..count..), y=0, size=2)
p2 <- train.df %>% filter(!is.na(train.df$pageviews) & pageviews <=28 & transactionRevenue>0) %>% group_by(pageviews) %>%
    ggplot(aes(x=pageviews, y=transactionRevenue)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue') +
    scale_x_continuous(breaks=seq(0, 28, by=5)) +
    coord_cartesian(x=c(0,28)) + labs(y="median of revenues") +
    geom_label(stat = "count", aes(label = ..count..), y=0, size=2)
grid.arrange(p1, p2)

```


```{r}
train.df$transaction <- ifelse(train.df$transactionRevenue > 0, 1, 0)

train.df %>% filter(!is.na(train.df$pageviews) & pageviews <=100) %>% group_by(pageviews) %>%
summarize('sessions'= n(), 'transactions'= sum(transaction), 'pctTransactions'=round(x=((transactions/sessions)*100),digits=1)) %>%
    ggplot(aes(x=pageviews, y=pctTransactions)) +
    geom_bar(stat='identity', fill='blue') +
    scale_x_continuous(breaks=seq(0, 100, by=5)) +
    geom_smooth()


```


```{r}

#setting missing values to zero (percent missing in train is 50.1%)
train.df$bounces[is.na(train.df$bounces)] <- 0L

train.df %>% filter(bounces==1) %>% summarize('number of bounces'=n(), 'revenues bounce sessions'=sum(transactionRevenue))


train.df %>% filter(pageviews==1) %>% summarize('number of 1-page sessions'=n(), 'revenues 1-page session'=sum(transactionRevenue))

h1 <- train.df %>% filter(hits <=200) %>% group_by(hits) %>%
summarize('sessions'= n(), 'transactions'= sum(transaction), 'pctTransactions'=round(x=((transactions/sessions)*100),digits=1)) %>%
    ggplot(aes(x=hits, y=pctTransactions)) +
    geom_bar(stat='identity', fill='blue') +
    geom_smooth()

#grid.arrange(h1)
train.df$ratioPageHits <- train.df$pageviews/train.df$hits

h2 <- train.df %>% filter(!is.na(pageviews)) %>%
    ggplot(aes(x=ratioPageHits, y=transactionRevenue)) +
    geom_line(stat='identity', col='blue') +
    coord_cartesian(y=c(0,100)) +
    scale_x_continuous(breaks=seq(0, 1, by=0.1))

#grid.arrange( h2)


c1 <- plotSessionsFlip(train.df, country, 20)
c2 <- plotRevenueFlip(train.df, country, 20)
grid.arrange(c1, c2, nrow=1)

train.df %>% group_by(country) %>%
summarize(sessions= n(), transactions=sum(transaction), totalRev=sum(transactionRevenue), sessionMean=totalRev/sessions, transactionMean=totalRev/transactions) %>%
filter(sessions>100 & transactions>10) %>% top_n(20, wt=sessionMean) %>% arrange(desc(sessionMean))
```
```{r}

train.df <- train.df %>% select(-one_of(c("campaignCode", "transaction", "ratioPageHits")))

fea_uniq_values <- sapply(train.df, n_distinct)
(fea_del <- names(fea_uniq_values[fea_uniq_values == 1]))

train.df <- train.df %>% select(-one_of(fea_del))
train.df$transactionRevenue <- train.df$transactionRevenue*1000000
train.df$pageviews[is.na(train.df$pageviews)] <- 0L
trainIndex <- 1:nrow(train.df)
splitTrainValidDate <- ymd("20170701")
splitTrainTestDate <-ymd("20180201")

```


```{r}
train.df$transactionRevenue <- log1p(train.df$transactionRevenue) 
is_na_val <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")

train.df <- train.df %>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))
```

```{r}

train.df <- train.df %>% mutate(isMobile = ifelse(isMobile, 1L, 0L),
            isTrueDirect = ifelse(isTrueDirect, 1L, 0L),
            adwordsClickInfo.isVideoAd = ifelse(!adwordsClickInfo.isVideoAd, 0L, 1L))
#There seems to be an issue with date and POSIXct after rbind
train.df$visitStartTime <- as.POSIXct(train.df$visitStartTime, tz="UTC", origin='1970-01-01')
train.df$date <- as.Date(train.df$date, origin='1970-01-01')
#new feature
train.df$sessionHourOfDay <- hour(train.df$visitStartTime)

train.df <- train.df %>%
select(-fullVisitorId, -visitId, -visitStartTime) %>% 
mutate_if(is.character, factor)
categorical_feature <- names(Filter(is.factor, train.df))

#Label encoding
train.df <- train.df %>% 
  mutate_if(is.factor, as.integer) %>% 
  glimpse()
```


```{r}
#splitting all into train, validation, and test set
#dtrain <- [trainIndex,]
#dtest <- all[-trainIndex,]
d <- train.df[train.df$date >= splitTrainValidDate,]
dtrain <- train.df[train.df$date < splitTrainValidDate,]
dtest <- d[d$date >= splitTrainTestDate,]
dval <- d[d$date < splitTrainTestDate,]
dtest$date <- NULL
dtrain$date <- NULL
dval$date <- NULL

trainLabel <- dtrain$transactionRevenue
valLable <- dval$transactionRevenue
test.origin <- dtest$transactionRevenue
dtrain$transactionRevenue<-NULL
dval$transactionRevenue<-NULL
```

```{r}
set.seed(123)
library(lightgbm)
lgb.train = lgb.Dataset(data=as.matrix(dtrain),label=trainLabel, categorical_feature =categorical_feature)
lgb.valid = lgb.Dataset(data=as.matrix(dval),label=valLable, categorical_feature =categorical_feature)

params <- list(objective="regression",
              metric="rmse",
              learning_rate=0.01)

lgb.model <- lgb.train(params = params,
                       data = lgb.train,
                       valids = list(val = lgb.valid),
                       learning_rate=0.01,
                       nrounds=1000,
                       verbose=1,
                       early_stopping_rounds=50,
                       eval_freq=100
                      )

```
