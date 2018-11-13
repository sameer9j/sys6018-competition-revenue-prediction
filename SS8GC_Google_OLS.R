library(readr)
library(jsonlite)
library(tidyverse)
library(feather)

#Specifying the column types
col_types <- cols(
  channelGrouping = col_character(),
  customDimensions = col_character(),
  date = col_datetime(),
  device = col_character(),
  fullVisitorId = col_character(),
  geoNetwork = col_character(),
  hits = col_skip(),
  socialEngagementType = col_skip(),
  totals = col_character(),
  trafficSource = col_character(),
  visitId = col_integer(),
  visitNumber = col_integer(),
  visitStartTime = col_integer()
)

# Convert Python array/dictionary string to JSON format
unsnake <- . %>%
  str_replace_all(c("\\[\\]" = "[{}]", # empty element must contain dictionary
                    "^\\[|\\]$" = "", # remove initial and final brackets
                    "(\\[|\\{|, |: |', )'" = "\\1\"", # open single- to double-quote (on key or value)
                    "'(\\]|\\}|: |, )" = '\"\\1'))

separate_json <- . %>%
  str_replace_all(c("\"[A-Za-z]+\": \"not available in demo dataset\"(, )?" = "",
                    ", \\}" = "}")) %>% # if last property in list was removed
  paste(collapse = ",") %>% paste("[", ., "]") %>% # As fromJSON() isn't vectorised
  fromJSON(., flatten = TRUE)

NMAX = Inf
df <- 
  bind_rows(
    read_csv("../input/ga-customer-revenue-prediction/train_v2.csv", col_types = col_types, n_max = NMAX) %>% mutate(test = F),
  ) %>%
  bind_cols(separate_json(.$device))        %>% select(-device) %>%
  bind_cols(separate_json(.$geoNetwork))    %>% select(-geoNetwork) %>%
  bind_cols(separate_json(.$totals))        %>% select(-totals) %>%
  bind_cols(separate_json(.$trafficSource)) %>% select(-trafficSource) %>%
  bind_cols(separate_json(unsnake(.$customDimensions))) %>% select(-customDimensions)

# Remove irrelevant fields, to save space
df$visits <- NULL
df$adwordsClickInfo.gclId <- NULL

# Identify types
df <-
  df %>%
  mutate_at(vars(hits:transactions, index), as.integer) %>%
  mutate(
    visitStartTime = lubridate::as_datetime(visitStartTime),
    transactionRevenue = as.numeric(transactionRevenue), # Target
    totalTransactionRevenue = as.numeric(transactionRevenue)
  )

df_tt <- 
  bind_rows(
    read_csv("../input/ga-customer-revenue-prediction/test_v2.csv", col_types = col_types, n_max = NMAX) %>% mutate(test = F),
  ) %>%
  bind_cols(separate_json(.$device))        %>% select(-device) %>%
  bind_cols(separate_json(.$geoNetwork))    %>% select(-geoNetwork) %>%
  bind_cols(separate_json(.$totals))        %>% select(-totals) %>%
  bind_cols(separate_json(.$trafficSource)) %>% select(-trafficSource) %>%
  bind_cols(separate_json(unsnake(.$customDimensions))) %>% select(-customDimensions)

# Remove irrelevant fields, to save space
df_tt$visits <- NULL 
df_tt$adwordsClickInfo.gclId <- NULL 

# Identify types
df_tt <-
  df_tt %>%
  mutate_at(vars(hits:transactions, index), as.integer) %>%
  mutate(
    visitStartTime = lubridate::as_datetime(visitStartTime)
  )

#https://www.kaggle.com/burritodan/gstore-1-flatten
df_mod = df
nrow(df_mod)

#Count missing values of the records
na_cnt = as.data.frame(apply(df_mod,2,function(x) sum(is.na(x))))
names(na_cnt) = "Value"
na_cnt$Name = rownames(na_cnt)
na_cnt

#Creating a function to find mode of a character variable
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Substituting missing values
df_mod$adwordsClickInfo.adNetworkType[is.na(df_mod$adwordsClickInfo.adNetworkType)]= "MIS"
df_mod$adwordsClickInfo.slot[is.na(df_mod$adwordsClickInfo.slot)]= "MIS"
df_mod$adwordsClickInfo.page[is.na(df_mod$adwordsClickInfo.page)]= 0
df_mod$adContent[is.na(df_mod$adContent)]= "MIS"
df_mod$transactions[is.na(df_mod$transactions)]= 0
df_mod$transactionRevenue[is.na(df_mod$transactionRevenue)]= 0
df_mod$timeOnSite[is.na(df_mod$timeOnSite)]= 0
df_mod$sessionQualityDim[is.na(df_mod$sessionQualityDim)]= 0
df_mod$pageviews[is.na(df_mod$pageviews)]= Mode(df_mod$pageviews)
df_mod$newVisits[is.na(df_mod$newVisits)]= 0
df_mod$bounces[is.na(df_mod$bounces)]= 0

#Combining different factors into one
browser=c("Chrome","Safari","Firefox","Internet Explorer","Android Webview","Edge","Samsung Internet","Opera Mini","Safari (in-app)","UC Browser","YaBrowser")
df_mod$browser[which(!(df_mod$browser %in% browser))]= "Other"
unique(df_mod$browser)

#Combining different factors into one
os1 = c("Android","Chrome OS")
os2 = c("Samsung","Windows Phone","BlackBerry","Macintosh","iOS")
os3 = c("SunOS","Firefox OS","Linux","Windows","Tizen","FreeBSD","OpenBSD","SunOS")
os4 = c("Playstation Vita","Nintendo 3DS","Nintendo Wi","Xbox","Nintendo WiiU")
df_mod$operatingSystemnew = "Other"
df_mod$operatingSystemnew[which((df_mod$operatingSystem %in% os1))]= "Self"
df_mod$operatingSystemnew[which((df_mod$operatingSystem %in% os2))]= "Competitors"
df_mod$operatingSystemnew[which((df_mod$operatingSystem %in% os3))]= "CompOS"
df_mod$operatingSystemnew[which((df_mod$operatingSystem %in% os4))]= "CompGaming"
unique(df_mod$operatingSystemnew)

#Analyzing the null values in the dataset
na_cnt = as.data.frame(apply(df_mod,2,function(x) sum(is.na(x))))
na_cnt

#Substituting missing values
df_tt$adwordsClickInfo.adNetworkType[is.na(df_tt$adwordsClickInfo.adNetworkType)]= "MIS"
df_tt$adwordsClickInfo.slot[is.na(df_tt$adwordsClickInfo.slot)]= "MIS"
df_tt$adwordsClickInfo.page[is.na(df_tt$adwordsClickInfo.page)]= 0
df_tt$adContent[is.na(df_tt$adContent)]= "MIS"
df_tt$timeOnSite[is.na(df_tt$timeOnSite)]= 0
df_tt$sessionQualityDim[is.na(df_tt$sessionQualityDim)]= 0
df_tt$pageviews[is.na(df_tt$pageviews)]= Mode(df_tt$pageviews)
df_tt$newVisits[is.na(df_tt$newVisits)]= 0
df_tt$bounces[is.na(df_tt$bounces)]= 0

#Combining different factors into one
browser=c("Chrome","Safari","Firefox","Internet Explorer","Android Webview","Edge","Samsung Internet","Opera Mini","Safari (in-app)","UC Browser","YaBrowser")
df_tt$browser[which(!(df_tt$browser %in% browser))]= "Other"
unique(df_tt$browser)

#Combining different factors into one
os1 = c("Android","Chrome OS")
os2 = c("Samsung","Windows Phone","BlackBerry","Macintosh","iOS")
os3 = c("SunOS","Firefox OS","Linux","Windows","Tizen","FreeBSD","OpenBSD","SunOS")
os4 = c("Playstation Vita","Nintendo 3DS","Nintendo Wi","Xbox","Nintendo WiiU")
df_tt$operatingSystemnew = "Other"
df_tt$operatingSystemnew[which((df_tt$operatingSystem %in% os1))]= "Self"
df_tt$operatingSystemnew[which((df_tt$operatingSystem %in% os2))]= "Competitors"
df_tt$operatingSystemnew[which((df_tt$operatingSystem %in% os3))]= "CompOS"
df_tt$operatingSystemnew[which((df_tt$operatingSystem %in% os4))]= "CompGaming"
unique(df_tt$operatingSystemnew)

#Analyzing the null values in the dataset
na_cnt_t = as.data.frame(apply(df_tt,2,function(x) sum(is.na(x))))
na_cnt_t

#Dropping irrelevant columns
drop_irr = c("region","metro","city","country","visitId","visitNumber","visitStartTime","totalTransactionRevenue","operatingSystem","adwordsClickInfo.isVideoAd","index","campaignCode","isTrueDirect","referralPath","keyword","value","networkDomain")
tr = as.data.frame(df_mod[,!names(df_mod) %in% drop_irr])
na_cnt = as.data.frame(apply(tr,2,function(x) sum(is.na(x))))
na_cnt

tt = as.data.frame(df_tt[,!names(df_tt) %in% drop_irr])
na_cnt = as.data.frame(apply(tt,2,function(x) sum(is.na(x))))
na_cnt

tr$transactionRevenue = log(1+tr$transactionRevenue)
smp1 = sample(nrow(tr),100000)
tr_dmp = tr[smp1,]

#Ran a 100000 sample of the entire dataset through linear regression
#Performed model selection using the inbuilt step function, and then modelling performed for the entire dataset
lreg = lm(tr_dmp$transactionRevenue ~. -fullVisitorId, data = tr_dmp)
summary(lreg)
step(lreg, direction = "both")

#Final Linear regression model
lreg_final = lm(formula = tr$transactionRevenue ~ channelGrouping + test + browser 
                + isMobile + subContinent + hits + pageviews + newVisits + sessionQualityDim + timeOnSite + medium + adwordsClickInfo.slot + operatingSystemnew, data = tr)
summary(lreg_final)

#Predicting the test dataset
tt_dmp =tt

tt_dmp$adwordsClickInfo.slot[which((tt_dmp$adwordsClickInfo.slot %in% c("Google search: Other","Google search: Top")))]= "Google Display Network"

pred = as.data.frame(predict(lreg_final, newdata= tt_dmp))

#Taking exponential of the predicted values
pred = exp(pred)

#Creating a dataframe with fullVisitorId and prediction values 
tt_new = as.data.frame(cbind(tt_dmp$fullVisitorId,pred))
names(tt_new) = c("fullVisitorId","pred")

#Aggregating the dataset across users
tt_agg = aggregate(tt_new$pred,by = list(tt_new$fullVisitorId), FUN = sum)
tt_agg

#Taking a log of the revenue
tt_agg$x = log(1+tt_agg$x)

#Writing the table
write.table(tt_agg, file = "c4_basic.csv", row.names=F, col.names=c("fullVisitorId","PredictedLogRevenue"), sep=",")

#Fitting Spline models
library(splines)

smp2 = sample(nrow(tr),50000)
tr_dmp1 = tr[smp2,]
lreg_chk = lm(transactionRevenue ~ hits + pageviews + sessionQualityDim + timeOnSite, data = tr_dmp1)
summary(lreg_chk)

#Analyzing different dof for the spline models, checking R-squared values as a measure to find the best model
lreg_chk = lm(transactionRevenue ~ bs(hits,3) + bs(pageviews,3) + bs(sessionQualityDim,3) + bs(timeOnSite,3), data = tr_dmp1)
summary(lreg_chk)

lreg_chk = lm(transactionRevenue ~ bs(hits,4) + bs(pageviews,4) + bs(sessionQualityDim,4) + bs(timeOnSite,4), data = tr_dmp1)
summary(lreg_chk)

lreg_chk = lm(transactionRevenue ~ bs(hits,5) + bs(pageviews,5) + bs(sessionQualityDim,5) + bs(timeOnSite,5), data = tr_dmp1)
summary(lreg_chk)

#Analyzing the different Spline models based on R-squared and p-values, we can observe that degree of freedom 3 is optimal for hits, pageviews, and timeOnSite, whereas degree of freedom 4 is optimal sessionQualityDim

#Fitting a spline model to the variables in earlier finalized linear model
lreg_spline = lm(formula = tr$transactionRevenue ~ channelGrouping + test + browser  + isMobile + subContinent + bs(hits,3) + bs(pageviews,3) + newVisits + bs(sessionQualityDim,4) + bs(timeOnSite,3) + medium + adwordsClickInfo.slot + operatingSystemnew, data = tr)
summary(lreg_spline)

tt_dmp1 =tt[,]
tt_dmp1$adwordsClickInfo.slot[which((tt_dmp1$adwordsClickInfo.slot %in% c("Google search: Other","Google search: Top")))]= "Google Display Network"

#Predicting the test dataset
pred1 = as.data.frame(predict(lreg_spline, newdata= tt_dmp1))
pred1 = exp(pred1)

#Creating a dataframe with fullVisitorId and prediction values 
tt_new1 = as.data.frame(cbind(tt_dmp1$fullVisitorId,pred1))
names(tt_new1) = c("ID","pred")

#Aggregating the dataset across users
tt_agg1 = aggregate(tt_new1$pred,by = list(tt_new1$ID), FUN = sum)
tt_agg1

#Taking a log of the revenue
tt_agg1$x = log(1+tt_agg1$x)

#Writing the table
write.table(tt_agg1, file = "c4_spline.csv", row.names=F, col.names=c("fullVisitorId","PredictedLogRevenue"), sep=",")