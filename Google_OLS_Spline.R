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
smp1 = sample(nrow(tr),50000)
tr_dmp = tr[smp1,]

#Ran a 50000 sample of the entire dataset through linear regression
#Performed model selection using the inbuilt step function, and then modelling performed for the entire dataset
lreg = lm(tr_dmp$transactionRevenue ~. -fullVisitorId, data = tr_dmp)
summary(lreg)

"
Call:
lm(formula = tr_dmp$transactionRevenue ~ . - fullVisitorId, data = tr_dmp)

Residuals:
    Min      1Q  Median      3Q     Max 
-81.063  -0.027  -0.005   0.013   8.425 

Residual standard error: 0.672 on 49764 degrees of freedom
Multiple R-squared:  0.8551,	Adjusted R-squared:  0.8544 
F-statistic:  1250 on 235 and 49764 DF,  p-value: < 2.2e-16
"

step(lreg, direction = "both")

"
Multiple steps as output, one of the steps depicted below:
Step:  AIC=-39905.46
tr_dmp$transactionRevenue ~ channelGrouping + date + hits + pageviews + 
bounces + newVisits + sessionQualityDim + timeOnSite + transactions + 
medium

Df Sum of Sq    RSS    AIC
<none>                                          22491 -39905
- bounces                          1         1  22492 -39905
+ isMobile                         1         0  22491 -39904
- timeOnSite                       1         2  22493 -39903
+ deviceCategory                   2         0  22491 -39902
+ adwordsClickInfo.adNetworkType   2         0  22491 -39902
- newVisits                        1         3  22494 -39901
+ continent                        5         2  22489 -39900
+ adwordsClickInfo.slot            3         0  22491 -39900
+ operatingSystemnew               4         0  22491 -39898
+ adwordsClickInfo.page            5         0  22491 -39896
- medium                           4         8  22499 -39896
- hits                             1         9  22500 -39887
+ browser                         11         0  22491 -39884
- channelGrouping                  5        14  22505 -39884
- date                             1        15  22506 -39874
+ subContinent                    21         4  22487 -39871
- pageviews                        1        22  22513 -39858
+ campaign                        25         1  22490 -39857
+ adContent                       45         9  22482 -39835
+ source                         106         6  22485 -39707
- sessionQualityDim                1       156  22647 -39563
- transactions                     1    103311 125802  46172

"

#Final Linear regression model
lreg_final = lm(formula = tr$transactionRevenue ~ channelGrouping + test + browser 
                + isMobile + subContinent + hits + pageviews + newVisits + sessionQualityDim + timeOnSite + medium + adwordsClickInfo.slot + operatingSystemnew, data = tr)
summary(lreg_final)

"
Call:
lm(formula = tr$transactionRevenue ~ channelGrouping + test + 
browser + isMobile + subContinent + hits + pageviews + newVisits + 
sessionQualityDim + timeOnSite + medium + adwordsClickInfo.slot + 
operatingSystemnew, data = tr)

Residuals:
Min      1Q  Median      3Q     Max 
-54.193  -0.168   0.051   0.138  21.238 

Residual standard error: 1.667 on 1708278 degrees of freedom
Multiple R-squared:  0.1825,	Adjusted R-squared:  0.1824 
F-statistic:  6573 on 58 and 1708278 DF,  p-value: < 2.2e-16

"

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

"
Call:
lm(formula = transactionRevenue ~ hits + pageviews + sessionQualityDim + 
timeOnSite, data = tr_dmp1)

Residuals:
Min       1Q   Median       3Q      Max 
-15.1261  -0.1358   0.1239   0.1582  20.4895 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)       -2.902e-01  9.120e-03 -31.820  < 2e-16 ***
hits              -6.843e-02  4.579e-03 -14.944  < 2e-16 ***
pageviews          2.005e-01  6.581e-03  30.464  < 2e-16 ***
sessionQualityDim  3.422e-02  1.033e-03  33.139  < 2e-16 ***
timeOnSite        -1.213e-04  2.836e-05  -4.277  1.9e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.661 on 49995 degrees of freedom
Multiple R-squared:  0.1869,	Adjusted R-squared:  0.1868 
F-statistic:  2873 on 4 and 49995 DF,  p-value: < 2.2e-16
"

#Analyzing different dof for the spline models, checking R-squared values as a measure to find the best model
lreg_chk = lm(transactionRevenue ~ bs(hits,3) + bs(pageviews,3) + bs(sessionQualityDim,3) + bs(timeOnSite,3), data = tr_dmp1)
summary(lreg_chk)

"
Call:
lm(formula = transactionRevenue ~ bs(hits, 3) + bs(pageviews, 
3) + bs(sessionQualityDim, 3) + bs(timeOnSite, 3), data = tr_dmp1)

Residuals:
Min       1Q   Median       3Q      Max 
-16.7185  -0.0980   0.0093   0.1132  21.4841 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                -0.009283   0.009126  -1.017  0.30907    
bs(hits, 3)1               -0.506559   0.751668  -0.674  0.50037    
bs(hits, 3)2              -37.926822   2.043530 -18.559  < 2e-16 ***
bs(hits, 3)3              -24.143095   2.543262  -9.493  < 2e-16 ***
bs(pageviews, 3)1           2.777331   0.567049   4.898 9.72e-07 ***
bs(pageviews, 3)2          46.526513   1.321466  35.208  < 2e-16 ***
bs(pageviews, 3)3          25.774067   1.994847  12.920  < 2e-16 ***
bs(sessionQualityDim, 3)1  -3.461861   0.188833 -18.333  < 2e-16 ***
bs(sessionQualityDim, 3)2   3.141147   0.329382   9.536  < 2e-16 ***
bs(sessionQualityDim, 3)3   6.239432   0.272672  22.883  < 2e-16 ***
bs(timeOnSite, 3)1          0.673273   0.219226   3.071  0.00213 ** 
bs(timeOnSite, 3)2         -2.130373   0.747218  -2.851  0.00436 ** 
bs(timeOnSite, 3)3         -9.788481   1.179658  -8.298  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.622 on 49987 degrees of freedom
Multiple R-squared:  0.2243,	Adjusted R-squared:  0.2241 
F-statistic:  1205 on 12 and 49987 DF,  p-value: < 2.2e-16
"

lreg_chk = lm(transactionRevenue ~ bs(hits,4) + bs(pageviews,4) + bs(sessionQualityDim,4) + bs(timeOnSite,4), data = tr_dmp1)
summary(lreg_chk)

"
Call:
lm(formula = transactionRevenue ~ bs(hits, 4) + bs(pageviews, 
4) + bs(sessionQualityDim, 4) + bs(timeOnSite, 4), data = tr_dmp1)

Residuals:
Min       1Q   Median       3Q      Max 
-16.6894  -0.0959   0.0371   0.0889  21.4441 

Coefficients: (3 not defined because of singularities)
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                -8.10007    1.73719  -4.663 3.13e-06 ***
bs(hits, 4)1               24.07722    2.54300   9.468  < 2e-16 ***
bs(hits, 4)2               23.52456    2.65540   8.859  < 2e-16 ***
bs(hits, 4)3              -13.74214    3.14598  -4.368 1.26e-05 ***
bs(hits, 4)4                     NA         NA      NA       NA    
bs(pageviews, 4)1         -25.76989    1.99459 -12.920  < 2e-16 ***
bs(pageviews, 4)2         -22.83632    2.12462 -10.748  < 2e-16 ***
bs(pageviews, 4)3          20.62299    2.23197   9.240  < 2e-16 ***
bs(pageviews, 4)4                NA         NA      NA       NA    
bs(sessionQualityDim, 4)1   0.02591    0.01670   1.551    0.121    
bs(sessionQualityDim, 4)2  -3.79903    0.21436 -17.722  < 2e-16 ***
bs(sessionQualityDim, 4)3   3.66760    0.35011  10.476  < 2e-16 ***
bs(sessionQualityDim, 4)4   5.98652    0.28101  21.303  < 2e-16 ***
bs(timeOnSite, 4)1          9.75562    1.17954   8.271  < 2e-16 ***
bs(timeOnSite, 4)2         10.43822    1.12454   9.282  < 2e-16 ***
bs(timeOnSite, 4)3          7.59501    1.70380   4.458 8.30e-06 ***
bs(timeOnSite, 4)4               NA         NA      NA       NA    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.622 on 49986 degrees of freedom
Multiple R-squared:  0.2245,	Adjusted R-squared:  0.2243 
F-statistic:  1113 on 13 and 49986 DF,  p-value: < 2.2e-16
"

lreg_chk = lm(transactionRevenue ~ bs(hits,5) + bs(pageviews,5) + bs(sessionQualityDim,5) + bs(timeOnSite,5), data = tr_dmp1)
summary(lreg_chk)

"
Call:
lm(formula = transactionRevenue ~ bs(hits, 5) + bs(pageviews, 
5) + bs(sessionQualityDim, 5) + bs(timeOnSite, 5), data = tr_dmp1)

Residuals:
Min       1Q   Median       3Q      Max 
-15.8454  -0.0284   0.0174   0.0174  20.6101 

Coefficients: (4 not defined because of singularities)
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                -0.5296     1.7613  -0.301 0.763658    
bs(hits, 5)1               25.8937     2.5432  10.182  < 2e-16 ***
bs(hits, 5)2               26.0399     2.5417  10.245  < 2e-16 ***
bs(hits, 5)3               23.3605     2.6478   8.823  < 2e-16 ***
bs(hits, 5)4               -7.0991     3.1518  -2.252 0.024302 *  
bs(hits, 5)5                    NA         NA      NA       NA    
bs(pageviews, 5)1         -27.7387     2.0005 -13.866  < 2e-16 ***
bs(pageviews, 5)2         -27.8880     1.9976 -13.961  < 2e-16 ***
bs(pageviews, 5)3         -21.4262     2.1183 -10.115  < 2e-16 ***
bs(pageviews, 5)4          12.7200     2.2604   5.627 1.84e-08 ***
bs(pageviews, 5)5               NA         NA      NA       NA    
bs(sessionQualityDim, 5)1  -5.8886     0.2805 -20.990  < 2e-16 ***
bs(sessionQualityDim, 5)2  -5.8525     0.2842 -20.596  < 2e-16 ***
bs(sessionQualityDim, 5)3  -9.8888     0.2513 -39.349  < 2e-16 ***
bs(sessionQualityDim, 5)4  -2.3017     0.5941  -3.874 0.000107 ***
bs(sessionQualityDim, 5)5       NA         NA      NA       NA    
bs(timeOnSite, 5)1          8.2916     1.1817   7.017 2.30e-12 ***
bs(timeOnSite, 5)2          8.0472     1.1839   6.797 1.08e-11 ***
bs(timeOnSite, 5)3          9.3020     1.1267   8.256  < 2e-16 ***
bs(timeOnSite, 5)4          4.3466     1.7118   2.539 0.011115 *  
bs(timeOnSite, 5)5              NA         NA      NA       NA    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.619 on 49983 degrees of freedom
Multiple R-squared:  0.2277,	Adjusted R-squared:  0.2274 
F-statistic: 920.8 on 16 and 49983 DF,  p-value: < 2.2e-16
"

#Analyzing the different Spline models based on R-squared and p-values, we can observe that degree of freedom 3 is optimal for hits, pageviews, and timeOnSite, whereas degree of freedom 4 is optimal sessionQualityDim

#Fitting a spline model to the variables in earlier finalized linear model
lreg_spline = lm(formula = tr$transactionRevenue ~ channelGrouping + test + browser  + isMobile + subContinent + bs(hits,3) + bs(pageviews,3) + newVisits + bs(sessionQualityDim,4) + bs(timeOnSite,3) + medium + adwordsClickInfo.slot + operatingSystemnew, data = tr)
summary(lreg_spline)

"
Call:
lm(formula = tr$transactionRevenue ~ channelGrouping + test + 
browser + isMobile + subContinent + bs(hits, 3) + bs(pageviews, 
3) + newVisits + bs(sessionQualityDim, 4) + bs(timeOnSite, 
3) + medium + adwordsClickInfo.slot + operatingSystemnew, 
data = tr)

Residuals:
Min       1Q   Median       3Q      Max 
-27.1801  -0.1895   0.0450   0.1489  24.0280 

Residual standard error: 1.633 on 1708269 degrees of freedom
Multiple R-squared:  0.2154,	Adjusted R-squared:  0.2154 
F-statistic:  7001 on 67 and 1708269 DF,  p-value: < 2.2e-16
"

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