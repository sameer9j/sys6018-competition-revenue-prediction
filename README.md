# sys6018-competition-revenue-prediction
In-class kaggle Competition 4: Google Analytics Customer Revenue Prediction

# Team members
Sameer Singh  (SS8GC): Primary data cleaning, OLS Linear modeling (R)
Ning Han      (NH4MQ): Nonlinear modeling (Python)
Andrew Evans  (ACE8P): Data cleaning, competition report

# Data Exploration and Data Cleaning 
Non-categorical data: 
  - date: create week of daty and month of year based on date. 
  - vistiNumber: Highly skewed varaible.Create bins based on the distribution. 
  - hits: Highly skewed variable. Create bins based on the distribution. 
  - adwordsClickInfo.page: fill na as 0. 
  - pageviews: fill na as 0.Create bbins based on the distribution. 
  - transactionRevenue: fill na as 0 and transform it using log. 
  
Categorical data: 
  - Browser: merge small disctribution levels as others.
  - operatingSystems: merge small distribution levels as others. 
  - metrodic: merge small distribution levels as others. 
  - bounces: fill na as 0. 
  - newVisits: fill na as 0. 
  - networkDomain: create clusters based KMeans. 
  - adContent:Merge small distribution level as others.
  - drop adwordsClickInfo.gclId 
  - drop adwordsClickInfo.isVideo 
  - drop campaignCode
  - isTrueDirect: fill na as "False". 
  - keyword: transform it as a uniform format ( lower case, cut off extra whitespace), build clusters for keywords.
  - source: merge small distribution level as others. 
  - drop referralPath 
  - create hasher for region, city, country. 
  - create dummy variables for the categories we pick. 
  
 Export Training and Test datasets:
   - export training_after csv.
   - export test_after csv. 
   
 # Build Models 
   - Create models: Linear Regression (OLS), Lasso, RandomForestRegressor, Earth(Spline)
   - GridSearchCV: Linear Regression, Lasso 
   - Sumbssion: Predict use randomforest model. 
   
   
  
   
  
  
  



