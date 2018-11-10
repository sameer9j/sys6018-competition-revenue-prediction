# -*- coding: utf-8 -*-
"""
Created on Thu Nov  8 14:11:40 2018

@author: nicol
"""

import pandas as pd 
import numpy as np 

test= pd.read_csv("C:/Users/nicol/OneDrive/Desktop/2018 Fall/SYS 6018/Google_Analytics/test_after.csv")
train= pd.read_csv("C:/Users/nicol/OneDrive/Desktop/2018 Fall/SYS 6018/Google_Analytics/train_after.csv")

y = train['transactionRevenue']
X =train.drop(["fullVisitorId","transactionRevenue"],axis=1)  


from sklearn.linear_model import LinearRegression
from sklearn.linear_model import Lasso
from sklearn.ensemble import RandomForestRegressor 
from pyearth import Earth



regression_OLS=LinearRegression()
regression_Lasso=Lasso(precompute=True,max_iter=10000,alpha=3.0)
regression_RF=RandomForestRegressor(max_leaf_nodes=100,max_features=100)
#regression_SVR=SVR(kernel='rbf', C=1e3, gamma=0.1)
regression_spline = Earth()

from sklearn import model_selection
X_train, X_test, y_train, y_test = model_selection.train_test_split(X, y, test_size=0.2)

print('training data has %d observation with %d features'% X_train.shape)
print('test data has %d observation with %d features'% X_test.shape)

from sklearn.metrics import mean_squared_error,explained_variance_score 

model_names = ['Linear Regression OLS','Linear Regression Lasso','Random Forest','Spline_regression']
model_list = [regression_OLS,regression_Lasso, regression_RF,regression_spline]
count = 0
for regression in model_list:
    model = regression.fit(X_train,y_train)
    y_preds = model.predict(X_test)
    loss = mean_squared_error(y_test,y_preds)
    variance = explained_variance_score(y_test,y_preds)
    print('Model mean squared error of %s is: %.3f'%(model_names[count],loss))
    print('Model explained variance error of %s is: %.3f'%(model_names[count],variance))
    count += 1


from sklearn.model_selection import GridSearchCV
def print_grid_search_best_result(gs):
    print ("Best parameters set:")
    best_parameters = gs.best_params_
    for param_name in sorted(parameters.keys()):
        print("\t%s: %r" % (param_name, best_parameters[param_name]))

from sklearn.metrics import make_scorer
import math
def root_mean_square_error(y_true, y_pred):
     mse = mean_squared_error(y_true,y_pred)
     return math.sqrt(mse)
rmse_score=make_scorer(root_mean_square_error,greater_is_better=False)
      
parameters = {
    'normalize':('True', 'False') 
}
Grid_LR = GridSearchCV(LinearRegression(),parameters, cv=5,scoring=rmse_score)
Grid_LR.fit(X_train, y_train)    
print_grid_search_best_result(Grid_LR)
Grid_LR.cv_results_
best_LR_model = Grid_LR.best_estimator_

parameters = {
    'normalize':('True', 'False'),
    'alpha':[0.5,1,1.5]   
}

Grid_Lasso=GridSearchCV(Lasso(),parameters,cv=5,scoring=rmse_score)
Grid_Lasso.fit(X_train,y_train)
print_grid_search_best_result(Grid_Lasso)
Grid_Lasso.cv_results_
best_Lasso_model = Grid_Lasso.best_estimator_

#Random Forest took so long, let us leave it as default for now
#Same for the spline method 

'''Compare the improved models ''' 
model_names = ['Optimized Linear Regression OLS','Optimized Linear Regression Lasso','Random Forest','Spline_regression']
model_list = [best_LR_model,best_Lasso_model, regression_RF,regression_spline]
count = 0
for regression in model_list:
    model = regression.fit(X_train,y_train)
    y_preds = model.predict(X_test)
    loss = mean_squared_error(y_test,y_preds)
    variance = explained_variance_score(y_test,y_preds)
    print('Model mean squared error of %s is: %.3f'%(model_names[count],loss))
    print('Model explained variance error of %s is: %.3f'%(model_names[count],variance))
    count += 1
    


#Random Forest still gives the best performance. 

model1=regression_RF.fit(X_train,y_train)
y_final = test['transactionRevenue']
X_final = test.drop(["fullVisitorId","transactionRevenue"],axis=1)
X_final.shape
y_preds = model1.predict(X_final)
test['transactionRevenue'] = y_preds
test['fullVisitorId']=test['fullVisitorId'].astype(str)
test['fullVisitorId']=test['fullVisitorId'].transform(lambda x: int(x))
test["fullVisitorId"] = test["fullVisitorId"].astype('category')

submission =pd.DataFrame(test.groupby('fullVisitorId')['transactionRevenue'].sum())
submission1= pd.read_csv("./Data/sample_submission.csv")
submission1['merge']=submission1["fullVisitorId"].apply(lambda x : int(x))
submission['fullVisitorId']=submission.index
submission.index=range(len(submission.index))
submission.columns =["PredictedLogRevenue","merge"]
a = pd.merge(submission1, submission, on='merge')
a = a.drop(["PredictedLogRevenue_x","merge"],axis=1)
a.columns=["fullVisitorId","PredictedLogRevenue"]
a.to_csv('nh_submission.csv',index=False)


