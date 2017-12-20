# -*- coding: utf-8 -*-
"""
Created on Sat Nov 11 23:35:05 2017

@author: Sohan Rai
"""
import pandas as pd
from sklearn import model_selection
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import ExtraTreesClassifier
df= pd.read_csv("C:\\Users\\Sohan Rai\\Documents\\project_dataset\\train_with_undersampling_allcat.csv")
dftest= pd.read_csv("C:\\Users\\Sohan Rai\\Documents\\project_dataset\\test_without_smote_mj_allcat.csv")
id=df['id']
target=df['target']
del df['id']
del df['target']
n=len(df)
X=df
Y=target
idtest=dftest['id']
del dftest['id']
Xtest=dftest
seed = 7
num_trees = 100
max_features = 34
max_depth= 8
#kfold = model_selection.KFold(n_splits=10, random_state=seed)
model = RandomForestClassifier(n_estimators=num_trees, max_features=max_features,max_depth=max_depth )
#results = model_selection.cross_val_score(model, X, Y, cv=kfold)
#print(results)
#a=model_selection.cross_val_predict()
#print(results.mean())
#results
#model1 = ExtraTreesClassifier(n_estimators=250,random_state=0)
#model1.fit(X, Y)
#pred1=model1.predict(Xtest)
#print(sum(pred1))
model.fit(X,Y)
pred=model.predict(Xtest)
print(sum(pred))
probs=model.predict_proba(Xtest)
output={'id':idtest,
        'target':probs[:,1]}
output=pd.DataFrame.from_dict(output)
output.to_csv("C:\\Users\\Sohan Rai\\Documents\\project_dataset\\output_5.csv",index=False)
