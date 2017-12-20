# -*- coding: utf-8 -*-
"""
Created on Sat Nov 11 23:35:05 2017

@author: Sohan Rai
"""
import pandas as pd
from sklearn import model_selection
from sklearn.ensemble import RandomForestClassifier
#df= pd.read_csv("C:\\Users\\Sohan Rai\\Documents\\project_dataset\\test_filtered_normal.csv")
df= pd.read_csv("C:\\Users\\Sohan Rai\\Documents\\project_dataset\\train_with_undersampling.csv")
ones=df.loc[df['target'] == 1]
zeros=df.loc[df['target'] == 0]
nz=len(zeros)
no=len(ones)
sp=3/2
trainones=ones.iloc[range(int(no/sp)),:]
testones=ones.iloc[int(no/sp):no,:]
trainzeros=zeros.iloc[range(int(nz/sp)),:]
testzeros=zeros.iloc[int(nz/sp):nz,:]
train = pd.concat([trainones.reset_index(drop=True),trainzeros], axis=0)
test=pd.concat([testones.reset_index(drop=True),testzeros], axis=0)
#df['a'].sample(4, replace=True, weights=df['b'])
Y=train['target']
Ytest=test['target']
del train['id']
del train['target']
del test['id']
del test['target']
X=train
Xtest=test
seed = 7
num_trees = 100
max_features = 34
max_depth=34
#kfold = model_selection.KFold(n_splits=10, random_state=seed)
model = RandomForestClassifier(n_estimators=num_trees, max_features=max_features,max_depth=max_depth,class_weight="balanced")
#results = model_selection.cross_val_score(model, X, Y, cv=kfold)
#print(results.mean())
#results
model.fit(X,Y)
pred=model.predict(Xtest)
probs=model.predict_proba(Xtest)
match=[i for i, j in zip(pred, Ytest) if i == j]
accuracy=100*len(match)/len(Ytest)
print(accuracy)
