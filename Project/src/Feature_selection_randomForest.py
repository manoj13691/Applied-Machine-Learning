# -*- coding: utf-8 -*-
"""
Created on Sat Nov 11 23:52:03 2017

@author: Sohan Rai
"""

# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
print(__doc__)

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn.ensemble import RandomForestClassifier

# Build a classification task using 3 informative features
"""
X, y = make_classification(n_samples=1000,
                           n_features=10,
                           n_informative=3,
                           n_redundant=0,
                           n_repeated=0,
                           n_classes=2,
                           random_state=0,
                           shuffle=False)
"""
df = pd.read_csv('''/Users/manekbahl/Downloads/train.csv''')
dftest = pd.read_csv('''/Users/manekbahl/Downloads/test.csv''')
m=len(df.columns)
n=len(df)
id=df['id']
target=df['target']
#testid=dftest['id']
#testtarget=dftest['target']
del df['id']
del df['target']
#del dftest['id']
#del dftest['target']
X=df
y=target
# Build a forest and compute the feature importances
forest = RandomForestClassifier(n_estimators=250,random_state=0)

forest.fit(X, y)
importances = forest.feature_importances_
std = np.std([tree.feature_importances_ for tree in forest.estimators_],
             axis=0)
indices = np.argsort(importances)[::-1]

# Print the feature ranking
print("Feature ranking:")

for f in range(X.shape[1]):
    print("%d. feature %d (%f)" % (f + 1, indices[f], importances[indices[f]]))
for f in range(X.shape[1]):    
    if (importances[indices[f]]<1.0/m):
        k=f-1
        break
#k=27  #Manually selected after examining the graph obtained above based on the level of significance needed by us  
newindices=indices[k:len(indices)]
df.drop(df.columns[newindices],axis=1,inplace=True)
dftest.drop(dftest.columns[newindices],axis=1,inplace=True)
# Plot the feature importances of the forest
plt.figure()
plt.title("Feature importances")
plt.bar(range(X.shape[1]), importances[indices],
       color="r", yerr=std[indices], align="center")
plt.xticks(range(X.shape[1]), indices)
plt.xlim([-1, X.shape[1]])
#plt.figure(figsize=(40,20))
plt.show()
id=pd.DataFrame(id)
result = pd.concat([id,target, df], axis=1)
df.to_csv('''/Users/manekbahl/Downloads/test.csvtrain_filtered_reduced_new.csv''',index=False)
dftest.to_csv('''/Users/manekbahl/Downloads/test.csvtest_filtered_reduced_new.csv''',index=False)
newindices
np.savetxt('''/Users/manekbahl/Downloads/importances.csv''', importances, delimiter=",")
np.savetxt('''/Users/manekbahl/Downloads/indices.csv''', indices, delimiter=",")
