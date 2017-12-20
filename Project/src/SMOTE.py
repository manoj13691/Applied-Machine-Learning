# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import matplotlib.pyplot as plt
from sklearn.datasets import make_classification
from sklearn.decomposition import PCA
import pandas as pd
import numpy as np
#__import__("imbalanced-learn.oversampling")
from imblearn.over_sampling import SMOTE
 
X=pd.read_csv("/Users/manekbahl/Downloads/train_without_smote_mj.csv")
y=X['target']
del X['target']
method=SMOTE(ratio = 0.33)
X_resampled = []
y_resampled = []
X_res_vis = []
X_res, y_res = method.fit_sample(X, y)
sum(y_res==1)
sum(y==1)
X_res=pd.DataFrame(X_res)
y_res=pd.DataFrame(y_res)
op=pd.concat([X_res.reset_index(drop=True),y_res],axis=1)
op.to_csv("/Users/manekbahl/Downloads/train_with_smote31.csv",index=True)