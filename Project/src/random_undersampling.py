# -*- coding: utf-8 -*-
"""
Created on Sun Nov 19 01:22:02 2017

@author: Sohan Rai
"""

import pandas as pd
import numpy as np
input=pd.read_csv("C:\\Users\\Sohan Rai\\Documents\\project_dataset\\train_without_smote_mj_allcat.csv")
tot=len(input)
ones=input.loc[input['target'] == 1]
zeros=input.loc[input['target'] == 0]
nz=len(zeros)
no=len(ones)
m=2*no
newzeros=zeros.take(np.random.permutation(len(zeros))[:m])
output = pd.concat([newzeros.reset_index(drop=True),ones], axis=0)
output.to_csv("C:\\Users\\Sohan Rai\\Documents\\project_dataset\\train_with_undersampling_allcat.csv",index=False)
###################################################################################################