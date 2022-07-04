import pandas as pd
import numpy as np
import math
import eli5
from eli5.sklearn import PermutationImportance
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV
import random

import _pickle as cPickle

from sklearn.metrics import roc_auc_score

threshold = [8,9,10,11,12,13]

index_cols = ['CUH']

data_train = pd.read_csv("CUH_SmartDR_20210727_mask_tensor_merged_missforest_train.txt",sep="\t",header=0,index_col=index_cols)
data_test = pd.read_csv("CUH_SmartDR_20210727_mask_tensor_merged_missforest_test.txt",sep="\t",header=0,index_col=index_cols)

n_rep = 10

for i in range(n_rep):

    for th in threshold:

        data_train_th = data_train.copy()
        data_test_th = data_test.copy()

        def get_xy(df_):
            X = df_.drop(['PostTiterLog2'],axis=1)
            y = [1 if df_['PostTiterLog2'].values[i] >= th else 0 for i in range(len(df_['PostTiterLog2']))]
            
            return X, y

        X_train, y_train = get_xy(data_train)
        X_test, y_test = get_xy(data_test)

        scoring = "roc_auc"

        params = {
            'class_weight': ["balanced"],
            'bootstrap': [True],
            'max_depth': [None, 2, 3, 4, 5, 6],
            'max_features': ['auto'],
            'max_leaf_nodes': [None],
            'min_impurity_decrease': [0.0],
            'min_impurity_split': [None],
            'min_weight_fraction_leaf': [0.0],
            'n_estimators': [4000],
            'n_jobs': [2],
            'oob_score': [False],
            'random_state': [random.seed(i)],
            'verbose': [0],
            'warm_start': [False]
        }


        gs = GridSearchCV(RandomForestClassifier(), params, scoring=scoring, verbose=2, n_jobs=6)
        gs.fit(X_train, y_train)

        with open("CUH_SmartDR_rfclass_upper"+str(th)+"_rep"+str(i)+"_GridSearchCV.sav", 'wb') as f:
        	cPickle.dump(gs, f)

        clf = gs.best_estimator_

        perm = PermutationImportance(clf, n_iter=10)
        perm.fit(X_test, y_test)

        imp_summary = pd.DataFrame(perm.feature_importances_)
        imp_summary.index = X_train.columns

        imp_summary.to_csv("CUH_SmartDR_rfclass_upper"+str(th)+"_rep"+str(i)+"_varimp.txt",sep="\t")

        data_train_th["predicted_PostTiterLog2"] = clf.predict_proba(X_train)[:,1]
        data_test_th["predicted_PostTiterLog2"] = clf.predict_proba(X_test)[:,1]

        data_train_th.to_csv("CUH_SmartDR_train_rfclass_upper"+str(th)+"_rep"+str(i)+"_predict.txt", index=False, sep="\t")
        data_test_th.to_csv("CUH_SmartDR_test_rfclass_upper"+str(th)+"_rep"+str(i)+"_predict.txt", index=False, sep="\t")
