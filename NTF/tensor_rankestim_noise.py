#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 17 14:12:48 2020

@author: naoki
"""
import pandas as pd
import numpy as np
import tensorly.decomposition as tsd
from sklearn.preprocessing import MinMaxScaler
mms=MinMaxScaler()
import matplotlib.pyplot as plt
import seaborn as sns
sns.set(font="Hiragino Mincho ProN")
from scipy.stats import pearsonr
from scipy.stats import spearmanr


#Windows
#location='C:/Users/naoqi/OneDrive - 千葉大学/ドキュメント/千葉医/課外活動/ML/小児病態　アレルギー解析/data/'
#Mac var
location='/Users/eiryokawakami/Dropbox/DataAnalysis/Chiba_Vaccine/'

tensor_data=pd.read_excel(location+"data/CUH_SmartDR_20210727.xlsx")
# pickup_data=pd.read_excel(location+"data/df_renew.xlsx")
# pickup_data.drop("Unnamed: 0",axis=1,inplace=True)

index=tensor_data["CUH"]
# sp_index=pickup_data.index
# sp_columns=pickup_data.columns

noise_levels = [0.05, 0.1, 0.2, 0.3, 0.4]

"""
==============================================================================
basic information
==============================================================================
"""

days=["1_1","1_2","1_3","1_4","1_5","1_6","1_7","2_1","2_2","2_3","2_4","2_5","2_6","2_7"]

items = []

for c in tensor_data.columns:
    if "2_7" in c:
        items.append(c.replace("2_7",""))

items.remove("other_symptoms")

"""
==============================================================================
3D_data
==============================================================================
"""
df_3D=[]
na_mask_3D=[]
nacount = pd.Series(np.zeros(len(index)))
nacount.index = index

for d in days:
    df=pd.DataFrame()
    for c in tensor_data.columns:
        if d in c:
            if not c[3].isdigit():
                df[c]=tensor_data[c]
    colnames=[]
    for c in df.columns:
        colnames.append(c.replace(d,""))
    print(colnames)
    print(len(colnames))
    df.columns=colnames
    df.index=index
    df = df.drop("other_symptoms", axis=1)
    na_mask = pd.DataFrame(np.ones(shape=df.shape))
    na_mask.index=index

    nacount = nacount + df.isnull().sum(axis=1)
    na_mask[df.isna()] = 0

    df = df.fillna(float(0))

    df_3D.append(df)
    na_mask_3D.append(na_mask)

plt.hist(nacount)

df_3D_trim = []
na_mask_3D_trim = []

for df in df_3D:
    df = df.loc[nacount < len(items)*len(days)*0.3,:]
    df_3D_trim.append(df)

for na_mask in na_mask_3D:
    print(na_mask)
    print(nacount < len(items)*len(days)*0.3)
    na_mask = na_mask.loc[nacount < len(items)*len(days)*0.3,:]
    na_mask_3D_trim.append(na_mask)
    
"""
df_7=pd.DataFrame()
for j in tensor_data.columns:
    if "7歳" in j:
        df_7[j]=tensor_data[j]
c=[]
for j in df_7.columns:
    a=j.split("_",1)
    c.append(a[1])
df_7.columns=c
df_7.index=index

"""
    

"""
================================================================================

================================================================================
"""
def calc_denoising_loss(data,na_mask,rank_max=10,noise=0.3,n_rep=20,n_iter_max=1000,method="KL",eps=1E-10):
    import math
    import random as rd
    import tensorly.cp_tensor as tsc
    import tensorly.decomposition as tsd
    err_summary=[]
    result=[]
    ages_array=[]
    mask=[]
    a_max=len(data)
    b_max=len(data[0].index)
    c_max=len(data[0].columns)
    for i in data:
        # d_array=mms.fit_transform(i)
        d_array = np.array(i)
        d_array=d_array.tolist()
        ages_array.append(d_array)

    mask=na_mask.copy()
    
    for n in range(n_rep):
        print(n)
        #print(data)
        all_loc=[]
        sam_loc=[]
        data_original=np.array(ages_array)
        data_addnoise=np.array(ages_array)
        data_mask=np.array(mask)
        err=[]
        for i in range(a_max):
            for j in range(b_max):
                for k in range(c_max):
                    if data_original[i,j,k] != 0:
                        all_loc.append([i,j,k])
        
        sample=list(range(len(all_loc)))
        sample=rd.sample(sample,math.floor(len(all_loc)*noise))
        
        for i in sample:
            loc=all_loc[i]
            data_addnoise[loc[0],loc[1],loc[2]]=float(0)
            data_mask[loc[0],loc[1],loc[2]]=0
            sam_loc.append(loc)
        
        
        for i in range(1,rank_max+1):
            # print(data_addnoise)
            # print(i)
            # print(n_iter_max)
            res=tsd.non_negative_parafac(tensor=data_addnoise,rank=i,n_iter_max=n_iter_max,mask=data_mask,init="random")
            res=tsc.cp_to_tensor(res)
            losssum=[]
            for j in sam_loc:
                x=data_original[j[0],j[1],j[2]]
                y=res[j[0],j[1],j[2]]
                if method=="MSE":
                    loss=(x-y)**2
                elif method == "KL":
                    loss=x*math.log(x/y)-x+y
                losssum.append(loss)
            err.append(sum(losssum)/len(sam_loc))
        err_summary.append(err)
    err_summary=pd.DataFrame(err_summary)
    for i in range(rank_max):
        result.append(np.average(err_summary[i]))

    return result,err_summary

def calc_ordinal_loss(data,na_mask,rank_max=10,n_rep=20,n_iter_max=1000,method="KL",eps=1E-10):
    import math
    import random as rd
    import tensorly.cp_tensor as tsc
    import tensorly.decomposition as tsd
    err_summary=[]
    result=[]
    ages_array=[]
    a_max=len(data)
    b_max=len(data[0].index)
    c_max=len(data[0].columns)
    mask = np.array(na_mask.copy())
    for i in data:
        # d_array=mms.fit_transform(i)
        d_array = np.array(i)
        d_array=d_array.tolist()
        ages_array.append(d_array) 

    all_loc=[]
    for i in range(a_max):
        for j in range(b_max):
            for k in range(c_max):
                all_loc.append([i,j,k])
    
    
    for n in range(n_rep):
        print(n)
        data_original=np.array(ages_array)
        err=[]
        
        for i in range(1,rank_max+1):
            # print(data_addnoise)
            # print(i)
            # print(n_iter_max)
            res=tsd.non_negative_parafac(tensor=data_original,rank=i,n_iter_max=n_iter_max,mask=mask,init="random")
            res=tsc.cp_to_tensor(res)
            losssum=[]
            for j in all_loc:
                x=data_original[j[0],j[1],j[2]]
                y=res[j[0],j[1],j[2]]
                if method=="MSE":
                    loss=(x-y)**2
                elif method == "KL":
                    loss=x*math.log(x/y)-x+y
                losssum.append(loss)
            err.append(sum(losssum)/len(all_loc))
        err_summary.append(err)
    err_summary=pd.DataFrame(err_summary)
    for i in range(rank_max):
        result.append(np.average(err_summary[i]))

    return result,err_summary

rank=10

for n in noise_levels:

    err_1=calc_denoising_loss(df_3D_trim,na_mask=na_mask_3D_trim,n_rep=50,rank_max=rank,noise=n,method="MSE")
    err_1[1].to_csv("CUH_SmartDR_20210727_denoising_mask_noise="+str(n)+"_rank_estim.txt",sep="\t",index=False)
    rank_list=list(range(1,rank+1))

    fig = plt.figure()
    plt.rcParams['font.family'] = 'Arial'
    plt.rcParams["font.size"] = 15
    plt.plot(rank_list,err_1[0])
    plt.yscale('log')
    plt.xlabel("rank")
    plt.ylabel("Loss")

    err=np.array(err_1[1])
    plt.boxplot(err)
    # plt.show()

    fig.savefig("CUH_SmartDR_20210727_denoising_mask_noise="+str(n)+"_rank_estim.pdf")

# rank=10
# err_2=calc_ordinal_loss(df_3D_trim,na_mask=na_mask_3D_trim,n_rep=50,rank_max=rank,method="MSE") 
# rank_list=list(range(1,rank+1))

# fig = plt.figure()
# plt.rcParams['font.family'] = 'Arial'
# plt.rcParams["font.size"] = 15
# plt.plot(rank_list,err_2[0])
# plt.xlabel("rank")
# plt.ylabel("Loss")

# err=np.array(err_2[1])
# plt.boxplot(err)
# # plt.show()

# fig.savefig("CUH_SmartDR_20210727_ordinal_mask_rank_estim.pdf")
