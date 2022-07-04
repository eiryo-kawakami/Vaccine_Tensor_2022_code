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
# location='/Users/eiryo/Dropbox (個人用)/DataAnalysis/Chiba_Vaccine/'

NMF_data=pd.read_excel("../../data/CUH_SmartDR_20210727.xlsx")
# pickup_data=pd.read_excel(location+"data/df_renew.xlsx")
# pickup_data.drop("Unnamed: 0",axis=1,inplace=True)

index=NMF_data["CUH"]
# sp_index=pickup_data.index
# sp_columns=pickup_data.columns

"""
==============================================================================
basic information
==============================================================================
"""

days=["1_1","1_2","1_3","1_4","1_5","1_6","1_7","2_1","2_2","2_3","2_4","2_5","2_6","2_7"]

items = []

for c in NMF_data.columns:
    if "2_7" in c:
        items.append(c.replace("2_7",""))

items.remove("other_symptoms")

"""
==============================================================================
3D_data
==============================================================================
"""

# plt.hist(nacount)

# df_3D_trim = []

# for df in df_3D:
#     df = df.loc[nacount < len(items)*len(days)*0.3,:]
#     df_3D_trim.append(df)
    
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
def calc_denoising_loss(data,na_mask,rank_max=10,n_rep=20,n_iter_max=1000,method="KL",eps=1E-10):
    import math
    import random as rd
    import tensorly.cp_tensor as tsc
    import tensorly.decomposition as tsd

    err_summary=[]
    result=[]
    a_max=1
    b_max=len(data.index)
    c_max=len(data.columns)
    # d_array=mms.fit_transform(i)
    d_array = np.array(data)
    # d_array=d_array.tolist()

    mask=na_mask.copy()
    
    for n in range(n_rep):
        print(n)
        #print(data)
        all_loc=[]
        sam_loc=[]
        data_original=d_array.copy()
        data_addnoise=d_array.copy()
        data_mask=np.array(mask)
        err=[]
        for j in range(b_max):
            for k in range(c_max):
                # print(data_original[j,k])
                if data_original[j,k] != 0:
                    all_loc.append([j,k])
        
        sample=list(range(len(all_loc)))
        sample=rd.sample(sample,math.floor(len(all_loc)*0.2))
        
        for i in sample:
            loc=all_loc[i]
            data_addnoise[loc[0],loc[1]]=float(0)
            data_mask[loc[0],loc[1]]=0
            sam_loc.append(loc)
        
        
        for i in range(1,rank_max+1):
            # print(data_addnoise)
            # print(i)
            # print(n_iter_max)
            res = tsd.non_negative_parafac(tensor=data_addnoise,rank=i,n_iter_max=n_iter_max,mask=data_mask,init="random")
            res=tsc.cp_to_tensor(res)
            losssum=[]
            for j in sam_loc:
                x=data_original[j[0],j[1]]
                y=res[j[0],j[1]]
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

def calc_ordinal_loss(data,rank_max=10,n_rep=20,n_iter_max=10000,method="KL",eps=1E-10):
    import math
    import random as rd
    import tensorly.cp_tensor as tsc
    import tensorly.decomposition as tsd
    err_summary=[]
    result=[]
    ages_array=[]
    b_max=len(data.index)
    c_max=len(data.columns)
    d_array = np.array(data)
    # d_array=d_array.tolist()
    # ages_array.append(d_array)

    all_loc=[]
    for j in range(b_max):
        for k in range(c_max):
            all_loc.append([j,k])
    
    
    for n in range(n_rep):
        print(n)
        data_original=d_array.copy()
        err=[]
        
        for i in range(1,rank_max+1):
            # print(data_addnoise)
            # print(i)
            # print(n_iter_max)
            model = NMF(n_components=i, init='nndsvd', max_iter=n_iter_max,random_state=n)
            W = model.fit_transform(data_original)
            res = model.inverse_transform(W)
            losssum=[]
            for j in all_loc:
                x=data_original[j[0],j[1]]
                y=res[j[0],j[1]]
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


df_3D={}
na_mask_3D={}
nacount = pd.Series(np.zeros(len(index)))
nacount.index = index

for d in days:
    df=pd.DataFrame()
    for c in NMF_data.columns:
        if d in c:
            if not c[3].isdigit():
                df[c]=NMF_data[c]
    colnames=[]
    for c in df.columns:
        colnames.append(c.replace(d,""))
    print(colnames)
    print(len(colnames))
    df.columns=colnames
    df.index=index
    df = df.drop("other_symptoms", axis=1)
    na_mask = pd.DataFrame(np.ones(shape=df.shape))
    na_mask.columns=df.columns
    na_mask.index=index

    nacount = nacount + df.isnull().sum(axis=1)
    na_mask = na_mask.mask(df.isnull(),0)

    df = df.fillna(float(0))

    df_3D[d] = df
    na_mask_3D[d] = na_mask


df_3D_trim = {}
na_mask_3D_trim = {}

for d in days:
    df = df_3D[d].copy()
    df = df.loc[nacount < len(items)*len(days)*0.3,:]
    df_3D_trim[d] = df

    na_mask = na_mask_3D[d].copy()
    na_mask = na_mask.loc[nacount < len(items)*len(days)*0.3,:]
    na_mask_3D_trim[d] = na_mask

for d in days:
    colnames = []
    for i in items:
        colnames.append(d+i)

    df_2D = df_3D_trim[d]
    na_mask = na_mask_3D_trim[d]
    # df_2D = df_2D.fillna(float(0))
    err_1=calc_denoising_loss(df_2D,na_mask=na_mask,n_rep=50,rank_max=rank,method="MSE")
    err_1[1].to_csv("CUH_SmartDR_20210727_time="+d+"_NMF_TensorLy_masking_rank_estim.txt",sep="\t",index=False)
    rank_list=list(range(1,rank+1))

    fig = plt.figure()
    plt.rcParams['font.family'] = 'Arial'
    plt.rcParams["font.size"] = 15
    plt.plot(rank_list,err_1[0])
    plt.xlabel("rank")
    plt.ylabel("Loss")

    err=np.array(err_1[1])
    plt.boxplot(err)
    # plt.show()

    fig.savefig("CUH_SmartDR_20210727_time="+d+"_NMF_TensorLy_masking_rank_estim.pdf")

# rank=10
# err_2=calc_ordinal_loss(df_2D.iloc[:,:(len(df_2D.columns)-1)],n_rep=50,rank_max=rank,method="MSE") 
# err_2[1].to_csv("CUH_SmartDR_20210727_NMF_ordinal_rank_estim.txt",sep="\t",index=False)
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

# fig.savefig("CUH_SmartDR_20210727_NMF_ordinal_rank_estim.pdf")

# """
# ==============================================================================
# tensor_decomposition(def function)
# ==============================================================================
# """    
# def decomposition(data,rank,item_sort,colnames):
#     df_array=[]
#     for i in data:
#         # d_array=mms.fit_transform(i)
#         d_array = np.array(i)
#         d_array=d_array.tolist()
#         x=d_array
#         df_array.append(d_array)
        
#     data_3dim=np.array(df_array)
#     res=tsd.non_negative_parafac(data_3dim,rank=rank,n_iter_max=1000)
#     time=pd.DataFrame(res[1][0])
#     time.to_csv("CUH_SmartDR_20210727_temporal_module.txt",sep="\t")
#     column=pd.DataFrame(res[1][2])
#     column.to_csv("CUH_SmartDR_20210727_spatial_module.txt",sep="\t")

#     outer=[]
#     outer_max = []
#     for i in range(rank):
#         a=np.array(time.iloc[:,i])
#         b=np.array(column.iloc[:,i])
#         a.T
#         c=np.outer(b,a)
#         c=pd.DataFrame(c)
#         outer_max.append(max(c.max()))
#         c.index=data[0].columns
#         c=c.reindex(index=item_sort)
#         c.columns=colnames
#         outer.append(c)

#     print(outer_max)

#     patient_score = pd.DataFrame(res[1][1])
    
#     for i in range(len(outer)):
#         basis=outer[i]/outer_max[i]
#         print(basis)
#         basis.to_csv("CUH_SmartDR_20210727_basis_"+str(i+1)+".txt",sep="\t")
#         fig = plt.figure()
#         sns.heatmap(basis,cmap="Reds")
#         # plt.show()
#         fig.savefig("CUH_SmartDR_20210727_basis"+str(i+1)+".pdf")
#         print(patient_score[i])
#         patient_score[i] = patient_score[i]*outer_max[i]
#         print(patient_score[i])

#     xlabels=[]
#     for i in range(rank):
#         xlabels.append("basis"+str(i+1))  
#     patient_score.columns = xlabels

#     return patient_score,outer,x


# def draw_heatmap(data,rank,metric,method="ward"): 
#     from scipy.cluster.hierarchy import linkage,dendrogram,cut_tree
#     from mpl_toolkits.axes_grid1 import make_axes_locatable
    
#     fig = plt.figure()
#     plt.rcParams['font.family'] = 'Arial'
#     plt.rcParams["font.size"] = 15
#     main_axes=plt.gca()
#     divider=make_axes_locatable(main_axes)
#     plt.sca(divider.append_axes("left",1.0,pad=0))
    
#     hierarchy=linkage(data,metric=metric,method=method)
#     cluster = np.concatenate(cut_tree(hierarchy,n_clusters=[3])).ravel().tolist()
#     print(cluster)
#     df_cluster = data.copy()
#     df_cluster.index = df_3D_trim[0].index
#     df_cluster["cluster"] = cluster
#     df_cluster.to_csv("CUH_SmartDR_20210727_patient_score_with_cluster.tsv",sep="\t")
#     ydendro=dendrogram(hierarchy,orientation="left",no_labels=True,
#                        distance_sort="descending",
#                        #link_color_func=lambda x:"black"
#                        )
#     plt.gca().set_axis_off()
    
#     data=data.reindex(index=ydendro["leaves"])

#     plt.sca(main_axes)
#     #plt.rcParams["image.cmap"]="Reds"
#     plt.imshow(data,aspect="auto",interpolation="none",cmap="Reds")
#     plt.colorbar()
#     xlabels=[]
#     for i in range(rank):
#         xlabels.append("group"+str(i+1))  
#     #plt.xticks(range(data.shape[0]),data.columns,size="small")
#     #plt.yticks(range(data.shape[0]),data.index,size="small")
#     plt.gca().xaxis.set_ticks_position("none")
#     plt.gca().yaxis.set_ticks_position("none")
#     plt.axis("off")
#     plt.gca().invert_yaxis()
#     fig.savefig("CUH_SmartDR_20210727_coef.pdf")

# """
# ==============================================================================
# tensor_decomposition(rank=3)
# ==============================================================================
# """

# res=decomposition(df_3D_trim,rank=4,item_sort=items,colnames=days)
# patient_score = res[0].copy()
# patient_score.index = df_3D_trim[0].index
# patient_score.to_csv("CUH_SmartDR_20210727_patient_score.tsv",sep="\t")
# ID=pd.DataFrame(res[0])
# draw_heatmap(ID,4,"euclidean") 


# # """
# # ==============================================================================
# # spearman
# # ==============================================================================
# # """
# # def spearman(data,sp_columns,group_columns):
# #     result=[]
# #     for i in group_columns:
# #         a=[]
# #         for j in sp_columns:
# #             b=spearmanr(data[j],data[i])
# #             a.append(b)
# #         c=pd.DataFrame(a,index=sp_columns,columns=["correlation coefficient","p_value"])
# #         c=c.sort_values("correlation coefficient",ascending=False)
# #         result.append(c)
# #     return result

# # group=[]
# # for i in range(rank):
# #     group.append("group_"+str(i+1))
# # ID.columns=group    
# # df=pd.concat([pickup_data,ID],axis=1)

# # df=df.dropna()
# # df_=df
# # df=spearman(df,sp_columns,group)

# # n=1
# # for i in df:
# #     i.to_excel(location+"pearson_group"+str(n)+".xlsx")
# #     n=n+1

# # """
# # df=pd.concat([df_7,ID],axis=1)
# # df=spearman(df,df_7.columns,group)
# # n=1
# # for i in df:
# #     i.to_excel(location+"7years_pearson_group"+str(n)+".xlsx")
# #     n=n+1

# # """



