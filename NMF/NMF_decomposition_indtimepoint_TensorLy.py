#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 17 14:12:48 2020

@author: naoki
"""
import pandas as pd
import numpy as np
from sklearn.decomposition import NMF
from sklearn.preprocessing import MinMaxScaler
mms=MinMaxScaler()
import matplotlib
import matplotlib.pyplot as plt
matplotlib.use('cairo')
import seaborn as sns
sns.set(font="Hiragino Mincho ProN")
from scipy.stats import pearsonr
from scipy.stats import spearmanr


#Windows
#location='C:/Users/naoqi/OneDrive - 千葉大学/ドキュメント/千葉医/課外活動/ML/小児病態　アレルギー解析/data/'
#Mac var
location='/Users/eiryokawakami/Dropbox/DataAnalysis/Chiba_Vaccine/'

NMF_data=pd.read_excel(location+"data/CUH_SmartDR_20210727.xlsx")
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
tensor_decomposition(def function)
==============================================================================
"""    
def decomposition(data,day,rank,colnames):
    model = NMF(n_components=rank, init='nndsvd', max_iter=10000,random_state=123)
    W = model.fit_transform(data)
    H=pd.DataFrame(model.components_)
    H.columns=colnames
    H.to_csv("CUH_SmartDR_20210727_NMF_time="+day+"_components.txt",sep="\t")

    patient_score = pd.DataFrame(W)
    
    fig = plt.figure()
    sns.heatmap(H,cmap="Reds")
    # plt.show()
    fig.savefig("CUH_SmartDR_20210727_NMF_time="+day+"_components.pdf")

    xlabels=[]
    for i in range(rank):
        xlabels.append("basis"+str(i+1))  
    patient_score.columns = xlabels

    return patient_score,H


def draw_heatmap(data,day,rank,metric,method="ward"): 
    from scipy.cluster.hierarchy import linkage,dendrogram,cut_tree
    from mpl_toolkits.axes_grid1 import make_axes_locatable
    
    fig = plt.figure()
    plt.rcParams['font.family'] = 'Arial'
    plt.rcParams["font.size"] = 15
    main_axes=plt.gca()
    divider=make_axes_locatable(main_axes)
    plt.sca(divider.append_axes("left",1.0,pad=0))
    
    hierarchy=linkage(data,metric=metric,method=method)
    cluster = np.concatenate(cut_tree(hierarchy,n_clusters=[3])).ravel().tolist()
    print(cluster)
    df_cluster = data.copy()
    df_cluster.index = df_2D.index
    df_cluster["cluster"] = cluster
    df_cluster.to_csv("CUH_SmartDR_20210727_NMF_time="+d+"_rank="+str(rank)+"_patient_score_with_cluster.tsv",sep="\t")
    ydendro=dendrogram(hierarchy,orientation="left",no_labels=True,
                       distance_sort="descending",
                       #link_color_func=lambda x:"black"
                       )
    plt.gca().set_axis_off()
    
    data=data.reindex(index=ydendro["leaves"])

    plt.sca(main_axes)
    #plt.rcParams["image.cmap"]="Reds"
    plt.imshow(data,aspect="auto",interpolation="none",cmap="Reds")
    plt.colorbar()
    xlabels=[]
    for i in range(rank):
        xlabels.append("group"+str(i+1))  
    #plt.xticks(range(data.shape[0]),data.columns,size="small")
    #plt.yticks(range(data.shape[0]),data.index,size="small")
    plt.gca().xaxis.set_ticks_position("none")
    plt.gca().yaxis.set_ticks_position("none")
    plt.axis("off")
    plt.gca().invert_yaxis()
    fig.savefig("CUH_SmartDR_20210727_NMF_rank="+str(rank)+"_patient_score.pdf")

"""
==============================================================================
tensor_decomposition(rank=3)
==============================================================================
"""

for d in days:
    colnames = []
    for i in items:
        colnames.append(d+i)

    df_2D = NMF_data.loc[:,colnames]
    df_2D.index = NMF_data["CUH"]
    nacount = df_2D.isnull().sum(axis=1)
    df_2D = df_2D.loc[nacount < len(colnames)*0.3,:]
    # df_2D = df_2D.fillna(float(0))

    rank_estim = pd.read_csv("CUH_SmartDR_20210727_time="+d+"_NMF_denoising_rank_estim.txt",sep="\t")
    print(rank_estim.median(axis=0))
    print(rank_estim.median(axis=0).idxmin())

    optim_rank = int(rank_estim.median(axis=0).idxmin()[0])+1

    print(optim_rank)

    res=decomposition(df_2D,day=d,rank=optim_rank,colnames=items)
    patient_score = res[0].copy()
    patient_score.index = df_2D.index
    patient_score.to_csv("CUH_SmartDR_20210727_NMF_time="+d+"_patient_score.tsv",sep="\t")
    # ID=pd.DataFrame(res[0])
    # draw_heatmap(ID,rank,"euclidean")


# """
# ==============================================================================
# spearman
# ==============================================================================
# """
# def spearman(data,sp_columns,group_columns):
#     result=[]
#     for i in group_columns:
#         a=[]
#         for j in sp_columns:
#             b=spearmanr(data[j],data[i])
#             a.append(b)
#         c=pd.DataFrame(a,index=sp_columns,columns=["correlation coefficient","p_value"])
#         c=c.sort_values("correlation coefficient",ascending=False)
#         result.append(c)
#     return result

# group=[]
# for i in range(rank):
#     group.append("group_"+str(i+1))
# ID.columns=group    
# df=pd.concat([pickup_data,ID],axis=1)

# df=df.dropna()
# df_=df
# df=spearman(df,sp_columns,group)

# n=1
# for i in df:
#     i.to_excel(location+"pearson_group"+str(n)+".xlsx")
#     n=n+1

# """
# df=pd.concat([df_7,ID],axis=1)
# df=spearman(df,df_7.columns,group)
# n=1
# for i in df:
#     i.to_excel(location+"7years_pearson_group"+str(n)+".xlsx")
#     n=n+1

# """
