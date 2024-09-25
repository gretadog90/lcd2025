#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 13 12:43:53 2024

@author: gretam
"""
#%% load modules
import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import seaborn as sns
from matplotlib.ticker import FormatStrFormatter
from matplotlib.lines import Line2D
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib

#%% user inputs 
# data root folder path
data_folder = '/Users/gretam/Documents/data/Lancet 2025/output/'
output='/Users/gretam/Documents/data/Lancet 2025/graphs/'

#population for 2015 and 2020 and 100m and 1km res
data2020=pd.read_csv(data_folder+'data2020.csv') 
data2015=pd.read_csv(data_folder+'data2015.csv') 

#%% graph

sns.set(style='whitegrid')
plt.figure(figsize=(20, 15))
fig, (ax1, ax2) = plt.subplots(1, 2)
fig.tight_layout()

#Panel A
a=sns.scatterplot(y=data2015['PopWeight_Avg_NDVI_2015'], x=data2015['PopWeight_Avg_NDVI_2015_100m'],color='gold', ax=ax1).set(xlabel='100m', 
              ylabel='1km', title='2015')
ax1.axline([0, 0], [1, 1])
 
#Panel B
b=sns.scatterplot(y=data2020['PopWeight_Avg_NDVI_2020'], x=data2020['PopWeight_Avg_NDVI_2020_100m'],color='gold', ax=ax2).set(xlabel='100m', 
              ylabel='1km', title='2020')

ax2.axline([0, 0], [1, 1])

filename=output+'compare pop res pop weighted ndvi.png' 
plt.savefig(filename ,dpi=300)
plt.show()


