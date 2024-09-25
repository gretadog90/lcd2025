#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 12 11:57:33 2024

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
data_folder = '/Users/gretam/Documents/data/Lancet 2025/'
output=data_folder+'graphs/'

#population for 2015 and 2020 and 100m and 1km res
ga_2015=pd.read_csv(data_folder+'2015/2015_ga.csv') 
ndvi_2015=pd.read_csv(data_folder+'2015/2015_summer.csv') 
ga_2020=pd.read_csv(data_folder+'2020/2020_ga.csv') 
ndvi_2020=pd.read_csv(data_folder+'2020/2020_summer.csv') 

#%% graph

sns.set(style='whitegrid')
plt.figure(figsize=(20, 15))
fig, (ax1, ax2) = plt.subplots(1, 2)
fig.tight_layout()

#Panel A
a=sns.scatterplot(y=ga_2015['mean'], x=ndvi_2015['mean'],color='teal', ax=ax1).set(xlabel='ndvi', 
              ylabel='ga', title='2015')
ax1.axline([0, 0], [1, 1])
 
#Panel B
b=sns.scatterplot(y=ga_2020['mean'], x=ndvi_2020['mean'],color='teal', ax=ax2).set(xlabel='ndvi', 
              ylabel='ga', title='2020')

ax2.axline([0, 0], [1, 1])

filename=output+'compare ndvi ga.png' 
plt.savefig(filename ,dpi=300)
plt.show()