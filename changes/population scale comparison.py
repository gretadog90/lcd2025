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
pop_2015_1km=pd.read_csv(data_folder+'2015/2015_pop_denom.csv') 
pop_2015_100m=pd.read_csv(data_folder+'2015/2015_pop_denom_100m.csv') 
pop_2020_1km=pd.read_csv(data_folder+'2020/2020_pop_denom.csv') 
pop_2020_100m=pd.read_csv(data_folder+'2020/2020_pop_denom_100m.csv') 

#%% graph

sns.set(style='whitegrid')
plt.figure(figsize=(20, 15))
fig, (ax1, ax2) = plt.subplots(1, 2)
fig.tight_layout()

#Panel A
a=sns.scatterplot(y=pop_2015_1km['sum'], x=pop_2015_100m['sum'],color='gold', ax=ax1).set(xlabel='100m', 
              ylabel='1km', title='2015')
ax1.axline([0, 0], [50000, 50000])
 
#Panel B
b=sns.scatterplot(y=pop_2020_1km['sum'], x=pop_2020_100m['sum'],color='gold', ax=ax2).set(xlabel='100m', 
              ylabel='1km', title='2020')

ax2.axline([0, 0], [50000, 50000])

filename=output+'compare pop res.png' 
plt.savefig(filename ,dpi=300)
plt.show()


