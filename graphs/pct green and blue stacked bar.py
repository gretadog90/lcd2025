#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct  2 08:34:42 2024

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
import matplotlib.lines as mlines
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib
import matplotlib.patches as mpatches

#%% load data
data_folder = '/Users/gretam/Documents/data/'
results = '/Users/gretam/Documents/data/Lancet 2025/graphs/'

#data from lcd
merged_results=pd.read_csv(data_folder+'Lancet 2025/output/stacked_bar.csv') 

#create a blue area indicator
merged_results["Blue_Area_2020"]=merged_results["GreenBlue_Area_2020"]-merged_results["Green_Area_2020"]
merged_results["Urban_Area_2020"]=1-merged_results["GreenBlue_Area_2020"]

#Africa=merged_results[merged_results["lc_group"]=="Africa"]
city_labels= merged_results['city']
stackedbar=merged_results[["seq", "Green_Area_2020", "Blue_Area_2020", "Urban_Area_2020"]]

#%% make graph
sns.set(style='white')
sns.set(font_scale = .2)
fig = plt.figure(figsize=(20, 70))
fig, ax = plt.subplots()


# create stacked bar chart for monthly temperatures
stackedbar.set_index('seq').plot(kind='barh', ax=ax,
                                stacked=True, color=['darkgreen', 'deepskyblue', 'lightgrey'],
                                width=.1)
                                
ax.set_yticks(range(len(city_labels)), labels=city_labels, size=3)
ax.legend().remove()
ax.set_facecolor('white')  


green = mpatches.Patch(color='darkgreen', label='Greenspace')
blue = mpatches.Patch(color='deepskyblue', label='Blue space')
urban = mpatches.Patch(color='lightgrey', label='Urban/non-vegetated')

plt.legend(handles=[green, blue, urban], title="Proportion of the urban area", 
          bbox_to_anchor=(1.01, 1),frameon=False, fontsize=4, title_fontsize=4.5)

filename=results+'greenbluearea.png' 
plt.savefig(filename, format='png', dpi=300, bbox_inches = "tight")
plt.show()
