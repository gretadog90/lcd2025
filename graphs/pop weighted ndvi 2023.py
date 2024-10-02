#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct  1 19:14:35 2024

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
merged_results=pd.read_csv(data_folder+'Lancet 2025/hia.csv') 

#lcd shapefiles
lcd_shapes = gpd.read_file(data_folder+'shapefiles/LCD_cities/LCD_cities.shp')
lcd_shapes["centroid"]=lcd_shapes.geometry.centroid.to_crs(epsg=4326)
lcd_shapes=lcd_shapes[["centroid", "ID_HDC_G0"]]


#turn into geo data frame with centriod from shapefile as geometry
gdf=pd.merge(left=merged_results, right=lcd_shapes, how='left', on='ID_HDC_G0')
gdf = gpd.GeoDataFrame(gdf, geometry="centroid")

gdf=gdf[(gdf['indicator_2023_100m'].notna())]
print(gdf['indicator_2023_100m'])

unique_values = gdf['indicator_2023_100m'].unique()

print(unique_values) 

#control which colors each panel shows up as
indicator=["Exceptionally High", "Very High", "High", "Moderate", "Low", "Very Low","Exceptionally Low"]
colors=['gold', 'darkgreen', 'green', 'mediumseagreen', 'lightgreen', 'honeydew', 'lightgoldenrodyellow']
color_dict=dict(zip(indicator, colors))

gdf['color'] = gdf['indicator_2023_100m'].apply(lambda x: color_dict[x])
print(gdf['indicator_2023_100m'])

#%% make graph
sns.set(style='whitegrid')
fig, ax = plt.subplots()
fig.tight_layout()

#load in world shape file for background
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))

world=world[world['continent']!='Antarctica']

world.plot(ax=ax, edgecolor='black',linewidth=.4, color='white')
ax.tick_params(labelsize=0)
ax.grid(False)
ax.axis('off')

ax=gdf.plot(c=gdf['color'], ax=ax,  marker=".", markersize=10)

Africa = mpatches.Patch(color='darkslategray', label='Exceptionally High (0.70+)')
CEA = mpatches.Patch(color='darkgreen', label='Very High (0.60-0.69)')
ESAO = mpatches.Patch(color='green', label='High (0.50-0.59)')
Europe = mpatches.Patch(color='mediumseagreen', label='Moderate (0.40-0.49)')
LA = mpatches.Patch(color='lightgreen', label='Low (0.30-0.39)')
NA = mpatches.Patch(color='honeydew', label='Very Low (0.20-0.29)')
SWA = mpatches.Patch(color='lightgoldenrodyellow', label='Exceptionally Low (0.10-0.19)')
plt.legend(handles=[Africa, CEA, ESAO, Europe, LA, NA, SWA], 
           loc=3, prop={'size': 4}, frameon=False, title="Level of urban greeness",
           title_fontsize=6)

filename=results+'indicator2023ndvimap.png' 
plt.savefig(filename, format='png', dpi=300, bbox_inches = "tight")
plt.show()

