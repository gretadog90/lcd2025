#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 10 14:11:40 2024

@author: gretam
"""

#%% load modules
import rasterio as rio
from rasterio.plot import show
import rioxarray as rxr
import numpy as np
import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
import pyproj
import os
from rasterio.warp import reproject, Resampling, calculate_default_transform
from shapely.geometry import box, mapping, Polygon, Point

#%% user inputs - #%% is how you section off code blocks in spyder
# data root folder path
data_folder = '/Users/gretam/Documents/data/shapefiles/'

# import shapefile using geopandas
smod_raw = gpd.read_file(data_folder+
                'GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_UC_V2_0.shp') 
print(smod_raw[['BU_m2_2020']])

#subset data to only what I need (id, city name, country name, geometry)
ghs_smod=smod_raw[['ID_UC_G0', 'geometry']]

#subset list to cities from Lancet 2023 and merge in the cleaned country/city names
edited_names=pd.read_csv('/Users/gretam/Documents/Git/lcd2025/UCDB cleaning and subsetting/edited_names.csv')
print(edited_names.columns.values)
edited_names['ID_UC_G0']=edited_names['id_hdc_g0']
gee_upload=pd.merge(left=smod_raw, right=edited_names, how='right', on='ID_UC_G0')

gee_upload=gee_upload[['ID_UC_G0','city', 'geometry']]
gee_upload = gpd.GeoDataFrame(gee_upload, geometry='geometry')
gee_upload.to_file(data_folder+'LCD_cities_2023.shp', driver='ESRI Shapefile')


