# -*- coding: utf-8 -*-
"""
Created on Tue Mar  1 19:53:11 2022

@author: Kyle
"""

import fastf1 as ff1

import os

from matplotlib import pyplot as plt
from fastf1 import plotting
import seaborn as sns
import pandas as pd

direct = os.getcwd()+'\\fastf1_cache'

ff1.Cache.enable_cache(direct)

# monza_quali = ff1.get_session(2019, 'Monza', 'Q')

# vettel = monza_quali.get_driver('VET')

# vettel.name

# plotting.setup_mpl()

# laps = monza_quali.load_laps(with_telemetry=True)
# fast_leclerc = laps.pick_driver('LEC').pick_fastest()
# lec_car_data = fast_leclerc.get_car_data()
# t = lec_car_data['Time']
# vCar = lec_car_data['Speed']

# # The rest is just plotting
# fig, ax = plt.subplots()
# ax.plot(t, vCar, label='Fast')
# ax.set_xlabel('Time')
# ax.set_ylabel('Speed [Km/h]')
# ax.set_title('Leclerc is')
# ax.legend()
# plt.show()

# Latest testing
testing_22 = ff1.get_session(2021, 'Abu Dhabi', 'R')

round_23_laps = testing_22.load_laps(with_telemetry=True)
round_23_laps.groupby('Compound')['LapTime'].mean()

# Filter out TrackStatus=4 for safety car laps
green_laps = round_23_laps.loc[round_23_laps['TrackStatus']!='4']

green_laps.groupby('Compound')['LapTime'].agg(['mean', 'count'])

# Plot Dist of Laptimes by Compound
green_laps['LapTime'] = green_laps['LapTime'] / pd.to_timedelta(1, unit = 's')
sns.boxplot(x = 'Compound', y = 'LapTime', data = green_laps)
sns.violinplot(x = 'Compound', y = 'LapTime', data = green_laps)

# Try to plot two drivers raw fast laps ontop of each other
plotting.setup_mpl()

fast_ver = green_laps.pick_driver('VER').pick_fastest()
ver_data = fast_ver.get_car_data()
t_ver = ver_data['Time']
v_ver = ver_data['Speed']

fast_ham = green_laps.pick_driver('HAM').pick_fastest()
ham_data = fast_ham.get_car_data()
t_ham = ham_data['Time']
v_ham = ham_data['Speed']

plt.plot(t_ham, v_ham, 'y--', t_ver, v_ver, 'r--')
plt.show()

# Do the same thing with Plotly

import plotly.express as px

fig_data = green_laps.loc[(green_laps['Driver']=='HAM') | (green_laps['Driver']=='VER')]

fig = px.line(data_frame=fig_data,
              x = 'LapNumber',
              y = 'LapTime',
              color = 'Driver')

import plotly.io as pio

pio.renderers

fig.show(renderer='browser')

# Try matplotlib

plt.style.use('fivethirtyeight')

x = range(1, int(green_laps[green_laps['Compound'] == 'HARD']['TyreLife'].max()))

plt.plot(x,
         green_laps[green_laps['Compound'] == 'HARD'].groupby('TyreLife').agg('mean')['LapTime'],
         label = "Hard Tyre")

plt.plot(range(1, int(green_laps[green_laps['Compound'] == 'MEDIUM']['TyreLife'].max() + 1)),
         green_laps[green_laps['Compound'] == 'MEDIUM'].groupby('TyreLife').agg('mean')['LapTime'],
         label = "Medium Tyre")

plt.plot(range(1, int(green_laps[green_laps['Compound'] == 'SOFT']['TyreLife'].max() + 1)),
         green_laps[green_laps['Compound'] == 'SOFT'].groupby('TyreLife').agg('mean')['LapTime'],
         label = "Soft Tyre")


plt.xlabel('Tyre Life in Laps', fontsize = 12,
           color = 'k')

plt.ylabel('Laptime in seconds', color = 'k')

plt.title('Laptime falloff by tyre and life', color = 'k')

plt.legend()

plt.tight_layout()

plt.show()
