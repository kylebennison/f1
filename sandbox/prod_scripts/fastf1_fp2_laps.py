# -*- coding: utf-8 -*-
"""
Created on Mon Mar 21 23:33:00 2022

@author: Kyle
"""

import fastf1 as ff1

import os

# from matplotlib import pyplot as plt
# from fastf1 import plotting
# import seaborn as sns
import pandas as pd

round_num = 3
session_name = 'FP2'
year = 2022

start = r'C:\Users\Kyle\Documents\Projects\Data Projects\f1'

direct = start+'\\fastf1_cache'

ff1.Cache.enable_cache(direct)

event = ff1.get_event(year, round_num)

eventname = event['EventName']

session = ff1.get_session(year, round_num, session_name)

session_data = session.load(laps=True, telemetry=True, weather=False)

laps = session.load_laps()

laps.to_csv(start + "\\timing_data\\" + str(year) + "_" +
            str(round_num) + "_" + session_name + "_laps.csv", index=False)

# Load Telemetry

telem_final = pd.DataFrame()

for driver in laps['Driver'].unique():
    print("Working on ", driver)

    driver_laps = laps.pick_driver(driver)
    timed_laps = driver_laps[driver_laps['LapTime'].isnull() == False]
    team = driver_laps['Team'].iloc[0]

    for i in range(len(timed_laps)):
        try:
            print("Lap ", i)
            lap_telem = timed_laps.iloc[[i]].get_car_data().add_distance()
            lap_telem['Driver'] = driver
            lap_telem['Team'] = team
            lap_telem['Lap'] = i

            telem_final = pd.concat([lap_telem, telem_final])
        except:
            print('exception raised, continuing')
            continue

telem_final.to_csv(start+"\\timing_data\\"+str(year)+"_" +
                   str(round_num)+"_"+session_name+"_telem_all.csv", index=False)
