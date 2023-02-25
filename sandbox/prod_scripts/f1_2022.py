# -*- coding: utf-8 -*-
"""
Created on Sat Mar 19 09:31:48 2022

@author: Kyle
"""

import fastf1 as ff1

import os

# from matplotlib import pyplot as plt
# from fastf1 import plotting
# import seaborn as sns
import pandas as pd

year = int(input(prompt="Year: "))
round_num = int(input(prompt="Round: "))
session_name = input("Session: ")

start = r'C:\Users\Kyle\Documents\Projects\Data Projects\f1'

direct = start+'\\fastf1_cache'

ff1.Cache.enable_cache(direct)

event = ff1.get_event(year, round_num)

session = ff1.get_session(year, round_num, session_name)

session_data = session.load(laps=True, telemetry=True, weather=False)

laps = session.load_laps()

laps.to_csv(start+"\\timing_data\\"+str(year)+"_"+str(round_num) +
            "_"+session_name+"_laps.csv", index=False)

telem_final = pd.DataFrame()

# Get fastest lap for each driver and get telemetry from lap
for driver in laps['Driver'].unique():
    try:
        telem = laps.pick_driver(
            driver).pick_fastest().get_car_data().add_distance()

    except:
        continue

    else:
        # Add column with driver name
        telem['Driver'] = driver

        team = laps.pick_driver(driver).pick_fastest()['Team']

        telem['Team'] = team

        # combine to master table
        telem_final = pd.concat([telem, telem_final])

telem_final.to_csv(start+"\\timing_data\\"+str(year)+"_" +
                   str(round_num)+"_"+session_name+"_telem_fastest.csv", index=False)
