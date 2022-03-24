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

start = r'C:\Users\Kyle\Documents\Projects\Data Projects\f1'

direct = start+'\\fastf1_cache'

ff1.Cache.enable_cache(direct)

event  = ff1.get_event(2022, 1)

session = ff1.get_session(2022, 1, 'Q')

session_data = session.load(laps = True, telemetry = True, weather = False)

laps = session.load_laps()

laps.to_csv(start+"\\timing_data\\2022_1_q_laps.csv", index = False)

telem_final = pd.DataFrame()

# Get fastest lap for each driver and get telemetry from lap
for driver in laps['Driver'].unique():
    telem = laps.pick_driver(driver).pick_fastest().get_car_data().add_distance()

    # Add column with driver name
    telem['Driver'] = driver
    
    team = laps.pick_driver(driver).pick_fastest()['Team']
    
    telem['Team'] = team
    
    # combine to master table
    telem_final = pd.concat([telem, telem_final])

telem_final.to_csv(start+"\\timing_data\\2022_1_q_telem_fastest.csv", index = False)
