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

round_num = 1
session_name = 'FP2'

start = r'C:\Users\Kyle\Documents\Projects\Data Projects\f1'

direct = start+'\\fastf1_cache'

ff1.Cache.enable_cache(direct)

event  = ff1.get_event(2022, round_num)

session = ff1.get_session(2022, round_num, session_name)

session_data = session.load(laps = True, telemetry = True, weather = False)

laps = session.load_laps()

laps.to_csv(start + "\\timing_data\\2022_" + str(round_num) + "_" + session_name + "_laps.csv", index = False)