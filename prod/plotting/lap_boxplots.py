import fastf1 as f1
import numpy as np
import pandas as pd

# from datetime import datetime
import seaborn as sns
from prod.utils import events

YEAR, ROUND, EVENT = events.next_event()

# TODO: Remove _testing_
sesh = f1.get_testing_session(year=YEAR, test_number=1, session_number=1)
# Load data
sesh.load()

# Get unique driver numbers from the session
driver_numbers = sesh.laps.DriverNumber.unique()

# Get each driver's fastest lap
fastest = pd.DataFrame()
for d in driver_numbers:
    fastest = pd.concat(
        [
            fastest,
            sesh.laps[sesh.laps["DriverNumber"] == d].pick_fastest().to_frame().T,
        ],
        axis=0,
    )

# Convert LapTime to seconds
sesh.laps["LapTime"] = sesh.laps["LapTime"] / np.timedelta64(1, "s")

# Boxplot of driver laps
sns.set_theme()

sns.boxplot(data=sesh.laps, y="Driver", x="LapTime", orient="h")

sns.despine()

# Boxplot of team laps
sns.set_theme()

sns.boxplot(data=sesh.laps, y="Team", x="LapTime", orient="h")

sns.despine()
