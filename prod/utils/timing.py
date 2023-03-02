""" Timing
"""
import fastf1 as f1
import pandas as pd


def get_driver_numbers(session: f1.core.Session):
    res = session.laps[["Driver", "DriverNumber"]].value_counts()

    driver_number_map = dict(
        zip(
            res.index.get_level_values("DriverNumber"),
            res.index.get_level_values("Driver"),
        )
    )

    return driver_number_map


def get_team_names(session: f1.core.Session):
    res = session.laps[["Driver", "Team"]].value_counts()

    driver_team_map = dict(
        zip(res.index.get_level_values("Driver"), res.index.get_level_values("Team"))
    )

    return driver_team_map


def get_telem(session: f1.core.Session):
    df = pd.DataFrame()

    for f in session.car_data.keys():
        new = session.car_data[f]
        new["Car"] = f
        df = pd.concat([df, new], axis=0)

    # Add driver names
    mapping = get_driver_numbers(session)
    df["Driver"] = df["Car"].map(mapping)

    # Add team names
    mapping = get_team_names(session)
    df["Team"] = df["Driver"].map(mapping)

    return df
