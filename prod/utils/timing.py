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


def get_telem(
    session: f1.core.Session, fastest_only: bool = False, drivers: list = None
):
    df = pd.DataFrame()

    # Get only the fastest lap per driver
    if fastest_only:
        for car in session.laps.DriverNumber.unique():
            lap = (
                session.laps.loc[session.laps["DriverNumber"] == car, :]
                .pick_fastest()
                .get_car_data()
                .add_distance()
            )
            lap["DriverNumber"] = car
            df = pd.concat([df, lap], axis=0)

    # Get all telem
    else:
        for f in session.car_data.keys():
            new = session.car_data[f]
            new["DriverNumber"] = f
            df = pd.concat([df, new], axis=0)

    # Add driver names
    mapping = get_driver_numbers(session)
    df["Driver"] = df["DriverNumber"].map(mapping)

    # Add team names
    mapping = get_team_names(session)
    df["Team"] = df["Driver"].map(mapping)

    return df
