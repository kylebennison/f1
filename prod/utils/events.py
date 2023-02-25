import fastf1 as f1
from datetime import datetime

# Constants
NOW = datetime.now()
YEAR = NOW.year


def next_event():

    # Get calendar
    calendar = f1.get_event_schedule(year=YEAR)
    next_event = calendar[calendar["EventDate"] > NOW].reset_index(drop=True).loc[0, :]

    # TODO: Return a namedtuple
    return YEAR, next_event["RoundNumber"], next_event
