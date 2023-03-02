""" Helper for colors and plotting
"""

# Team Colors
color_map = {
    "alfa romeo": "#900000",
    "alphatauri": "#2b4562",
    "alpine": "#0090ff",
    "aston martin": "#006f62",
    "ferrari": "#dc0000",
    "haas f1 team": "#ffffff",
    "mclaren": "#ff8700",
    "mercedes": "#00d2be",
    "red bull racing": "#0600ef",
    "williams": "#005aff",
}


# Order list of colors same as list of teams
def order_colors(teams_list: list):
    """Supply a list of ordered teams and return a list of colors in the same order"""
    colors = []
    # Lowercase teams
    teams_list = [x.lower() for x in teams_list]

    # Create list of colors
    for x in teams_list:
        colors.append(color_map[x])

    return colors
