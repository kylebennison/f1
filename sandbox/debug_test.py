import pandas as pd
import numpy as np

print("Starting up...")

cols = ['a', 'b', 'c']

rows = [np.array([0,1,2]), np.array([0,1,2]), np.array([0,1,2])]

zipped = dict(zip(cols, rows))

print(pd.DataFrame(rows, cols))
print(pd.DataFrame(zipped))

df = pd.DataFrame(rows, columns = cols)

print(df)