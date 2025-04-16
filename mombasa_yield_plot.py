# import pandas
import numpy as np
import pandas as pd
from numpy.random import randn
import matplotlib.pyplot as plt
import seaborn as sns
import datetime
import matplotlib.dates as mdates

df1=pd.read_excel('Mombasa_Week.xlsx',sheet_name='Sheet1')

fig, ax = plt.subplots(figsize=(8.5,5.5))
sns.set_context(font_scale=3)
sns.regplot(x='Week',y='Yield',data=df1,ax=ax)
ax.set_xlabel('Week',fontsize=14,fontweight='bold')
ax.set_ylabel('Yield (%)',fontsize=14,fontweight='bold')

plt.show()

fig, ax = plt.subplots(figsize=(8.5,5.5))
sns.set_context(font_scale=3)
sns.regplot(x='Tests',y='Yield',data=df1,ax=ax)
ax.set_xlabel('Number of Weekly Tests',fontsize=14,fontweight='bold')
ax.set_ylabel('Yield (%)',fontsize=14,fontweight='bold')

plt.show()