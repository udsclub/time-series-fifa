import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import math
import tqdm
from datetime import *


STAMP = datetime.now().strftime('%Y-%m-%d_%H:%M')

is_normed_y = True
sel_col = ['colour', 'rareness', 'player_id', 'revision']
# test on lr -> output/stat_feature_result.json ->

df = pd.read_csv("../data/player_price.csv")
df = df.rename(columns = {'xbox_price': 'price'})
#fillna
filledna_df = pd.DataFrame()
for id in tqdm.tqdm(df["player_id"].unique()):
    price_id = df[df["player_id"] == id].copy()
    price_id['price'].replace(0, np.nan, inplace=True)
    price_id['price'].bfill(inplace=True)
    price_id['price'].ffill(inplace=True)
    filledna_df = pd.concat([filledna_df, price_id])
df = filledna_df
#log
if is_normed_y:
    df["price"] = np.log(df["price"])

df_stats = pd.read_csv('../data/player_stats.csv')
df_stats = df_stats[sel_col]
df_stats['revision'] = df_stats['revision'] == 'Normal'
df_stats['revision'] = df_stats['revision'].astype(np.int8)
#df_stats_obj = df_stats.select_dtypes(include=['object']) 
#col_dummy_df = pd.get_dummies(df_stats_obj, prefix=df_stats_obj.columns)
#df_stats.drop(df_stats_obj.columns, inplace=True, axis=1)
#df_stats = df_stats.join(col_dummy_df)

for col in df_stats.select_dtypes(include=['object']).columns:
    factorized_array = pd.factorize(df_stats[col])
    df_stats[col]=factorized_array[0]
    #print(col, factorized_array[1])
    

sample_subm = pd.read_csv("../short_term_competition_benchmarks/kaggle_sample_submission.csv")
sample_subm["player_id"] = sample_subm["id"].apply(lambda x: int(x.split("_")[0]))
sample_subm["Date"] = sample_subm["id"].apply(lambda x: np.datetime64(x.split("_")[1]))

df['Date'] = pd.to_datetime(df['timestamp'], unit='ms')
df_player_count = df.groupby(['Date'])['player_id'].count()
df_player_count = df_player_count.rename('player_count')

df = df[df["player_id"].isin(sample_subm["player_id"].unique())]
df.drop(["timestamp", "ps_price", "player_name"], axis=1, inplace=True)


pred_size = len(sample_subm['Date'].unique()) #7

df = df[df['Date'] > df['Date'].max() - timedelta(days=90)]
#df_merged = df_ps[sel_col].merge(df_good_id, right_index=True, left_on='player_id')
#df['avg_businnes_day'] = df[['price','Date']].groupby('Date').mean()


#df['ap_weekday'] = df[['weekday','price']].groupby('weekday').mean()


def process_date(df):
    df['year'] = df['Date'].apply(lambda x: x.year)
    df['month'] = df['Date'].apply(lambda x: x.month)
    df['day'] = df['Date'].apply(lambda x: x.day)
    df['weekday'] = df['Date'].apply(lambda x: x.weekday())
    df['is_cheap'] = un_normalize(df['price'])<50000
    df['is_cheap'] = df['is_cheap'].astype(np.int8)
    return df

def un_normalize(y):
    return np.exp(y)
    
def add_day_to_test(X):
    max_date = X.Date.max()
    X['to_max_days'] = date_max - X.Date
    X['to_max_days'] = X['to_max_days'].apply(lambda x : np.int8(x / np.timedelta64(1, 'D'))).astype(np.int8)
    return X

def add_ap_for_period_by_id(df, n_days=7):
    """
    not working
    """
    temp = df
    temp = temp.groupby('player_id')['price'].mean()
    temp = temp.rename("ap_for_period_by_id")
    return df.merge(temp, right_index=True, left_on='player_id')

def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):
    """
    Frame a time series as a supervised learning dataset.
    Arguments:
        data: Sequence of observations as a list or NumPy array.
        n_in: Number of lag observations as input (X).
        n_out: Number of observations as output (y).
        dropnan: Boolean whether or not to drop rows with NaN values.
    Returns:
        Pandas DataFrame of series framed for supervised learning.
    """
    n_vars = 1 if type(data) is list else data.shape[1]
    df = pd.DataFrame(data)
    cols, names = list(), list()
    # input sequence (t-n, ... t-1)
    for i in range(n_in, 0, -1):
        cols.append(df.shift(i))
        names += [('var%d(t-%d)' % (j + 1, i)) for j in range(n_vars)]
    # forecast sequence (t, t+1, ... t+n)
    for i in range(0, n_out):
        cols.append(df.shift(-i))
        if i == 0:
            names += [('var%d(t)' % (j + 1)) for j in range(n_vars)]
        else:
            names += [('var%d(t+%d)' % (j + 1, i)) for j in range(n_vars)]
    # put it all together
    agg = pd.concat(cols, axis=1)
    agg.columns = names
    # drop rows with NaN values
    if dropnan:
        agg.dropna(inplace=True)
    return agg