import random
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.neighbors import NearestNeighbors
import numpy as np

df = pd.read_csv('/home/marius/VL_Unterlagen/DataAnaProject/Lyudmila/data/camel/Dataset_large_R.csv')#, encoding = 'utf-8')


def smote(dataframe, features, oversPerc, k, p, filename):

    """
    Function takes pandas dataframe and applies Synthetic Minority Oversampling to it.
    :param dataframe: input dataframe in pandas format
    :param oversPerc: amount of oversampling in percent, usually a multiple of 101.
    :param features: specify features for distance computation
    :param n: number of randomly chosen minority observations among the k neighrest neighbors
    :param p:
    :return:
    """

    minority_obs = dataframe.loc[df['Failure'] == 'FAILURE'] # filter rows from Failure subset (minority class)
    minority_obs_np = minority_obs[features].to_numpy() #take columns of interest and create np array for knn
    minority_obs_np = minority_obs_np[~np.isnan(minority_obs_np).any(axis=1)]
    knn = NearestNeighbors(n_neighbors=k)
    knn.fit(minority_obs_np)
    NearestNeighbors(algorithm='auto', leaf_size=30, n_neighbors = k+1, p=p,
                     radius=2.0) # add 1 to numberNN to get numberNN "real" neighbors, every obs is its nearest neighbor
    #p=3 gives euclidean distance
    neighbors_dict = {} # dict for saving neighbors of each point
    # look up k+1 neighbors:
    neighbors = knn.kneighbors(minority_obs_np, return_distance=False, n_neighbors = k+1)
    for item in neighbors:
        neighbors_dict[item[0]] = item[1:].tolist() # exclude first entry of each item in neighbors (point itself)
    random_neighbors = {} # drawn random subset of knn depending on the desired oversampling rate:
    for key in neighbors_dict:
        random_neighbors[key] = []
        for _ in range(oversPerc//100):
            #print(neighbors_dict[key])
            random_neighbors[key].append(minority_obs_np[neighbors_dict[key][random.randint(0,3)]])
    synthetic_obs = []
    for key in random_neighbors:
         for value in random_neighbors[key]:
    #         #print(minority_obs_np[key]) random.uniform(0,1) * (random_neighbors[key][value] - minority_obs_np[key]))
    #         print(minority_obs_np[key])
    #         print(random.uniform(0,1))
              synthetic_obs.append(minority_obs_np[key] + random.uniform(0,1) * (value - minority_obs_np[key]))
    synthetic_obs = np.array(synthetic_obs)
    minority_obs_np_ext = np.concatenate((minority_obs_np, synthetic_obs))
    minority_obs_np_ext = pd.DataFrame(data=minority_obs_np_ext[1:,:])
    minority_obs_np_ext.columns = features
    minority_obs_np_ext.to_csv(filename)

features = ['eq', 'lnatres', 'roa', 'naasset', 'sc', 'bro', 'asset', #questionable: variable naasset equal to non-perform assets?
            'chbal', 'intan', 'lnreres', 'lnremult', 'lnrecons', 'lnrenres', 'lnci', 'lncon']
smote(df, features, oversPerc=400, k=4, p=2, filename='/home/marius/Desktop/Dataset_large_R_ext.csv')












