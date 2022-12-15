## Do a grid search on the number of layers and neurons
## Everything is hard coded and should be called only from the python notebook
import sys

import numpy as np
from sklearn.neural_network import MLPRegressor
from itertools import permutations
from sklearn.model_selection import GridSearchCV
from sklearn.utils import shuffle
import pickle

# Generate all possible combinations of layers and neurons
# Param:
#   max_layers: maximum number of layers
#   max_neurons: maximum number of neurons in each layer
#   step: increments of number of neurons in each layer
#   min_neurons: minimum number of neurons in each layer
def generate_nn_configs(max_layers, max_neurons, step = 1, min_neurons = 1):
    configs = []
    for l in range(1, max_layers+1):
        if l == 1:
            configs.extend([(x) for x in range(min_neurons, max_neurons + 1, step)])
        else:
            configs.extend([x for x in permutations(range(min_neurons, max_neurons + 1, step), l)])
    return configs


# Loading data
X = np.loadtxt(open("../data/X.csv", "rb"), delimiter=",")
Y = np.loadtxt(open("../data/Y.csv", "rb"), delimiter=",")

# Balancing the training set so that there are about
# equal number of statistically significant/insignificant cases
pos_index = [i for i in range(Y.shape[0]) if (Y[i,0] < 0.05) & (Y[i,1] < 0.05)]
pos_index_0 = [i for i in range(Y.shape[0]) if Y[i,0] < 0.05]
pos_index_1 = [i for i in range(Y.shape[0]) if Y[i,1] < 0.05]
X_pos = X[pos_index,:]
X_pos_0 = X[pos_index_0,:]
X_pos_1 = X[pos_index_1,:]
Y_pos = Y[pos_index,:]
Y_pos_0 = Y[pos_index_0,:]
Y_pos_1 = Y[pos_index_1,:]
X = np.concatenate((X_pos, X, X_pos, X_pos_1))
Y = np.concatenate((Y_pos, Y, Y_pos, Y_pos_1))

#print((Y[:,0] < 0.05).sum()/Y.shape[0])
#print((Y[:,1] < 0.05).sum()/Y.shape[0])
#sys.exit()

# Generate all configurations
hidden_layer_configs = generate_nn_configs(max_layers = 3,
                                           max_neurons = 36,
                                           step = 4,
                                           min_neurons = 4)
# Set up MLPRegressor
reg = MLPRegressor(solver='adam',
                   alpha=1e-5,
                   random_state=1)
# Set up parameter grid
grid_param = dict(hidden_layer_sizes=hidden_layer_configs)
# Set up grid search engine
grid = GridSearchCV(estimator=reg, param_grid=grid_param, n_jobs=-1, cv=3)
# Do grid search
grid_result = grid.fit(X, Y)

# Print best parameters
print("Best: %f using %s" % (grid_result.best_score_, grid_result.best_params_))

# Save
with open('grid_result', 'wb') as grid_result_file:
    pickle.dump(grid_result, grid_result_file)