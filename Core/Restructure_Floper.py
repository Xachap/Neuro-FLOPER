import keras
import numpy as np
import calendar                 #Time packets are for creating new archives
import time
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.utils import plot_model

from NerualNet_Floper import NN_f
from Layer_Floper import layer_f
from Node_Floper import node_f

def restructure(model,dlabels):

    counter_layers = 1
    #Create the input layer
    n_nodes = len(model.layers[0].get_weights()[0])     #Number of nodes in the layer
    nodes = []                                          #Array of nodes
    variables = []                                      #Array of input variables
    i = 1                                               
    while i <= n_nodes:
        var = "X"+str(i)                                #Define the name of the variable
        variables.append(var)                           #Append to array of variables
        nodes.append(node_f("td("+var+")",[],0))     #Head of the predicate
        i += 1
        
    input_layer = layer_f(n_nodes, nodes,counter_layers,"") #Create the input layer
    last_layer = input_layer.nodes                      #Nodes of the previous layer are saved
    counter_layers += 1                                 #Increment the number of layers
    
    #Creates the hidden layers
    n_hidden = len(model.layers)-1                      #Calculate the number of hidden layers
    i = 1
    h_layers = []                                       #Array of hidden layers
    variables = str(variables).replace("[","").replace("]","").replace("'","")
    
    while i <= n_hidden:
        weights = model.layers[counter_layers-2].get_weights()      #Weights and bias of the layer
        n_nodes = len(weights[0][0])                                #Number of nodes in the layer
        j = 1                                                       #Counter for nodes
        nodes = []
        function = "@"+str(model.layers[counter_layers-2].activation).split(" ")[1]
        while j <= n_nodes:
            var = "node_"+str(j)+"_"+str(counter_layers)+"("+variables+")"
            k = 0                                                   #Counter for nodes in the previous layer
            parents = {}                                            #Create a diccionary for each parent and their weight
            while k < len(last_layer):
                parents[last_layer[k].id_n] = weights[0][k][j-1]    #Add each parent to the current node
                k += 1
            bias = weights[1][j-1]
            nodes.append(node_f(var,parents,bias))                     #Define the new node
            j += 1
        last_layer = nodes
        layer = layer_f(n_nodes,nodes,counter_layers,function)
        h_layers.append(layer)      #Define the new hidden layer
        i += 1
        counter_layers += 1
    
    #Create the output layer
    weights = model.layers[counter_layers-2].get_weights()      #Weights of the layer
    n_nodes = len(weights[0][0])                                   #Number of nodes in the layer
    j = 1                                                       #Counter for nodes
    nodes = []
    function = "@"+str(model.layers[counter_layers-2].activation).split(" ")[1]
    lines = [line.rstrip('\n') for line in open(dlabels)]
    lines = normalizeStrings(lines)
    while j <= n_nodes:
        var = lines[j-1]+"("+variables+")"
        k = 0                                                   #Counter for nodes in the previous layer
        parents = {}                                            #Create a diccionary for each parent and their weight
        while k < len(last_layer):
            parents[last_layer[k].id_n] = weights[0][k][j-1]    #Add each parent to the current node
            k += 1
        bias = weights[1][j-1]
        nodes.append(node_f(var,parents,bias))                       #Define the new node
        j += 1
    output_layer = layer_f(n_nodes,nodes,counter_layers,function)          #Define the output layer
    Neural_net = NN_f(input_layer,h_layers,output_layer)
    print Neural_net
    return Neural_net

def normalizeStrings(lines):
    i = 0
    while i < len(lines):
        lines[i] = lines[i].lower()
        lines[i] = lines[i].replace("-","_")
        i +=1
    return lines
    
