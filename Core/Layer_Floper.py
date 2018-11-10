import keras
import numpy as np
import calendar                 #Time packets are for creating new archives
import time

class layer_f:
    def __init__(self, n_nodes, nodes, n_layers, activation):
        self.n_nodes = n_nodes  #Number of nodes in the layer
        self.nodes = nodes      #Array of nodes
        self.n_layers = n_layers    #Number of the layer inside the network
        self.activation = activation    #Activation function for the layer

    def __str__(self):
        string = "Number of nodes: "+str(self.n_nodes)+"\nLayer n: "+str(self.n_layers)+"\nActivation function: "+self.activation+"\n Nodes: \n"
        for node in self.nodes:
            string += "\t"+str(node)
        return string
