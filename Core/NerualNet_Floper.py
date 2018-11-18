import keras
import numpy as np
import calendar                 #Time packets are for creating new archives
import time
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.utils import plot_model

class NN_f:
    def __init__(self,input_l, hidden_ls, output_l):
        self.input_f = input_l  #Input layer
        self.hidden_fs = hidden_ls  #Array of hidden layers
        self.output_f = output_l    #Output layer
        #self.to_Floper()
        
    def __str__(self):
        string = "Input layer:\n -Number of entry nodes: "+str(self.input_f.n_nodes)+"\n\nHidden layers: "+str(len(self.hidden_fs))+"\n - Layers:\n"
        for layer in self.hidden_fs:
            string += str(layer)
        string += "\n\nOutput layer:\n"+str(self.output_f)        
        return string

    def create_node(self, node, layer_activation):
        aux = node.id_n + "<- "+layer_activation+"( @add("+str(node.bias)+", @add ( "       
	keys = sorted(node.p_nodes.keys())
	keys.reverse()
	n_nodes_prev = len(node.p_nodes.keys())
        n_nodes_counter = n_nodes_prev
        if n_nodes_prev == 1:
            parent, weight = node.p_nodes.items()[0]
            aux += "@prod("+str(weight)+","+parent+"), 0"
        else:
            while len(keys)>0:
		parent = keys.pop()
                if n_nodes_counter ==1:
                    aux += "@prod("+str(node.p_nodes.get(parent))+","+parent+")"
                elif n_nodes_counter == 2:
                    aux += "@prod("+str(node.p_nodes.get(parent))+","+parent+"), "
                    n_nodes_counter = n_nodes_counter - 1
                else:
                    aux += "@prod("+str(node.p_nodes.get(parent))+","+parent+"), @add( "
                    n_nodes_counter = n_nodes_counter - 1
        for i in range(2,n_nodes_prev):
            aux += ")"
        aux = aux[:len(aux)-1]
        aux += ")))).\n"
        return aux

    #Only for fully-connected sequential models
    def to_Floper(self):
        f = open('0-fuzzy.fpl','w')     #Create the file
        aux = ""                                                                #Auxiliar array for each clause
        for layer in self.hidden_fs:
            for node in layer.nodes:
                aux = self.create_node(node,layer.activation)
                f.write(aux)
        f.write("\n")        
        for node in self.output_f.nodes:
                aux = self.create_node(node,self.output_f.activation)
                f.write(aux)
        f.close()
