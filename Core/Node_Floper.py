import keras
import numpy as np
import calendar                 #Time packets are for creating new archives
import time

class node_f:
    def __init__(self, id_n, p_nodes, bias):
        self.id_n = id_n
        self.p_nodes = p_nodes
        self.bias = bias

    def __str__(self):
        return str(self.id_n)+"\n\t Parents: "+str(self.p_nodes)+"\n\t Bias: "+str(self.bias)+"\n\n"
