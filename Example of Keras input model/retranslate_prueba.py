import keras
import numpy as np
import difflib 
from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasClassifier
from keras.utils import np_utils
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold
from sklearn.preprocessing import LabelEncoder
from sklearn.pipeline import Pipeline
from keras.models import load_model

if __name__ == "__main__":
	model = Sequential()
	model = load_model("net.h5")
	
	#Modify activation function
	print model.layers[0].activation
	print len(model.layers)
	model.layers[0].activation = keras.activations.softmax
	print model.layers[0].activation

	#Prueba diff
	file1 = "node_1_2(X1, X2, X3, X4)<- @relu( @add(0.6244668, @add ( @prod(0.70569754,td(X2)), @add( @prod(-0.3849561,td(X4)), @add( @prod(0.731073,td(X1)), @prod(-0.7602397,td(X3)))))))."
	file2 = "node_1_2(X1, X2, X3, X4)<- @relu( @add(0.5, @add ( @prod(0.70569754,td(X2)), @add( @prod(-0.2,td(X4)), @add( @prod(0.731073,td(X1)), @prod(-0.8,td(X3)))))))."
	print file1.split()
	print "\n\n"
	print file2.split()
	#diff = difflib.context_diff(file1.split(), file2.split())
	#for l in diff:
		#print l
	#Modify weight
	
