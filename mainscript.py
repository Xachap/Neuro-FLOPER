import sys
import os
import keras
import numpy as np
import calendar                 #Time packets are for creating new archives
import time
import ImageQt
import sys
import pandas
import random
import subprocess

from PyQt4.QtWebKit import QWebView
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.utils import plot_model
from keras.models import load_model
from PyQt4 import QtGui, QtCore
from sklearn.preprocessing import LabelEncoder
from Core.Restructure_Floper import restructure
from keras.utils.vis_utils import model_to_dot
from Core.NerualNet_Floper import NN_f

import mainwindow
from testeditor import Ui_TestEditor
from tuner import Ui_TunerWindow
from About import Ui_About
from trust import Ui_Trustability
from viewer import Ui_ViewerWindow

try:
    _encoding = QtGui.QApplication.UnicodeUTF8
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig)

class App(QtGui.QMainWindow, mainwindow.Ui_NeuroFloper):
    model = Sequential()
    labels = []
    conversion = ""
    model = False
    test = False
    mod = ["#@s1"]
    item = ""
    nmod = 1
    isModified = False
    df = ""
    
    def __init__(self):
        super(self.__class__, self).__init__()
        self.setupUi(self)
        
        self.actionDelete_Model.triggered.connect(self.delete_keras)
        self.actionLoad_Keras_Model.triggered.connect(self.browse_keras)
        self.actionFrom_csv.triggered.connect(self.load_csvcases)
        self.actionFrom_test.triggered.connect(self.load_testcases)
        self.actionSave_as_FLOPER_program.triggered.connect(self.save_Floper)
        self.SaveFLOPER.clicked.connect(self.save_Floper)
        self.actionExit.triggered.connect(self.exit_app)
	self.actionModify_Test_Cases.triggered.connect(self.modifyTestCases)
	self.actionEdit_FLOPER_program.triggered.connect(self.editFloperProgram)
	self.actionAbout.triggered.connect(self.about)
	self.Tune.clicked.connect(self.tune)
	self.actionTune_it.triggered.connect(self.tune)
        
        self.actionDelete_Model.setEnabled(False)
        self.Tune.setEnabled(False)
        self.actionTune_it.setEnabled(False)
        self.actionSave_as_FLOPER_program.setEnabled(False)
        self.actionEdit_FLOPER_program.setEnabled(False)
        self.SaveFLOPER.setEnabled(False)
	self.actionModify_Test_Cases.setEnabled(False)
        
        TestTag = "No test file loaded"
        self.testlabel.setText(_translate("MainWindow", TestTag, None))
	self.testlabel.setStyleSheet('color: red')
	
	GeneralTag = "No model loaded"
        self.label_2.setText(_translate("MainWindow", GeneralTag, None))
	self.label_2.setStyleSheet('color: red')

 ###########################################################################################
 #########			For load a Keras model				############
 ###########################################################################################

    def browse_keras(self):
        directory = str(QtGui.QFileDialog.getOpenFileName(self,"Select a Keras model (.h5)","/"))
        dlabels = str(QtGui.QFileDialog.getOpenFileName(self,"Select a labels file for the model choosed",directory))
        self.model = load_model(directory)

        image = plot_model(self.model, to_file='model.png', show_shapes=True, rankdir='LR')
        scene = QtGui.QGraphicsScene()
        scene.addPixmap(QtGui.QPixmap("model.png"))
        self.Image.setScene(scene)
        
        self.conversion = restructure(self.model,dlabels)
        activations = getActivations(self.conversion)
        self.FunctionsView.addItems(activations)
	self.FunctionsView.itemClicked.connect(self.modifyActivations)
        
        GeneralTag = "General information for: "+str(directory.split("/")[-1])
        self.label_2.setText(_translate("MainWindow", GeneralTag, None))
	self.label_2.setStyleSheet('color: green')
        
        self.LayerNumber.display(len(self.conversion.hidden_fs)+2)
        self.InputNumber.display(self.conversion.input_f.n_nodes)
        self.OutputNumber.display(self.conversion.output_f.n_nodes)

        self.actionDelete_Model.setEnabled(True)
        self.actionSave_as_FLOPER_program.setEnabled(True)
        self.actionEdit_FLOPER_program.setEnabled(True)
        self.SaveFLOPER.setEnabled(True)
	self.conversion.to_Floper()

	self.model = True
	self.checkTune()
	

 ###########################################################################################
 #########			For load and edit test cases			############
 ###########################################################################################


    def load_csvcases(self):
        directory = str(QtGui.QFileDialog.getOpenFileName(self,"Select a .csv file","/"))
        self.df = pandas.read_csv(directory, header=None)
	self.getTrust()

	TestTag = str(directory.split("/")[-1])+" loaded!"
        self.testlabel.setText(_translate("MainWindow", TestTag, None))
	self.testlabel.setStyleSheet('color: green')

	self.actionDelete_Model.setEnabled(True)
	self.actionModify_Test_Cases.setEnabled(True)
	self.test = True
	self.checkTune()

    def load_testcases(self):
        directory = str(QtGui.QFileDialog.getOpenFileName(self,"Select a .test file","/"))
	
	TestTag = str(directory.split("/")[-1])+" loaded!"
        self.testlabel.setText(_translate("MainWindow", TestTag, None))
	self.testlabel.setStyleSheet('color: green')

	self.actionDelete_Model.setEnabled(True)
	self.actionModify_Test_Cases.setEnabled(True)

	self.window = QtGui.QMainWindow()
	self.ui = Ui_TestEditor()
	self.ui.setupUi(self.window)
	self.window.show()
	self.ui.okButton.clicked.connect(self.saveTestCases)
	self.ui.CancelButton.clicked.connect(self.exit_testcases)
	self.ui.okButton.setStyleSheet('color: green')
	self.ui.CancelButton.setStyleSheet('color: red')
	
	with open(directory) as f:
    	    with open("0-tmp_test.fpl", "w") as f1:
        	for line in f:
             	    f1.write(line)
		    self.ui.textBrowser.insertPlainText(line)

	self.actionModify_Test_Cases.setEnabled(True)
	self.test = True
	self.checkTune()

    def getTrust(self):
	self.windowTr = QtGui.QMainWindow()
	self.ui = Ui_Trustability()
	self.ui.setupUi(self.windowTr)
	self.windowTr.show()
	self.ui.okButton.clicked.connect(self.saveTrust)
	self.ui.okButton.setStyleSheet('color: green')

    def saveTrust(self):
	try:
            val = float(self.ui.lineEdit.text())
	    if val>=0 and val<=1:
	        self.windowTr.close()
                self.csv_to_test(val)
	    else:
		QtGui.QErrorMessage.showMessage(self, 'Error in the introduced number, please enter a valid one')
        except ValueError:
            QtGui.QErrorMessage.showMessage(self, 'Error in the introduced number, please enter a valid one')
            
    def csv_to_test(self,value):
        columns = len(self.df.columns)-1
        dataset = self.df.values
        X = dataset[:,0:columns].astype(float)
        Y = dataset[:,columns]
        encoder = LabelEncoder()
        encoder.fit(Y)
        classes = list(encoder.classes_)
        string = ""
	
	self.window = QtGui.QMainWindow()
	self.ui = Ui_TestEditor()
	self.ui.setupUi(self.window)
	self.window.show()
	self.ui.okButton.clicked.connect(self.saveTestCases)
	self.ui.CancelButton.clicked.connect(self.exit_testcases)
	self.ui.okButton.setStyleSheet('color: green')
	self.ui.CancelButton.setStyleSheet('color: red')

        f = open('0-tmp_test.fpl','w')
        for index, rows in self.df.iterrows():
            for clas in classes:
                if clas == rows[columns]:
		    number = round(random.uniform(value, 1),3)
                    string = str(number)+" -> "+str(self.normalize(clas))+"("
                else:
		    number = round(random.uniform(0, 1-value),3)
                    string = str(number)+" -> "+str(self.normalize(clas))+"("
                j = 0
                while j < columns:
                    if j == (columns - 1):
                        string +=str(rows[j])+").\n"
                    else:
                        string += str(rows[j])+", "
                    j += 1
                f.write(string)
		self.ui.textBrowser.insertPlainText(string)
	self.df = ""
        f.close()

    def modifyTestCases(self):
	self.window = QtGui.QMainWindow()
	self.ui = Ui_TestEditor()
	self.ui.setupUi(self.window)
	self.window.show()
	self.ui.okButton.clicked.connect(self.saveTestCases)
	self.ui.CancelButton.clicked.connect(self.exit_testcases)
	self.ui.okButton.setStyleSheet('color: green')
	self.ui.CancelButton.setStyleSheet('color: red')
	self.ui.okButton.setText(_translate("TestEditor", "Save", None))

	with open("0-tmp_test.fpl", "r") as f1:
            for line in f1:
		self.ui.textBrowser.insertPlainText(line)

    def saveTestCases(self):
	f = open('0-tmp_test.fpl','w')
	string = self.ui.textBrowser.toPlainText()
	f.write(string)
	f.close()
	self.window.close()

 
 ###########################################################################################
 #########			For edit a Floper Program			############
 ###########################################################################################


    def editFloperProgram(self):
	self.window2 = QtGui.QMainWindow()
	self.ui = Ui_TestEditor()
	self.ui.setupUi(self.window2)
	self.window2.resize(1024, 600)
	self.window2.show()
	self.ui.okButton.clicked.connect(self.saveFloperProgram)
	self.ui.CancelButton.clicked.connect(self.exit_modifyFloper)
	self.window2.setWindowTitle(_translate("TestEditor", "FLOPER Editor", None))
	self.ui.okButton.setText(_translate("TestEditor", "Save", None))
	self.ui.okButton.setStyleSheet('color: green')
	self.ui.CancelButton.setStyleSheet('color: red')

	with open("0-fuzzy.fpl", "r") as f1:
            for line in f1:
		self.ui.textBrowser.insertPlainText(line)
	
    def saveFloperProgram(self):
	f = open('0-fuzzy.fpl','w')
	string = self.ui.textBrowser.toPlainText()
	f.write(string)
	f.close()
	if self.isModified == False:
	    self.label_2.setText(_translate("MainWindow", self.label_2.text()+" -> Modified", None))
	    self.isModified = True
	self.window2.close()
	
	QtGui.QMessageBox.question(self, 'Model saved',
                                        "Your model has been saved succesfully!",
                                        QtGui.QMessageBox.Ok)

    def exit_modifyFloper(self):
        self.window2.close()

 ###########################################################################################
 #########			For modify activation functions			############
 ###########################################################################################


    def modifyActivations(self, widgetItem):
	self.item = widgetItem
	self.windowT = QtGui.QMainWindow()
	self.ui = Ui_TunerWindow()
	self.ui.setupUi(self.windowT)
	self.windowT.show()
	self.ui.cancelButton.setStyleSheet('color: red')
	self.ui.saveButton.setStyleSheet('color: green')
	self.ui.cancelButton.clicked.connect(self.exit_modifyAct)
	self.ui.saveButton.clicked.connect(self.saveAct)
	self.ui.layerName.setText(_translate("TunerWindow", "Modify this layer with a new aggregator", None))
        self.ui.isModified.setText(_translate("TunerWindow", "Actual aggregator: "+str(self.item.text()).split(" ")[1], None))

	self.ui.comboBox.insertSeparator(9)
	pointer = 9
	
	for item in self.mod:
	    pointer += 1
	    if item != self.mod[-1]:
		self.ui.comboBox.insertItem(pointer,item,None)
	    else:
		self.ui.comboBox.insertSeparator(pointer)
		self.ui.comboBox.insertItem(pointer+1,item,None)
	

    def saveAct(self):
	tlist = str(self.item.text()).split(" ")
	targetNum = tlist[-1]
	self.modifyLayer(targetNum)
	self.windowT.close()

    def exit_modifyAct(self):
	self.windowT.close()

    def modifyLayer(self, num):
	new = str(self.ui.comboBox.currentText())

	if num == "layer":
  	    self.conversion.output_f.activation = new
	else:
	    self.conversion.hidden_fs[int(num)-2].activation = new
	
	activations = getActivations(self.conversion)
	self.FunctionsView.clear()
        self.FunctionsView.addItems(activations)
	
	self.conversion.to_Floper()

	if self.isModified == False:
	    self.label_2.setText(_translate("MainWindow", self.label_2.text()+" -> Modified", None))
	    self.isModified = True

	if new[0] == "#":
	    self.nmod += 1
	    self.mod.append("#@s"+str(self.nmod))	

###########################################################################################
    def tune(self):
	self.windowv = QtGui.QMainWindow()
	self.ui = Ui_ViewerWindow()
	self.ui.setupUi(self.windowv)
	self.windowv.show()
	self.ui.okButton.setStyleSheet('color: green')
	self.ui.okButton.clicked.connect(self.exit_viewer)
	
	TestTag = self.testlabel.text()
        self.testlabel.setText(_translate("MainWindow", "Please wait", None))
	self.testlabel.setStyleSheet('color: yellow')

	GeneralTag = self.label_2.text()
        self.label_2.setText(_translate("MainWindow", "Running thresholded algorithm...", None))
	self.label_2.setStyleSheet('color: yellow')

	result = subprocess.check_output("swipl -f webfloper.pl -g 'web_tuning('0', 10000),halt'",shell=True)
	
	self.ui.WebView.setHtml('<html><header><title>Result:</title></header><body>'+result+'</body></html>')

        self.testlabel.setText(_translate("MainWindow", TestTag, None))
	self.testlabel.setStyleSheet('color: green')
	self.label_2.setText(_translate("MainWindow", GeneralTag, None))
	self.label_2.setStyleSheet('color: green')

    def exit_viewer(self):
	self.windowv.close()

    def normalize(self,string):
        aux = ""
        aux = string.lower()
        aux = aux.replace("-","_")
        return aux
        
    def delete_keras(self):
        self.model = Sequential()
        scene = QtGui.QGraphicsScene()
        self.Image.setScene(scene)

        GeneralTag = "No model loaded"
        self.label_2.setText(_translate("MainWindow", GeneralTag, None))
	self.label_2.setStyleSheet('color: red')

	TestTag = "No test file loaded"
        self.testlabel.setText(_translate("MainWindow", TestTag, None))
	self.testlabel.setStyleSheet('color: red')

        self.FunctionsView.clear()

        self.LayerNumber.display(0)
        self.InputNumber.display(0)
        self.OutputNumber.display(0)

        self.actionDelete_Model.setEnabled(False)
        self.Tune.setEnabled(False)
        self.actionTune_it.setEnabled(False)
        self.actionSave_as_FLOPER_program.setEnabled(False)
        self.actionEdit_FLOPER_program.setEnabled(False)
        self.SaveFLOPER.setEnabled(False)
	self.actionModify_Test_Cases.setEnabled(False)
	self.model = False
	self.test = False

    def save_Floper(self):
        self.conversion.to_Floper()
        
	name = QtGui.QFileDialog.getSaveFileName(self, 'Save File')
	
	with open("0-fuzzy.fpl") as f:
    	    with open(name+".fl", "w") as f1:
        	for line in f:
             	    f1.write(line)

    def checkTune(self):
        if self.model == True and self.test == True:
	    self.Tune.setEnabled(True)
	    self.actionTune_it.setEnabled(False)

    def about(self):
	self.windowA = QtGui.QMainWindow()
	self.ui = Ui_About()
	self.ui.setupUi(self.windowA)
	self.windowA.show()
	self.ui.okButton.clicked.connect(self.exit_about)
	
	sceneA = QtGui.QGraphicsScene()
        sceneA.addPixmap(QtGui.QPixmap("logo_about.png"))
        self.ui.graphicsView.setScene(sceneA)

    def exit_about(self):
	self.windowA.close()

    def exit_testcases(self):
        self.window.close()

    def exit_app(self):
        sys.exit()

def getActivations(conversion):
    arr = list()
    for layer in conversion.hidden_fs:
         text = "- " + layer.activation + " in layer " + str(layer.n_layers)
         arr.append(text)
    text = "- " + conversion.output_f.activation + " in output layer"
    arr.append(text)
    return arr
        

def main():
    app = QtGui.QApplication(sys.argv)
    form = App()
    form.show()
    app.exec_()


if __name__ == '__main__':
    main()
