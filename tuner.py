# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'tuner.ui'
#
# Created by: PyQt4 UI code generator 4.12.1
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    def _fromUtf8(s):
        return s

try:
    _encoding = QtGui.QApplication.UnicodeUTF8
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig, _encoding)
except AttributeError:
    def _translate(context, text, disambig):
        return QtGui.QApplication.translate(context, text, disambig)

class Ui_TunerWindow(object):
    def setupUi(self, TunerWindow):
        TunerWindow.setObjectName(_fromUtf8("TunerWindow"))
        TunerWindow.resize(647, 185)
	TunerWindow.setMinimumSize(QtCore.QSize(647, 185))
	TunerWindow.setMaximumSize(QtCore.QSize(647, 185))
        self.centralwidget = QtGui.QWidget(TunerWindow)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        self.verticalLayout = QtGui.QVBoxLayout(self.centralwidget)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
        self.layerName = QtGui.QLabel(self.centralwidget)
        font = QtGui.QFont()
        font.setBold(True)
        font.setWeight(75)
        self.layerName.setFont(font)
        self.layerName.setObjectName(_fromUtf8("layerName"))
        self.horizontalLayout_2.addWidget(self.layerName)
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem)
        self.isModified = QtGui.QLabel(self.centralwidget)
        self.isModified.setObjectName(_fromUtf8("isModified"))
        self.horizontalLayout_2.addWidget(self.isModified)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        self.line_2 = QtGui.QFrame(self.centralwidget)
        self.line_2.setFrameShape(QtGui.QFrame.HLine)
        self.line_2.setFrameShadow(QtGui.QFrame.Sunken)
        self.line_2.setObjectName(_fromUtf8("line_2"))
        self.verticalLayout.addWidget(self.line_2)
        self.comboBox = QtGui.QComboBox(self.centralwidget)
        self.comboBox.setObjectName(_fromUtf8("comboBox"))
        self.comboBox.addItem(_fromUtf8("")) #.setStyleSheet('color: red')
        self.comboBox.addItem(_fromUtf8(""))
        self.comboBox.addItem(_fromUtf8(""))
        self.comboBox.addItem(_fromUtf8(""))
        self.comboBox.addItem(_fromUtf8(""))
        self.verticalLayout.addWidget(self.comboBox)
        self.line = QtGui.QFrame(self.centralwidget)
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName(_fromUtf8("line"))
        self.verticalLayout.addWidget(self.line)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setSpacing(6)
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.cancelButton = QtGui.QPushButton(self.centralwidget)
        self.cancelButton.setObjectName(_fromUtf8("cancelButton"))
        self.horizontalLayout.addWidget(self.cancelButton)
        spacerItem1 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem1)
        self.saveButton = QtGui.QPushButton(self.centralwidget)
        self.saveButton.setObjectName(_fromUtf8("saveButton"))
        self.horizontalLayout.addWidget(self.saveButton)
        self.verticalLayout.addLayout(self.horizontalLayout)
        TunerWindow.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(TunerWindow)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 647, 26))
        self.menubar.setObjectName(_fromUtf8("menubar"))
        TunerWindow.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(TunerWindow)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        TunerWindow.setStatusBar(self.statusbar)

        self.retranslateUi(TunerWindow)
        QtCore.QMetaObject.connectSlotsByName(TunerWindow)

    def retranslateUi(self, TunerWindow):
        TunerWindow.setWindowTitle(_translate("TunerWindow", "Activation Function Tuner", None))
        self.layerName.setText(_translate("TunerWindow", "Layer", None))
        self.isModified.setText(_translate("TunerWindow", "Modified", None))
        self.comboBox.setItemText(0, _translate("TunerWindow", "@sigmoid", None))
        self.comboBox.setItemText(1, _translate("TunerWindow", "@linear", None))
        self.comboBox.setItemText(2, _translate("TunerWindow", "@softplus", None))
        self.comboBox.setItemText(3, _translate("TunerWindow", "@softsign", None))
        self.comboBox.setItemText(4, _translate("TunerWindow", "@relu", None))
        #self.comboBox.setItemText(5, _translate("TunerWindow", "@leaky_relu", None))
        self.comboBox.setItemText(5, _translate("TunerWindow", "@tanh", None))
        #self.comboBox.setItemText(7, _translate("TunerWindow", "@arctan", None))
        #self.comboBox.setItemText(8, _translate("TunerWindow", "@sinusoid", None))
        self.cancelButton.setText(_translate("TunerWindow", "Cancel", None))
        self.saveButton.setText(_translate("TunerWindow", "Save", None))

