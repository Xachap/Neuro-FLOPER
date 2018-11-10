# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'testeditor.ui'
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

class Ui_TestEditor(object):
    def setupUi(self, TestEditor):
        TestEditor.setObjectName(_fromUtf8("TestEditor"))
        TestEditor.resize(800, 600)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8("logo_black.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        TestEditor.setWindowIcon(icon)
        self.centralwidget = QtGui.QWidget(TestEditor)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        self.verticalLayout = QtGui.QVBoxLayout(self.centralwidget)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.textBrowser = QtGui.QTextEdit(self.centralwidget)		#Here
        self.textBrowser.setObjectName(_fromUtf8("textBrowser"))
        self.verticalLayout.addWidget(self.textBrowser)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        self.CancelButton = QtGui.QPushButton(self.centralwidget)
        self.CancelButton.setObjectName(_fromUtf8("CancelButton"))
        self.horizontalLayout.addWidget(self.CancelButton)
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.okButton = QtGui.QPushButton(self.centralwidget)
        self.okButton.setObjectName(_fromUtf8("okButton"))
        self.horizontalLayout.addWidget(self.okButton)
        self.verticalLayout.addLayout(self.horizontalLayout)
        TestEditor.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(TestEditor)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 800, 26))
        self.menubar.setObjectName(_fromUtf8("menubar"))
        TestEditor.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(TestEditor)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        TestEditor.setStatusBar(self.statusbar)

        self.retranslateUi(TestEditor)
        QtCore.QMetaObject.connectSlotsByName(TestEditor)

    def retranslateUi(self, TestEditor):
        TestEditor.setWindowTitle(_translate("TestEditor", "Test Cases Editor", None))
        self.CancelButton.setText(_translate("TestEditor", "Cancel", None))
        self.okButton.setText(_translate("TestEditor", "Ok", None))

