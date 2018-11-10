# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'viewer.ui'
#
# Created by: PyQt4 UI code generator 4.12.1
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui
from PyQt4.QtWebKit import QWebView

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

class Ui_ViewerWindow(object):
    def setupUi(self, ViewerWindow):
        ViewerWindow.setObjectName(_fromUtf8("ViewerWindow"))
        ViewerWindow.resize(733, 324)
        self.centralwidget = QtGui.QWidget(ViewerWindow)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        self.verticalLayout = QtGui.QVBoxLayout(self.centralwidget)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.WebView = QWebView(self.centralwidget)
        self.WebView.setObjectName(_fromUtf8("WebView"))
        self.verticalLayout.addWidget(self.WebView)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem)
        self.okButton = QtGui.QPushButton(self.centralwidget)
        self.okButton.setObjectName(_fromUtf8("okButton"))
        self.horizontalLayout.addWidget(self.okButton)
        self.verticalLayout.addLayout(self.horizontalLayout)
        ViewerWindow.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(ViewerWindow)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 733, 26))
        self.menubar.setObjectName(_fromUtf8("menubar"))
        ViewerWindow.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(ViewerWindow)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        ViewerWindow.setStatusBar(self.statusbar)

        self.retranslateUi(ViewerWindow)
        QtCore.QMetaObject.connectSlotsByName(ViewerWindow)

    def retranslateUi(self, ViewerWindow):
        ViewerWindow.setWindowTitle(_translate("ViewerWindow", "Tuning Result", None))
        self.okButton.setText(_translate("ViewerWindow", "Ok", None))

