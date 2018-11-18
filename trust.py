# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'trust.ui'
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

class Ui_Trustability(object):
    def setupUi(self, Trustability):
        Trustability.setObjectName(_fromUtf8("Trustability"))
        Trustability.resize(460, 198)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(_fromUtf8("logo_black.png")), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        Trustability.setWindowIcon(icon)
        self.centralwidget = QtGui.QWidget(Trustability)
        self.centralwidget.setObjectName(_fromUtf8("centralwidget"))
        self.verticalLayout = QtGui.QVBoxLayout(self.centralwidget)
        self.verticalLayout.setObjectName(_fromUtf8("verticalLayout"))
        self.label = QtGui.QLabel(self.centralwidget)
        self.label.setObjectName(_fromUtf8("label"))
        self.verticalLayout.addWidget(self.label)
        self.lineEdit = QtGui.QLineEdit(self.centralwidget)
        self.lineEdit.setObjectName(_fromUtf8("lineEdit"))
        self.verticalLayout.addWidget(self.lineEdit)
        self.horizontalLayout_2 = QtGui.QHBoxLayout()
        self.horizontalLayout_2.setObjectName(_fromUtf8("horizontalLayout_2"))
        self.label_3 = QtGui.QLabel(self.centralwidget)
        self.label_3.setObjectName(_fromUtf8("label_3"))
        self.horizontalLayout_2.addWidget(self.label_3)
        spacerItem = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout_2.addItem(spacerItem)
        self.label_2 = QtGui.QLabel(self.centralwidget)
        self.label_2.setObjectName(_fromUtf8("label_2"))
        self.horizontalLayout_2.addWidget(self.label_2)
        self.verticalLayout.addLayout(self.horizontalLayout_2)
        self.line = QtGui.QFrame(self.centralwidget)
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName(_fromUtf8("line"))
        self.verticalLayout.addWidget(self.line)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName(_fromUtf8("horizontalLayout"))
        spacerItem1 = QtGui.QSpacerItem(40, 20, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.horizontalLayout.addItem(spacerItem1)
        self.okButton = QtGui.QPushButton(self.centralwidget)
        self.okButton.setObjectName(_fromUtf8("okButton"))
        self.horizontalLayout.addWidget(self.okButton)
        self.verticalLayout.addLayout(self.horizontalLayout)
        Trustability.setCentralWidget(self.centralwidget)
        self.menubar = QtGui.QMenuBar(Trustability)
        self.menubar.setGeometry(QtCore.QRect(0, 0, 460, 26))
        self.menubar.setObjectName(_fromUtf8("menubar"))
        Trustability.setMenuBar(self.menubar)
        self.statusbar = QtGui.QStatusBar(Trustability)
        self.statusbar.setObjectName(_fromUtf8("statusbar"))
        Trustability.setStatusBar(self.statusbar)

        self.retranslateUi(Trustability)
        QtCore.QMetaObject.connectSlotsByName(Trustability)

    def retranslateUi(self, Trustability):
        Trustability.setWindowTitle(_translate("Trustability", "Trustability Factor", None))
        self.label.setText(_translate("Trustability", "Please introduce a trustability factor in the range of 0 to 1:", None))
        self.label_3.setText(_translate("Trustability", "0 = 0%", None))
        self.label_2.setText(_translate("Trustability", "1 = 100%", None))
        self.okButton.setText(_translate("Trustability", "Ok", None))

