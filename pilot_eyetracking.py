# -*- coding: utf-8 -*-
"""
Created on Wed May 13 16:56:35 2015

@author: ryphil
"""

###
#Gonna analyze pupil data?
###
#bring in .asc
#make a table of it
import numpy as np
import matplotlib.pyplot as plt

f = open('100.asc', 'r')  # We need to re-open the file
data = f.read()
for line in f:
        print(repr(line))
f.close()

