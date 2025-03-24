##################
### Python 3.x ###
##################

# AUTHOR: F. Trillaud <ftrillaudp@gmail.com>
# DATE: 02/18/2020

# to run in python terminal: exec(open("ccdThermalParaontacts.py").read())
# or python ccdThermalParaontacts.py at the shell

# Libraries:
import math
import itertools
import csv
import matplotlib as mpl
import matplotlib.pyplot as plt


### Figure #######
# plot with various axes scales
plt.figure(3, figsize = (10,6))
plt.style.use('grayscale')
plt.rc('xtick', labelsize = 14)    # fontsize of the tick labels
plt.rc('ytick', labelsize = 14)    # fontsize of the tick labels
plt.grid(False)
#plt.xlim((-0.2, 2))
#plt.ylim((0, 20))
plt.xlabel(r'$x$ (-)', fontsize = 20)
plt.ylabel(r'$y$ (-)', fontsize = 20)
marker = itertools.cycle(('.', '+', 's', 'o', '*', 'v', 'H', 'D'))

X_1, Y_1 = list(), list()
### Open external file ###
with open('data-lua.dat') as csvfile:
    readCSV = csv.reader(csvfile, delimiter='\t')
    for row in readCSV:
      X_1.append(float(row[0]))
      Y_1.append(float(row[1]))

plt.plot(X_1, Y_1, color='red', linestyle='-', marker='o', markersize ='5')


plt.savefig('data-lua.png', format = 'png', dpi = 300, bbox_inches = 'tight')
plt.show()
