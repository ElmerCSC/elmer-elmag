# AUTHOR: F. Trillaud <ftrillaudp@gmail.com>
# DATE: 03/13/2021

# Libraries:
import itertools
import numpy as np
# Functions
import matplotlib.pyplot as plt

def func_FD(x,xd,x0,A):
  return 1.0/(A*np.exp((x-xd)/x0)+1)
  
X = np.arange(0, 2, 0.1)
Y = func_FD(X,1,0.05,1)

marker = itertools.cycle(('.', '+', 's', 'o', '*', 'v', 'H', 'D'))

### Figures #######
plt.rc('xtick', labelsize = 11)    # fontsize of the tick labels
plt.rc('ytick', labelsize = 11)    # fontsize of the tick labels
plt.grid(True)
# plot with various axes scales
plt.figure(1, figsize = (6,6))
plt.xlabel(r'$t$ (s)', fontsize = 16)
plt.ylabel(r'$f$ (-)', fontsize = 16)

plt.plot(X, Y, color='red', linestyle='-', marker='o', markersize ='4', label='Maximum temperature')

plt.savefig('./figure-FermiDirac.png', format = 'png', dpi = 300, bbox_inches = 'tight')
plt.show()
