# post-processing routine using matplotlib

import numpy as np
import matplotlib.pyplot as plt
#from scipy import interpolate, signal


# name of the file with scalar data
postfile_parallel = 'results/transient_results.dat.0'
postfile_serial = 'results/transient_results.dat'

#resolve parallel or serial data
import os.path
if os.path.exists(postfile_parallel):
    postfile = postfile_parallel
else:
    postfile = postfile_serial


# with torque groups
t, M, Mnf, iu, iv, iw, uu, uv, uw, icomp, iew = np.loadtxt(postfile, unpack=True, usecols=(6,2,5,7,15,23,8,16,24,9,11))
i1,i2,i3,i4,i5,i6,i7,i8,i9,i10 = np.loadtxt(postfile, unpack=True, usecols=(31,37,43,49,55,61,67,73,79,85))

#t, M, iu, iv, iw, uu, uv, uw = np.loadtxt(postfile, unpack=True, usecols=(5,2,6,13,20,8,15,22))
#i1,i2,i3,i4,i5,i6,i7,i8,i9,i10 = np.loadtxt(postfile, unpack=True, usecols=(27,43,49,55,61,67,73,79,85,91))


plt.figure()
plt.plot(t, M*0.160, label="band")
plt.grid(True)
plt.hold(True)
plt.plot(t, Mnf*4*0.160, label="nodal")
plt.ylabel("Torque")
plt.legend(loc='best')

plt.figure()
plt.plot(t, iu, label="i_u")
plt.grid(True)
plt.hold(True)
plt.plot(t, iv, label="i_v")
plt.plot(t, iw, label="i_w")
plt.ylabel("Current")
plt.legend(loc='best')

plt.figure()
plt.plot(t, iu, label="source")
plt.grid(True)
plt.hold(True)
plt.plot(t, icomp, lw=3, label="component")
plt.plot(t, iew,  label="ew")
plt.ylabel("Currents U-phase")
plt.legend(loc='best')


'''
plt.figure()
plt.plot(t, uu, label="U_u")
plt.grid(True)
plt.hold(True)
plt.plot(t, uv, label="U_v")
plt.plot(t, uw, label="U_w")
plt.ylabel("Voltage")
plt.legend(loc='best')
'''

plt.figure()
plt.plot(t, uu*iu+uv*iv+uw*iw, label="U_u")
plt.ylabel("Power")
plt.grid(True)



plt.figure()
plt.plot(t, i4, label="i4")
plt.grid(True)
plt.hold(True)
plt.plot(t, i5, label="i5")
plt.plot(t, i6, label="i6")
plt.plot(t, i7, label="i7")
plt.plot(t, i3, label="i3")
plt.plot(t, i8, label="i8")
plt.plot(t, i1, label="i1")
plt.plot(t, i2, label="i2")
plt.plot(t, i9, label="i9")
plt.plot(t, i10, label="i10")
plt.plot(t, i1+i2+i3+i4+i5+i6+i7+i8+i9+i10, label="sum of pole currents")
plt.ylabel("Rotor Current")
plt.legend(loc='best')
plt.xlabel("Time, $t$ [s]")


plt.show()
