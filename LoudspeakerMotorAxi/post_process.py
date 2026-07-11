"""
Post-process ElmerFEM simulation of a loudspeaker motor.

0. Coil info
1. Load data
2. Display flux density
3. Extract B in airgap
4. Get Bl(x) using a line integration

Info:
When B-flux in airgap is plot, we use -B. Because the magnet is magnetized 
toward +y, the magnetic field crosses the airgap in the -x "direction". Hence, 
when using sample_over_line, the B-flux appears negative. We flip the result
simply for visualization purpose.

Bl is divided by 2 because we use a voice-coil with parallel configuration.
"""

import pyvista as pv
import numpy as np
import matplotlib.pyplot as plt

#%% helper function
def closest_value(values, target):
    idx = min(range(len(values)), key=lambda i: abs(values[i] - target))
    return idx, values[idx]

#%% some info about the voice-coil
# general parameters
H      = 16                 # height of winding
dwire  = 0.250              # wire diameter
layers = 4                  # number of layers (we consider 2 layers connected in parallel)
turns  = int((H/dwire) * 4) # number of turns

# geometrical info
xin    = 14.25              # radius of first layer
xout   = xin+layers*dwire   # radius of last layer
x_vc   = (xin+xout)/2       # x-axis center position of voice-coil
y_off  = 0                  # voice-coil offset compared to top-plate (-in, +out) -> only affects post-processing
y_vc   = 32+y_off           # y-axis center position of voice-coil (center vc => center top-plate)

# derived values
lwire = np.round((2*np.pi*x_vc) * turns * 1e-3, 2) # total length of wire (in meters)

#%% Load data
pv_mesh = pv.read("loudspeaker_axi/case_t0001.vtu")

#%% Plot results
# streamlines through voice-coil
streamlines = pv_mesh.streamlines(vectors="magnetic flux density", 
                                  n_points=15, 
                                  pointa=(x_vc * 1e-3, (y_vc-H/2)*1e-3 , 0), # 1e-3 to put mm back in meters
                                  pointb=(x_vc * 1e-3, (y_vc+H/2)*1e-3, 0),
                                  max_length=0.1,
                                  integrator_type=45,
                                  integration_direction="both",
                                  )

plotter = pv.Plotter()
plotter.add_mesh(pv_mesh, scalars="magnetic flux density e", 
                 show_edges=True, edge_opacity=0.1, n_colors=20,
                 cmap="BuPu", clim=(0, 3), lighting=False)
plotter.add_mesh(streamlines, lighting=False, color="k", opacity=0.2)

# lock 2D on xy plane
plotter.view_xy()
plotter.enable_2d_style()
plotter.show()

#%% Extract B in airgap
# x, y, z coordinates of airgap line
disp  = 12                                            # displacement of VC in air-gap (our reference is the voice-coil center)
start = np.array([x_vc, y_vc-disp - H/2, 0]) * 1e-3   # start (including bottom of voice-coil -> for Bl integration purpose)
stop  = np.array([x_vc, y_vc+disp + H/2, 0]) * 1e-3   # stop  (inclucing top of voice-coil -> for Bl integration purpose)

# extract line accross result
res  = 2000 # number of points
line = pv_mesh.sample_over_line(start, stop, resolution=res, progress_bar=False)

# get B-flux on line -> x, y & z components of the magnetic field are returned
B = line["magnetic flux density"]
x = np.linspace(-disp-H/2, disp+H/2, res+1)

# plot B-flux
fig, ax = plt.subplots() 
ax.plot(x, -B[:, 0], 'k')
ax.grid()
ax.set(xlabel="Displacement [mm]", ylabel="B [T]")

#%% calculate Bl from B and voice-coil dimensions
# Note that we use "xstep", "xdisp", but this does not represent movement on 
# the x axis. It follows the Bl(x) notation.
xstep = 0.1
xdisp = np.arange(-disp, disp+xstep, xstep)
Bl    = np.zeros(len(xdisp))

for i in range(len(xdisp)):
    istart = xdisp[i] - H/2
    istop  = xdisp[i] + H/2
    
    # bounds to integrate over voice-coil length -> get closest value
    ind0, _ = closest_value(x, istart)
    ind1, _ = closest_value(x, istop)

    # get the average of the field on voice-coil length
    # Bl is divided by 2 to account for the parallel config of the voice-coil
    Bl[i] = np.mean(B[ind0:ind1, 0]) * lwire / 2 

# plot Bl(x)
fig, ax = plt.subplots()
ax.plot(xdisp, -Bl, color="k", label="Bl(x)")
ax.plot(-xdisp, -Bl, color="k", linestyle=":", label="Bl(-x)")
ax.set(xlabel="Displacement [mm]", ylabel="Force Factor [T.m]")
ax.grid()
ax.legend()
plt.show()
