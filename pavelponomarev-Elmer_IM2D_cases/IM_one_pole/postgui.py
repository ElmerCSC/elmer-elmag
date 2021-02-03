from __future__ import print_function

import matplotlib
#matplotlib.use('TkAgg')

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
from matplotlib.backend_bases import key_press_handler
from matplotlib.figure import Figure

import numpy as np

import sys
if sys.version_info[0] < 3:
    import Tkinter as Tk
else:
    import tkinter as Tk


#########################  Params and path ##################################
# name of the file with names
namefile = 'results/transient_results.dat.names'#'results/scalars.dat.names'

# default list of scalars for ploting
scalars_def = ['res: i_s']

# name of the file with scalar data
postfile_parallel = 'results/transient_results.dat.0'#'results/scalars.dat.0'
postfile_serial = 'results/transient_results.dat'#'results/scalars.dat'

#resolve parallel or serial data
import os.path
if os.path.exists(postfile_parallel):
    postfile = postfile_parallel
else:
    postfile = postfile_serial


######################### Functions ########################################

def read_names(fnames):
    lines=[]
    with open(fnames) as f:
        lines = f.readlines()
        lines = [x.strip() for x in lines] 
    nnames = lines[6:]

    names_no_num = []
    for name in nnames:
        names_no_num.append(name[name.find(":")+2:])
    print("Number of variable names read from the names file:", len(names_no_num))
    return names_no_num

def read_data():
    global data
    data = np.loadtxt(postfile, unpack=True)
    print("Number of scalars read from the dat file:", len(data))

def on_key_event(event):
    print('you pressed %s' % event.char)
 
def _quit(arg):
    root.quit()     # stops mainloop
    root.destroy() 

def refresh_data(event=[]):
    read_data()
    refresh_plot()
    print('data refreshed')

def refresh_plot(event=[]):
    selection = lb.curselection()

    a.cla()
    #a.hold(True) # it is depricated in newest 2.0 matplotlib
    a.grid()

    for idx in selection:      
        line = data[idx]
        a.plot(list(data[timeidx]),list(line), '.-', lw=2, label=var_names[idx])

    a.legend(loc='best')
    a.relim()
    canvas.draw()
    
################################# Main ###############################
# init the GUI
root = Tk.Tk()
root.title('Scalars Post GUI')
root.resizable(0,0)
root.bind('<Key>', on_key_event)
root.bind('<space>', refresh_data)
root.bind('<Escape>', _quit)

#data=[]
read_data()

# add listbox to the left
lb = Tk.Listbox(root, selectmode=Tk.MULTIPLE, width=25)
lb.bind('<<ListboxSelect>>', refresh_plot)
lb.grid(row=0, column=0, sticky=Tk.W+Tk.E+Tk.N+Tk.S)

bt = Tk.Button(root, text='Refresh data', command=refresh_data)
bt.grid(row=1, column=0, sticky=Tk.W+Tk.E+Tk.S)

# read names from the names file and populate with them the listbox
var_names = read_names(namefile)
for item in var_names:
    lb.insert(Tk.END, item)

# get default items and make them be preselected
active = []
timeidx = 0
for scalar_def in scalars_def:
    for idx, var in enumerate(var_names):
        if var == 'res: time':
            timeidx = idx
        if scalar_def == var:
            active.append(idx)

for item in active:
    lb.select_set(item)


# add plot widgets to the right
plot_frame = Tk.Frame(root) 
plot_frame.grid(row=0, column=1, rowspan=2)

f = Figure(figsize=(10, 7), dpi=75)
a = f.add_subplot(111)

#plot all default scalars
for item in active:
    a.plot(data[timeidx], data[item], '.-', lw=2, label=var_names[item])
a.grid()
a.legend(loc='best')

# add figure and mpl toolbar to the plot_frame
canvas = FigureCanvasTkAgg(figure=f, master=plot_frame)
canvas.show()
canvas.get_tk_widget().pack()
toolbar = NavigationToolbar2TkAgg(canvas, plot_frame)
toolbar.pack()


# set Tk window foreground on MacOSX
import platform
if platform.system() == 'Darwin':
    import os
    os.system('''/usr/bin/osascript -e 'tell app "Finder" to set frontmost of process "Python" to true' ''')


root.mainloop()
