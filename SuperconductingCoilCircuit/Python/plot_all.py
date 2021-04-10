import pandas as pd
import matplotlib.pyplot as plt

data_coil = pd.read_csv('RESU/coil.dat',delim_whitespace=True,header=None)
data_coil_extraction = pd.read_csv('RESU/coil_extraction.dat',delim_whitespace=True,header=None)

names_file = open('RESU/coil_extraction.dat.names')
columns = [line.split('res: ')[1].replace('\n','') for line in names_file.readlines() if 'res:' in line]
data_coil.columns = columns
data_coil_extraction.columns = columns

data = pd.concat([data_coil, data_coil_extraction])

print (data)
plt.plot(data['time']*1e3, data['i_component(1)'], label='Coil')
plt.plot(data['time']*1e3, data['i_d1'], label='Dump Resistor')
plt.plot(data['time']*1e3, data['i_s1'], label='Source')
plt.xlabel("Time(ms)")
plt.ylabel("Current(A)")
plt.legend()
plt.savefig('Figures/all_current.png')

plt.cla()
plt.plot(data['time']*1e3, data['v_component(1)']*1e-3, label='Coil')
plt.plot(data['time']*1e3, data['v_d1']*1e-3, label='Dump Resistor')
plt.plot(data['time']*1e3, data['v_s1']*1e-3, label='Source')
plt.xlabel("Time(ms)")
plt.ylabel("Voltage(kV)")
plt.legend()
plt.savefig('Figures/all_voltage.png')


