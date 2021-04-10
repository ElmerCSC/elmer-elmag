import pandas as pd
import matplotlib.pyplot as plt

data = pd.read_csv('RESU/coil_extraction.dat',delim_whitespace=True,header=None)

names_file = open('RESU/coil_extraction.dat.names')
data.columns = [line.split('res: ')[1].replace('\n','') for line in names_file.readlines() if 'res:' in line]

print (data)
plt.plot(data['time'], data['i_component(1)'], label='Coil')
plt.plot(data['time'], data['i_d1'], label='Dump Resistor')
plt.plot(data['time'], data['i_s1'], label='Source')
plt.legend()
plt.savefig('Figures/extraction.png')


