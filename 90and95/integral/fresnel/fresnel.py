import numpy as np 
from matplotlib import pyplot as plt 

data = np.loadtxt("fresnel.txt",delimiter=",")
ts=data[:,0]
cxs=data[:,1]
sxs=data[:,2]

plt.plot(cxs,sxs)
plt.show()
