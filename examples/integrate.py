import scipy.integrate
import math
def f(x): 
  return math.exp(-x)/(1+math.exp(-x))**2
print(scipy.integrate.quad(f, 0, 1))