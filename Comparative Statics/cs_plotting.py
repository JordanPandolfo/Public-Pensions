import numpy as np
import os 
import pandas as pd
import matplotlib.pyplot as plt
from itertools import cycle

#plt.style.use('ggplot')
plt.style.use('seaborn')
#print(plt.style.available)

#Import dataframe
os.chdir('/home/pando004/Desktop/PublicPension/Comparative Statics')

# Baseline parameters 
theta_base = .92
fr_base = .72
benefit_base = 34
public_base = .1
risky_base = .74
discount_base = .072
cola_base = 0
pvl_base = 108000000

time_grid = np.arange(0,45)

# MAKE SURE EXACT SAME NUMBER AS CS ITERATIONS
lines = ["-","--",":","."]
linecycler = cycle(lines)

# determine upper and lower bounds for each variable of interest
fr_lb = .5
fr_ub = 1.1
fr_vol_lb = 0
fr_vol_ub = .3
pen_tax_lb = 0
pen_tax_ub = .2
pen_tax_vol_lb = 0
pen_tax_vol_ub = .08
liab_lb = 0
liab_ub = 750
tax_lb = 0
tax_ub = .3

# import grids 
theta_grid = np.asarray(( pd.read_csv('theta_grid.csv',header=None,delim_whitespace=True)))[0]
cola_grid = np.asarray(( pd.read_csv('cola_grid.csv',header=None,delim_whitespace=True)))[0]
benefit_grid = np.asarray(( pd.read_csv('benefit_grid.csv',header=None,delim_whitespace=True)))[0]
sector_grid = np.asarray(( pd.read_csv('sector_grid.csv',header=None,delim_whitespace=True)))[0]
port_grid = np.asarray(( pd.read_csv('port_grid.csv',header=None,delim_whitespace=True)))[0]
fr_grid = np.asarray(( pd.read_csv('fr_grid.csv',header=None,delim_whitespace=True)))[0]
discount_grid = np.asarray(( pd.read_csv('discount_grid.csv',header=None,delim_whitespace=True)))[0]


# Varying ARC Contribution
theta_vals = np.asarray(( pd.read_csv('cs_theta.csv',header=None,delim_whitespace=True)))

plt.close('all')
plt.figure(1)
plt.subplot(2,3,1)
[plt.plot(time_grid,theta_vals[5 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.xticks([])
plt.ylim(fr_lb,fr_ub)
plt.title('Funded Ratio')
plt.subplot(2,3,2)
[plt.plot(time_grid,theta_vals[6 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(fr_vol_lb,fr_vol_ub)
plt.title('Funded Ratio Vol')
plt.xticks([])
plt.subplot(2,3,3)
[plt.plot(time_grid,theta_vals[3 + 8*i,:],next(linecycler),label=r'$\theta=$ %s' %round(theta_grid[i],2)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_lb,pen_tax_ub)
plt.title('Pension Tax')
plt.legend()
plt.xticks([])
plt.subplot(2,3,4)
[plt.plot(time_grid,theta_vals[4 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_vol_lb,pen_tax_vol_ub)
plt.title('Pension Tax Vol')
plt.subplot(2,3,5)
[plt.plot(time_grid,theta_vals[7 + 8*i,:]/(1000*1000),next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(liab_lb,liab_ub)
plt.title('Liabilities')
plt.subplot(2,3,6)
[plt.plot(time_grid,theta_vals[1 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(tax_lb,tax_ub)
plt.title('Total Tax')
plt.suptitle(r'Varying $\theta$')
plt.savefig('cs_theta.jpg',format='jpg')

# Varying size of public sector
theta_vals = np.asarray(( pd.read_csv('cs_prob.csv',header=None,delim_whitespace=True)))

plt.figure(2)
plt.subplot(2,3,1)
[plt.plot(time_grid,theta_vals[5 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.xticks([])
plt.ylim(fr_lb,fr_ub)
plt.title('Funded Ratio')
plt.subplot(2,3,2)
[plt.plot(time_grid,theta_vals[6 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(fr_vol_lb,fr_vol_ub)
plt.title('Funded Ratio Vol')
plt.xticks([])
plt.subplot(2,3,3)
[plt.plot(time_grid,theta_vals[3 + 8*i,:],next(linecycler),label=r'$p=$ %s' %round(sector_grid[i],2)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_lb,pen_tax_ub)
plt.title('Pension Tax')
plt.legend()
plt.xticks([])
plt.subplot(2,3,4)
[plt.plot(time_grid,theta_vals[4 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_vol_lb,pen_tax_vol_ub)
plt.title('Pension Tax Vol')
plt.subplot(2,3,5)
[plt.plot(time_grid,theta_vals[7 + 8*i,:]/(1000*1000),next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(liab_lb,liab_ub)
plt.title('Liabilities')
plt.subplot(2,3,6)
[plt.plot(time_grid,theta_vals[1 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(tax_lb,tax_ub)
plt.title('Total Tax')
plt.suptitle(r'Varying Size of Public Sector')
plt.savefig('cs_sector.jpg',format='jpg')

# Varying size of benefit
theta_vals = np.asarray(( pd.read_csv('cs_benefit.csv',header=None,delim_whitespace=True)))

plt.figure(3)
plt.subplot(2,3,1)
[plt.plot(time_grid,theta_vals[5 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.xticks([])
plt.ylim(fr_lb,fr_ub)
plt.title('Funded Ratio')
plt.subplot(2,3,2)
[plt.plot(time_grid,theta_vals[6 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(fr_vol_lb,fr_vol_ub)
plt.title('Funded Ratio Vol')
plt.xticks([])
plt.subplot(2,3,3)
[plt.plot(time_grid,theta_vals[3 + 8*i,:],next(linecycler),label=r'$b=$ %s' %int(benefit_grid[i])) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_lb,pen_tax_ub)
plt.title('Pension Tax')
plt.legend()
plt.xticks([])
plt.subplot(2,3,4)
[plt.plot(time_grid,theta_vals[4 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_vol_lb,pen_tax_vol_ub)
plt.title('Pension Tax Vol')
plt.subplot(2,3,5)
[plt.plot(time_grid,theta_vals[7 + 8*i,:]/(1000*1000),next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(liab_lb,liab_ub)
plt.title('Liabilities')
plt.subplot(2,3,6)
[plt.plot(time_grid,theta_vals[1 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(tax_lb,tax_ub)
plt.title('Total Tax')
plt.suptitle(r'Varying Size of Benefit')
plt.savefig('cs_benefit.jpg',format='jpg')

# Varying cola coverage
theta_vals = np.asarray(( pd.read_csv('cs_cola.csv',header=None,delim_whitespace=True)))

plt.figure(4)
plt.subplot(2,3,1)
[plt.plot(time_grid,theta_vals[5 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.xticks([])
plt.ylim(fr_lb,fr_ub)
plt.title('Funded Ratio')
plt.subplot(2,3,2)
[plt.plot(time_grid,theta_vals[6 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(fr_vol_lb,fr_vol_ub)
plt.title('Funded Ratio Vol')
plt.xticks([])
plt.subplot(2,3,3)
[plt.plot(time_grid,theta_vals[3 + 8*i,:],next(linecycler),label='miss= %s' %round(cola_grid[i],2)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_lb,pen_tax_ub)
plt.title('Pension Tax')
plt.legend()
plt.xticks([])
plt.subplot(2,3,4)
[plt.plot(time_grid,theta_vals[4 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_vol_lb,pen_tax_vol_ub)
plt.title('Pension Tax Vol')
plt.subplot(2,3,5)
[plt.plot(time_grid,theta_vals[7 + 8*i,:]/(1000*1000),next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(liab_lb,liab_ub)
plt.title('Liabilities')
plt.subplot(2,3,6)
[plt.plot(time_grid,theta_vals[1 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(tax_lb,tax_ub)
plt.title('Total Tax')
plt.suptitle(r'Varying COLA Coverage')
plt.savefig('cs_cola.jpg',format='jpg')


# Varying discount factor
theta_vals = np.asarray(( pd.read_csv('cs_discount.csv',header=None,delim_whitespace=True)))

plt.figure(5)
plt.subplot(2,3,1)
[plt.plot(time_grid,theta_vals[5 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.xticks([])
plt.ylim(fr_lb,fr_ub)
plt.title('Funded Ratio')
plt.subplot(2,3,2)
[plt.plot(time_grid,theta_vals[6 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(fr_vol_lb,fr_vol_ub)
plt.title('Funded Ratio Vol')
plt.xticks([])
plt.subplot(2,3,3)
[plt.plot(time_grid,theta_vals[3 + 8*i,:],next(linecycler),label=r'$r^s=$ %s' %round(discount_grid[i],2)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_lb,pen_tax_ub)
plt.title('Pension Tax')
plt.legend()
plt.xticks([])
plt.subplot(2,3,4)
[plt.plot(time_grid,theta_vals[4 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_vol_lb,pen_tax_vol_ub)
plt.title('Pension Tax Vol')
plt.subplot(2,3,5)
[plt.plot(time_grid,theta_vals[7 + 8*i,:]/(1000*1000),next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(liab_lb,liab_ub)
plt.title('Liabilities')
plt.subplot(2,3,6)
[plt.plot(time_grid,theta_vals[1 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(tax_lb,tax_ub)
plt.title('Total Tax')
plt.suptitle(r'Varying State Discount Factor')
plt.savefig('cs_discount.jpg',format='jpg')

# Varying initial funded ratio
theta_vals = np.asarray(( pd.read_csv('cs_fr.csv',header=None,delim_whitespace=True)))

plt.figure(6)
plt.subplot(2,3,1)
[plt.plot(time_grid,theta_vals[5 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.xticks([])
plt.ylim(fr_lb,fr_ub)
plt.title('Funded Ratio')
plt.subplot(2,3,2)
[plt.plot(time_grid,theta_vals[6 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(fr_vol_lb,fr_vol_ub)
plt.title('Funded Ratio Vol')
plt.xticks([])
plt.subplot(2,3,3)
[plt.plot(time_grid,theta_vals[3 + 8*i,:],next(linecycler),label=r'$\chi=$ %s' %round(fr_grid[i],2)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_lb,pen_tax_ub)
plt.title('Pension Tax')
plt.legend()
plt.xticks([])
plt.subplot(2,3,4)
[plt.plot(time_grid,theta_vals[4 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_vol_lb,pen_tax_vol_ub)
plt.title('Pension Tax Vol')
plt.subplot(2,3,5)
[plt.plot(time_grid,theta_vals[7 + 8*i,:]/(1000*1000),next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(liab_lb,liab_ub)
plt.title('Liabilities')
plt.subplot(2,3,6)
[plt.plot(time_grid,theta_vals[1 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(tax_lb,tax_ub)
plt.title('Total Tax')
plt.suptitle(r'Varying Initial Funded Ratio')
plt.savefig('cs_fr.jpg',format='jpg')

# Varying risky portfolio share
theta_vals = np.asarray(( pd.read_csv('cs_port.csv',header=None,delim_whitespace=True)))

plt.figure(7)
plt.subplot(2,3,1)
[plt.plot(time_grid,theta_vals[5 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.xticks([])
plt.ylim(fr_lb,fr_ub)
plt.title('Funded Ratio')
plt.subplot(2,3,2)
[plt.plot(time_grid,theta_vals[6 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(fr_vol_lb,fr_vol_ub)
plt.title('Funded Ratio Vol')
plt.xticks([])
plt.subplot(2,3,3)
[plt.plot(time_grid,theta_vals[3 + 8*i,:],next(linecycler),label=r'$\alpha^s=$ %s' %round(port_grid[i],2)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_lb,pen_tax_ub)
plt.title('Pension Tax')
plt.legend()
plt.xticks([])
plt.subplot(2,3,4)
[plt.plot(time_grid,theta_vals[4 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(pen_tax_vol_lb,pen_tax_vol_ub)
plt.title('Pension Tax Vol')
plt.subplot(2,3,5)
[plt.plot(time_grid,theta_vals[7 + 8*i,:]/(1000*1000),next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(liab_lb,liab_ub)
plt.title('Liabilities')
plt.subplot(2,3,6)
[plt.plot(time_grid,theta_vals[1 + 8*i,:],next(linecycler)) for i in range( int(np.shape(theta_vals)[0]/8) )]
plt.ylim(tax_lb,tax_ub)
plt.title('Total Tax')
plt.suptitle(r'Varying Pension Risky Portfolio Share')
plt.savefig('cs_port.jpg',format='jpg')

