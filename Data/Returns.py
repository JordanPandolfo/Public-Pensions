import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os 

os.chdir("/home/pando004/Desktop/PublicPension/Data")

df      = pd.read_csv('Returns.csv',sep=",",encoding="cp1252")     # state-level data from Public Pensions Database

ret_total = []
rate_total = []

# for each year 
for at,date in enumerate(df.fy.unique()):

    # create temp object 
    avg_ret = 0
    avg_rate = 0
    
    # for all plans in that year, calculate total liabilities 
    temp_ret = float(np.nansum( df[ df.fy == date ]['InvestmentReturn_1yr']*df[ df.fy == date ]['ActLiabilities_GASB'] )/np.nansum( df[ df.fy == date ]['ActLiabilities_GASB'] ))
    
    temp_rate = float(np.nansum( df[ df.fy == date ]['InvestmentReturnAssumption_GASB']*df[ df.fy == date ]['ActLiabilities_GASB'] )/np.nansum( df[ df.fy == date ]['ActLiabilities_GASB'] ))
            
    # record year number
    ret_total.append( temp_ret )
    rate_total.append( temp_rate )


# compute compounded expected return 

init_expect = 1
init_actual = 1

recorder_expect = [init_expect]
recorder_actual = [init_actual]

for at,date in enumerate(df.fy.unique()):
    
    
    init_expect = (1+rate_total[at])*init_expect
    
    init_actual = (1+ret_total[at])*init_actual 

    recorder_expect.append( init_expect )
    recorder_actual.append( init_actual )


print(recorder_expect[-1])
print(recorder_actual[-1])


#plt.style.use('ggplot')

from matplotlib.ticker import MaxNLocator

plt.close('all')
ax = plt.figure().gca()
ax.plot(np.arange(2000,2021),recorder_expect,lw=3,label='Target')
ax.plot(np.arange(2000,2021),recorder_actual,lw=3,ls='--',label='Actual') 
ax.xaxis.set_major_locator(MaxNLocator(integer=True))
ax.legend(fontsize=15)
ax.set_xlabel('Year',fontsize=15)
ax.set_ylabel('Cumulative Portfolio Value',fontsize=15)
ax.set_title('Target versus Actual Public Pension Returns',fontsize=15)
plt.savefig('return_data.jpg',format='jpg')


