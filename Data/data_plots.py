import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os 

os.chdir("/home/pando004/Desktop/PublicPension/Data")

df      = pd.read_csv('StateData.csv',sep=",",encoding="cp1252")     # state-level data from Public Pensions Database

df = df[[ 'StateAbbrev','fy','PlanName','SocSecCovered','PercentReqContPaid',
         'InvestmentReturnAssumption_GASB',
         'ActLiabilities_GASB','ActFundedRatio_GASB']]

# drop pre-2001 observations
df = df[df.fy>= 2001]

# drop this OK plan because missing ARC contribution data
df = df[~(df.PlanName == 'Oklahoma Municipal Employees') ]

# Step 2: create consolidated panel at the state-level 

# create new dataframe for consolidated state values 
df_con = pd.DataFrame( columns = [ 'State','Year','theta','discount','fr','pvl' ] )

# for each state    
for idx,state in enumerate(df.StateAbbrev.unique()):

    for at,date in enumerate(df.fy.unique()):
        
        weights = df[ (df.StateAbbrev == state) & (df.fy == date)]['ActLiabilities_GASB']/np.nansum( df[ (df.StateAbbrev == state) & (df.fy == date)]['ActLiabilities_GASB'] )
               
        theta_temp = np.nansum( weights*df[ (df.StateAbbrev == state) & (df.fy == date)]['PercentReqContPaid'] )
        
        discount_temp = np.nansum( weights*df[ (df.StateAbbrev == state) & (df.fy == date)]['InvestmentReturnAssumption_GASB'] )

        fr_temp = np.nansum( weights*df[ (df.StateAbbrev == state) & (df.fy == date)]['ActFundedRatio_GASB'] )

        pvl_temp = np.nansum( df[ (df.StateAbbrev == state) & (df.fy == date)]['ActLiabilities_GASB'] )
        
        # append dataframe 
        df_append = pd.DataFrame( [[state,date,theta_temp,discount_temp,fr_temp,pvl_temp]],columns= ['State','Year','theta','discount','fr','pvl'] )
        
        df_con = df_con.append(df_append, ignore_index=True)
        
        

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                              # 
#   Univariate Trends with Volatility Bands    #
#                                              # 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

avg_fr = []
sd_fr  = []

agg_pvl = [] 

arc = [] 
sd_arc = []

for at,date in enumerate(df_con.Year.unique()):

    weights = df_con[ df_con.Year == date ]['pvl']/np.sum( df_con[ df_con.Year == date ]['pvl'] )

    avg_fr_temp = np.sum( weights*df_con[ df_con.Year == date ]['fr']  )

    sd_fr_temp = ( 51*np.sum( weights*( df_con[ df_con.Year == date ]['fr'] - avg_fr_temp )**2 )/50 )**(0.5)

    agg_pvl_temp = np.sum( df_con[ df_con.Year == date ]['pvl']  )/(1000*1000*1000)
    
    arc_temp = np.sum(weights*df_con[ df_con.Year == date ]['theta'] )
    
    sd_arc_temp = ( 51*np.sum( weights*( df_con[ df_con.Year == date ]['theta'] - arc_temp )**2 )/50 )**(0.5)
    
    avg_fr.append( avg_fr_temp )
    sd_fr.append( sd_fr_temp )
    agg_pvl.append( agg_pvl_temp )
    arc.append( arc_temp )
    sd_arc.append( sd_arc_temp)
    
avg_fr = np.asarray((avg_fr))
sd_fr  = np.asarray((sd_fr))    
agg_pvl = np.asarray((agg_pvl))
arc = np.asarray((arc))
sd_arc = np.asarray(( sd_arc ))
years = np.asarray(( df_con.Year.unique() ))


from matplotlib.ticker import MaxNLocator

plt.close('all')
ax = plt.figure().gca()
ax.plot(np.arange(2001,2021),avg_fr,lw=3,ls='--')
ax.fill_between(np.arange(2001,2021),avg_fr - sd_fr , avg_fr + sd_fr, alpha=.5   ) 
ax.xaxis.set_major_locator(MaxNLocator(integer=True))
ax.set_xlabel('Year',fontsize=15)
ax.set_ylabel('Funded Ratio',fontsize=15)
ax.set_title('Average Funded Ratio Over Time',fontsize=15)
plt.savefig('fr_data.jpg',format='jpg')

plt.close('all')
fig, (ax1,ax2) = plt.subplots(2,1) #.gca()
ax1.plot(np.arange(2001,2021),avg_fr,lw=3,ls='--')
ax1.fill_between(np.arange(2001,2021),avg_fr - sd_fr , avg_fr + sd_fr, alpha=.5   ) 
ax1.xaxis.set_major_locator(MaxNLocator(integer=True))
#ax1.set_xlabel('Year',fontsize=15)
ax1.set_xticks([])
ax1.set_ylabel('Funded Ratio',fontsize=15)
ax1.set_title('Weighted Funded Ratio',fontsize=15)

#ax2 = plt.figure().gca()
ax2.plot(np.arange(2001,2021),agg_pvl,lw=3,label='Liabilities')
ax2.plot(np.arange(2001,2021),avg_fr*agg_pvl,label='Assets',lw=3,ls='--')
ax2.legend(fontsize=15)
ax2.xaxis.set_major_locator(MaxNLocator(integer=True))
ax2.set_xlabel('Year',fontsize=15)
ax2.set_ylabel('$,Trillion',fontsize=15)
ax2.set_title('Asset and Liability Components',fontsize=15)
plt.savefig('joint_data.jpg',format='jpg')



plt.close('all')
ax = plt.figure().gca()
ax.plot(np.arange(2001,2021),agg_pvl,lw=3,label='Liabilities')
ax.plot(np.arange(2001,2021),avg_fr*agg_pvl,label='Assets',lw=3,ls='--')
ax.legend(fontsize=15)
ax.xaxis.set_major_locator(MaxNLocator(integer=True))
ax.set_xlabel('Year',fontsize=15)
ax.set_ylabel('$,Trillion',fontsize=15)
ax.set_title('Pension Aggregates Over Time',fontsize=15)
plt.savefig('pvl_data.jpg',format='jpg')

plt.close('all')
ax = plt.figure().gca()
ax.plot(np.arange(2001,2021),arc,lw=3,ls='--')
ax.fill_between(np.arange(2001,2021),arc - sd_arc , arc + sd_arc, alpha=.5   ) 
ax.xaxis.set_major_locator(MaxNLocator(integer=True))
ax.set_xlabel('Year',fontsize=15)
ax.set_ylabel('Fraction',fontsize=15)
ax.set_title('Fraction of Required Contributions Paid Over Time',fontsize=15)
plt.savefig('arc_data.jpg',format='jpg')






avg_dists = np.zeros(( 45,80 ))

for i in range(51):
    
    for j in range(45):
        
        avg_dists[j,:] = avg_dists[j,:] + full_panel_distributioncsv[i*45+j,:]/51
 
plt.close('all')
[plt.plot(avg_dists[i,:]) for i in range(45)]

 
plt.close('all')
plt.plot(np.arange(20,100),avg_dists[0,:],lw=3,label='Year 0')
#plt.plot(np.arange(20,100),avg_dists[9,:],lw=3,label='Year 10')
plt.plot(np.arange(20,100),avg_dists[19,:],ls='--',lw=3,label='Year 20')
#plt.plot(np.arange(20,100),avg_dists[29,:],lw=3,label='Year 30')
plt.plot(np.arange(20,100),avg_dists[39,:],ls=':',lw=3,label='Year 40')
plt.legend(fontsize=15)
plt.xlabel('Age Group',fontsize=15)
plt.ylabel('Proportion',fontsize=15)
plt.title('Distribution of Age Cohorts over Time',fontsize=15)
plt.savefig('dist.jpg',format='jpg')
