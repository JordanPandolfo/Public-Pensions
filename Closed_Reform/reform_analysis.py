#
#   Jordan Pandolfo, 09-27-2021
#
#   Program to plot model outcomes under closed reform scenario
#
#
#   Input files: StateData.csv
#                CostLiving.csv   
#
#

#~~~~~~~~~~~~~~~~~~~~~~#
#                      #     
#   Import Packages    #
#                      # 
#~~~~~~~~~~~~~~~~~~~~~~#
import numpy as np
import pandas as pd
import os
import matplotlib.pyplot as plt

#plt.style.available
#plt.style.use('ggplot')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                   #     
#   Import Baseline Model Output    #
#                                   # 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Import dataframe
os.chdir("/home/pando004/Desktop/PublicPension/Baseline")

ass_base    =  np.asarray(( pd.read_csv('avg_ass.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
pvl_base    =  np.asarray(( pd.read_csv('pvl.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
fr_base     =  np.asarray(( pd.read_csv('fr.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
fr_vol_base =  np.asarray(( pd.read_csv('fr_vol.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

pension_tax_base =  np.asarray(( pd.read_csv('pension_tax.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
total_tax_base   =  np.asarray(( pd.read_csv('total_tax.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
tax_vol_base     =  np.asarray(( pd.read_csv('tax_vol.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

ce_pub_base      =  np.asarray(( pd.read_csv('ce_pub_base.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
ce_priv_base     =  np.asarray(( pd.read_csv('ce_priv_base.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
cohort_pub_base  =  np.asarray(( pd.read_csv('cohort_pub.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
cohort_priv_base =  np.asarray(( pd.read_csv('cohort_priv.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

os.chdir("/home/pando004/Desktop/PublicPension")
state_names =  np.asarray(( pd.read_csv('state_names.csv',sep=",",encoding="cp1252",header=None) ))[:,0]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                 #     
#   Import Reform Model Output    #
#                                 # 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Import dataframe
os.chdir("/home/pando004/Desktop/PublicPension/Closed_Reform")

ass_reform    =  np.asarray(( pd.read_csv('avg_ass_closed.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
pvl_reform    =  np.asarray(( pd.read_csv('pvl_closed.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
fr_reform     =  np.asarray(( pd.read_csv('fr_closed.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
fr_vol_reform =  np.asarray(( pd.read_csv('fr_vol_closed.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

pension_tax_reform =  np.asarray(( pd.read_csv('pension_tax_closed.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
total_tax_reform   =  np.asarray(( pd.read_csv('total_tax_closed.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
tax_vol_reform     =  np.asarray(( pd.read_csv('tax_vol_closed.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

ce_pub_reform      =  np.asarray(( pd.read_csv('ce_pub_close.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
ce_priv_reform     =  np.asarray(( pd.read_csv('ce_priv_close.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

# drop outlier observations//states
state_drop_list = []
state_list = np.arange(51)

for i in range(len(state_drop_list)):
    
    drop_idx = np.where(state_list == state_drop_list[i])[0][0]
    
    state_list = np.delete(state_list,drop_idx)
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                            #
#   Plots and Data Output    #
#                            #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
plt.close('all')

# compute average baseline funded ratio with volatility bands
agg_fr_base = np.zeros(45)
agg_fr_close = np.zeros(45)

agg_vol_base = np.zeros(45)
agg_vol_reform = np.zeros(45)

for i in range(45):
    
    total_pvl = 0 
    total_pvl_close = 0 
    for idx,ival in enumerate(state_list):
        total_pvl = total_pvl + pvl_base[ival*45:ival*45+45][i]
        total_pvl_close = total_pvl_close + pvl_reform[ival*45:ival*45+45][i]
        
    
    for idx,ival in enumerate(state_list):
        
        agg_fr_base[i] = agg_fr_base[i] + pvl_base[ival*45:ival*45+45][i]*fr_base[ival*45:ival*45+45][i] 
        agg_fr_close[i] = agg_fr_close[i] + pvl_reform[ival*45:ival*45+45][i]*fr_reform[ival*45:ival*45+45][i] 

    agg_fr_base[i] = agg_fr_base[i]/total_pvl
    agg_fr_close[i] = agg_fr_close[i]/total_pvl_close
    
    # compute weighted standard deviation
    base_sd = 0
    close_sd = 0
    for idx,ival in enumerate(state_list):
        
        base_sd = base_sd + pvl_base[ival*45:ival*45+45][i]*(fr_base[ival*45:ival*45+45][i]  - agg_fr_base[i])**2/total_pvl
        close_sd = close_sd + pvl_reform[ival*45:ival*45+45][i]*(fr_reform[ival*45:ival*45+45][i]  - agg_fr_close[i])**2/total_pvl_close

    agg_vol_base[i] = np.sqrt( base_sd*len(state_list)/(len(state_list)-1) )
    agg_vol_reform[i] = np.sqrt( close_sd*len(state_list)/(len(state_list)-1) )

plt.close('all')
# change in funded ratio
plt.figure(1)
plt.plot(np.arange(45),agg_fr_base,label='Baseline',lw=3)
plt.fill_between(np.arange(45),agg_fr_base - agg_vol_base , agg_fr_base + agg_vol_base,alpha=.5   ) 
plt.plot(np.arange(45),agg_fr_close,'g',label='Reform',lw=3,ls='--')
plt.fill_between(np.arange(45),agg_fr_close - agg_vol_reform , agg_fr_close + agg_vol_reform,facecolor='g',alpha=.5   ) 
plt.legend(fontsize=15)
plt.xlabel('Years Since Reform',fontsize=15)
plt.ylabel('Funded Ratio',fontsize=15)
plt.title('Impact of Closed Pension Reform on Funded Ratio',fontsize=15)
plt.ylim(.45,1.1)
plt.savefig('fr_closed.jpg',format='jpg')


# compute average baseline funded ratio with volatility bands
agg_pvl_base = np.zeros(45)
agg_pvl_close = np.zeros(45)

pvl_vol_base = np.zeros(45)
pvl_vol_reform = np.zeros(45)

for i in range(45):
    
    total_pvl = 0 
    total_pvl_close = 0 
    for idx,ival in enumerate(state_list):
        total_pvl = total_pvl + pvl_base[ival*45:ival*45+45][i]/(1000*1000)
        total_pvl_close = total_pvl_close + pvl_reform[ival*45:ival*45+45][i]/(1000*1000)
        
    
    for idx,ival in enumerate(state_list):
        
        agg_pvl_base[i] = agg_pvl_base[i] + pvl_base[ival*45:ival*45+45][i]*pvl_base[ival*45:ival*45+45][i]/(1000*1000*1000*1000)
        agg_pvl_close[i] = agg_pvl_close[i] + pvl_reform[ival*45:ival*45+45][i]*pvl_reform[ival*45:ival*45+45][i]/(1000*1000*1000*1000)

    agg_pvl_base[i] = agg_pvl_base[i]/total_pvl
    agg_pvl_close[i] = agg_pvl_close[i]/total_pvl_close
    
    # compute weighted standard deviation
    base_sd = 0
    close_sd = 0
    for idx,ival in enumerate(state_list):
        
        base_sd = base_sd + pvl_base[ival*45:ival*45+45][i]*(pvl_base[ival*45:ival*45+45][i]/(1000*1000)  - agg_pvl_base[i])**2/(total_pvl*1000*1000)
        close_sd = close_sd + pvl_reform[ival*45:ival*45+45][i]*(pvl_reform[ival*45:ival*45+45][i]/(1000*1000)  - agg_pvl_close[i])**2/(total_pvl_close*1000*1000)

    pvl_vol_base[i] = np.sqrt( base_sd*len(state_list)/(len(state_list)-1) )
    pvl_vol_reform[i] = np.sqrt( close_sd*len(state_list)/(len(state_list)-1) )


# change in pension liabilities
plt.figure(2)
plt.plot(np.arange(45),agg_pvl_base,label='Baseline',lw=3)
#plt.fill_between(np.arange(45),agg_pvl_base - pvl_vol_base , agg_pvl_base + pvl_vol_base,alpha=.5   ) 
plt.plot(np.arange(45),agg_pvl_close,'g',label='Reform',lw=3,ls='--')
#plt.fill_between(np.arange(45),agg_pvl_close - pvl_vol_reform , agg_pvl_close + pvl_vol_reform,facecolor='g',alpha=.5   ) 
plt.legend(fontsize=15)
plt.xlabel('Years Since Reform',fontsize=15)
plt.ylabel('Pension Liabilities ($, Billion)',fontsize=15)
plt.title('Impact of Closed Pension Reform on Liabilities',fontsize=15)
plt.savefig('pvl_closed.jpg',format='jpg')

np.savetxt('pvl_closed.csv',agg_pvl_close,delimiter=',')


# # baseline funded ratios for each state
#plt.figure(1)
#[plt.plot(np.arange(45),fr_reform[ival*45:ival*45+45]) for idx,ival in enumerate(state_list)]
#plt.xlabel('Time',fontsize=15)
#plt.ylabel('Funded Ratio',fontsize=15)
#plt.title('Baseline Scenario Funded Ratio',fontsize=15)
#plt.ylim(0.25,1.25)

# # baseline taxes
# plt.figure(2)
# [plt.plot(np.arange(45),total_tax_base[ival*45:ival*45+45]) for idx,ival in enumerate(state_list)]
# plt.xlabel('Time',fontsize=15)
# plt.ylabel('Average Tax Rate',fontsize=15)
# plt.title('Baseline Scenario Taxes',fontsize=15)

# % Change in baseline taxes

agg_tax_base = np.zeros(45)
agg_tax_close = np.zeros(45)

tax_vol_base_vol = np.zeros(45)
tax_vol_reform_vol = np.zeros(45)

for i in range(45):
    
    total_pvl = 0 
    total_pvl_close = 0 
    for idx,ival in enumerate(state_list):
        total_pvl = total_pvl + pvl_base[ival*45:ival*45+45][i]
        total_pvl_close = total_pvl_close + pvl_reform[ival*45:ival*45+45][i]
        
    
    for idx,ival in enumerate(state_list):
        
        agg_tax_base[i] = agg_tax_base[i] + pvl_base[ival*45:ival*45+45][i]*total_tax_base[ival*45:ival*45+45][i] 
        agg_tax_close[i] = agg_tax_close[i] + pvl_reform[ival*45:ival*45+45][i]*total_tax_reform[ival*45:ival*45+45][i] 

    agg_tax_base[i] = agg_tax_base[i]/total_pvl
    agg_tax_close[i] = agg_tax_close[i]/total_pvl_close
    
    # compute weighted standard deviation
    base_sd = 0
    close_sd = 0
    for idx,ival in enumerate(state_list):
        
        base_sd = base_sd +   pvl_base[ival*45:ival*45+45][i]*  (total_tax_base[ival*45:ival*45+45][i]    - agg_tax_base[i])**2/total_pvl
        close_sd = close_sd + pvl_reform[ival*45:ival*45+45][i]*(total_tax_reform[ival*45:ival*45+45][i]  - agg_tax_close[i])**2/total_pvl_close

    tax_vol_base_vol[i] = np.sqrt( base_sd*len(state_list)/(len(state_list)-1) )
    tax_vol_reform_vol[i] = np.sqrt( close_sd*len(state_list)/(len(state_list)-1) )

plt.figure(3)
plt.plot(np.arange(45),100*agg_tax_base,label='Baseline',lw=3)
#plt.fill_between(np.arange(45),agg_tax_base - tax_vol_base_vol , agg_tax_base + tax_vol_base_vol,alpha=.5   ) 
plt.plot(np.arange(45),100*agg_tax_close,'g',label='Reform',lw=3,ls='--')
#plt.fill_between(np.arange(45),agg_tax_close - tax_vol_reform_vol , agg_tax_close + tax_vol_reform_vol,facecolor='g',alpha=.5   ) 
plt.ylim(10,15)
plt.legend(fontsize=15)
plt.xlabel('Years Since Reform',fontsize=15)
plt.ylabel('Total State Tax (%)',fontsize=15)
plt.title('Impact of Closed Pension Reform on Taxes',fontsize=15)
plt.savefig('tax_closed.jpg',format='jpg')


np.savetxt('taxes_closed.csv',agg_tax_close,delimiter=',')

# plt.figure(3)
# [plt.plot(np.arange(45),total_tax_reform[ival*45:ival*45+45]-total_tax_base[ival*45:ival*45+45] ) for idx,ival in enumerate(state_list)]
# plt.xlabel('Time Since Reform',fontsize=15)
# plt.ylabel('Change, Relative to Baseline',fontsize=15)
# plt.title('Change in Tax Rates',fontsize=15)

# welfare analysis 
ce_priv_base = ce_priv_base.reshape(51,45,201)
ce_pub_base  = ce_pub_base.reshape(51,45,201)

ce_priv_reform = ce_priv_reform.reshape(51,45,201)
ce_pub_reform  = ce_pub_reform.reshape(51,45,201)

priv_dist    = cohort_priv_base.reshape(51,45,201)
pub_dist     = cohort_pub_base.reshape(51,45,201)

welfare_private = np.zeros((51,45))
welfare_public  = np.zeros((51,45))

pub_mask = np.empty((51,45),dtype=bool)
priv_mask = np.empty((51,45),dtype=bool)

for j in range(51):  
    for i in range(45):
        
        welfare_private[j,i] = 100*np.sum( priv_dist[j,i,:]*( (ce_priv_reform[j,i,:]-ce_priv_base[j,i,:])/ce_priv_base[j,i,:]   ) )
        welfare_public[j,i]  = 100*np.sum( pub_dist[j,i,:]*(   (ce_pub_reform[j,i,:]-ce_pub_base[j,i,:])/ce_pub_base[j,i,:]   ) )

        
    pub_mask[j]  = np.isfinite(welfare_public[j])
    priv_mask[j] = np.isfinite(welfare_private[j])
       
# create interpolated average welfare data by state for each sector and total state
from scipy import interpolate 

os.chdir("/home/pando004/Desktop/PublicPension")
sector_welfare = np.zeros(( 3, 51 ))
full_panel    =  np.asarray(( pd.read_csv('full_panel_distribution.csv',sep=",",encoding="cp1252",header=None) ))
StateDataValuescsv    =  np.asarray(( pd.read_csv('StateDataValues.csv',sep=",",encoding="cp1252",header=None) ))
os.chdir("/home/pando004/Desktop/PublicPension/Closed_Reform")

for j in range(51):  # for each state

    # create interpolators for public and private welfare
    f_pub  = interpolate.interp1d(np.arange(45)[pub_mask[0]],welfare_public[j,:][pub_mask[0]])
    f_priv = interpolate.interp1d(np.arange(45)[pub_mask[0]],welfare_private[j,:][pub_mask[0]])

    # create interpolated welfare holders
    interp_welfare_pub = np.zeros(45)
    interp_welfare_priv = np.zeros(45)

    for i in range(45): # for each age cohort 
    
        interp_welfare_pub[i]  = f_pub( np.arange(45)[i] )
        interp_welfare_priv[i] = f_priv( np.arange(45)[i] )
        
    # public sector proportion 
    pub_prop = StateDataValuescsv[j,6]
    
    # compute sector average
    current_dist = full_panel[j*45,:45]/np.sum(full_panel[j*45,:45])
    
    sector_welfare[0,j] = np.sum( current_dist*interp_welfare_pub )
    sector_welfare[1,j] = np.sum( current_dist*interp_welfare_priv )
    
    # compute state average 
    sector_welfare[2,j] = pub_prop*sector_welfare[0,j] + (1-pub_prop)*sector_welfare[1,j]



# unweighted averages across states, by age cohort
pub_welfare = np.zeros(45)
priv_welfare = np.zeros(45)

pub_vol = np.zeros(45)
priv_vol = np.zeros(45)
for i in range(45):  
    
    avg_welfare_pub = 0
    avg_welfare_priv = 0
    
    for idx,ival in enumerate(state_list):
        avg_welfare_pub = avg_welfare_pub + welfare_public[ival,i]
        avg_welfare_priv = avg_welfare_priv + welfare_private[ival,i]
        
    pub_welfare[i] = avg_welfare_pub/len(state_list)
    priv_welfare[i] = avg_welfare_priv/len(state_list)
    
    # deviation 
    sd_welfare_pub = 0
    sd_welfare_priv = 0
    for idx,ival in enumerate(state_list):
        sd_welfare_pub = sd_welfare_pub + ( welfare_public[ival,i] - pub_welfare[i] )**2
        sd_welfare_priv = sd_welfare_priv + ( welfare_private[ival,i] - priv_welfare[i] )**2
        
    pub_vol[i] = np.sqrt( sd_welfare_pub/(len(state_list)-1) )
    priv_vol[i] = np.sqrt( sd_welfare_priv/(len(state_list)-1) )

plt.figure(4)
plt.plot((20+np.arange(45))[pub_mask[0]],pub_welfare[pub_mask[0]],lw=3,label='Public')
plt.fill_between((20+np.arange(45))[pub_mask[0]],pub_welfare[pub_mask[0]]-pub_vol[pub_mask[0]],pub_welfare[pub_mask[0]]+pub_vol[pub_mask[0]],alpha=0.5)
plt.plot((20+np.arange(45))[priv_mask[0]],priv_welfare[priv_mask[0]],'g',lw=3,ls='--',label='Private')
plt.fill_between((20+np.arange(45))[pub_mask[0]],priv_welfare[pub_mask[0]]-priv_vol[pub_mask[0]],priv_welfare[pub_mask[0]]+priv_vol[pub_mask[0]],facecolor='g',alpha=0.5)
plt.legend(fontsize=15)
plt.xlabel('Age Cohort',fontsize=15)
plt.ylabel('Welfare Change (%)',fontsize=15)
plt.title('Welfare Effects of Closed Pension, by Sector',fontsize=15)
plt.savefig('welfare_closed.jpg',format='jpg')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                             #  
#   Generate Tables Output    #
#                             #  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# table for baseline parameters used in state data values
d = {'State': state_names,
     'Worker Ratio': StateDataValuescsv[:,0],
     'Theta': (100*StateDataValuescsv[:,1]).astype(int)/100,
     'Discount': (100*100*StateDataValuescsv[:,2]).astype(int)/100,
     'FR': (100*StateDataValuescsv[:,3]).astype(int)/100,
     'Pension': (StateDataValuescsv[:,4]).astype(int),
     'Wage Scale': StateDataValuescsv[:,5],
     'Sector': (100*StateDataValuescsv[:,6]).astype(int)/100,
     'Portfolio': (100*StateDataValuescsv[:,7]).astype(int),
     'PVL': (StateDataValuescsv[:,8]/(1000*1000)).astype(int),
     'Cost': StateDataValuescsv[:,9],
     'SS': StateDataValuescsv[:,10],
     'Total': ((StateDataValuescsv[:,4]).astype(int) + StateDataValuescsv[:,10]*16.5).astype(int)
     } 

df = pd.DataFrame( data = d )

with open( os.path.join(os.getcwd(), "baseline.tex"), "w"
  )  as file:
        file.write( df[[ 'State','Theta','Discount','PVL','FR','Pension','Total','Sector','Portfolio']].to_latex(
            #float_format="%.2f",
            header= ['State','$\theta$','$r^p$','PVL','$\chi^0$','$\bar{b}','$b^{pub}$','Sector','$\alpha^p$'],
            index=False,
            caption="This is caption",
            label="tab:parameter_table",
            column_format = "ccccccccc") )

# table for baseline forecasts
holder  = np.zeros(( 51, 15 ))

# for each state 
for i in range(51):
        
    # assets
    holder[i,0] = ass_base[i*45:i*45+45][14]
    holder[i,1] = ass_base[i*45:i*45+45][29]
    holder[i,2] = ass_base[i*45:i*45+45][44]
    
    # PVL
    holder[i,3] = pvl_base[i*45:i*45+45][14] 
    holder[i,4] = pvl_base[i*45:i*45+45][29] 
    holder[i,5] = pvl_base[i*45:i*45+45][44] 
    
    # FR
    holder[i,6] = (100*fr_base[i*45:i*45+45][14]).astype(int)/100 
    holder[i,7] = fr_base[i*45:i*45+45][29] 
    holder[i,8] = fr_base[i*45:i*45+45][44] 

    # Tax
    holder[i,9] = total_tax_base[i*45:i*45+45][14]
    holder[i,10] = total_tax_base[i*45:i*45+45][29] 
    holder[i,11] = total_tax_base[i*45:i*45+45][44] 

    # Tax Vol
    holder[i,12] = tax_vol_base[i*45:i*45+45][14]
    holder[i,13] = tax_vol_base[i*45:i*45+45][29]
    holder[i,14] = tax_vol_base[i*45:i*45+45][44]    
    
d = {'State': state_names,
     '15 Yr A': (holder[:,0]/(1000*1000)).astype(int),
     '30 Yr A': (holder[:,1]/(1000*1000)).astype(int),
     '45 Yr A': (holder[:,2]/(1000*1000)).astype(int),
     '15 Yr P': (holder[:,3]/(1000*1000)).astype(int),
     '30 Yr P': (holder[:,4]/(1000*1000)).astype(int),
     '45 Yr P': (holder[:,5]/(1000*1000)).astype(int),
     '15 Yr F': (100*holder[:,6]).astype(int),
     '30 Yr F': (100*holder[:,7]).astype(int),
     '45 Yr F': (100*holder[:,8]).astype(int),
     '15 Yr T': 100*holder[:,9],
     '30 Yr T': 100*holder[:,10],
     '45 Yr T': 100*holder[:,11],
     '15 Yr V': 100*holder[:,12],
     '30 Yr V': 100*holder[:,13],
     '45 Yr V': 100*holder[:,14]} 

df = pd.DataFrame( data = d )

with open( os.path.join(os.getcwd(), "forecast.tex"), "w"
  )  as file:
        file.write( df[[ 'State','15 Yr P','30 Yr P','45 Yr P',
                                 '15 Yr F','30 Yr F','45 Yr F',
                                 '15 Yr T','30 Yr T','45 Yr T',
                                 '15 Yr V','30 Yr V','45 Yr V']].to_latex(
            float_format="%.1f",
            header= ['State','15y','30y','45y','15y','30y','45y','15y','30y','45y','15y','30y','45y'],
            index=False,
            caption="This is caption",
            label="tab:forecast_table",
            column_format = "ccccccccccccc") )


# table for reform values
holder  = np.empty(( 51, 15 ))

# for each state 
for i in range(51):
        
    # assets
    holder[i,0] = (100*( ass_reform[i*45:i*45+45][14] - ass_base[i*45:i*45+45][14])/ass_base[i*45:i*45+45][14] )
    holder[i,1] = (100*( ass_reform[i*45:i*45+45][29] - ass_base[i*45:i*45+45][29])/ass_base[i*45:i*45+45][29] )
    holder[i,2] = (100*( ass_reform[i*45:i*45+45][44] - ass_base[i*45:i*45+45][44])/ass_base[i*45:i*45+45][44]  )
    
    # PVL
    holder[i,3] = (100*( pvl_reform[i*45:i*45+45][14] - pvl_base[i*45:i*45+45][14])/pvl_base[i*45:i*45+45][14] )
    holder[i,4] = (100*( pvl_reform[i*45:i*45+45][29] - pvl_base[i*45:i*45+45][29])/pvl_base[i*45:i*45+45][29] )
    holder[i,5] = (100*( pvl_reform[i*45:i*45+45][44] - pvl_base[i*45:i*45+45][44])/pvl_base[i*45:i*45+45][44] )
    
    # FR
    holder[i,6] = (100*( fr_reform[i*45:i*45+45][14] - fr_base[i*45:i*45+45][14])/fr_base[i*45:i*45+45][14] )
    holder[i,7] = (100*( fr_reform[i*45:i*45+45][29] - fr_base[i*45:i*45+45][29])/fr_base[i*45:i*45+45][29] )
    holder[i,8] = (100*( fr_reform[i*45:i*45+45][44] - fr_base[i*45:i*45+45][44])/fr_base[i*45:i*45+45][44] )

    # Tax
    holder[i,9] = (100*( total_tax_reform[i*45:i*45+45][14] - total_tax_base[i*45:i*45+45][14])/total_tax_base[i*45:i*45+45][14] )
    holder[i,10] = (100*( total_tax_reform[i*45:i*45+45][29] - total_tax_base[i*45:i*45+45][29])/total_tax_base[i*45:i*45+45][29] )
    holder[i,11] = (100*( total_tax_reform[i*45:i*45+45][44] - total_tax_base[i*45:i*45+45][44])/total_tax_base[i*45:i*45+45][44] )

    # Tax Vol
    holder[i,12] = (100*( tax_vol_reform[i*45:i*45+45][14] - tax_vol_base[i*45:i*45+45][14])/tax_vol_base[i*45:i*45+45][14] )
    holder[i,13] = (100*( tax_vol_reform[i*45:i*45+45][29] - tax_vol_base[i*45:i*45+45][29])/tax_vol_base[i*45:i*45+45][29]) 
    holder[i,14] = (100*( tax_vol_reform[i*45:i*45+45][44] - tax_vol_base[i*45:i*45+45][44])/tax_vol_base[i*45:i*45+45][44]) 
    
  
d = {'State': state_names,
     '15 Yr A': holder[:,0],
     '30 Yr A': holder[:,1],
     '45 Yr A': holder[:,2],
     '15 Yr P': holder[:,3],
     '30 Yr P': holder[:,4],
     '45 Yr P': holder[:,5],
     '15 Yr F': holder[:,6],
     '30 Yr F': holder[:,7],
     '45 Yr F': holder[:,8],
     '15 Yr T': holder[:,9],
     '30 Yr T': holder[:,10],
     '45 Yr T': holder[:,11],
     '15 Yr V': holder[:,12],
     '30 Yr V': holder[:,13],
     '45 Yr V': holder[:,14]} 

df = pd.DataFrame( data = d )

with open( os.path.join(os.getcwd(), "closed_reform.tex"), "w"
  )  as file:
        file.write( df[[ 'State','15 Yr P','30 Yr P','45 Yr P',
                                 '15 Yr F','30 Yr F','45 Yr F',
                                 '15 Yr T','30 Yr T','45 Yr T',
                                 '15 Yr V','30 Yr V','45 Yr V']].to_latex(
            float_format="%.1f",
            header= ['State','15y','30y','45y','15y','30y','45y','15y','30y','45y','15y','30y','45y'],
            index=False,
            caption="This is caption",
            label="tab:close_table",
            column_format = "ccccccccccccc") )


                                     
# table for welfare gains by age cohort and sector
    
d = {'State': state_names,
     '20 Yr Pub': welfare_public[:,0],
     '30 Yr Pub': welfare_public[:,9],
     '40 Yr Pub': welfare_public[:,19],
     '50 Yr Pub': welfare_public[:,29],
     '60 Yr Pub': welfare_public[:,39],
     '20 Yr Priv': welfare_private[:,0],
     '30 Yr Priv': welfare_private[:,9],
     '40 Yr Priv': welfare_private[:,19],
     '50 Yr Priv': welfare_private[:,29],
     '60 Yr Priv': welfare_private[:,39],
     'Priv Avg'  : sector_welfare[1,:],
     'Pub Avg'   : sector_welfare[0,:],
     'State Avg' : sector_welfare[2,:],
     } 

df = pd.DataFrame( data = d )

with open( os.path.join(os.getcwd(), "welfare.tex"), "w"
  )  as file:
        file.write( df[[ 'State','20 Yr Pub','30 Yr Pub','40 Yr Pub','50 Yr Pub','60 Yr Pub',
                                 '20 Yr Priv','30 Yr Priv','40 Yr Priv','50 Yr Priv','60 Yr Priv',
                                 'Pub Avg','Priv Avg','State Avg']].to_latex(
            float_format="%.2f",
            header= ['State','20 Yr','30 Yr','40 Yr','50 Yr','60 Yr',
                             '20 Yr','30 Yr','40 Yr','50 Yr','60 Yr',
                             'Pub Avg','Priv Avg','State Avg'],
            index=False,
            caption="This is caption",
            label="tab:welfare_close",
            column_format = "cccccccccccccc") )
                                     
   
                                     
                                     
os.chdir("/home/pando004/Desktop/PublicPension/ScatterPlots")  
np.savetxt('pvl_base.csv',pvl_base,delimiter=',')
np.savetxt('fr_base.csv',fr_base,delimiter=',')
np.savetxt('tax_base.csv',total_tax_base,delimiter=',')

np.savetxt('pvl_close.csv',pvl_reform,delimiter=',')
np.savetxt('fr_close.csv',fr_reform,delimiter=',')
np.savetxt('tax_close.csv',total_tax_reform,delimiter=',')

np.savetxt('welfare_state_close.csv',sector_welfare[2,:])                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
"""
 compute state-specific welfare heat map
"""


# just export sector_welfare 
np.savetxt('state_welfare.csv',sector_welfare[2,:],delimiter=',')

import chart_studio.plotly.plotly as py
import pandas as pd

py.tools.set_credentials_file(username='jor.pandolfo', api_key='oc311sp5dsBHLryMchRQ')
py.tools.set_config_file(world_readable=True,
                             sharing='public')

df = pd.DataFrame( data = {'code':state_names,'welfare':state_welfare} )

for col in df.columns:
    df[col] = df[col].astype(str)

scl = [[0.0, 'rgb(242,240,247)'],[0.2, 'rgb(218,218,235)'],[0.4, 'rgb(188,189,220)'],\
            [0.6, 'rgb(158,154,200)'],[0.8, 'rgb(117,107,177)'],[1.0, 'rgb(84,39,143)']]

data = [ dict(
        type='choropleth',
        colorscale = scl,
        autocolorscale = False,
        locations = df['code'],
        z = df['welfare'].astype(float),
        locationmode = 'USA-states',
        marker = dict(
            line = dict (
                color = 'rgb(255,255,255)',
                width = 2
            )
        ),
        colorbar = dict(
            title = "Welfare Change (%)"
        )
    ) ]

layout = dict(
        title = 'Welfare Effects of Closed Pension Reform',
        geo = dict(
            scope='usa',
            projection=dict( type='albers usa' ),
            showlakes = True,
            lakecolor = 'rgb(255, 255, 255)',
        ),
    )

fig = dict( data=data, layout=layout )

url = py.plot( fig, filename='d3-cloropleth-map1' )




