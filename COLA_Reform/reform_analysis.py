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
os.chdir("/home/pando004/Desktop/PublicPension")
bshare    =  np.asarray(( pd.read_csv('benefit_share.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

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
os.chdir("/home/pando004/Desktop/PublicPension/COLA_Reform")

ass_reform    =  np.asarray(( pd.read_csv('avg_ass.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
pvl_reform    =  np.asarray(( pd.read_csv('pvl.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
fr_reform     =  np.asarray(( pd.read_csv('fr.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
fr_vol_reform =  np.asarray(( pd.read_csv('fr_vol.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

pension_tax_reform =  np.asarray(( pd.read_csv('pension_tax.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
total_tax_reform   =  np.asarray(( pd.read_csv('total_tax.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
tax_vol_reform     =  np.asarray(( pd.read_csv('tax_vol.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

ce_pub_reform      =  np.asarray(( pd.read_csv('ce_pub_base.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
ce_priv_reform     =  np.asarray(( pd.read_csv('ce_priv_base.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

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
plt.title('Impact of COLA Freeze on Funded Ratio',fontsize=15)
plt.ylim(.45,1.1)
plt.savefig('fr_cola.jpg',format='jpg')


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


os.chdir("/home/pando004/Desktop/PublicPension/Closed_Reform")
pvl_closed_reform    =  np.asarray(( pd.read_csv('pvl_closed.csv',sep=",",encoding="cp1252",header=None) ))

os.chdir("/home/pando004/Desktop/PublicPension/Hybrid_Reform")
pvl_hybrid_reform    =  np.asarray(( pd.read_csv('pvl_hybrid.csv',sep=",",encoding="cp1252",header=None) ))
os.chdir("/home/pando004/Desktop/PublicPension/COLA_Reform")

# change in pension liabilities
plt.figure(2)
plt.plot(np.arange(45),agg_pvl_base,label='Baseline',lw=3)
plt.plot(np.arange(45),pvl_closed_reform,label='Closed Reform',lw=3,ls='--')
plt.plot(np.arange(45),pvl_hybrid_reform,'g',label='Hybrid Reform',lw=3,ls=':')
plt.plot(np.arange(45),agg_pvl_close,'k',marker='o',ls='',label='COLA Reform') 
plt.legend(fontsize=15)
plt.xlabel('Years Since Reform',fontsize=15)
plt.ylabel('Pension Liabilities ($, Billion)',fontsize=15)
plt.title('Impact of COLA Freeze on Liabilities',fontsize=15)
plt.savefig('pvl_cola.jpg',format='jpg')




# # baseline funded ratios for each state
# plt.figure(1)
# [plt.plot(np.arange(45),fr_base[ival*45:ival*45+45]) for idx,ival in enumerate(state_list)]
# plt.xlabel('Time',fontsize=15)
# plt.ylabel('Funded Ratio',fontsize=15)
# plt.title('Baseline Scenario Funded Ratio',fontsize=15)
# plt.ylim(0.25,1.25)

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

os.chdir("/home/pando004/Desktop/PublicPension/Closed_Reform")
taxes_closed_reform    =  np.asarray(( pd.read_csv('taxes_closed.csv',sep=",",encoding="cp1252",header=None) ))
os.chdir("/home/pando004/Desktop/PublicPension/Hybrid_Reform")
taxes_hybrid_reform    =  np.asarray(( pd.read_csv('taxes_hybrid.csv',sep=",",encoding="cp1252",header=None) ))
os.chdir("/home/pando004/Desktop/PublicPension/COLA_Reform")


plt.figure(3)
plt.plot(np.arange(45),100*agg_tax_base,label='Baseline',lw=3)
#plt.fill_between(np.arange(45),agg_tax_base - tax_vol_base_vol , agg_tax_base + tax_vol_base_vol,alpha=.5   ) 
plt.plot(np.arange(45),100*taxes_closed_reform,label='Closed Reform',lw=3,ls='--')
plt.plot(np.arange(45),100*taxes_hybrid_reform,'g',label='Hybrid Reform',lw=3,ls=':')
plt.plot(np.arange(45),100*agg_tax_close,'k',marker='o',ls='',label='COLA Reform') 
plt.ylim(10,15)
plt.legend(fontsize=15)
plt.xlabel('Years Since Reform',fontsize=15)
plt.ylabel('Total State Tax (%)',fontsize=15)
plt.title('Impact of COLA Pension Reform on Taxes',fontsize=15)
plt.savefig('tax_cola.jpg',format='jpg')

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
os.chdir("/home/pando004/Desktop/PublicPension/COLA_Reform")

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
plt.title('Welfare Effects of COLA Freeze, by Sector',fontsize=15)
plt.savefig('welfare_cola.jpg',format='jpg')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                             #  
#   Generate Tables Output    #
#                             #  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# table for reform values
holder  = np.empty(( 51, 15 ))

state_names_alpha = np.array(['AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA','HI', 'ID',
                        'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA','MI', 'MN', 'MS',
                        'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY','NC', 'ND', 'OH', 'OK',
                        'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX','UT', 'VT', 'VA', 'WA', 'WV',
                        'WI', 'WY'], dtype=object)
    
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
            label="tab:cola_table", 
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
            label="tab:welfare_cola",
            column_format = "cccccccccccccc") )
                                     
                                     
os.chdir("/home/pando004/Desktop/PublicPension/ScatterPlots")  
np.savetxt('welfare_priv_cola.csv',sector_welfare[1,:])                                     
np.savetxt('welfare_pub_cola.csv',sector_welfare[0,:])                                     
np.savetxt('welfare_pub20_cola.csv',welfare_public[:,0])                                     
np.savetxt('welfare_pub55_cola.csv',welfare_public[:,34])                                     
