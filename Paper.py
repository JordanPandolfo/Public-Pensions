

"""
 import packages
"""
import numpy as np
import matplotlib.pyplot as plt


# number of states being evaluated
size = 2
#size = len(state_listcsv)

state_names = [ 'AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID',
                'IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS',
                'MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK',
                'OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV',
                'WI','WY']


"""
 plot output (state-specific)
"""
#plt.style.use('ggplot')
plt.style.use('seaborn')


state_listcsv = np.array((23,3))

state_id = 0 
print('State of Internest:', state_names[state_listcsv[state_id]-1] )

#~~~~~~~~~~~~~~~~~~~~~~~#
#                       #
#   State Aggregates    #
#                       #
#~~~~~~~~~~~~~~~~~~~~~~~#

plt.close('all')
plt.figure(1)
plt.plot(2011+np.arange(0,45),(avg_asscsv[state_id*45:(state_id+1)*45]/(1000)**2)/(pvl_statecsv[state_id*45:(state_id+1)*45]/(1000)**2),label='Measure 1',lw=3)
plt.plot(2011+np.arange(0,45),frcsv[state_id*45:(state_id+1)*45],label='Measure 2',lw=3) 
plt.legend(fontsize=15)
plt.title('%s Baseline Funded Ratio'%state_names[state_listcsv[state_id]-1],fontsize=15)
plt.xlabel('Year',fontsize=15)
plt.ylabel('Funded Ratio',fontsize=15)

plt.figure(2)
plt.plot(2011+np.arange(0,45),(avg_asscsv[state_id*45:(state_id+1)*45]/(1000)**2),label='%s A'%state_names[state_listcsv[state_id]-1],lw=3) 
plt.plot(2011+np.arange(0,45),(pvl_statecsv[state_id*45:(state_id+1)*45]/(1000)**2),ls='--',label='%s L'%state_names[state_listcsv[state_id]-1],lw=3)
plt.legend(fontsize=15)
#plt.ylim(5,15)
plt.xlabel('Year',fontsize=15)
plt.ylabel('Aggregates ($, Billion)',fontsize=15)
plt.title('%s Pension Assets and Liabilities'%state_names[state_listcsv[state_id]-1],fontsize=15)

plt.figure(3)
plt.plot(2011+np.arange(0,45),100*total_taxcsv[state_id*45:(state_id+1)*45],label='Baseline',lw=3) 
plt.fill_between(2011+np.arange(0,45), 100*(total_taxcsv[state_id*45:(state_id+1)*45]-tax_volcsv[state_id*45:(state_id+1)*45]), 
                                       100*(total_taxcsv[state_id*45:(state_id+1)*45]+tax_volcsv[state_id*45:(state_id+1)*45]),
                                       alpha=.5   ) 
plt.legend(fontsize=15)
plt.xlabel('Year',fontsize=15)
plt.ylabel('Tax (%)',fontsize=15)
plt.title('%s Tax Policy'%state_names[state_listcsv[state_id]-1],fontsize=15)

plt.figure(4)
plt.plot(2011+np.arange(0,45),100*pension_taxcsv[state_id*45:(state_id+1)*45],label='Baseline',lw=3) 
plt.legend(fontsize=15)
plt.xlabel('Year',fontsize=15)
plt.ylabel('Tax (%)',fontsize=15)
plt.title('%s Pension Tax'%state_names[state_listcsv[state_id]-1],fontsize=15)

plt.figure(5)
plt.plot(2011+np.arange(0,45),normcostcsv[state_id*45:(state_id+1)*45]/(1000*1000),label='Baseline',lw=3) 
plt.legend(fontsize=15)
plt.xlabel('Year',fontsize=15)
plt.ylabel('Normal Cost ($ Billion)',fontsize=15)
plt.title('%s Normal Cost'%state_names[state_listcsv[state_id]-1],fontsize=15)

plt.figure(6)
plt.plot(np.arange(0,36),pension_valuecsv[44:],label='Baseline',lw=3) 
plt.legend(fontsize=15)
plt.xlabel('Worker Retirement Year',fontsize=15)
plt.ylabel('Real Pension Value ($, Thousand)',fontsize=15)
plt.title('%s Pension Value, Adjusted for Inflation'%state_names[state_listcsv[state_id]-1],fontsize=15)


#~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                          #
#   Individual Outcomes    #
#                          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~#
plt.figure(7)
plt.plot(20+np.arange(0,45),priv_riskcsv[state_id*45:(state_id+1)*45],label='Private',lw=3) 
plt.plot(20+np.arange(0,45),pub_riskcsv[state_id*45:(state_id+1)*45],label='Public',lw=3) 
plt.legend(fontsize=15)
plt.xlabel('Worker Age',fontsize=15)
plt.ylabel('Risky Portfolio Share',fontsize=15)
plt.title('%s Risky Portfolio Shares by Worker Sector'%state_names[state_listcsv[state_id]-1],fontsize=15)

plt.figure(8)
plt.subplot(1,2,1)
plt.plot(20+np.arange(0,45),priv_save_ratecsv[state_id*45:(state_id+1)*45],label='Measure 1',lw=3) 
plt.plot(20+np.arange(0,45),priv_save_rate1csv[state_id*45:(state_id+1)*45],label='Measure 2',lw=3) 
plt.legend(fontsize=15)
plt.xlabel('Worker Age',fontsize=15)
plt.ylabel('Risky Portfolio Share',fontsize=15)
plt.title('Private Worker',fontsize=15)
plt.subplot(1,2,2)
plt.plot(20+np.arange(0,45),pub_save_ratecsv[state_id*45:(state_id+1)*45],label='Measure 1',lw=3) 
plt.plot(20+np.arange(0,45),pub_save_rate1csv[state_id*45:(state_id+1)*45],label='Measure 2',lw=3) 
plt.legend(fontsize=15)
plt.xlabel('Worker Age',fontsize=15)
plt.ylabel('Risky Portfolio Share',fontsize=15)
plt.title('Public Worker',fontsize=15)
plt.suptitle('%s Worker Savings Rates'%state_names[state_listcsv[state_id]-1],fontsize=15)



#~~~~~~~~~~~~~~~~~~~~~~#
#                      #
#   Welfare Metrics    #
#                      #
#~~~~~~~~~~~~~~~~~~~~~~#

# re-shape welfare arrays
ce_priv_base = ce_priv_basecsv.reshape(45,201)
ce_priv_commit  = ce_priv_commitcsv.reshape(45,201)

ce_pub_base  = ce_pub_basecsv.reshape(45,201)
ce_pub_commit   = ce_pub_commitcsv.reshape(45,201)
ce_pub_dc   = ce_pub_DCswitchcsv.reshape(45,201)

priv_dist    = cohort_privcsv.reshape(45,201)
pub_dist = cohort_pubcsv.reshape(45,201)

welfare_private_commit = np.zeros((45))
welfare_public_commit  = np.zeros((45))

welfare_public_dc  = np.zeros((45))

for i in range(45):
    
    welfare_private_commit[i] = 100*np.sum( priv_dist[i,:]*( (ce_priv_commit[i,:]-ce_priv_base[i,:])/ce_priv_base[i,:]   ) )
    welfare_public_commit[i]  = 100*np.sum( pub_dist[i,:]*( (ce_pub_commit[i,:]-ce_pub_base[i,:])/ce_pub_base[i,:]   ) )

    welfare_public_dc[i]  = 100*np.sum( pub_dist[i,:]*( (ce_pub_dc[i,:]-ce_pub_base[i,:])/ce_pub_base[i,:]   ) )

pub_mask  = np.isfinite(welfare_public_commit)
pub_mask_dc  = np.isfinite(welfare_public_dc)
priv_mask = np.isfinite(welfare_private_commit)
       
plt.figure(5)
plt.subplot(1,2,1)
plt.plot(np.arange(0,45)[priv_mask],welfare_private_commit[priv_mask],lw=3,label='Private',marker='o'   ) 
plt.axhline(0,ls='--',alpha=.7)
plt.xlabel('Age Cohort',fontsize=15)
plt.ylabel('CE Welfare Gain (%)',fontsize=15)
plt.title('Private Sector Workers')
plt.subplot(1,2,2)
plt.plot(np.arange(0,45)[pub_mask],welfare_public_commit[pub_mask],lw=3,marker='o'   ) 
plt.axhline(0,ls='--',alpha=.7)
plt.xlabel('Age Cohort',fontsize=15)
plt.ylabel('CE Welfare Gain (%)',fontsize=15)
plt.title('Public Sector Workers')
plt.suptitle('%s CE Welfare Gains from Full Commitment, by Sector'%state_names[state_listcsv[state_id]-1],fontsize=15)

plt.figure(6)
plt.plot(np.arange(0,45)[pub_mask_dc],welfare_public_dc[pub_mask_dc],lw=3,marker='o'   ) 
plt.axhline(0,ls='--',alpha=.7)
plt.xlabel('Age Cohort',fontsize=15)
plt.ylabel('CE Welfare Gain (%)',fontsize=15)
plt.title('%s Public Worker Welfare Gains from DC Plan with 5p Wage Hike'%state_names[state_listcsv[state_id]-1],fontsize=15)



