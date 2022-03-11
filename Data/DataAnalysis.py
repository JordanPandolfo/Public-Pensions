#
#   Jordan Pandolfo, 08-25-2021
#
#   Program to compute/collect state-by-state 
#       fiscal & demographic data
#
#   Main objective is to collect data on the (1) ratio
#    of annuitants to workers (2) contribution rates as
#    % of ARC (3) discount factor (4) funded ratio (5) value
#    of pension benefits (6) pension risky asset portfolio 
#    share and (7) present value of pension liabilities
#
#
#   Input files: StateData.csv
#                CostLiving.csv   
#
#   Output files: StateDataValues.csv
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

#~~~~~~~~~~~~~~~~~~~~~~~~#
#                        #     
#   Import State Data    #
#                        # 
#~~~~~~~~~~~~~~~~~~~~~~~~#
#Import dataframe
os.chdir("/home/pando004/Desktop/PublicPension/Data")

df      = pd.read_csv('StateData.csv',sep=",",encoding="cp1252")     # state-level data from Public Pensions Database
df_cost = pd.read_csv('CostLiving.csv',sep=",",encoding="cp1252")    # cost-of-living and public/private wage gap data
df_size = pd.read_csv('SectorSize.csv',sep=",",encoding="cp1252")    # relative size of public sector

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                        #     
#   Collect Relevant State-Level Data    #
#                                        # 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Step 1: create drop variables non-relevant to exercise
df = df[[ 'StateAbbrev','fy','PlanName','SocSecCovered','PercentReqContPaid',
         'InvestmentReturnAssumption_GASB','EQTotal_Actl','PETotal_Actl','HFTotal_Actl',
         'COMDTotal_Actl','RETotal_Actl','AltMiscTotal_Actl','TotMembership','actives_tot',
         'beneficiaries_tot','BeneficiaryBenefit_avg','ActLiabilities_GASB','ActFundedRatio_GASB']]

# drop pre-2001 observations
df = df[df.fy>= 2001]

# drop this OK plan because missing ARC contribution data
df = df[~(df.PlanName == 'Oklahoma Municipal Employees') ]

# Step 2: create consolidated panel at the state-level 

# create new dataframe for consolidated state values 
df_con = pd.DataFrame({ 'State': list(df.StateAbbrev.unique()) })

# create subset of consolidated variables 
df_con['worker_ratio'] = np.nan
df_con['theta']        = np.nan
df_con['discount']     = np.nan
df_con['funded_ratio'] = np.nan
df_con['pension']      = np.nan
df_con['portfolio']    = np.nan
df_con['pvl']          = np.nan
df_con['cost_living'] = np.nan
df_con['sector_size'] = np.nan
df_con['ss']          = np.nan

# for each state    
for idx,state in enumerate(df.StateAbbrev.unique()):
        
    # record cost of living 
    if state == 'DC': 
        df_con.loc[df_con.State == state, 'cost_living'] = 115
    else:
        df_con.loc[df_con.State == state, 'cost_living'] = np.float( df_cost[ df_cost.State == state ]['Cost'] )   

    # record size of public sector 
    df_con.loc[df_con.State == state, 'sector_size'] = np.float( df_size[ df_size.State == state ]['Size'] )   
        
    # record consolidated PVL 
    df_con.loc[df_con.State == state, 'pvl'] = np.nansum(df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['ActLiabilities_GASB']) 
    
    # record weighted-average funded ratio 
    weights = df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['ActLiabilities_GASB']/np.nansum(df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['ActLiabilities_GASB'])
    
    # record weighted-average social security coverage 
    df_con.loc[df_con.State == state, 'ss'] = np.nansum( weights*df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['SocSecCovered'] )
    
    df_con.loc[df_con.State == state, 'funded_ratio'] = np.nansum( weights*df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['ActFundedRatio_GASB'] )

    # record weighted-average discount rate 
    df_con.loc[df_con.State == state, 'discount'] = np.nansum( weights*df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['InvestmentReturnAssumption_GASB'] )
    
    # record weighted-average pension benefit 
    df_con.loc[df_con.State == state, 'pension'] = np.nansum( weights*df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['BeneficiaryBenefit_avg'] )*(1 + (df_con[df_con.State == state]['cost_living']-100)/100 )
    
    # record weighted-average risky asset portfolio share 
    df_con.loc[df_con.State == state, 'portfolio'] = np.nansum( weights*(df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['EQTotal_Actl']+
                                                                      df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['PETotal_Actl']+
                                                                      df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['HFTotal_Actl']+
                                                                      df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['COMDTotal_Actl']+
                                                                      df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['RETotal_Actl']+
                                                                      df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['AltMiscTotal_Actl']))
    
    # record worker ratio    
    df_con.loc[df_con.State == state, 'worker_ratio'] = (np.nansum( df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['beneficiaries_tot'])/
                                                         np.nansum( df[ (df.fy == 2019) & (df.StateAbbrev == state) ]['actives_tot'] ))
    
    # record weighted-average % Contribution of ARC
    avg_arc = 0
    for idxx,plan in enumerate( df[ (df.StateAbbrev == state) & (df.fy >= 2019) ].PlanName.unique() ):
        
        if np.asarray((weights))[idxx]*0 == 0:
            
            # truncate data at 2.5th and 97.5th percentile to control for outliers 
            lb = np.nanquantile( df[ (df.PlanName == plan) & (df.fy >= 2010)]['PercentReqContPaid'] , .025)
            ub = np.nanquantile( df[ (df.PlanName == plan) & (df.fy >= 2010)]['PercentReqContPaid'] , .975)
            
            df.loc[ (df.PlanName == plan) & (df.fy >= 2010) & (df.PercentReqContPaid<lb), 'PercentReqContPaid'] = lb
            df.loc[ (df.PlanName == plan) & (df.fy >= 2010) & (df.PercentReqContPaid>ub), 'PercentReqContPaid'] = ub
            
            avg_arc = avg_arc + np.asarray((weights))[idxx]*np.nanmean(df[ (df.PlanName == plan) & (df.fy >= 2010) ]['PercentReqContPaid'])
    
    df_con.loc[df_con.State == state, 'theta'] = avg_arc 
    
# merge in sector wage gap data 
df_con['wage_scale'] = 0.0899


#~~~~~~~~~~~~~~~~~~#
#                  #     
#   Output Data    #
#                  # 
#~~~~~~~~~~~~~~~~~~#
df_con[['State','worker_ratio','theta','discount','funded_ratio',
        'pension','wage_scale','sector_size','portfolio','pvl','cost_living','ss']].to_csv('/home/pando004/Desktop/PublicPension/Data/StateDataValues.csv')


