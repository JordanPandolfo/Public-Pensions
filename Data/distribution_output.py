#
#   Jordan Pandolfo, 08-25-2021
#
#   Program to compute state-by-state age
#       distribution over time
#
#   Input files: Participation.csv
#                Population2020.csv
#                Population2030.csv
#                Population2040.csv
#                StateData.csv
#
#   Output files: full_panel_distribution.csv
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
os.chdir("/home/pando004/Desktop/PublicPension/Data")

df2020     = pd.read_csv('Population2020.csv')                   # current population
df2030     = pd.read_csv('Population2030.csv')                   # 2030 forecast
df2040     = pd.read_csv('Population2040.csv')                   # 2040 forecast
part       = pd.read_csv('Participation.csv')      # labor force participation rates 
state_info = pd.read_csv('StateDataValues.csv')                  # state pension data


df2020 = df2020.sort_values('State')
df2030 = df2030.sort_values('State')
df2040 = df2040.sort_values('State')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                                 #  
#   Generate Panel of Age Cohort Distributions    #
#                for Wach State                   #
#                                                 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  


#
# Procedure 
#
#   Step 1: Compute growth rates for 2020 --> 2030 and 2030 --> 2040
#
#   Step 2: Drop population and labor force data for age groups < 20
#
#   Step 3: Apply growth rates to compute distribution forecasts
#
#

# Step 1
dfgrowth1 = df2020[['State']]
dfgrowth1['rate2024'] = 0
dfgrowth1['rate2529'] = 0
dfgrowth1['rate3034'] = 0
dfgrowth1['rate3539'] = 0
dfgrowth1['rate4044'] = 0
dfgrowth1['rate4549'] = 0
dfgrowth1['rate5054'] = 0
dfgrowth1['rate5559'] = 0
dfgrowth1['rate6064'] = 0
dfgrowth1['rate6569'] = 0
dfgrowth1['rate7074'] = 0
dfgrowth1['rate7579'] = 0
dfgrowth1['rate8084'] = 0
dfgrowth1['rate8589'] = 0
dfgrowth1['rate9094'] = 0
dfgrowth1['rate9599'] = 0

dfgrowth2 = df2020[['State']]
dfgrowth2['rate2024'] = 0
dfgrowth2['rate2529'] = 0
dfgrowth2['rate3034'] = 0
dfgrowth2['rate3539'] = 0
dfgrowth2['rate4044'] = 0
dfgrowth2['rate4549'] = 0
dfgrowth2['rate5054'] = 0
dfgrowth2['rate5559'] = 0
dfgrowth2['rate6064'] = 0
dfgrowth2['rate6569'] = 0
dfgrowth2['rate7074'] = 0
dfgrowth2['rate7579'] = 0
dfgrowth2['rate8084'] = 0
dfgrowth2['rate8589'] = 0
dfgrowth2['rate9094'] = 0
dfgrowth2['rate9599'] = 0

for idx,state in enumerate(df2020.State.unique()):
    
    dfgrowth1.loc[dfgrowth1.State == state, 'rate2024'] = (1/5)*(df2030[df2030.State == state]['20to24'] - df2020[df2020.State == state]['20to24'])/df2020[df2020.State == state]['20to24']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate2529'] = (1/5)*(df2030[df2030.State == state]['25to29'] - df2020[df2020.State == state]['25to29'])/df2020[df2020.State == state]['25to29']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate3034'] = (1/5)*(df2030[df2030.State == state]['30to34'] - df2020[df2020.State == state]['30to34'])/df2020[df2020.State == state]['30to34']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate3539'] = (1/5)*(df2030[df2030.State == state]['35to39'] - df2020[df2020.State == state]['35to39'])/df2020[df2020.State == state]['35to39']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate4044'] = (1/5)*(df2030[df2030.State == state]['40to44'] - df2020[df2020.State == state]['40to44'])/df2020[df2020.State == state]['40to44']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate4549'] = (1/5)*(df2030[df2030.State == state]['45to49'] - df2020[df2020.State == state]['45to49'])/df2020[df2020.State == state]['45to49']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate5054'] = (1/5)*(df2030[df2030.State == state]['50to54'] - df2020[df2020.State == state]['50to54'])/df2020[df2020.State == state]['50to54']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate5559'] = (1/5)*(df2030[df2030.State == state]['55to59'] - df2020[df2020.State == state]['55to59'])/df2020[df2020.State == state]['55to59']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate6064'] = (1/5)*(df2030[df2030.State == state]['60to64'] - df2020[df2020.State == state]['60to64'])/df2020[df2020.State == state]['60to64']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate6569'] = (1/5)*(df2030[df2030.State == state]['65to69'] - df2020[df2020.State == state]['65to69'])/df2020[df2020.State == state]['65to69']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate7074'] = (1/5)*(df2030[df2030.State == state]['70to74'] - df2020[df2020.State == state]['70to74'])/df2020[df2020.State == state]['70to74']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate7579'] = (1/5)*(df2030[df2030.State == state]['75to79'] - df2020[df2020.State == state]['75to79'])/df2020[df2020.State == state]['75to79']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate8084'] = (1/5)*(df2030[df2030.State == state]['80to84'] - df2020[df2020.State == state]['80to84'])/df2020[df2020.State == state]['80to84']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate8589'] = (1/5)*(df2030[df2030.State == state]['85plus'] - df2020[df2020.State == state]['85plus'])/df2020[df2020.State == state]['85plus']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate9094'] = (1/5)*(df2030[df2030.State == state]['85plus'] - df2020[df2020.State == state]['85plus'])/df2020[df2020.State == state]['85plus']
    dfgrowth1.loc[dfgrowth1.State == state, 'rate9599'] = (1/5)*(df2030[df2030.State == state]['85plus'] - df2020[df2020.State == state]['85plus'])/df2020[df2020.State == state]['85plus']

    dfgrowth2.loc[dfgrowth2.State == state, 'rate2024'] = (1/5)*(df2040[df2040.State == state]['20to24'] - df2030[df2030.State == state]['20to24'])/df2030[df2030.State == state]['20to24']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate2529'] = (1/5)*(df2040[df2040.State == state]['25to29'] - df2030[df2030.State == state]['25to29'])/df2030[df2030.State == state]['25to29']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate3034'] = (1/5)*(df2040[df2040.State == state]['30to34'] - df2030[df2030.State == state]['30to34'])/df2030[df2030.State == state]['30to34']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate3539'] = (1/5)*(df2040[df2040.State == state]['35to39'] - df2030[df2030.State == state]['35to39'])/df2030[df2030.State == state]['35to39']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate4044'] = (1/5)*(df2040[df2040.State == state]['40to44'] - df2030[df2030.State == state]['40to44'])/df2030[df2030.State == state]['40to44']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate4549'] = (1/5)*(df2040[df2040.State == state]['45to49'] - df2030[df2030.State == state]['45to49'])/df2030[df2030.State == state]['45to49']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate5054'] = (1/5)*(df2040[df2040.State == state]['50to54'] - df2030[df2030.State == state]['50to54'])/df2030[df2030.State == state]['50to54']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate5559'] = (1/5)*(df2040[df2040.State == state]['55to59'] - df2030[df2030.State == state]['55to59'])/df2030[df2030.State == state]['55to59']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate6064'] = (1/5)*(df2040[df2040.State == state]['60to64'] - df2030[df2030.State == state]['60to64'])/df2030[df2030.State == state]['60to64']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate6569'] = (1/5)*(df2040[df2040.State == state]['65to69'] - df2030[df2030.State == state]['65to69'])/df2030[df2030.State == state]['65to69']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate7074'] = (1/5)*(df2040[df2040.State == state]['70to74'] - df2030[df2030.State == state]['70to74'])/df2030[df2030.State == state]['70to74']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate7579'] = (1/5)*(df2040[df2040.State == state]['75to79'] - df2030[df2030.State == state]['75to79'])/df2030[df2030.State == state]['75to79']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate8084'] = (1/5)*(df2040[df2040.State == state]['80to84'] - df2030[df2030.State == state]['80to84'])/df2030[df2030.State == state]['80to84']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate8589'] = (1/5)*(df2040[df2040.State == state]['85plus'] - df2030[df2030.State == state]['85plus'])/df2030[df2030.State == state]['85plus']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate9094'] = (1/5)*(df2040[df2040.State == state]['85plus'] - df2030[df2030.State == state]['85plus'])/df2030[df2030.State == state]['85plus']
    dfgrowth2.loc[dfgrowth2.State == state, 'rate9599'] = (1/5)*(df2040[df2040.State == state]['85plus'] - df2030[df2030.State == state]['85plus'])/df2030[df2030.State == state]['85plus']

# Step 2

# keep only labor force participation rates for first 9 age groups (20-24, 25-29,...,60-64)
part = np.asarray((part))[0][1:10]


# Step 3


# intialize array to store all distributions
all_state_store = np.zeros(( len(state_info['State'].unique()), 45, 80 ))  # each state, each year (1 to 45) and each age cohort (20 to 100)
# holder of state names
state_name = []

for at,state in enumerate( df2020['State'].unique() ):
    
    # identify worker ratio
    ratio = state_info[ state_info.State == state ]['worker_ratio']

    # identify growth rates   
    growth1 = np.asarray((dfgrowth1[ dfgrowth1.State == state]))[0][1:]
    growth2 = np.asarray((dfgrowth2[ dfgrowth2.State == state]))[0][1:]    

    # Create initial distribution
    Phi = np.asarray((df2020[ df2020.State == state ]))[0][6:]
    
    # expand 85+ category into 85-89, 90-94, 95-99 where they recieve (1/2,1/3,1/6) of 85+ category, respectively
    Phi = np.concatenate(( Phi, np.array(( Phi[-1]/3, Phi[-1]/6 )) ))
    Phi[-3] = Phi[-3]/2 
    
    # apply participation rates to 'working age' distribution
    Phitilde = np.zeros( len(Phi) )
    
    #Phitilde[:9] = part[:9]*Phi[:9]/100
    Phitilde[:9] = Phi[:9]
    
    # apply annuitant ratio to 'retiree age' distribution
    #kappa = float( ratio*sum( Phitilde[:9] )/sum( Phi[9:] ) )
    kappa = 1
    
    Phitilde[9:] = kappa*Phi[9:]
    
    # normalize
    Phitilde = Phitilde/np.sum(Phitilde)
    
    # Apply growth rates
    Pop_Dist = np.zeros(( 45, 80 ))  # 45 years and for 80 age cohorts
    
    # take Phitilde, convert from 14 cohort groups to 80
    Pop_Dist[0,:5] = Phitilde[0]/5
    Pop_Dist[0,5:10] = Phitilde[1]/5
    Pop_Dist[0,10:15] = Phitilde[2]/5
    Pop_Dist[0,15:20] = Phitilde[3]/5
    Pop_Dist[0,20:25] = Phitilde[4]/5
    Pop_Dist[0,25:30] = Phitilde[5]/5
    Pop_Dist[0,30:35] = Phitilde[6]/5
    Pop_Dist[0,35:40] = Phitilde[7]/5
    Pop_Dist[0,40:45] = Phitilde[8]/5
    Pop_Dist[0,45:50] = Phitilde[9]/5
    Pop_Dist[0,50:55] = Phitilde[10]/5
    Pop_Dist[0,55:60] = Phitilde[11]/5
    Pop_Dist[0,60:65] = Phitilde[12]/5
    Pop_Dist[0,65:70] = Phitilde[13]/5
    Pop_Dist[0,70:75] = Phitilde[14]/5
    Pop_Dist[0,75:]   = Phitilde[15]/5
    
    for i in range(1,45):
        
        if i < 10:  # first 10 year
            
            Pop_Dist[i,:5]    = Pop_Dist[i-1,:5]*(   1+growth1[0])
            Pop_Dist[i,5:10]  = Pop_Dist[i-1,5:10]*( 1+growth1[1])
            Pop_Dist[i,10:15] = Pop_Dist[i-1,10:15]*(1+growth1[2])
            Pop_Dist[i,15:20] = Pop_Dist[i-1,15:20]*(1+growth1[3])
            Pop_Dist[i,20:25] = Pop_Dist[i-1,20:25]*(1+growth1[4])
            Pop_Dist[i,25:30] = Pop_Dist[i-1,25:30]*(1+growth1[5])
            Pop_Dist[i,30:35] = Pop_Dist[i-1,30:35]*(1+growth1[6])
            Pop_Dist[i,35:40] = Pop_Dist[i-1,35:40]*(1+growth1[7])
            Pop_Dist[i,40:45] = Pop_Dist[i-1,40:45]*(1+growth1[8])
            Pop_Dist[i,45:50] = Pop_Dist[i-1,45:50]*(1+growth1[9])
            Pop_Dist[i,50:55] = Pop_Dist[i-1,50:55]*(1+growth1[10])
            Pop_Dist[i,55:60] = Pop_Dist[i-1,55:60]*(1+growth1[11])
            Pop_Dist[i,60:65] = Pop_Dist[i-1,60:65]*(1+growth1[12])
            Pop_Dist[i,65:70] = Pop_Dist[i-1,65:70]*(1+growth1[13])
            Pop_Dist[i,70:75] = Pop_Dist[i-1,70:75]*(1+growth1[14])
            Pop_Dist[i,75:]   = Pop_Dist[i-1,75:]*(1+growth1[15])
            
        else:
    
            Pop_Dist[i,:5]    = Pop_Dist[i-1,:5]*(   1+growth2[0])
            Pop_Dist[i,5:10]  = Pop_Dist[i-1,5:10]*( 1+growth2[1])
            Pop_Dist[i,10:15] = Pop_Dist[i-1,10:15]*(1+growth2[2])
            Pop_Dist[i,15:20] = Pop_Dist[i-1,15:20]*(1+growth2[3])
            Pop_Dist[i,20:25] = Pop_Dist[i-1,20:25]*(1+growth2[4])
            Pop_Dist[i,25:30] = Pop_Dist[i-1,25:30]*(1+growth2[5])
            Pop_Dist[i,30:35] = Pop_Dist[i-1,30:35]*(1+growth2[6])
            Pop_Dist[i,35:40] = Pop_Dist[i-1,35:40]*(1+growth2[7])
            Pop_Dist[i,40:45] = Pop_Dist[i-1,40:45]*(1+growth2[8])
            Pop_Dist[i,45:50] = Pop_Dist[i-1,45:50]*(1+growth2[9])
            Pop_Dist[i,50:55] = Pop_Dist[i-1,50:55]*(1+growth2[10])
            Pop_Dist[i,55:60] = Pop_Dist[i-1,55:60]*(1+growth2[11])
            Pop_Dist[i,60:65] = Pop_Dist[i-1,60:65]*(1+growth2[12])
            Pop_Dist[i,65:70] = Pop_Dist[i-1,65:70]*(1+growth2[13])
            Pop_Dist[i,70:75] = Pop_Dist[i-1,70:75]*(1+growth2[14])
            Pop_Dist[i,75:]   = Pop_Dist[i-1,75:]*(1+growth2[15])
            
        Pop_Dist[i] = Pop_Dist[i]/np.sum(Pop_Dist[i])
        
    # record state population distribution
    all_state_store[at,:,:] = Pop_Dist
    
    state_name.append( state )

#[plt.plot(all_state_store[i,0,:]) for i in range(51)] 

os.chdir("/home/pando004/Desktop/PublicPension/Data")
# export distribution
np.savetxt("full_panel_distribution.csv", np.reshape( all_state_store, ( len(state_info['State'].unique())*45,80) ), delimiter=",")

# recover using the following procedure
#from numpy import genfromtxt
#A = np.reshape( genfromtxt('full_panel_distribution.csv',delimiter=','), (len(state_info['State'].unique()),45,80) )




# statistics 
current_young = np.zeros(51)
current_old = np.zeros(51)

current_young_work = np.zeros(51)

fif_young = np.zeros(51)
fif_old = np.zeros(51)

thir_young = np.zeros(51)
thir_old = np.zeros(51)

four_young = np.zeros(51)
four_old = np.zeros(51)


for at in range(51):
    
    current_young[at] = int(100*np.sum(all_state_store[at,0,:15]))
    current_old[at] = int(100*np.sum(all_state_store[at,0,45:]))
    
    current_young_work[at] = int(100*np.sum(all_state_store[at,0,:30])/np.sum(all_state_store[at,0,:45]))

    fif_young[at] = int(100*np.sum(all_state_store[at,14,:15]))
    fif_old[at] = int(100*np.sum(all_state_store[at,14,45:]))

    thir_young[at] = int(100*np.sum(all_state_store[at,29,:15]))
    thir_old[at] = int(100*np.sum(all_state_store[at,29,45:]))

    four_young[at] = int(100*np.sum(all_state_store[at,44,:15]))
    four_old[at] = int(100*np.sum(all_state_store[at,44,45:]))

d = {'State': df2020.State.unique(),
     '0 Yr Young': current_young,
     '0 Yr Old': current_old,
     '15 Yr Young': fif_young,
     '15 Yr Old': fif_old,
     '30 Yr Young': thir_young,
     '30 Yr Old': thir_old,
     '45 Yr Young': four_young,
     '45 Yr Old': four_old
     } 

df = pd.DataFrame( data = d )

with open( os.path.join(os.getcwd(), "demographic.tex"), "w"
  )  as file:
        file.write( df[[ 'State','0 Yr Young','0 Yr Old','15 Yr Young',
                                 '15 Yr Old','30 Yr Young','30 Yr Old',
                                 '45 Yr Young','45 Yr Old']].to_latex(
            float_format="%.1f",
            header= ['State','0Y Young','0Y Old','15Y Young','15Y Old','30Y Young','30Y Old','45Y Young','45Y Old'],
            index=False,
            caption="This is caption",
            label="tab:forecast_table",
            column_format = "ccccccccc") )

os.chdir("/home/pando004/Desktop/PublicPension/ScatterPlots")  
np.savetxt('curr_young_work.csv',current_young_work,delimiter=',')
np.savetxt('curr_old.csv',current_old,delimiter=',')
np.savetxt('fif_old.csv',fif_old,delimiter=',')
np.savetxt('thir_old.csv',thir_old,delimiter=',')
np.savetxt('four_old.csv',four_old,delimiter=',')