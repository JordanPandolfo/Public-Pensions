#~~~~~~~~~~~~~~~~~~~~~~#
#                      #     
#   Import Packages    #
#                      # 
#~~~~~~~~~~~~~~~~~~~~~~#
import numpy as np
import pandas as pd
import os
import matplotlib.pyplot as plt
import statsmodels.api as sm

#Import dataframe
os.chdir("/home/pando004/Desktop/PublicPension")
state_vals =  np.asarray(( pd.read_csv('StateDataValues.csv',sep=",",encoding="cp1252",header=None) )).astype('float')
state_names =  np.asarray(( pd.read_csv('state_names.csv',sep=",",encoding="cp1252",header=None) ))[:,0]

os.chdir("/home/pando004/Desktop/PublicPension/ScatterPlots")

tax_base =  np.asarray(( pd.read_csv('tax_base.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
pvl_base =  np.asarray(( pd.read_csv('pvl_base.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
fr_base =  np.asarray(( pd.read_csv('fr_base.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

tax_close =  np.asarray(( pd.read_csv('tax_close.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
pvl_close =  np.asarray(( pd.read_csv('pvl_close.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
fr_close =  np.asarray(( pd.read_csv('fr_close.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

welfare_state_close =  np.asarray(( pd.read_csv('welfare_state_close.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
welfare_priv_hybrid =  np.asarray(( pd.read_csv('welfare_priv_hybrid.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
welfare_pub_hybrid =  np.asarray(( pd.read_csv('welfare_pub_hybrid.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')

curr_young_work =  np.asarray(( pd.read_csv('curr_young_work.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
curr_old =  np.asarray(( pd.read_csv('curr_old.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
fif_old =  np.asarray(( pd.read_csv('fif_old.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
thir_old =  np.asarray(( pd.read_csv('thir_old.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
four_old =  np.asarray(( pd.read_csv('four_old.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
old_growth = (four_old-curr_old)/curr_old

welfare_priv_cola =  np.asarray(( pd.read_csv('welfare_priv_cola.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
welfare_pub_cola =  np.asarray(( pd.read_csv('welfare_pub_cola.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
welfare_pub20_cola =  np.asarray(( pd.read_csv('welfare_pub20_cola.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')
welfare_pub55_cola =  np.asarray(( pd.read_csv('welfare_pub55_cola.csv',sep=",",encoding="cp1252",header=None) ))[:,0].astype('float')


plt.close('all')
pub_size = state_vals[:,6]
poly = np.polyfit(pub_size, welfare_state_close, 1)
y_line = pub_size * poly[0] + poly[1]


plt.figure(1)
plt.scatter( pub_size, welfare_state_close )
plt.plot(pub_size, y_line,'black',alpha=.75)
plt.title(r'Public Sector Size',fontsize=15)
#plt.xlabel(r'$\theta$')
plt.ylabel('Welfare Change (%)',fontsize=15)




plt.close('all')
theta = state_vals[:,1]
poly = np.polyfit(theta, welfare_state_close, 1)
y_line = theta * poly[0] + poly[1]


plt.figure(1)
plt.subplot(2,2,1)
plt.scatter( theta, welfare_state_close )
plt.plot(theta, y_line,'black',alpha=.75)
plt.title(r'Contribution Fraction $\theta$ of ARC',fontsize=15)
#plt.xlabel(r'$\theta$')
plt.ylabel('Welfare Change (%)',fontsize=15)

fr = state_vals[:,3]
poly = np.polyfit(fr, welfare_state_close, 1)
y_line = fr * poly[0] + poly[1]

plt.subplot(2,2,2)
plt.scatter( fr, welfare_state_close )
plt.plot(fr, y_line,'black',alpha=.75)
plt.title(r'Initial Funded Ratio $\chi^0$',fontsize=15)
#plt.xlabel(r'$\chi^0$')




pub_size = state_vals[:,6]
poly = np.polyfit(pub_size, welfare_state_close, 1)
y_line = pub_size * poly[0] + poly[1]


plt.subplot(2,2,3)
plt.scatter( pub_size, welfare_state_close )
plt.plot(pub_size, y_line,'black',alpha=.75)
plt.title(r'Public Sector Size',fontsize=15)
plt.ylabel('Welfare Change (%)',fontsize=15)

benefit = state_vals[:,4]
poly = np.polyfit(benefit, welfare_state_close, 1)
y_line = benefit * poly[0] + poly[1]

plt.subplot(2,2,4)

plt.figure(1)
plt.scatter( benefit, welfare_state_close )
plt.plot(benefit, y_line,'black',alpha=.75)
plt.title('Current Pension Benefit',fontsize=15)
#plt.xlabel(r'$\theta$')
#plt.ylabel('Welfare Change (%)',fontsize=15)

plt.suptitle('Average Welfare Effects and State Characteristics',fontsize=20)


#poly = np.polyfit(four_old, welfare_state_close, 1)
#y_line = four_old* poly[0] + poly[1]

#plt.scatter( four_old, welfare_state_close )
#plt.plot(four_old, y_line,'black',alpha=.75)
#plt.title('% of Retirees in 45 Years',fontsize=15)
#plt.xlabel('% of Retirees')



discount = state_vals[:,2]
poly = np.polyfit(discount, welfare_state_close, 1)
y_line = discount * poly[0] + poly[1]

plt.figure(3)
plt.scatter( discount, welfare_state_close )
plt.plot(discount, y_line,'black',alpha=.75)
plt.title('Discount Factor and Welfare')
plt.xlabel('Discount Factor')


xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
    np.reshape(pub_size,(51,1)),
    

    ) ),axis=1  )
model = sm.OLS(welfare_state_close, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()

xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
    np.reshape(pub_size,(51,1)),
    
    np.reshape(fr,(51,1)),
    np.reshape(theta,(51,1)),  

    ) ),axis=1  )
model = sm.OLS(welfare_state_close, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()

xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
    np.reshape(pub_size,(51,1)),
    
    np.reshape(fr,(51,1)),
    np.reshape(theta,(51,1)),
    
    np.reshape(discount,(51,1))  ,
    np.reshape(discount,(51,1))*np.reshape(fr,(51,1)) ,
    np.reshape(discount,(51,1))*np.reshape(theta,(51,1)),

    ) ),axis=1  )
model = sm.OLS(welfare_state_close, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()

xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
    np.reshape(pub_size,(51,1)),
    
    np.reshape(fr,(51,1)),
    np.reshape(theta,(51,1)),
    
    np.reshape(discount,(51,1))  ,
    np.reshape(discount,(51,1))*np.reshape(fr,(51,1)) ,
    np.reshape(discount,(51,1))*np.reshape(theta,(51,1)),
    
    np.reshape(curr_old,(51,1)),
    np.reshape(four_old,(51,1))
    
    ) ),axis=1  )


model = sm.OLS(welfare_state_close, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()






sr_tax = []
mr_tax = []
lr_tax = []

for i in range(51):
    sr_tax.append( tax_base[45*i+4] )
    mr_tax.append( tax_base[45*i+19] )
    lr_tax.append( tax_base[45*i+35] )

sr_tax = np.asarray((sr_tax))
mr_tax = np.asarray((mr_tax))
lr_tax = np.asarray((lr_tax))


#
#
#   now do it for hybrid reform: have to do separate regressions for private and public
#
#

#
#
#   now do it for COLA reform: have to do separate regressions for private and public
#
#

# private sector welfare 
plt.close('all')
poly = np.polyfit(theta, welfare_priv_hybrid, 1)
y_line = theta * poly[0] + poly[1]


plt.figure(1)
plt.subplot(2,2,1)
plt.scatter( theta, welfare_priv_hybrid )
plt.plot(theta, y_line,'black',alpha=.75)
plt.title(r'Contribution Fraction $\theta$ of ARC',fontsize=15)
#plt.xlabel(r'$\theta$')
plt.ylabel('Welfare Change (%)',fontsize=15)

poly = np.polyfit(fr, welfare_priv_hybrid, 1)
y_line = fr * poly[0] + poly[1]

plt.subplot(2,2,2)
plt.scatter( fr, welfare_priv_hybrid )
plt.plot(fr, y_line,'black',alpha=.75)
plt.title(r'Initial Funded Ratio $\chi^0$',fontsize=15)
#plt.xlabel(r'$\chi^0$')




poly = np.polyfit(pub_size, welfare_priv_hybrid, 1)
y_line = pub_size * poly[0] + poly[1]


plt.subplot(2,2,3)
plt.scatter( pub_size, welfare_priv_hybrid )
plt.plot(pub_size, y_line,'black',alpha=.75)
plt.title(r'Public Sector Size',fontsize=15)
plt.ylabel('Welfare Change (%)',fontsize=15)

poly = np.polyfit(benefit, welfare_priv_hybrid, 1)
y_line = benefit * poly[0] + poly[1]

plt.subplot(2,2,4)

plt.figure(1)
plt.scatter( benefit, welfare_priv_hybrid )
plt.plot(benefit, y_line,'black',alpha=.75)
plt.title('Current Pension Benefit',fontsize=15)
#plt.xlabel(r'$\theta$')
#plt.ylabel('Welfare Change (%)',fontsize=15)

plt.suptitle('Average Private Sector Welfare Effects and State Characteristics',fontsize=20)

xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
    np.reshape(pub_size,(51,1)),
    

    ) ),axis=1  )
model = sm.OLS(welfare_priv_hybrid, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()


xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
    np.reshape(pub_size,(51,1)),
    
    np.reshape(fr,(51,1)),
    np.reshape(theta,(51,1)),  

    ) ),axis=1  )
model = sm.OLS(welfare_priv_hybrid, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()

xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
    np.reshape(pub_size,(51,1)),
    
    np.reshape(fr,(51,1)),
    np.reshape(theta,(51,1)),
    
    np.reshape(discount,(51,1))  ,
    np.reshape(discount,(51,1))*np.reshape(fr,(51,1)) ,
    np.reshape(discount,(51,1))*np.reshape(theta,(51,1)),

    ) ),axis=1  )
model = sm.OLS(welfare_priv_hybrid, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()

xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
    np.reshape(pub_size,(51,1)),
    
    np.reshape(fr,(51,1)),
    np.reshape(theta,(51,1)),
    
    np.reshape(discount,(51,1))  ,
    np.reshape(discount,(51,1))*np.reshape(fr,(51,1)) ,
    np.reshape(discount,(51,1))*np.reshape(theta,(51,1)),
    
    np.reshape(curr_old,(51,1)),
    np.reshape(four_old,(51,1))
    
    ) ),axis=1  )


model = sm.OLS(welfare_priv_hybrid, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()


# public sector welfare 
plt.close('all')

plt.figure(1)
social = state_vals[:,10]
poly = np.polyfit(social, welfare_pub_hybrid, 1)
y_line = social * poly[0] + poly[1]


plt.figure(1)
plt.subplot(2,2,1)
plt.scatter( social, welfare_pub_hybrid )
plt.plot(social, y_line,'black',alpha=.75)
plt.title('Social Security Coverage',fontsize=15)
#plt.xlabel(r'$\theta$')
plt.ylabel('Welfare Change (%)',fontsize=15)



poly = np.polyfit(fr, welfare_pub_hybrid, 1)
y_line = fr * poly[0] + poly[1]

plt.subplot(2,2,2)
plt.scatter( fr, welfare_pub_hybrid )
plt.plot(fr, y_line,'black',alpha=.75)
plt.title(r'Initial Funded Ratio $\chi^0$',fontsize=15)
#plt.xlabel(r'$\chi^0$')




poly = np.polyfit(pub_size, welfare_pub_hybrid, 1)
y_line = pub_size * poly[0] + poly[1]


plt.subplot(2,2,3)
plt.scatter( pub_size, welfare_pub_hybrid )
plt.plot(pub_size, y_line,'black',alpha=.75)
plt.title(r'Public Sector Size',fontsize=15)
plt.ylabel('Welfare Change (%)',fontsize=15)

poly = np.polyfit(benefit, welfare_pub_hybrid, 1)
y_line = benefit * poly[0] + poly[1]

plt.subplot(2,2,4)

plt.scatter( benefit, welfare_pub_hybrid )
plt.plot(benefit, y_line,'black',alpha=.75)
plt.title('Current Pension Benefit',fontsize=15)
#plt.xlabel(r'$\theta$')
#plt.ylabel('Welfare Change (%)',fontsize=15)

plt.suptitle('Public Sector Welfare Effects and State Characteristics',fontsize=20)


xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
    
    ) ),axis=1  )
model = sm.OLS(welfare_pub_hybrid, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()

xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
    
    np.reshape(social,(51,1))

    ) ),axis=1  )
model = sm.OLS(welfare_pub_hybrid, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()

xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
        
    np.reshape(social,(51,1)),
    
    np.reshape(pub_size,(51,1)), 

    ) ),axis=1  )
model = sm.OLS(welfare_pub_hybrid, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()


xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
        
    np.reshape(social,(51,1)),
    
    np.reshape(pub_size,(51,1)), 
    
    np.reshape(fr,(51,1)),
    np.reshape(theta,(51,1)),

    np.reshape(discount,(51,1))  
    
    ) ),axis=1  )
model = sm.OLS(welfare_pub_hybrid, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                           #
#   COLA Reform Analysis    #
#                           #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# public sector

plt.close('all')

poly = np.polyfit(social, welfare_pub20_cola, 1)
y_line = social * poly[0] + poly[1]
poly = np.polyfit(social, welfare_pub55_cola, 1)
y_line1 = social * poly[0] + poly[1]


fig,ax = plt.subplots(2,2)
ax[0,0].scatter( social, welfare_pub20_cola )
ax[0,0].plot(social, y_line)
ax[0,0].set_ylabel('Welfare Change (%)',fontsize=15)
ax[0,0].set_title('Social Security Coverage', fontsize=15)
ax1 = ax[0,0].twinx()
ax1.scatter( social, welfare_pub55_cola,c='g',alpha=.5,marker='x')
ax1.plot(social,y_line1,c='g',alpha=.5)


poly = np.polyfit(fr, welfare_pub20_cola, 1)
y_line = fr * poly[0] + poly[1]
poly = np.polyfit(fr, welfare_pub55_cola, 1)
y_line1 = fr * poly[0] + poly[1]

ax[0,1].scatter( fr, welfare_pub20_cola )
ax[0,1].plot(fr, y_line)
ax[0,1].set_title(r'Initial Funded Ratio $\chi^0$', fontsize=15)
ax1 = ax[0,1].twinx()
ax1.scatter( fr, welfare_pub55_cola,c='g',alpha=.5,marker='x')
ax1.plot(fr,y_line1,c='g',alpha=.5)


poly = np.polyfit(pub_size, welfare_pub20_cola, 1)
y_line = pub_size * poly[0] + poly[1]
poly = np.polyfit(pub_size, welfare_pub55_cola, 1)
y_line1 = pub_size * poly[0] + poly[1]

ax[1,0].scatter( pub_size, welfare_pub20_cola )
ax[1,0].plot(pub_size, y_line)
ax[1,0].set_ylabel('Welfare Change (%)',fontsize=15)
ax[1,0].set_title('Public Sector Size', fontsize=15)
ax1 = ax[1,0].twinx()
ax1.scatter( pub_size, welfare_pub55_cola,c='g',alpha=.5,marker='x')
ax1.plot(pub_size,y_line1,c='g',alpha=.5)

poly = np.polyfit(benefit, welfare_pub20_cola, 1)
y_line = benefit * poly[0] + poly[1]
poly = np.polyfit(benefit, welfare_pub55_cola, 1)
y_line1 = benefit * poly[0] + poly[1]


ax[1,1].scatter( benefit, welfare_pub20_cola,label='20 Year Old' )
ax[1,1].plot(benefit, y_line)
ax[1,1].set_title('Current Pension Benefit', fontsize=15)
ax1 = ax[1,1].twinx()
ax1.scatter( benefit, welfare_pub55_cola,c='g',alpha=.5,marker='x',label='55 Year Old')
ax1.plot(benefit,y_line1,c='g',alpha=.5)

plt.suptitle('Public Sector Welfare Effects and State Characteristics',fontsize=20)

handles,labels = [],[]
for ax in fig.axes:
    for h,l in zip(*ax.get_legend_handles_labels()):
        handles.append(h)
        labels.append(l)
plt.legend(handles,labels,fontsize=15)






# 20 Year old Public Worker
xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
        
    np.reshape(social,(51,1)),
    
    np.reshape(pub_size,(51,1)), 
    
    np.reshape(fr,(51,1)),
    np.reshape(theta,(51,1)),
    np.reshape(discount,(51,1)),  
    
    ) ),axis=1  )
model = sm.OLS(welfare_pub20_cola, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()


# 55 Year old Public Worker
xval = np.concatenate( (( 
    np.ones((51,1)),
    np.reshape(benefit,(51,1)),
        
    np.reshape(social,(51,1)),
    
    np.reshape(pub_size,(51,1)), 
    
    np.reshape(fr,(51,1)),
    np.reshape(theta,(51,1)),

    np.reshape(discount,(51,1)),  
    
    ) ),axis=1  )
model = sm.OLS(welfare_pub55_cola, xval).fit(cov_type="HC0") ## sm.OLS(output, input)
model.summary()


# Average Private Worker
xval = np.concatenate( (( 
    np.ones((51,1)),
    
    np.reshape(benefit,(51,1)),
    np.reshape(pub_size,(51,1)), 
    
    np.reshape(fr,(51,1))  ) ),axis=1  ) ,
    
    np.reshape(theta,(51,1)),

    np.reshape(discount,(51,1)),  
    np.reshape(curr_old,(51,1)),
    np.reshape(four_old,(51,1))
    
    ) ),axis=1  )
model = sm.OLS(welfare_priv_cola, xval).fit(cov_type="HC0") 
model.summary()

