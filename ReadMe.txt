Public Pension Directory

Data Files
- full_panel_distribution.csv: demographic data for each state

- StateDataValues.csv: state-level calibration parameters

- state_names.csv 

Programs
- globals_module.f90: contains subroutines for solving lifecycle problem as well as global parameters

- main.f90: main program for solving OLG model for Baseline and Reform scenarios
	- Baseline model output (to be copied and pasted into the Baseline sub-directory)
		- avg_ass.csv: average pension assets over time, for each state
		- ce_priv_base.csv: consumption-equivalent welfare for each age cohort of private workers, each state
		- ce_pub_base.csv: same but for public workers
		- cohort_priv.csv: private worker wealth cross-section for each age cohort, each state 
		- cohort_pub.csv: wealth cross-section for public workers
		- fr.csv: average funded ratio over time, for each state
		- fr_vol.csv: volatility of simulated funded ratio
		- normcost.csv: normal costs for pension fund
		- pension_tax.csv: state tax necessary to pay normal costs and amortized unfunded liability
		- pvl.csv: total pension liabilities
		- tax_vol.csv: volatility of total taxes
		- total_tax.csv: total taxes (including those related to pension and wages)
		- ufl.csv: unfunded liability of pension
		- wage_tax.csv: tax specific to public sector wages

	- Reform model output (to be copied and pasted into respective sub-directory)
		- same output as baseline model with added "_{title}" where title = (closed, hybrid, cola) for the specific reform

- toolbox.f90: collection of helpful subroutines
