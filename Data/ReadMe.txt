Data Directory

Summary: all raw data is kept in this directory.  The main filtered data files which are used in the main directory are
	full_panel_distribution.csv, StateDataValues.csv and state_names.csv

Data Files
- StateData.csv: contains fiscal/pension information from state financial reports, collected through the PPD

- CostLiving.csv: cost of living index for each state

- SectorSize.csv: proportion of public workers in each state, from BEA

- Participation.csv: labor force participation rates by age group, from BEA

- Population2020(2030)(2040).csv: each states forecast population by age group, from UVA Weldon Cooper

- Returns.csv: 1-year investment returns for each pension fund over the sample period 2000-2020

Programs
- main.py: 
	- Inputs
		- StateData.csv
		- CostLiving.csv
		- SectorSize.csv

	- Outputs
		- StateDataValues.csv: state-level parameters which correspond to Tables 2 and 4
		- state_names.csv 

- data_plots.py:
	- Inputs
		- StateData.csv
		- full_panel_distribution.csv

	- Outputs
		- Figures 1,2 and 4

- distribution_output.py: 
	- Inputs
		- Participation.csv
		- Population2020.csv
		- Population2030.csv
		- Population2040.csv
		- StateData.csv

	- Outputs
		- full_panel_distribution.csv: Ns x Nt x Na array where Ns = 51 (# of states), Nt=45 (# of forecast years)
										 and Na = 80 (# of lifecycle years). Each entry contains the proportion of
										 population which corresponds to that age cohort, that time and that state

		- curr_young_work.csv: proportion of population between ages 20 and 50, for each state
		- curr_old.csv: proportion of retired population, for each state
		- fif_old.csv: proportion of retired population in 15 years, for each state
		- thir_old.csv: proportion of retired population in 30 years, for each state
		- four_old.csv: proportion of retired population in 45 years, for each state
		- demographic.tex: Table 3

- Returns.py: 
	- Inputs
		- Returns.csv

	- Outputs
		- Figure 3		
	

