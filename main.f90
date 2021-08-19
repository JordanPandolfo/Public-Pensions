
include "globals_module.f90"

program PortfolioChoice

    use globals

    implicit none

    integer                     :: ii,tt,kk, jjj
    integer, dimension(1)       :: state_list
    real*8, dimension(50,10)     :: state_info_holder
    real*8, dimension(10,50)     :: state_info_holder1
    real*8, dimension(50*JR,JJ) :: state_dem_holder
    real*8, dimension(JJ,50*JR) :: state_dem_holder1
    real*8 :: curr_pvl, pvl_per_capita, temp_ben_temp, temp_year
    integer :: iterator
    real*8, dimension(JR) :: wage_share

    ! start the clock
    call tic()

    ! open all output files

    ! recording consumption-equivalent welfare
    open(21,file="ce_pub_base.csv")
    open(22,file="ce_priv_base.csv")
!    open(23,file="ce_pub_DCswitch.csv")
!    open(24,file="ce_priv_DCswitch.csv")
!    open(100,file="ce_pub_commit.csv")
!    open(101,file="ce_priv_commit.csv")

    ! recording cross-section of agents
    open(25,file="cohort_pub.csv")
    open(26,file="cohort_priv.csv")

    open(27,file="priv_risk.csv")
    open(28,file="pub_risk.csv")
    open(30,file="priv_save_rate.csv")
    open(31,file="priv_save_rate1.csv")
    open(32,file="pub_save_rate.csv")
    open(33,file="pub_save_rate1.csv")

    ! aggregates under the baseline
    open(34,file="pvl_state.csv")
    open(35,file="pvl_alt.csv")
    open(38,file="avg_ass.csv")
    open(40,file="total_tax.csv")
    open(41,file="wage_tax.csv")
    open(42,file="pension_tax.csv")
    open(47,file="tax_vol.csv")
    open(49,file="state_list.csv")
    open(50,file="fr.csv")
    open(51,file="fr_vol.csv")
    open(54,file="normcost.csv")
    open(56,file="ufl.csv")

    ! aggregates under full commitment
!    open(200,file="avg_ass_commit.csv")
!    open(201,file="total_tax_commit.csv")
!    open(202,file="wage_tax_commit.csv")
!    open(203,file="pension_tax_commit.csv")
!    open(204,file="tax_vol_commit.csv")
!    open(205,file="fr_commit.csv")
!    open(206,file="fr_vol_commit.csv")
!    open(207,file="pvl_state_commit.csv")
!    open(208,file="pvl_alt_commit.csv")
!    open(209,file="normcost_commit.csv")
!    open(210,file="ufl_commit.csv")

    ! pension value
    open(211,file="pension_value.csv")

    ! create array of states
    state_list = (/  23  /) ! looking at the state of Minnesota
    write(49,*) state_list

    ! read in state panel data
    open(12,file='StateDataValues.csv')
    read(12,*) state_info_holder1
    close(12)

    ! read in state demographic data
    open(13,file='full_panel_distribution.csv')
    read(13,*) state_dem_holder1
    close(13)

    state_info_holder = transpose(state_info_holder1)
    state_dem_holder = transpose(state_dem_holder1)

    ! for each state in array
    do ii=1,size(state_list)

        ! import state information (parameters and demographic change)
        ARC_share      = 0.78d0 !state_info_holder(state_list(ii),2)  ! pre- verus post-2007 MN = (1.23, .78)
        state_discount = state_info_holder(state_list(ii),3)/100d0
        current_status = 0.78d0 !state_info_holder(state_list(ii),4)/100d0   ! pre- versus post 2007 = (.93,.78)
        b              = state_info_holder(state_list(ii),5)
        prop_pub       = state_info_holder(state_list(ii),7)
        port_share     = state_info_holder(state_list(ii),8)

        cost_living    = state_info_holder(state_list(ii),9)

        curr_pvl  = state_info_holder(state_list(ii),10)

        ! use state discount factor
        alt_r_base = state_discount
        alt_r_ref  = state_discount

        ! set age demographic information
		age_probs = state_dem_holder( (1+ JR*(state_list(ii)-1) ):(JR*state_list(ii)),: )

		write(*,*)
		write(*,*) 'STATE:', state_list(ii)
		write(*,*) 'Theta:', ARC_share
		write(*,*) 'PVL Discount factor:', state_discount
		write(*,*) 'Funded ratio:', current_status
		write(*,*) 'pension:', b
		write(*,*) 'size of public sector:', prop_pub
		write(*,*) 'protfolio share:', port_share
		write(*,*) 'PVL:', curr_pvl
		write(*,*)

		write(*,*) 'Target rate of return:', port_share*( mu_r + r_f) + (1d0-port_share)*r_f

        ! compute total pop s.t. curr_pvl = PVL(0)
        eff_pub(JR:JJ) = 0d0
        do tt = 1, JR-1
            eff_pub(tt) = common_earnings_pub(tt+19d0)
        enddo

        do tt=1,JR-1
            wage_share(tt) = sum(eff_pub(1:tt))/sum(eff_pub(1:(JR-1)))
        enddo

        temp_ben_temp = 0d0
        iterator = 0
        do jjj = 1,80  ! for each year jjj in the future  (everything more than 80 years is not accrued)
            temp_year = 0d0
            do kk= JR, JJ  ! for each retiree age in year jjj
                if (kk-jjj >= JR) then ! if already retired today, full benefit accrual
                    temp_year = temp_year + prop_pub*b*age_probs(1+jjj,kk)
                else

                    if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                        if (jjj >= JR) then
                            temp_year = temp_year + prop_pub*b*age_probs(JR,kk)*wage_share(kk-jjj)
                        else
                            temp_year = temp_year + prop_pub*b*age_probs(1+jjj,kk)*wage_share(kk-jjj)
                        endif
                    endif

                endif
            enddo
            temp_ben_temp = temp_ben_temp + temp_year/(1d0+state_discount)**jjj
        enddo

        pvl_per_capita = temp_ben_temp
        total_pop = curr_pvl/pvl_per_capita
        write(*,*) 'total iterations:', iterator

        write(*,*) 'PVL per Cap:', pvl_per_capita
        write(*,*) 'total pop:', total_pop

        !---------------------------!
        !                           !
        !   Solve baseline model    !
        !                           !
        !---------------------------!

        ! initialize baseline
        call initialize_baseline()

        tt_com = 1
        call solve_household_pub()
        call solve_household_priv()
        tt_com = 5
        call solve_household_pub()
        call solve_household_priv()
        tt_com = 10
        call solve_household_pub()
        call solve_household_priv()
        tt_com = 20
        call solve_household_pub()
        call solve_household_priv()
        tt_com = 30
        call solve_household_pub()
        call solve_household_priv()
        tt_com = 35
        call solve_household_pub()
        call solve_household_priv()
        tt_com = 40
        call solve_household_pub()
        call solve_household_priv()

        ! compute ce-welfare by age cohort
        call CE_compute_pub(updated_status_pre)
        call CE_compute_priv(updated_status_pre)

        ! output the welfare measurements
        call output_baseline()


        !-------------------------------------------------------------------------------------!
        !                                                                                     !
        !   solve model for age t=1 agents where they receive a DC with compensation of y%    !
        !                       holding tax policy constant                                   !
        !                                                                                     !
        !-------------------------------------------------------------------------------------!

        ! keep the baseline initialization

!        ! only change the contribution rate that workers get
!        dc_con =  dc_rate
!
!        ! and drop the DB pension
!        pen_pub(:,JR:JJ) = ss_benefit_pub
!
!        tt_com = 1
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 5
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 10
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 20
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 30
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 35
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 40
!        call solve_household_pub()
!        call solve_household_priv()
!
!        ! compute ce-welfare by age cohort
!        call CE_compute_pub(updated_status_pre)
!        call CE_compute_priv(updated_status_pre)
!
!        ! output the welfare measurements
!        call output_DCswitch()

        !------------------------------------------------------------!
        !                                                            !
        !   Solve counterfactual where they pay 100% of their ARC    !
        !        allowing fiscal policy to adjust/respond            !
        !                                                            !
        !------------------------------------------------------------!

        ! full commitment to funding the pension fund
!        ARC_share = 1d0
!
!        ! initialize baseline (need to reset the pension system and taxes)
!        call initialize_baseline()
!
!        tt_com = 1
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 5
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 10
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 20
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 30
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 35
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 40
!        call solve_household_pub()
!        call solve_household_priv()
!
!        ! compute ce-welfare by age cohort
!        call CE_compute_pub(updated_status_pre)
!        call CE_compute_priv(updated_status_pre)
!
!        ! output the welfare measurements
!        call output_commit()


        !---------------------------------------------------------!
        !                                                         !
        !   Simulate baseline model with no demographic change    !
        !                                                         !
        !---------------------------------------------------------!

        ! reset commitment to funding the pension
        ARC_share = 0.78d0 !state_info_holder(state_list(ii),2)

        ! reset baseline again
        call initialize_baseline()

        ! set to initial age cohort distribution
        tt_com = 1

        ! solve household problems
        call solve_household_pub()
        call solve_household_priv()

        ! simulate baseline model: obtain cash-in-hand distributions for each age cohort
        call simulate_pub()
        call Phi_construct_pub()

        call simulate_priv()
        call Phi_construct_priv()

        ! simulate fiscal aggregates
        call simulator()

        ! simulated output for worker problems
        call output_simulator()

    enddo

    ! close all output files
    close(21)
    close(22)
    close(23)
    close(24)
    close(100)
    close(101)

    close(25)
    close(26)

    close(27)
    close(28)
    close(30)
    close(31)
    close(32)
    close(33)

    close(34)
    close(35)
    close(38)
    close(40)
    close(41)
    close(42)
    close(47)
    close(49)
    close(50)
    close(51)
    close(54)
    close(56)

    close(211)

    ! stop the clock
    call toc()

contains

    ! initializes all remaining variables
    subroutine initialize_baseline()

        implicit none
        real*8 :: temp(NSR, 2), iterator
        real*8, dimension(JJ,JR) :: joint
        integer :: tt, jjj, kk
        real*8, dimension(JR) :: wage_share
        real*8 :: temp_ben_hold, temp_ben_temp, temp_year, temp_period, temp_work, btilde
        real*8, dimension(JR) :: ages

        ! set survival probabilities
        psi = (/  0.0d0, 0.000726d0, 0.000806d0, 0.000868d0, 0.000905d0, 0.000924d0, &
                    0.000939d0, 0.000957d0, 0.000977d0, 0.001002d0, 0.001030d0, &
                    0.001061d0, 0.001094d0, 0.001127d0, 0.001161d0, 0.001200d0, &
                    0.001251d0, 0.001317d0, 0.001394d0, 0.001480d0, 0.001575d0, &
                    0.001680d0, 0.001802d0, 0.001950d0, 0.002130d0, 0.002344d0, &
                    0.002575d0, 0.002824d0, 0.003112d0, 0.003437d0, 0.003787d0, &
                    0.004146d0, 0.004509d0, 0.004884d0, 0.005282d0, 0.005708d0, &
                    0.006167d0, 0.006651d0, 0.007156d0, 0.007673d0, 0.008210d0, &
                    0.008784d0, 0.009408d0, 0.010083d0, 0.010819d0, 0.011628d0, &
                    0.012530d0, 0.013534d0, 0.014658d0, 0.015888d0, 0.017236d0, &
                    0.018831d0, 0.020693d0, 0.022723d0, 0.024884d0, 0.027216d0, &
                    0.029822d0, 0.032876d0, 0.036328d0, 0.040156d0, 0.044699d0, &
                    0.049419d0, 0.054529d0, 0.060341d0, 0.067163d0, 0.074785d0, &
                    0.083577d0, 0.093319d0, 0.103993d0, 0.115643d0, 0.128300d0, &
                    0.141986d0, 0.156706d0, 0.172451d0, 0.189191d0, 0.206875d0, &
                    0.225433d0, 0.244768d0, 0.264767d0, 0.285296d0, 0.306203d0  /)

		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! 														  !
		!    initialize earnings process and retirement income    !
		!														  !
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        eff_pub(JR:JJ) = 0d0
        eff_priv(JR:JJ) = 0d0

        do tt = 1, JR-1
            eff_pub(tt) = common_earnings_pub(tt+19d0)
            eff_priv(tt) = common_earnings_priv(tt+19d0)
        enddo

        do tt=1,JR-1
            wage_share(tt) = sum(eff_pub(1:tt))/sum(eff_pub(1:(JR-1)))
        enddo

        ! private pension covers social security
        pen_priv = 0d0
        pen_priv(JR:JJ) = ss_benefit_priv       ! private workers receive social security

        ! public pension includes DB and is inflation-adjusted
        ! Assume a 1.8% rate of inflation, and MN has a 1% COLA; thus, purchasing power declines by .8% per year.

        pen_pub  = 0d0

        do tt=1,JR  ! for each working age cohort, receive potential reduction in pension benefit
            do jjj=JR,JJ

                pen_pub(tt,jjj) = (1d0-.008d0)**(jjj-JR)*b + ss_benefit_pub  ! .08% miss COLA
                !pen_pub(tt,jjj) = b + ss_benefit_pub  ! full cost of living

            enddo
        enddo

        write(211,*) pen_pub(1,:)


        ! DC contribution rates for public workers
        dc_con = 0d0

		!!!!!!!!!!!!!!!!!!!!!!!!!!
		! 						 !
		!    initialize grids    !
		! 						 !
		!!!!!!!!!!!!!!!!!!!!!!!!!!
        call normal_discrete(zeta_priv, dist_zeta_priv, 0d0, sigma_zeta_priv)                         ! zeta shocks
        zeta_priv = exp(zeta_priv)

        call normal_discrete((/NS, NR/), temp, dist_epsvtheta_priv, (/0d0, 0d0/), &         ! eps-vtheta shocks
                                                  (/sigma_eps_priv, sigma_vtheta_priv/), rho_priv)
        eps_priv(:) = exp(temp(:, 1))
        vtheta_priv(:)  = temp(:, 2)

        call normal_discrete(zeta_pub, dist_zeta_pub, 0d0, sigma_zeta_pub)                         ! zeta shocks
        zeta_pub = exp(zeta_pub)

        call normal_discrete((/NS, NR/), temp, dist_epsvtheta_pub, (/0d0, 0d0/), &         ! eps-vtheta shocks
                                                  (/sigma_eps_pub, sigma_vtheta_pub/), rho_pub)
        eps_pub(:) = exp(temp(:, 1))
        vtheta_pub(:)  = temp(:, 2)

        call grid_Cons_Grow(a_pub, a_l, a_u, a_grow)                                   ! asset grid for public workers
        call grid_Cons_Grow(a_priv, a_l, a_u, a_grow)                                  ! asset grid for private workers

        call grid_Cons_Grow(X_pub, X_l_pub, X_u_pub, X_grow)                           ! wealth grid for public workers
        call grid_Cons_Grow(X_priv, X_l_priv, X_u_priv, X_grow)                        ! wealth grid for private workers

        call grid_Cons_Grow(chi_grid,chi_a,chi_b,chi_grow)                             ! funded ratio grid

		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		!														   !
		!    compute aggregate benefits and pension liabilities    !
		! 														   !
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! aggregate benefit distributions
        do tt = 1,JR  ! for each time period (NOT AGE COHORT)
            Agg_B_hold(tt) = total_pop*b*prop_pub*sum( age_probs(tt,JR:) )
        enddo

		arc_rate = state_discount  !alt_r_base

        ! compute pension liabilities, per period
        do tt = 1,JR  ! for each year in the future

            temp_ben_hold = 0d0
            temp_ben_temp = 0d0

            do jjj = 1,80  ! for each year t+j in the future  (everything more than 80 years is not accrued)

                temp_year = 0d0

                do kk= JR, JJ  ! for each retiree age in year t+j

                    if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                        if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                            temp_year = temp_year + total_pop*prop_pub*b*age_probs(JR,kk)
                        else
                            temp_year = temp_year + total_pop*prop_pub*b*age_probs(tt+jjj,kk)
                        endif

                    else

                        if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                            if ((tt+jjj) > JR) then
                                temp_year = temp_year + total_pop*prop_pub*b*age_probs(JR,kk)*wage_share(kk-jjj)
                            else
                                temp_year = temp_year + total_pop*prop_pub*b*age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                            endif
                        endif

                    endif

                enddo

                ! add up and discount total cash flows for year t+j
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+alt_r_base)**jjj
                temp_ben_temp = temp_ben_temp + temp_year/(1d0+state_discount)**jjj

			enddo

			! pricing liabilities with alternative rate
            PV_L_hold(tt) = temp_ben_hold

			! pricing liabilities with state discount factor
			PV_L_temp(tt) = temp_ben_temp

        enddo

        ! compute normal cost each period: Norm_Cost(tt)
        do tt=1,JR  ! for each time period

            temp_period = 0d0
            do kk = 1,JR ! for each working cohort

                ! compute newly accrued benefit
                btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )

                temp_work = 0d0
                do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                    temp_work = temp_work + btilde*product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
                enddo

                temp_period = temp_period + total_pop*prop_pub*age_probs(tt,kk)*temp_work
            enddo

            Norm_Cost(tt) = temp_period
        enddo

		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		!										 !
		!    compute wage tax and pension tax    !
		!										 !
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do tt=1,JR  ! for each time period (NOT AGE COHORT)

			tax_base(tt) = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) + (1d0-prop_pub)*age_probs(tt,1:JR)*eff_priv(1:JR) )

			implied_tax(tt) = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )/tax_base(tt)

        enddo

        bond_tax = 0d0

		updated_status_pre = PV_L_temp(1)*current_status/PV_L_hold(1)    ! funded ratio, adjusting for alternative discount factor

        tax_mat = tax_populate()									 ! tax matrix
        chi_mat = chi_populate() 									 ! funded ratio matrix

    end subroutine

    subroutine output_baseline()

        implicit none

        integer :: tt

        do tt = 1,JR

            write(21,'(f12.5)') CE_val_pub(tt,:)
            write(22,'(f12.5)') CE_val_priv(tt,:)

        enddo

    end subroutine

    subroutine output_DCswitch()

        implicit none

        integer :: tt
        real*8, dimension(JR) :: time

        do tt=1,JR
            time(tt) = tt
        enddo


        do tt = 1,JR

            write(23,'(f12.5)') CE_val_pub(tt,:)
            write(24,'(f12.5)') CE_val_priv(tt,:)

        enddo

    end subroutine

    subroutine output_commit()

        implicit none

        integer :: tt
        real*8, dimension(JR) :: time

        do tt=1,JR
            time(tt) = tt
        enddo


        do tt = 1,JR

            write(100,'(f12.5)') CE_val_pub(tt,:)
            write(101,'(f12.5)') CE_val_priv(tt,:)

        enddo

    end subroutine


    subroutine simulate_pub()

    implicit none

    real*8 :: death_status, chi_status, xp, varphix, varphiy
    real*8 :: varphia, con, port, ap, zeta, rshock, port_return, tax, asset_post, act_con, arc_temp, un_temp, pen_return, &
                eps_shock, eps
    integer :: ii,tt, ixl, ixr, iyl, iyr, ial, iar

    death_status = 0d0
    recorder_pub = 0d0
    recorder_pub(:,:,5) = 1d0     !labeled 1 initially.  if 0, implies agent still alive at that age

    do tt=1,nsim   ! for each simulant

        ! initialize
        death_status = 0d0
        asset_post = updated_status_pre*PV_L_hold(1)  ! initial funded status of pension
        tax = 0d0
        eps = 0d0

        ii = 1
        ap = 0d0
        port = 0.5d0

        do while (death_status == 0d0)

            if (ii<JR) then  ! still working

                ! draw labor income, market shock
                call simulate_normal(zeta,0d0,sigma_zeta_pub)       ! transient shock
                call simulate_normal(eps_shock,0d0,sigma_eps_pub)   ! persistent shock

                call simulate_normal(rshock,mu_r+r_f,sigma_vtheta_pub)
                call simulate_normal(pen_return, port_share*mu_r + (1d0-port_share)*r_f, port_share**2d0*sigma_vtheta_pub )

                ! compute period pension tax and post-contribution funded status

                ! portfolio return
                port_return = port*(1d0+rshock)+(1d0-port)*(1d0+r_f)

                ! update pension funded status
                chi_status = ((1d0+pen_return)*asset_post - Agg_B_hold(1))/PV_L_hold(1)

                ! update cash-in-hand
                if (chi_status >= 1d0) then  ! fully funded

                    ! compute normal cost contribution and tax
                    act_con = ARC_share*Norm_Cost(1)
                    tax = act_con/((total_pop*sum(age_probs(1,1:JR)*(prop_pub*eff_pub(1:JR) + &
                                                    (1d0-prop_pub)*eff_priv(1:JR) )) ))

                    xp = port_return*ap/exp(eps_shock) + (1d0 - tax - implied_tax(1))*eff_pub(ii)*exp( zeta )

                    ! if funded status above surplus bound
                    if (chi_status >= surplus_bound) then
                        asset_post = surplus_bound*PV_L_hold(1)         ! next-period assets, pre-return
                    else
                        asset_post = chi_status*PV_L_hold(1) + act_con  ! next-period assets, pre-return
                    endif

                else    ! underfunded

                    act_con = ARC_share*ARC(chi_status,PV_L_hold(1),1)   ! required contribution

                    tax = act_con/((total_pop*sum(age_probs(1,1:JR)*(prop_pub*eff_pub(1:JR) + &
                                                        (1d0-prop_pub)*eff_priv(1:JR) )) ))

                    xp = port_return*ap/exp(eps_shock) + (1d0-tax- implied_tax(1))*eff_pub(ii)*exp( zeta )  ! with tax

                    asset_post = chi_status*PV_L_hold(1) + act_con

                endif

                call linint_Grow(xp,X_l_pub,X_u_pub,X_grow,NX,ixl,ixr,varphix)
                call linint_Grow(chi_status,chi_a,chi_b,chi_grow,chi_n,iyl,iyr,varphiy)

                ! compute consumption decision
                if (varphix <- varphiy) then  ! bilinear interpolation

                    con = varphix*c_pub(1,ii,ixl,iyl) + &
                            (varphiy-varphix)*c_pub(1,ii,ixr,iyl) + &
                            (1d0-varphiy)*c_pub(1,ii,ixr,iyr)

                else

                    con = varphiy*c_pub(1,ii,ixl,iyl) + &
                            (varphix-varphiy)*c_pub(1,ii,ixl,iyr) + &
                            (1d0-varphix)*c_pub(1,ii,ixr,iyr)

                endif

                con = max(con, 1d-10)
                ap = xp - con

                ! compute portfolio decision

                call linint_Grow(ap,a_l,a_u,a_grow,NA,ial,iar,varphia)

                if (varphia <- varphiy) then  ! bilinear interpolation

                    port = varphia*omega_plus_pub(1,ii,ial,iyl) + &
                            (varphiy-varphia)*omega_plus_pub(1,ii,iar,iyl) + &
                            (1d0-varphiy)*omega_plus_pub(1,ii,iar,iyr)

                else

                    port = varphiy*omega_plus_pub(1,ii,ial,iyl) + &
                            (varphia-varphiy)*omega_plus_pub(1,ii,ial,iyr) + &
                            (1d0-varphia)*omega_plus_pub(1,ii,iar,iyr)

                endif

                recorder_pub(tt,ii,1) = xp
                recorder_pub(tt,ii,2) = con
                recorder_pub(tt,ii,3) = ap
                recorder_pub(tt,ii,4) = port
                recorder_pub(tt,ii,5) = death_status
                recorder_pub(tt,ii,6) = tax
                recorder_pub(tt,ii,7) = chi_status
                recorder_pub(tt,ii,8) = (1d0- implied_tax(1))*eff_pub(ii)*exp( zeta )
                recorder_pub(tt,ii,9) = eff_pub(ii)*exp( zeta )

                ii = ii + 1

                call simulate_bernoulli(death_status,psi(ii))

            else     ! retired

                ! draw market shock
                call simulate_normal(rshock,mu_r+r_f,sigma_vtheta_pub)

                ! portfolio return
                port_return = port*(1d0+rshock)+(1d0-port)*(1d0+r_f)

                ! update cash-in-hand
                !if (ii==JR) then  ! first year of retirement
                !
                !    call simulate_normal(pen_return, port_share*mu_r + (1d0-port_share)*r_f, port_share**2d0*sigma_vtheta_pub )
                !    chi_status = ((1d0+pen_return)*asset_post - Agg_B_hold(1))/PV_L_hold(1)
                !    if (chi_status >= surplus_bound) then  ! receive pension surplus distribution
                !        xp = port_return*ap + pen_pub(1,ii) + &
                !                    (1d0-phi_g)*PV_L_hold(1)*(chi_status-surplus_bound)/(total_pop*age_probs(1,ii)*prop_pub)! no tax
                !    else
                !        xp = port_return*ap + pen_pub(1,ii) ! no tax
                !    endif
                !
                !else
                xp = port_return*ap + pen_pub(1,ii) ! no tax
                !endif

                call linint_Grow(xp,X_l_pub,X_u_pub,X_grow,NX,ixl,ixr,varphix)

                con = varphix*c_pub(1,ii,ixl,1) + (1-varphix)*c_pub(1,ii,ixr,1)
                con = max(con, 1d-10)

                ap = xp - con

                ! compute portfolio decision

                call linint_Grow(ap,a_l,a_u,a_grow,NA,ial,iar,varphia)


                port = varphia*omega_plus_pub(1,ii,ial,1) + (1-varphia)*omega_plus_pub(1,ii,iar,1)

                recorder_pub(tt,ii,1) = xp
                recorder_pub(tt,ii,2) = con
                recorder_pub(tt,ii,3) = ap
                recorder_pub(tt,ii,4) = port
                recorder_pub(tt,ii,5) = death_status
                recorder_pub(tt,ii,6) = 0d0
                recorder_pub(tt,ii,7) = 0d0

                ii = ii + 1

                call simulate_bernoulli(death_status,psi(ii))

                endif

            if (ii >= JJ) then
                death_status = 1d0
            endif

        enddo
    enddo

    end subroutine

    subroutine Phi_construct_pub()

        implicit none

        integer :: tt, ii, iterator, ixl, ixr
        real*8, dimension(JJ,nsim) :: xhold
        integer, dimension(JJ) :: ithold
        real*8 :: varphi

        cohort_dist_pub = 0d0

        do ii=1,JR  ! for each age cohort

            iterator = 1

            do tt=1,nsim  ! for each simulant

                if (recorder_pub(tt,ii,5) == 0) then  ! agent still alive

                    !write(*,*) 'Age:',ii,'wealth:',recorder_pub(tt,ii,1)

                    xhold(ii,iterator) = recorder_pub(tt,ii,1)

                    iterator = iterator + 1

                endif

            enddo

            ithold(ii) = iterator -1

        enddo

        ! compute cohort distribution over cash-in-hand
        do ii = 1,JR    ! for each age cohort

            do tt = 1, ithold(ii)   ! for each living simulant at that age


                if ( (xhold(ii,tt) > X_l_pub) .AND. (xhold(ii,tt)<X_u_pub) ) then
                    call linint_Grow(xhold(ii,tt),X_l_pub,X_u_pub,X_grow,NX,ixl,ixr,varphi)

                    cohort_dist_pub(ii,ixl) = cohort_dist_pub(ii,ixl) + varphi
                    cohort_dist_pub(ii,ixr) = cohort_dist_pub(ii,ixr) + (1d0-varphi)
                endif

            enddo

			do tt =1,NX
                if (cohort_dist_pub(ii,tt) < 0d0) then
					cohort_dist_pub(ii,tt) = 0d0
				endif
			enddo

            if (sum(cohort_dist_pub(ii,:)) == 0d0 ) then
                cohort_dist_pub(ii,:) = 0d0
            else
                cohort_dist_pub(ii,:) = cohort_dist_pub(ii,:)/sum(cohort_dist_pub(ii,:))
            endif

        enddo

    end subroutine

    subroutine simulate_priv()

    implicit none

    real*8 :: death_status, chi_status, xp, varphix, varphiy
    real*8 :: varphia, con, port, ap, zeta, rshock, port_return, tax, asset_post, act_con, arc_temp, un_temp, pen_return, &
                eps_shock, eps
    integer :: ii,tt, ixl, ixr, iyl, iyr, ial, iar

    death_status = 0d0
    recorder_priv = 0d0
    recorder_priv(:,:,5) = 1d0     !labeled 1 initially.  if 0, implies agent still alive at that age

    do tt=1,nsim   ! for each simulant

        ! initialize
        death_status = 0d0
        asset_post = updated_status_pre*PV_L_hold(1)  ! initial funded status of pension
        tax = 0d0
        eps = 0d0

        ii = 1
        ap = 0d0
        port = 0.5d0

        do while (death_status == 0d0)

            if (ii<JR) then  ! still working

                ! draw labor income, market shock
                call simulate_normal(zeta,0d0,sigma_zeta_priv)
                call simulate_normal(eps_shock,0d0,sigma_eps_priv)   ! persistent shock

                call simulate_normal(rshock,mu_r+r_f,sigma_vtheta_priv)

                call simulate_normal(pen_return, port_share*mu_r + (1d0-port_share)*r_f, port_share**2d0*sigma_vtheta_priv )

                ! portfolio return
                port_return = port*(1d0+rshock)+(1d0-port)*(1d0+r_f)

                ! update pension funded status
                chi_status = ((1d0+pen_return)*asset_post - Agg_B_hold(1))/PV_L_hold(1)

                ! update cash-in-hand
                if (chi_status >= 1d0) then  ! fully funded

                    act_con = ARC_share*Norm_Cost(1)   ! required contribution

                    tax = act_con/((total_pop*sum(age_probs(1,1:JR)*(prop_pub*eff_pub(1:JR) + &
                                                        (1d0-prop_pub)*eff_priv(1:JR) )) ))

                    xp = port_return*ap/exp(eps_shock) + (1d0 - tax - implied_tax(1))*eff_priv(ii)*exp( zeta ) ! no tax

                    if (chi_status >= surplus_bound) then
                        asset_post = surplus_bound*PV_L_hold(1)  ! next-period assets, pre-return
                    else
                        asset_post = chi_status*PV_L_hold(1)+act_con  ! next-period assets, pre-return
                    endif

                else    ! underfunded

                    act_con = ARC_share*ARC(chi_status,PV_L_hold(1),1)   ! required contribution

                    tax = act_con/((total_pop*sum(age_probs(1,1:JR)*(prop_pub*eff_pub(1:JR) + &
                                                        (1d0-prop_pub)*eff_priv(1:JR) )) ))

                    xp = port_return*ap/exp(eps_shock) + (1d0-tax - implied_tax(1))*eff_priv(ii)*exp( zeta )  ! with tax

                    asset_post = chi_status*PV_L_hold(1) + act_con

                endif

                call linint_Grow(xp,X_l_priv,X_u_priv,X_grow,NX,ixl,ixr,varphix)
                call linint_Grow(chi_status,chi_a,chi_b,chi_grow,chi_n,iyl,iyr,varphiy)

                ! compute consumption decision
                if (varphix <- varphiy) then  ! bilinear interpolation

                    con = varphix*c_priv(1,ii,ixl,iyl) + &
                            (varphiy-varphix)*c_priv(1,ii,ixr,iyl) + &
                            (1d0-varphiy)*c_priv(1,ii,ixr,iyr)

                else

                    con = varphiy*c_priv(1,ii,ixl,iyl) + &
                            (varphix-varphiy)*c_priv(1,ii,ixl,iyr) + &
                            (1d0-varphix)*c_priv(1,ii,ixr,iyr)

                endif

                con = max(con, 1d-10)
                ap = xp - con

                ! compute portfolio decision

                call linint_Grow(ap,a_l,a_u,a_grow,NA,ial,iar,varphia)

                if (varphia <- varphiy) then  ! bilinear interpolation

                    port = varphia*omega_plus_priv(1,ii,ial,iyl) + &
                            (varphiy-varphia)*omega_plus_priv(1,ii,iar,iyl) + &
                            (1d0-varphiy)*omega_plus_priv(1,ii,iar,iyr)

                else

                    port = varphiy*omega_plus_priv(1,ii,ial,iyl) + &
                            (varphia-varphiy)*omega_plus_priv(1,ii,ial,iyr) + &
                            (1d0-varphia)*omega_plus_priv(1,ii,iar,iyr)

                endif

                recorder_priv(tt,ii,1) = xp
                recorder_priv(tt,ii,2) = con
                recorder_priv(tt,ii,3) = ap
                recorder_priv(tt,ii,4) = port
                recorder_priv(tt,ii,5) = death_status
                recorder_priv(tt,ii,6) = tax
                recorder_priv(tt,ii,7) = chi_status
                recorder_priv(tt,ii,8) = (1d0- implied_tax(1))*eff_priv(ii)*exp( zeta )
                recorder_priv(tt,ii,9) = eff_priv(ii)*exp( zeta )

                ii = ii + 1

                call simulate_bernoulli(death_status,psi(ii))

            else     ! retired

                ! draw market shock
                call simulate_normal(rshock,mu_r+r_f,sigma_vtheta_priv)

                ! portfolio return
                port_return = port*(1d0+rshock)+(1d0-port)*(1d0+r_f)

                ! update cash-in-hand
                xp = port_return*ap + pen_priv(ii) ! no tax

                call linint_Grow(xp,X_l_priv,X_u_priv,X_grow,NX,ixl,ixr,varphix)

                con = varphix*c_priv(1,ii,ixl,1) + (1d0-varphix)*c_priv(1,ii,ixr,1)
                con = max(con, 1d-10)

                ap = xp - con

                ! compute portfolio decision

                call linint_Grow(ap,a_l,a_u,a_grow,NA,ial,iar,varphia)


                port = varphia*omega_plus_priv(1,ii,ial,1) + (1d0-varphia)*omega_plus_priv(1,ii,iar,1)

                recorder_priv(tt,ii,1) = xp
                recorder_priv(tt,ii,2) = con
                recorder_priv(tt,ii,3) = ap
                recorder_priv(tt,ii,4) = port
                recorder_priv(tt,ii,5) = death_status
                recorder_priv(tt,ii,6) = 0d0
                recorder_priv(tt,ii,7) = 0d0

                ii = ii + 1

                call simulate_bernoulli(death_status,psi(ii))

            endif

            if (ii >= JJ) then
                death_status = 1d0
            endif

        enddo
    enddo

    end subroutine

    subroutine Phi_construct_priv()

        implicit none

        integer :: tt, ii, iterator, ixl, ixr
        real*8, dimension(JJ,nsim) :: xhold
        integer, dimension(JJ) :: ithold
        real*8 :: varphi

        cohort_dist_priv = 0d0

        do ii=1,JR  ! for each age cohort

            iterator = 1

            do tt=1,nsim  ! for each simulant

                if (recorder_priv(tt,ii,5) == 0) then  ! agent still alive

                    xhold(ii,iterator) = recorder_priv(tt,ii,1)

                    iterator = iterator + 1

                endif

            enddo

            ithold(ii) = iterator -1

        enddo

        ! compute cohort distribution over cash-in-hand
        do ii = 1,JR    ! for each age cohort

            do tt = 1, ithold(ii)   ! for each living simulant at that age


                if ( (xhold(ii,tt) > X_l_priv) .AND. (xhold(ii,tt)<X_u_priv) ) then

                    call linint_Grow(xhold(ii,tt),X_l_priv,X_u_priv,X_grow,NX,ixl,ixr,varphi)

                    cohort_dist_priv(ii,ixl) = cohort_dist_priv(ii,ixl) + varphi
                    cohort_dist_priv(ii,ixr) = cohort_dist_priv(ii,ixr) + (1d0-varphi)

                endif

            enddo

			do tt =1,NX
				if (cohort_dist_priv(ii,tt) < 0d0) then
					cohort_dist_priv(ii,tt) = 0d0
				endif
			enddo


            if (sum(cohort_dist_priv(ii,:)) == 0d0 ) then
                cohort_dist_priv(ii,:) = 0d0
            else
                cohort_dist_priv(ii,:) = cohort_dist_priv(ii,:)/sum(cohort_dist_priv(ii,:))
            endif

        enddo

    end subroutine

	subroutine simulator()

		! want to capture (1) average pension tax, (2) volatility pension tax (3) average funded ratio
		!    (a) over time (b) pre-reform and (c) post-reform
		implicit none

		real*8, dimension(JR) :: avg_pen_tax_pre, avg_pen_tax_post, vol_pen_tax_pre, vol_pen_tax_post, &
								 avg_fund_pre, avg_fund_post, avg_ass_pre, avg_ass_post, fund_pre_vol, fund_post_vol, &
								 avg_ufl_pre, avg_ufl_post

		integer, parameter :: Nsim = 100000       ! number of simulations
		real*8, dimension(JR) :: shocks
		integer :: ii,tt,jjj, kk
		real*8, dimension(4,Nsim,JR) :: recorder_pre, recorder_post   ! record (1) funded ratio (2) pension tax (3) assets (4) amortized ufl
		real*8, dimension(4,JR) :: tax_recorder_pre, tax_recorder_post   ! (1) wage tax, (2) DC tax (3) total tax (4) bond financing
		real*8 :: temp_c, temp_assets, tax_base, iterator
		real*8, dimension(Nsim) :: sim_var
		real*8, dimension(JR)   :: time_var

		real*8, dimension(JR) :: Agg_B_hold_simbase, PV_L_hold_simbase, PV_L_temp_simbase, Agg_B_hold_simref, PV_L_hold_simref, &
                                 PV_L_temp_simref, norm_cost_base, norm_cost_ref

        real*8, dimension(JR) :: time_grid
        real*8 :: temp_ben_hold, temp_ben_temp, temp_year, temp_period, temp_work, btilde
        real*8, dimension(JR) :: wage_share, ages


        ! wage shares
        do tt=1,JR-1
            wage_share(tt) = sum(eff_pub(1:tt))/sum(eff_pub(1:(JR-1)))
        enddo

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                                                                !
        !   compute pension liabilities under baseline (same as scenario with theta=1)   !
        !                                                                                !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

        do tt = 1,JR  ! for each year in the future
            temp_ben_hold = 0d0
            temp_ben_temp = 0d0
            do jjj = 1,80  ! for each year t+j in the future  (everything more than 80 years is not accrued)
                temp_year = 0d0
                do kk= JR, JJ  ! for each retiree age in year t+j

                    if (kk-jjj >= JR) then ! if already retired today, full benefit accrual
                        if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                            temp_year = temp_year + total_pop*prop_pub*b*age_probs(JR,kk)
                        else
                            temp_year = temp_year + total_pop*prop_pub*b*age_probs(tt+jjj,kk)
                        endif
                    else
                        if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                            if ((tt+jjj) > JR) then
                                temp_year = temp_year + total_pop*prop_pub*b*age_probs(JR,kk)*wage_share(kk-jjj)
                            else
                                temp_year = temp_year + total_pop*prop_pub*b*age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                            endif
                        endif
                    endif
                enddo
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+alt_r_base)**jjj
                temp_ben_temp = temp_ben_temp + temp_year/(1d0+state_discount)**jjj
			enddo
			! pricing liabilities with alternative rate
            PV_L_hold_simbase(tt) = temp_ben_hold
            PV_L_hold_simref(tt) = temp_ben_hold

			! pricing liabilities with state discount factor
			PV_L_temp_simbase(tt) = temp_ben_temp
			PV_L_temp_simref(tt) = temp_ben_temp

        enddo

        ! aggregate benefit distributions
        do tt = 1,JR  ! for each time period (NOT AGE COHORT)
            Agg_B_hold_simbase(tt) = total_pop*b*prop_pub*sum( age_probs(tt,JR:) )
            Agg_B_hold_simref(tt) = total_pop*b*prop_pub*sum( age_probs(tt,JR:) )
        enddo

        ! compute normal cost
        do tt=1,JR  ! for each time period
            temp_period = 0d0
            do kk = 1,JR ! for each working cohort
                btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                temp_work = 0d0
                do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                    temp_work = temp_work + btilde*product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
                enddo
                temp_period = temp_period + total_pop*prop_pub*age_probs(tt,kk)*temp_work
            enddo
            norm_cost_base(tt) = temp_period
        enddo

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                                                   !
        !   record taxes for paying public wages (DC tax + bond tax = 0)    !
        !                                                                   !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! wage tax is the same
        do tt=1,JR  ! for each time period (NOT AGE COHORT)
			tax_base = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) + (1d0-prop_pub)*age_probs(tt,1:JR)*eff_priv(1:JR) )
			tax_recorder_pre(1,tt)  = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )/tax_base
			tax_recorder_post(1,tt) = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )/tax_base
        enddo

        ! DC tax is the same
        tax_recorder_pre(2,:)  = 0d0
        tax_recorder_post(2,:) = 0d0

        ! bond tax is the same
        tax_recorder_pre(4,:)  = 0d0
        tax_recorder_post(4,:) = 0d0

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                !
        !   simulate baseline economy    !
        !                                !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! pre-reform
		write(*,*) 'Baseline Simulation'
		do ii=1,Nsim  ! for each simulation

			! initialize market return shocks
			call simulate_normal(shocks,r_f+mu_r,sigma_vtheta_priv)

			! for each time period
			do tt=1,JR

				! if first period
				if (tt == 1) then

					! current funded ratio
					temp_c = updated_status_pre
					recorder_pre(1,ii,tt) = temp_c

                    ! compute tax base
                    tax_base = total_pop*sum( age_probs(tt,1:JR)*( prop_pub*eff_pub(1:JR) &
                                                 + (1d0 - prop_pub)*eff_priv(1:JR) ))

					! current tax
					if (recorder_pre(1,ii,tt) > 1d0) then  ! if fully funded
						recorder_pre(2,ii,tt) = ARC_share*norm_cost_base(tt)/tax_base   ! tax for normal cost

						if (recorder_pre(1,ii,tt) > surplus_bound) then
							temp_assets = surplus_bound*PV_L_hold_simbase(tt)
							recorder_pre(4,ii,tt) = 0d0
							recorder_pre(3,ii,tt) = temp_assets
						else
							temp_assets = temp_c*PV_L_hold_simbase(tt) + ARC_share*norm_cost_base(tt)
							recorder_pre(4,ii,tt) = 0d0
							recorder_pre(3,ii,tt) = temp_assets
						endif

					else

						recorder_pre(2,ii,tt) = ARC_share*(  norm_cost_base(tt) + &
                            (1d0-temp_c)*PV_L_hold_simbase(tt)*arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)  )/tax_base

						temp_assets = temp_c*PV_L_hold_simbase(tt) + ARC_share*(  norm_cost_base(tt) + &
                            (1d0-temp_c)*PV_L_hold_simbase(tt)*arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)  )

                        recorder_pre(4,ii,tt) = (1d0-temp_c)*PV_L_hold_simbase(tt)*&
                        arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)

						recorder_pre(3,ii,tt) = temp_assets

					endif

				else ! time period greater than 1

					! compute beginning-of-period funded ratio
                    temp_c = (((1d0+shocks(tt))*port_share+(1d0+r_f)*(1d0-port_share))*temp_assets - &
                                   Agg_B_hold_simbase(tt) )/PV_L_hold_simbase(tt)

					recorder_pre(1,ii,tt) = temp_c

                    tax_base = total_pop*sum( age_probs(tt,1:JR)*( prop_pub*eff_pub(1:JR) &
                                                 + (1d0 - prop_pub)*eff_priv(1:JR) ))

					! current tax
					if (recorder_pre(1,ii,tt) > 1d0) then

						recorder_pre(2,ii,tt) = ARC_share*norm_cost_base(tt)/tax_base

						if (recorder_pre(1,ii,tt) > surplus_bound) then
							temp_assets = surplus_bound*PV_L_hold_simbase(tt)
							recorder_pre(4,ii,tt) = 0d0
							recorder_pre(3,ii,tt) = temp_assets
						else
							temp_assets = temp_c*PV_L_hold_simbase(tt) + ARC_share*norm_cost_base(tt)
							recorder_pre(4,ii,tt) = 0d0
							recorder_pre(3,ii,tt) = temp_assets
						endif

					else

						recorder_pre(2,ii,tt) = ARC_share*(  norm_cost_base(tt) + &
						(1d0-temp_c)*PV_L_hold_simbase(tt)*arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)  )/tax_base

						temp_assets = temp_c*PV_L_hold_simbase(tt) + ARC_share*(  norm_cost_base(tt) + &
                            (1d0-temp_c)*PV_L_hold_simbase(tt)*arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)  )
                        recorder_pre(4,ii,tt) = (1d0-temp_c)*PV_L_hold_simbase(tt)*&
                        arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)
						recorder_pre(3,ii,tt) = temp_assets

					endif
				endif

			enddo
		enddo

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                    !
        !   simulate economy with theta=1    !
        !                                    !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		write(*,*) 'Full Commitment Simulation'
		do ii=1,Nsim  ! for each simulation

			! initialize market return shocks
			call simulate_normal(shocks,r_f+mu_r,sigma_vtheta_priv)

			! for each time period
			do tt=1,JR

				! if first period
				if (tt == 1) then

					! current funded ratio
					temp_c = updated_status_pre
					recorder_post(1,ii,tt) = temp_c

                    ! compute tax base
                    tax_base = total_pop*sum( age_probs(tt,1:JR)*( prop_pub*eff_pub(1:JR) &
                                                 + (1d0 - prop_pub)*eff_priv(1:JR) ))

					! current tax
					if (recorder_post(1,ii,tt) > 1d0) then  ! if fully funded
						recorder_post(2,ii,tt) = 1d0*norm_cost_base(tt)/tax_base   ! tax for normal cost

						if (recorder_post(1,ii,tt) > surplus_bound) then
							temp_assets = surplus_bound*PV_L_hold_simbase(tt)
							recorder_post(4,ii,tt) = 0d0
							recorder_post(3,ii,tt) = temp_assets
						else
							temp_assets = temp_c*PV_L_hold_simref(tt) + 1d0*norm_cost_base(tt)
							recorder_post(4,ii,tt) = 0d0
							recorder_post(3,ii,tt) = temp_assets
						endif

					else

						recorder_post(2,ii,tt) = 1d0*(  norm_cost_base(tt) + &
                            (1d0-temp_c)*PV_L_hold_simref(tt)*arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)  )/tax_base

						temp_assets = temp_c*PV_L_hold_simref(tt) + 1d0*(  norm_cost_base(tt) + &
                            (1d0-temp_c)*PV_L_hold_simref(tt)*arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)  )

                        recorder_post(4,ii,tt) = (1d0-temp_c)*PV_L_hold_simref(tt)*&
                        arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)

						recorder_post(3,ii,tt) = temp_assets

					endif

				else ! time period greater than 1

					! compute beginning-of-period funded ratio
                    temp_c = (((1d0+shocks(tt))*port_share+(1d0+r_f)*(1d0-port_share))*temp_assets - &
                                   Agg_B_hold_simref(tt) )/PV_L_hold_simref(tt)

					recorder_post(1,ii,tt) = temp_c

                    tax_base = total_pop*sum( age_probs(tt,1:JR)*( prop_pub*eff_pub(1:JR) &
                                                 + (1d0 - prop_pub)*eff_priv(1:JR) ))

					! current tax
					if (recorder_post(1,ii,tt) > 1d0) then

						recorder_post(2,ii,tt) = 1d0*norm_cost_base(tt)/tax_base

						if (recorder_post(1,ii,tt) > surplus_bound) then
							temp_assets = surplus_bound*PV_L_hold_simref(tt)
							recorder_post(4,ii,tt) = 0d0
							recorder_post(3,ii,tt) = temp_assets
						else
							temp_assets = temp_c*PV_L_hold_simref(tt) + 1d0*norm_cost_base(tt)
							recorder_post(4,ii,tt) = 0d0
							recorder_post(3,ii,tt) = temp_assets
						endif

					else

						recorder_post(2,ii,tt) = 1d0*(  norm_cost_base(tt) + &
						(1d0-temp_c)*PV_L_hold_simref(tt)*arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)  )/tax_base

						temp_assets = temp_c*PV_L_hold_simref(tt) + 1d0*(  norm_cost_base(tt) + &
                            (1d0-temp_c)*PV_L_hold_simref(tt)*arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)  )
                        recorder_post(4,ii,tt) = (1d0-temp_c)*PV_L_hold_simref(tt)*&
                        arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)
						recorder_post(3,ii,tt) = temp_assets

					endif
				endif

			enddo
		enddo

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                         !
        !   record fiscal aggregates and rates    !
        !                                         !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! total tax (on average)
		do tt=1,JR

            tax_recorder_pre(3,tt)  = tax_recorder_pre(1,tt)  + tax_recorder_pre(2,tt)  + sum(recorder_pre(2,:,tt))/Nsim
            tax_recorder_post(3,tt) = tax_recorder_post(1,tt) + tax_recorder_post(2,tt) + sum(recorder_post(2,:,tt))/Nsim + &
                                      tax_recorder_post(4,tt)

		enddo

		! record moments
		do tt=1,JR

			time_var(tt) = tt

			! pre-reform
			avg_fund_pre(tt)    = sum(recorder_pre(1,:,tt))/Nsim
			avg_pen_tax_pre(tt) = sum(recorder_pre(2,:,tt))/Nsim
			avg_ass_pre(tt)     = sum(recorder_pre(3,:,tt))/Nsim

            fund_pre_vol(tt)    = ( sum( ( recorder_pre(1,:,tt) - avg_fund_pre(tt) )**2d0 )/Nsim )**(0.5d0)
			vol_pen_tax_pre(tt) = (sum( (recorder_pre(2,:,tt) - avg_pen_tax_pre(tt) )**2d0)/Nsim)**(0.5d0)

			! post-reform
			avg_fund_post(tt)    = sum(recorder_post(1,:,tt))/Nsim
			avg_pen_tax_post(tt) = sum(recorder_post(2,:,tt))/Nsim
			avg_ass_post(tt)     = sum(recorder_post(3,:,tt))/Nsim

            fund_post_vol(tt)    = ( sum( ( recorder_post(1,:,tt) - avg_fund_post(tt) )**2d0 )/Nsim )**(0.5d0)
			vol_pen_tax_post(tt) = (sum( (recorder_post(2,:,tt) - avg_pen_tax_post(tt) )**2d0)/Nsim)**(0.5d0)

			avg_ufl_pre(tt)      = sum( recorder_pre(4,:,tt) )/Nsim
			avg_ufl_post(tt)     = sum( recorder_post(4,:,tt) )/Nsim

		enddo

        do tt = 1,JR

            ! baseline aggregates
            write(38,*) avg_ass_pre(tt)
            write(40,*) tax_recorder_pre(3,tt)
            write(41,*) tax_recorder_pre(1,tt)
            write(42,*) avg_pen_tax_pre(tt)
            write(47,*) vol_pen_tax_pre(tt)
            write(50,*) avg_fund_pre(tt)
            write(51,*) fund_pre_vol(tt)
            write(34,*) PV_L_temp_simbase(tt)
            write(35,*) PV_L_hold_simbase(tt)
            write(54,*) norm_cost_base(tt)
            write(56,*) avg_ufl_pre(tt)

            ! full commitment aggregates
!            write(200,*) avg_ass_post(tt)
!            write(201,*) tax_recorder_post(3,tt)
!            write(202,*) tax_recorder_post(1,tt)
!            write(203,*) avg_pen_tax_post(tt)
!            write(204,*) vol_pen_tax_post(tt)
!            write(205,*) avg_fund_post(tt)
!            write(206,*) fund_post_vol(tt)
!            write(207,*) PV_L_temp_simref(tt)
!            write(208,*) PV_L_hold_simref(tt)
!            write(209,*) norm_cost_base(tt)
!            write(210,*) avg_ufl_post(tt)


        enddo

    end subroutine

    subroutine output_simulator()

        implicit none

        integer :: ii, tt, kk, iterator_priv, iterator_pub
        real*8, dimension(JJ) :: avg_wealth_pub, avg_con_pub, avg_sav_pub, avg_port_pub, &
                        avg_save_rate_priv,avg_save_rate_priv1,avg_save_rate_pub,avg_save_rate_pub1, avg_inc_priv

        real*8, dimension(JJ) :: avg_wealth_priv, avg_con_priv, avg_sav_priv, avg_port_priv
        real*8, dimension(JJ) :: age_grid
        real*8, dimension(JR) :: time_grid

        real*8, dimension(JR) :: Agg_B_hold_simbase, PV_L_hold_simbase, PV_L_temp_simbase, Agg_B_hold_simref, &
                                 PV_L_hold_simref, PV_L_temp_simref
        real*8 :: iterator

        do tt = 1,JR
            write(25,'(f12.5)') cohort_dist_pub(tt,:)
            write(26,'(f12.5)') cohort_dist_priv(tt,:)
        enddo

        ! as function of age, compute average (i) wealth, (ii) consumption, (iii) savings, (iv) portfolio

        ! initialize
        avg_wealth_pub = 0d0
        avg_con_pub = 0d0
        avg_sav_pub = 0d0
        avg_port_pub = 0d0
        avg_wealth_priv = 0d0
        avg_con_priv = 0d0
        avg_sav_priv = 0d0
        avg_port_priv = 0d0

        avg_inc_priv = 0d0

        do ii=1,JJ  ! for each age

            iterator_pub  = 0
            iterator_priv = 0

            do tt=1,nsim  ! for each simulant

                ! if public simulant still alive
                if (recorder_pub(tt,ii,5) == 0) then

                    ! record values
                    avg_wealth_pub(ii) = avg_wealth_pub(ii) + recorder_pub(tt,ii,1)
                    avg_con_pub(ii)    = avg_con_pub(ii)    + recorder_pub(tt,ii,2)
                    avg_sav_pub(ii)    = avg_sav_pub(ii)    + recorder_pub(tt,ii,3)
                    avg_port_pub(ii)   = avg_port_pub(ii)   + recorder_pub(tt,ii,4)

                    avg_save_rate_pub(ii) = avg_save_rate_pub(ii) + (recorder_pub(tt,ii,8)-recorder_pub(tt,ii,2))/&
                    recorder_pub(tt,ii,8)
                    avg_save_rate_pub1(ii) = avg_save_rate_pub1(ii) + (recorder_pub(tt,ii,9)-recorder_pub(tt,ii,2))/&
                    recorder_pub(tt,ii,9)

                    ! record iteration number
                    iterator_pub = iterator_pub + 1
                endif

                ! if private simulant still alive
                if (recorder_priv(tt,ii,5) == 0) then

                    ! record values
                    avg_wealth_priv(ii) = avg_wealth_priv(ii) + recorder_priv(tt,ii,1)
                    avg_con_priv(ii)    = avg_con_priv(ii)    + recorder_priv(tt,ii,2)
                    avg_sav_priv(ii)    = avg_sav_priv(ii)    + recorder_priv(tt,ii,3)
                    avg_port_priv(ii)   = avg_port_priv(ii)   + recorder_priv(tt,ii,4)

                    avg_save_rate_priv(ii) = avg_save_rate_priv(ii) + (recorder_priv(tt,ii,8)-recorder_priv(tt,ii,2))/&
                    recorder_priv(tt,ii,8)

                    avg_inc_priv(ii) = avg_inc_priv(ii) + recorder_priv(tt,ii,8)

                    avg_save_rate_priv1(ii) = avg_save_rate_priv1(ii) + (recorder_priv(tt,ii,9)-recorder_priv(tt,ii,2))/&
                    recorder_priv(tt,ii,9)

                    ! record iteration number
                    iterator_priv = iterator_priv + 1
                endif

            enddo

            ! for age group, compute mean
            avg_wealth_pub(ii) = avg_wealth_pub(ii)/iterator_pub
            avg_con_pub(ii)    = avg_con_pub(ii)/iterator_pub
            avg_sav_pub(ii)    = avg_sav_pub(ii)/iterator_pub
            avg_port_pub(ii)   = avg_port_pub(ii)/iterator_pub

            avg_save_rate_pub(ii) = avg_save_rate_pub(ii)/iterator_pub
            avg_save_rate_pub1(ii) = avg_save_rate_pub1(ii)/iterator_pub

            avg_wealth_priv(ii) = avg_wealth_priv(ii)/iterator_priv
            avg_con_priv(ii)    = avg_con_priv(ii)/iterator_priv
            avg_sav_priv(ii)    = avg_sav_priv(ii)/iterator_priv
            avg_port_priv(ii)   = avg_port_priv(ii)/iterator_priv

            avg_save_rate_priv(ii) = avg_save_rate_priv(ii)/iterator_priv
            avg_save_rate_priv1(ii) = avg_save_rate_priv1(ii)/iterator_priv

            avg_inc_priv(ii) = avg_inc_priv(ii)/iterator_priv

        enddo

        do tt = 1,JJ

            write(27,'(f12.5)') avg_port_priv(tt)
            write(28,'(f12.5)') avg_port_pub(tt)

            write(30,'(f12.5)') avg_save_rate_priv(tt)
            write(31,'(f12.5)') avg_save_rate_priv1(tt)

            write(32,'(f12.5)') avg_save_rate_pub(tt)
            write(33,'(f12.5)') avg_save_rate_pub1(tt)

        enddo

    end subroutine


end program
