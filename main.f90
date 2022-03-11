
include "globals_module.f90"

program PortfolioChoice

    use globals

    implicit none

    integer                     :: ii,tt,kk, jjj

    integer, dimension(51)       :: state_list

    real*8, dimension(51,11)     :: state_info_holder
    real*8, dimension(11,51)     :: state_info_holder1
    real*8, dimension(51*JR,JJ) :: state_dem_holder
    real*8, dimension(JJ,51*JR) :: state_dem_holder1
    real*8 :: curr_pvl, pvl_per_capita, temp_ben_temp, temp_year
    integer :: iterator
    real*8, dimension(JR) :: wage_share

    ! start the clock
    call tic()

    ! create array of states  !
    state_list = (/  1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,&
                     21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,&
                     38,39,40,41,42,43,44,45,46,47,48,49,50,51  /)

    open(49,file="state_list.csv")
    write(49,*) state_list
    close(49)

    ! read in state panel data
    open(12,file='StateDataValues.csv')
    read(12,*) state_info_holder1
    close(12)
    state_info_holder = transpose(state_info_holder1)

    ! read in state demographic data
    open(13,file='full_panel_distribution.csv')
    read(13,*) state_dem_holder1
    close(13)
    state_dem_holder = transpose(state_dem_holder1)

    ! open files
    open(21,file="ce_pub_base.csv")     ! consumption-equivalent welfare
    open(22,file="ce_priv_base.csv")

    open(25,file="cohort_pub.csv")      ! recording cross-section of agents
    open(26,file="cohort_priv.csv")

    open(333,file="benefit_share.csv")

    open(27,file="priv_risk.csv")
    open(28,file="pub_risk.csv")
    open(30,file="priv_save_rate.csv")
    open(31,file="priv_save_rate1.csv")
    open(32,file="pub_save_rate.csv")
    open(33,file="pub_save_rate1.csv")

    open(34,file="pvl.csv")       ! aggregates under the baseline
    open(38,file="avg_ass.csv")
    open(40,file="total_tax.csv")
    open(41,file="wage_tax.csv")
    open(42,file="pension_tax.csv")
    open(47,file="tax_vol.csv")
    open(50,file="fr.csv")
    open(51,file="fr_vol.csv")
    open(54,file="normcost.csv")
    open(56,file="ufl.csv")

    open(502,file="avg_ass_closed.csv")
    open(503,file="total_tax_closed.csv")
    open(504,file="wage_tax_closed.csv")
    open(505,file="pension_tax_closed.csv")
    open(506,file="tax_vol_closed.csv")
    open(507,file="fr_closed.csv")
    open(508,file="fr_vol_closed.csv")
    open(509,file="pvl_closed.csv")

    open(500,file="ce_pub_close.csv")
    open(501,file="ce_priv_close.csv")

    ! for each state in array
    do ii=1,size(state_list)

        ! import state information
        ARC_share      = state_info_holder(state_list(ii),2)   !.92d0!
        current_status = state_info_holder(state_list(ii),4)   !.72d0!
        b              = state_info_holder(state_list(ii),5)    !34d0!
        prop_pub       = state_info_holder(state_list(ii),7)   !.1d0!
        port_share     = state_info_holder(state_list(ii),8)    !.74d0!
        state_discount = state_info_holder(state_list(ii),3) !.072d0!
        net_cola_loss  = 0d0
        cost_living    = state_info_holder(state_list(ii),10)
        ss_coverage    = state_info_holder(state_list(ii),11)

        curr_pvl  = state_info_holder(state_list(ii),9)   !108000000d0!

        ! set age demographic information
		age_probs = state_dem_holder( (1+ JR*(state_list(ii)-1) ):(JR*state_list(ii)),: )

		write(*,*)
		write(*,*) 'STATE:', state_list(ii)
		write(*,*) 'Theta:', ARC_share
		write(*,*) 'PVL Discount factor:', state_discount
		write(*,*) 'Funded ratio:', current_status
		write(*,*) 'pension:', b
		write(*,*) 'size of public sector:', prop_pub
		write(*,*) 'portfolio share:', port_share
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

        !----------------------------------!
        !                                  !
        !   Comparative Static Exercise    !
        !                                  !
        !----------------------------------!

        !call comparative_static()

        !---------------------------!
        !                           !
        !   Solve baseline model    !
        !                           !
        !---------------------------!

        ! initialize baseline
        call initialize_baseline()

!        write(*,*) 'baseline model'
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
!        tt_com = 15
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 20
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 25
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 30
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 31
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 32
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 33
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 34
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 35
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 36
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 38
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 40
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 41
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 45
!        call solve_household_pub()
!        call solve_household_priv()
!
!        ! compute ce-welfare by age cohort
!        call CE_compute_pub(updated_status_pre)
!        call CE_compute_priv(updated_status_pre)
!
!        ! output the welfare measurements
!        call output_baseline()

        !------------------------------!
        !                              !
        !   Solve closed plan model    !
        !                              !
        !------------------------------!

!        ! initialize baseline
!        call initialize_closed_reform()
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
!        tt_com = 15
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 20
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 25
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
!        tt_com = 45
!        call solve_household_pub()
!        call solve_household_priv()
!
!        ! compute ce-welfare by age cohort
!        call CE_compute_pub(updated_status_post)
!        call CE_compute_priv(updated_status_post)
!
!        ! output the welfare measurements
!        call output_reform()

        !------------------------------!
        !                              !
        !   Solve hybrid plan model    !
        !                              !
        !------------------------------!

!        ! initialize baseline
!        call initialize_hybrid_reform()
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
!        tt_com = 15
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 20
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 25
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 30
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 31
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 32
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 33
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 34
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 36
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 38
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 40
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 41
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 45
!        call solve_household_pub()
!        call solve_household_priv()
!
!        ! compute ce-welfare by age cohort
!        call CE_compute_pub(updated_status_post)
!        call CE_compute_priv(updated_status_post)
!
!        ! output the welfare measurements
!        call output_reform()

        !------------------------------!
        !                              !
        !   Solve COLA Freeze Model    !
        !                              !
        !------------------------------!

!        net_cola_loss = 0.015d0
!
!        ! initialize baseline
!        call initialize_cola_reform()
!
!        write(*,*) 'baseline model'
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
!        tt_com = 15
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 20
!        call solve_household_pub()
!        call solve_household_priv()
!        tt_com = 25
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
!        tt_com = 45
!        call solve_household_pub()
!        call solve_household_priv()
!
!        ! compute ce-welfare by age cohort
!        call CE_compute_pub(updated_status_post)
!        call CE_compute_priv(updated_status_post)
!
!        ! output the welfare measurements
!        call output_baseline()

        !---------------------------------------------------------!
        !                                                         !
        !   Simulate baseline model with no demographic change    !
        !                                                         !
        !---------------------------------------------------------!

!        ! re-initialize
!        call initialize_baseline()
!
!        ! set to initial age cohort distribution
!        tt_com = 1
!
!        ! solve household problems
!        call solve_household_pub()
!        call solve_household_priv()
!
!        ! simulate baseline model: obtain cash-in-hand distributions for each age cohort
!        call simulate_pub()
!        call Phi_construct_pub()
!
!        call simulate_priv()
!        call Phi_construct_priv()

        ! simulate fiscal aggregates
        !call simulator()
        !call simulator_closed()
        !call simulator_hybrid()
        !call simulator_cola()

        ! simulated output for worker problems
        !call output_simulator()

    enddo

    close(21)
    close(22)

    close(25)      ! recording cross-section of agents
    close(26)

    close(27)
    close(28)
    close(30)
    close(31)
    close(32)
    close(33)

    close(34)       ! aggregates under the baseline
    close(38)
    close(40)
    close(41)
    close(42)
    close(47)
    close(50)
    close(51)
    close(54)
    close(56)

    close(502)
    close(503)
    close(504)
    close(505)
    close(506)
    close(507)
    close(508)
    close(509)

    close(500)
    close(501)

    close(502)
    close(503)
    close(504)
    close(505)
    close(506)
    close(507)
    close(508)
    close(509)

    ! stop the clock
    call toc()

contains

    subroutine comparative_static()

        implicit none


        real*8 :: base_param
        real*8, dimension(4) :: theta_grid, sector_grid, benefit_grid, cola_grid, discount_grid, fr_grid, port_grid

        real*8, dimension(JR) :: cs_assets, cs_tax, cs_wage_tax, cs_pension_tax,&
                                                 cs_pension_tax_vol, cs_fr, cs_fr_vol, cs_pvl

        ! initialize model
        call initialize_baseline()

        ! populate grids for relevant parameters
        call grid_Cons_Equi(theta_grid,.5d0,1.1d0)       ! ARC commitment
        call grid_Cons_Equi(sector_grid,.01d0,0.35d0)     ! public sector size
        call grid_Cons_Equi(benefit_grid,5d0,40d0)       ! size of benefit
        call grid_Cons_Equi(cola_grid,0d0,0.03d0)        ! cola coverage
        call grid_Cons_Equi(discount_grid,r_f,.09d0)     ! state pension discount rate
        call grid_Cons_Equi(fr_grid,.5d0,1.1d0)          ! funded ratio
        call grid_Cons_Equi(port_grid,0d0,1d0)           ! pension portfolio

        open(307,file="theta_grid.csv")
        write(307,*) theta_grid
        close(307)
        open(308,file="sector_grid.csv")
        write(308,*) sector_grid
        close(308)
        open(309,file="benefit_grid.csv")
        write(309,*) benefit_grid
        close(309)
        open(310,file="cola_grid.csv")
        write(310,*) cola_grid
        close(310)
        open(311,file="discount_grid.csv")
        write(311,*) discount_grid
        close(311)
        open(312,file="fr_grid.csv")
        write(312,*) fr_grid
        close(312)
        open(313,file="port_grid.csv")
        write(313,*) port_grid
        close(313)

        ! cs on % of ARC
        base_param = ARC_share   ! record baseline value
        open(300,file="cs_theta.csv")

        do ii=1,size(theta_grid)

            ARC_share = theta_grid(ii)

            call initialize_baseline()
            call cs_simulator(cs_assets,     cs_tax,            cs_wage_tax,&
                              cs_pension_tax,cs_pension_tax_vol,cs_fr,&
                              cs_fr_vol,     cs_pvl)

            write(300,*) cs_assets
            write(300,*) cs_tax
            write(300,*) cs_wage_tax
            write(300,*) cs_pension_tax
            write(300,*) cs_pension_tax_vol
            write(300,*) cs_fr
            write(300,*) cs_fr_vol
            write(300,*) cs_pvl
        enddo

        ARC_share = base_param  ! return to baseline value
        close(300)

        ! cs on size of public sector
        base_param = prop_pub   ! record baseline value
        open(301,file="cs_prob.csv")

        do ii=1,size(sector_grid)

            prop_pub = sector_grid(ii)

            call initialize_baseline()
            call cs_simulator(cs_assets,     cs_tax,            cs_wage_tax,&
                              cs_pension_tax,cs_pension_tax_vol,cs_fr,&
                              cs_fr_vol,     cs_pvl)

            write(301,*) cs_assets
            write(301,*) cs_tax
            write(301,*) cs_wage_tax
            write(301,*) cs_pension_tax
            write(301,*) cs_pension_tax_vol
            write(301,*) cs_fr
            write(301,*) cs_fr_vol
            write(301,*) cs_pvl
        enddo

        prop_pub = base_param  ! return to baseline value
        close(301)

        ! cs on size of pension benefit
        base_param = b   ! record baseline value
        open(302,file="cs_benefit.csv")

        do ii=1,size(benefit_grid)

            b = benefit_grid(ii)

            call initialize_baseline()
            call cs_simulator(cs_assets,     cs_tax,            cs_wage_tax,&
                              cs_pension_tax,cs_pension_tax_vol,cs_fr,&
                              cs_fr_vol,     cs_pvl)

            write(302,*) cs_assets
            write(302,*) cs_tax
            write(302,*) cs_wage_tax
            write(302,*) cs_pension_tax
            write(302,*) cs_pension_tax_vol
            write(302,*) cs_fr
            write(302,*) cs_fr_vol
            write(302,*) cs_pvl
        enddo

        b = base_param  ! return to baseline value
        close(302)

        ! cs on cola
        base_param = net_cola_loss   ! record baseline value
        open(303,file="cs_cola.csv")

        do ii=1,size(cola_grid)

            net_cola_loss = cola_grid(ii)

            call initialize_baseline()
            call cs_simulator(cs_assets,     cs_tax,            cs_wage_tax,&
                              cs_pension_tax,cs_pension_tax_vol,cs_fr,&
                              cs_fr_vol,     cs_pvl)

            write(303,*) cs_assets
            write(303,*) cs_tax
            write(303,*) cs_wage_tax
            write(303,*) cs_pension_tax
            write(303,*) cs_pension_tax_vol
            write(303,*) cs_fr
            write(303,*) cs_fr_vol
            write(303,*) cs_pvl
        enddo

        net_cola_loss = base_param  ! return to baseline value
        close(303)

        ! cs on discount rate
        base_param = state_discount   ! record baseline value
        open(304,file="cs_discount.csv")

        do ii=1,size(discount_grid)

            state_discount = discount_grid(ii)

            call initialize_baseline()
            call cs_simulator(cs_assets,     cs_tax,            cs_wage_tax,&
                              cs_pension_tax,cs_pension_tax_vol,cs_fr,&
                              cs_fr_vol,     cs_pvl)

            write(304,*) cs_assets
            write(304,*) cs_tax
            write(304,*) cs_wage_tax
            write(304,*) cs_pension_tax
            write(304,*) cs_pension_tax_vol
            write(304,*) cs_fr
            write(304,*) cs_fr_vol
            write(304,*) cs_pvl
        enddo

        state_discount = base_param  ! return to baseline value
        close(304)

        ! cs on initial funded ratio
        base_param = current_status   ! record baseline value
        open(305,file="cs_fr.csv")

        do ii=1,size(fr_grid)

            current_status = fr_grid(ii)

            call initialize_baseline()
            call cs_simulator(cs_assets,     cs_tax,            cs_wage_tax,&
                              cs_pension_tax,cs_pension_tax_vol,cs_fr,&
                              cs_fr_vol,     cs_pvl)

            write(305,*) cs_assets
            write(305,*) cs_tax
            write(305,*) cs_wage_tax
            write(305,*) cs_pension_tax
            write(305,*) cs_pension_tax_vol
            write(305,*) cs_fr
            write(305,*) cs_fr_vol
            write(305,*) cs_pvl
        enddo

        current_status = base_param  ! return to baseline value
        close(305)

        ! cs on portfolio share
        base_param = port_share   ! record baseline value
        open(306,file="cs_port.csv")

        do ii=1,size(port_grid)

            port_share = port_grid(ii)

            call initialize_baseline()
            call cs_simulator(cs_assets,     cs_tax,            cs_wage_tax,&
                              cs_pension_tax,cs_pension_tax_vol,cs_fr,&
                              cs_fr_vol,     cs_pvl)

            write(306,*) cs_assets
            write(306,*) cs_tax
            write(306,*) cs_wage_tax
            write(306,*) cs_pension_tax
            write(306,*) cs_pension_tax_vol
            write(306,*) cs_fr
            write(306,*) cs_fr_vol
            write(306,*) cs_pvl
        enddo

        port_share = base_param  ! return to baseline value
        close(306)


    end subroutine

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
        pen_pub  = 0d0

        do tt=1,JR  ! for each working age cohort, receive potential reduction in pension benefit
            do jjj=JR,JJ
                pen_pub(tt,jjj) = (1d0-net_cola_loss)**(jjj-JR)*b + ss_coverage*ss_benefit_pub  ! .08% miss COLA
            enddo
        enddo

        !open(211,file="pension_value.csv",position='append')  ! pension value
        write(211,*) pen_pub(1,:)
        flush(211)
!        close(211)

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
        do tt = 1,JR  ! for each future time period

            Agg_B_hold(tt) = 0d0

            do kk = JR,JJ ! for each retired age group
                ! compute real value of pension distributions
                Agg_B_hold(tt) = Agg_B_hold(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*b*prop_pub*age_probs(tt,kk)
            enddo
        enddo

        write(333,*) 100d0*sum(Agg_B_hold(:10))/sum(Agg_B_hold)

		arc_rate = state_discount  ! rate at which the unfunded liability is amortized

        ! compute pension liabilities, per period
        do tt = 1,JR  ! for each year tt in the future

            temp_ben_hold = 0d0

            do jjj = 1,80  ! for each year tt+jjj in the future  (everything more than 80 years is not accrued)

                temp_year = 0d0

                do kk= JR, JJ  ! for each retiree age in year t+j (if age is kk in year tt+jjj,
                               !                                    then current age is kk-jjj in year tt)

                    if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                        if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(JR,kk)
                        else
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(tt+jjj,kk)
                        endif

                    else

                        if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                            if ((tt+jjj) > JR) then
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                        age_probs(JR,kk)*wage_share(kk-jjj)
                            else
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                    age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                            endif
                        endif

                    endif

                enddo

                ! add up and discount total cash flows for year t+j
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj

			enddo

			! pricing liabilities with state discount rate
            PV_L_hold(tt) = temp_ben_hold

        enddo


        ! compute normal cost each period: Norm_Cost(tt)
        do tt=1,JR  ! for each time period

            temp_period = 0d0
            do kk = 1,JR ! for each working cohort

                ! compute newly accrued benefit
                btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )

                temp_work = 0d0
                do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                    temp_work = temp_work + (1d0-net_cola_loss)**(jjj-1d0)*btilde*&
                                product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
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

		updated_status_pre = current_status    ! funded ratio, adjusting for anything else

        tax_mat = tax_populate()									 ! tax matrix
        chi_mat = chi_populate() 									 ! funded ratio matrix

    end subroutine


    subroutine initialize_cola_reform()

        implicit none
        real*8 :: temp(NSR, 2), iterator
        real*8, dimension(JJ,JR) :: joint
        integer :: tt, jjj, kk
        real*8, dimension(JR) :: wage_share, PV_L_old
        real*8 :: temp_ben_hold, temp_ben_temp, temp_year, temp_period, temp_work, btilde
        real*8, dimension(JR) :: ages


		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! 														  !
		!    initialize earnings process and retirement income    !
		!														  !
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! public pension includes DB and is inflation-adjusted
        pen_pub  = 0d0

        do tt=1,JR  ! for each working age cohort, receive potential reduction in pension benefit
            do jjj=JR,JJ
                pen_pub(tt,jjj) = (1d0-net_cola_loss)**(jjj-JR)*b + ss_coverage*ss_benefit_pub  ! .08% miss COLA
            enddo
        enddo

        !open(211,file="pension_value.csv",position='append')  ! pension value
        write(211,*) pen_pub(1,:)
        flush(211)
!        close(211)

        ! DC contribution rates for public workers
        dc_con = 0d0

		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		!														   !
		!    compute aggregate benefits and pension liabilities    !
		! 														   !
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! aggregate benefit distributions
        do tt = 1,JR  ! for each future time period

            Agg_B_hold(tt) = 0d0

            do kk = JR,JJ ! for each retired age group
                ! compute real value of pension distributions
                Agg_B_hold(tt) = Agg_B_hold(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*b*prop_pub*age_probs(tt,kk)
            enddo
        enddo

		arc_rate = state_discount  ! rate at which the unfunded liability is amortized

        ! compute pension liabilities, per period
        do tt = 1,JR  ! for each year tt in the future

            temp_ben_hold = 0d0

            do jjj = 1,80  ! for each year tt+jjj in the future  (everything more than 80 years is not accrued)

                temp_year = 0d0

                do kk= JR, JJ  ! for each retiree age in year t+j (if age is kk in year tt+jjj,
                               !                                    then current age is kk-jjj in year tt)

                    if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                        if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(JR,kk)
                        else
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(tt+jjj,kk)
                        endif

                    else

                        if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                            if ((tt+jjj) > JR) then
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                        age_probs(JR,kk)*wage_share(kk-jjj)
                            else
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                    age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                            endif
                        endif

                    endif

                enddo

                ! add up and discount total cash flows for year t+j
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj

			enddo

			! pricing liabilities with state discount rate
            PV_L_hold(tt) = temp_ben_hold

        enddo

        ! compute old pension liabilities to update funded ratio
        do tt = 1,JR  ! for each year tt in the future

            temp_ben_hold = 0d0

            do jjj = 1,80  ! for each year tt+jjj in the future  (everything more than 80 years is not accrued)

                temp_year = 0d0

                do kk= JR, JJ  ! for each retiree age in year t+j (if age is kk in year tt+jjj,
                               !                                    then current age is kk-jjj in year tt)

                    if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                        if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                            temp_year = temp_year + total_pop*prop_pub*b*age_probs(JR,kk)
                        else
                            temp_year = temp_year + total_pop*prop_pub*b*age_probs(tt+jjj,kk)
                        endif

                    else

                        if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                            if ((tt+jjj) > JR) then
                                temp_year = temp_year + total_pop*prop_pub*b*&
                                                                    age_probs(JR,kk)*wage_share(kk-jjj)
                            else
                                temp_year = temp_year + total_pop*prop_pub*b*&
                                                                    age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                            endif
                        endif

                    endif

                enddo

                ! add up and discount total cash flows for year t+j
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj

			enddo

			! pricing liabilities with state discount rate
            PV_L_old(tt) = temp_ben_hold

        enddo


        ! compute normal cost each period: Norm_Cost(tt)
        do tt=1,JR  ! for each time period

            temp_period = 0d0
            do kk = 1,JR ! for each working cohort

                ! compute newly accrued benefit
                btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )

                temp_work = 0d0
                do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                    temp_work = temp_work + (1d0-net_cola_loss)**(jjj-1d0)*btilde*&
                                product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
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

		updated_status_post = current_status*PV_L_old(1)/PV_L_hold(1)    ! funded ratio, adjusting for anything else

        tax_mat = tax_populate()									 ! tax matrix
        chi_mat = chi_populate() 									 ! funded ratio matrix

    end subroutine



    subroutine initialize_closed_reform()

        implicit none
        real*8 :: temp(NSR, 2), iterator
        real*8, dimension(JJ,JR) :: joint
        integer :: tt, jjj, kk
        real*8, dimension(JR) :: wage_share
        real*8 :: temp_ben_hold, temp_ben_temp, temp_year, temp_period, temp_work, btilde
        real*8, dimension(JR) :: ages

        ! plan closure shows up in the computation of liabilities, normal costs and tax associated with wages (to pay for DC)

        do tt=1,JR-1
            wage_share(tt) = sum(eff_pub(1:tt))/sum(eff_pub(1:(JR-1)))
        enddo

		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		!														   !
		!    compute pension liabilities    !
		! 														   !
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! compute pension liabilities, per period
        do tt = 1,JR  ! for each year tt in the future

            temp_ben_hold = 0d0

            do jjj = 1,80  ! for each year tt+jjj in the future  (everything more than 80 years is not accrued)

                temp_year = 0d0

                do kk= JR, JJ  ! for each retiree age in year tt+jjj (if age is kk in year tt+jjj,
                               !                                    then current age is kk-jjj in year tt)

                    ! Under plan closure, benefits only accrue to those working or retired today (i.e. kk >= tt + jjj)
                    if (kk >= (tt+jjj) ) then

                        if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                            if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(JR,kk)
                            else
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(tt+jjj,kk)
                            endif

                        else

                            if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                                if ((tt+jjj) > JR) then
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                            age_probs(JR,kk)*wage_share(kk-jjj)
                                else
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                        age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                                endif
                            endif

                        endif

                    endif
                enddo

                ! add up and discount total cash flows for year t+j
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj

			enddo

			! pricing liabilities with state discount rate
            PV_L_hold(tt) = temp_ben_hold

        enddo

        ! compute normal cost each period: Norm_Cost(tt)
        do tt=1,JR  ! for each time period

            temp_period = 0d0
            do kk = 1,JR ! for each working cohort

                ! normal cost only accrues if agent working at time zero (i.e. kk >= tt)
                if ( kk>= tt ) then
                    ! compute newly accrued benefit
                    btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )

                    temp_work = 0d0
                    do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                        temp_work = temp_work + (1d0-net_cola_loss)**(jjj-1d0)*btilde*&
                                    product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
                    enddo

                    temp_period = temp_period + total_pop*prop_pub*age_probs(tt,kk)*temp_work

                endif

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

            implied_tax(tt) = ( sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )  +       &
                                sum( prop_pub*age_probs(tt,1:tt)*eff_pub(1:tt)*closed_dc_rate )  &
                            )/tax_base(tt)


        enddo

        bond_tax = 0d0

		updated_status_post = current_status    ! funded ratio, adjusting for anything else

        tax_mat = tax_populate()									 ! tax matrix
        chi_mat = chi_populate() 									 ! funded ratio matrix

    end subroutine


    subroutine initialize_hybrid_reform()

        implicit none
        real*8 :: temp(NSR, 2), iterator
        real*8, dimension(JJ,JR) :: joint
        integer :: tt, jjj, kk
        real*8, dimension(JR) :: wage_share, PV_L_old
        real*8 :: temp_ben_hold, temp_ben_temp, temp_year, temp_period, temp_work, btilde
        real*8, dimension(JR) :: ages

        ! benefit reductions for current public workers
        ben_red = (/ 0.5d0   , 0.5125d0, 0.525d0 , 0.5375d0, 0.55d0  , 0.5625d0, 0.575d0 , 0.5875d0, &
                     0.6d0   , 0.6125d0, 0.625d0 , 0.6375d0, 0.65d0  , 0.6625d0, 0.675d0 , 0.6875d0, &
                     0.7d0   , 0.7125d0, 0.725d0 , 0.7375d0, 0.75d0  , 0.7625d0, 0.775d0 , 0.7875d0, &
                     0.8d0   , 0.8125d0, 0.825d0 , 0.8375d0, 0.85d0  , 0.8625d0, 0.875d0 , 0.8875d0, &
                     0.9d0   , 0.9125d0, 0.925d0 , 0.9375d0, 0.95d0  , 0.9625d0, 0.975d0 , 0.9875d0, &
                     1d0     , 1d0     , 1d0     , 1d0     , 1d0 /)

        pen_pub  = 0d0

        do tt=1,JR  ! for each working age cohort, receive potential reduction in pension benefit
            do jjj=JR,JJ
                pen_pub(tt,jjj) = ben_red(tt)*(1d0-net_cola_loss)**(jjj-JR)*b + ss_coverage*ss_benefit_pub  ! .08% miss COLA
            enddo
        enddo

        ! contributions to hybrid plan
        dc_con = 0d0
        dc_con(1:40) = hybrid_dc_rate

        ! plan closure shows up in the computation of liabilities, normal costs and tax associated with wages (to pay for DC)

        do tt=1,JR-1
            wage_share(tt) = sum(eff_pub(1:tt))/sum(eff_pub(1:(JR-1)))
        enddo

		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		!								   !
		!    compute pension liabilities   !
		! 								   !
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! aggregate benefit distributions
        do tt = 1,JR  ! for each future time period

            Agg_B_hold(tt) = 0d0

            if (tt<=5) then    ! if all retirees have original benefit

                do kk = JR,JJ ! for each retired age group
                    ! compute real value of pension distributions
                    Agg_B_hold(tt) = Agg_B_hold(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*b*prop_pub*age_probs(tt,kk)
                enddo


            else

                do kk = JR,JJ ! for each retired age group
                    ! if 40 or older at time tt
                    if (kk-tt >= 40d0) then

                        Agg_B_hold(tt) = Agg_B_hold(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*b*prop_pub*age_probs(tt,kk)

                    else
                        Agg_B_hold(tt) = Agg_B_hold(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*&
                                                            ben_red(kk-tt)*b*prop_pub*age_probs(tt,kk)

                    endif

                enddo

            endif

        enddo

        ! compute pension liabilities, per period
        do tt = 1,JR  ! for each year tt in the future

            temp_ben_hold = 0d0

            do jjj = 1,80  ! for each year tt+jjj in the future  (everything more than 80 years is not accrued)

                temp_year = 0d0

                do kk= JR, JJ  ! for each retiree age in year tt+jjj (if age is kk in year tt+jjj,
                               !                                    then age is kk-jjj in year tt
                               !                                    current age is kk-jjj-tt    )

                    ! if age is 40 or greater at time zero (i.e. kk-jjj-tt >= 40) receive full pension
                    if (kk-jjj-tt >= 40d0) then

                        if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                            if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(JR,kk)
                            else
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(tt+jjj,kk)
                            endif
                        else

                            if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                                if ((tt+jjj) > JR) then
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                            age_probs(JR,kk)*wage_share(kk-jjj)
                                else
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                        age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                                endif
                            endif

                        endif

                    else  ! receive the reduced benefit

                        ! if fully reduced benefit (i.e. if age is 0 or less at time zero, kk-jjj-tt <0)
                        if (kk-jjj-tt <0d0) then

                            if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                                if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                                            ben_red(1)*b*age_probs(JR,kk)
                                else
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                                            ben_red(1)*b*age_probs(tt+jjj,kk)
                                endif
                            else

                                if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                                    if ((tt+jjj) > JR) then
                                        temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*ben_red(1)*b*&
                                                                                age_probs(JR,kk)*wage_share(kk-jjj)
                                    else
                                        temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*ben_red(1)*b*&
                                                                            age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                                    endif
                                endif

                            endif

                        ! partially reduced benefit (i.e. age between 0 and 40 at time zero)
                        else

                            if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                                if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                                ben_red(kk-jjj-tt)*b*age_probs(JR,kk)
                                else
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                                ben_red(kk-jjj-tt)*b*age_probs(tt+jjj,kk)
                                endif
                            else

                                if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                                    if ((tt+jjj) > JR) then
                                        temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                            ben_red(kk-jjj-tt)*b*age_probs(JR,kk)*wage_share(kk-jjj)
                                    else
                                        temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                            ben_red(kk-jjj-tt)*b*age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                                    endif
                                endif

                            endif


                        endif

                    endif

                enddo

                ! add up and discount total cash flows for year t+j
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj

			enddo

			! pricing liabilities with state discount rate
            PV_L_hold(tt) = temp_ben_hold

        enddo

        ! compute normal cost each period: Norm_Cost(tt)
        do tt=1,JR  ! for each time period

            temp_period = 0d0
            do kk = 1,JR ! for each working cohort

                ! normal cost only accrues if agent working at time zero (i.e. kk >= tt)
                if ( kk>= tt ) then

                    ! compute newly accrued benefit

                    if (kk-tt>=40d0) then   ! if 40 or older at time 0, full benefit
                        btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                    elseif (kk-tt<0d0) then  ! if not working at time 0
                        btilde = ben_red(1)*b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                    else
                        btilde = ben_red(kk-tt)*b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                    endif

                    temp_work = 0d0
                    do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                        temp_work = temp_work + (1d0-net_cola_loss)**(jjj-1d0)*btilde*&
                                    product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
                    enddo

                    temp_period = temp_period + total_pop*prop_pub*age_probs(tt,kk)*temp_work

                endif

            enddo

            Norm_Cost(tt) = temp_period
        enddo

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !                                                                  !
        !   Calculate Baseline PVL to update the Funded Ratio at Time 0    !
        !                                                                  !
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                ! compute pension liabilities, per period
        do tt = 1,JR  ! for each year tt in the future

            temp_ben_hold = 0d0

            do jjj = 1,80  ! for each year tt+jjj in the future  (everything more than 80 years is not accrued)

                temp_year = 0d0

                do kk= JR, JJ  ! for each retiree age in year t+j (if age is kk in year tt+jjj,
                               !                                    then current age is kk-jjj in year tt)

                    if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                        if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(JR,kk)
                        else
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(tt+jjj,kk)
                        endif

                    else

                        if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                            if ((tt+jjj) > JR) then
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                        age_probs(JR,kk)*wage_share(kk-jjj)
                            else
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                    age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                            endif
                        endif

                    endif

                enddo

                ! add up and discount total cash flows for year t+j
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj

			enddo

			! pricing liabilities with state discount rate
            PV_L_old(tt) = temp_ben_hold

        enddo



		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		!										 !
		!    compute wage tax and pension tax    !
		!										 !
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do tt=1,JR  ! for each time period (NOT AGE COHORT)

			tax_base(tt) = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) + (1d0-prop_pub)*age_probs(tt,1:JR)*eff_priv(1:JR) )

            if (tt <= 6d0) then
                implied_tax(tt) = ( sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )  +       &
                                    sum( prop_pub*age_probs(tt,tt:39+tt)*eff_pub(tt:39+tt)*hybrid_dc_rate ) + &
                                    sum( prop_pub*age_probs(tt,1:tt)*eff_pub(1:tt)*hybrid_dc_rate )  &  ! future generations of workers
                                )/tax_base(tt)

            else

                implied_tax(tt) = ( sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )  +       &
                                    sum( prop_pub*age_probs(tt,tt:45)*eff_pub(tt:45)*hybrid_dc_rate ) + &
                                    sum( prop_pub*age_probs(tt,1:tt)*eff_pub(1:tt)*hybrid_dc_rate )  &  ! future generations of workers
                                )/tax_base(tt)


            endif


        enddo

        bond_tax = 0d0

		updated_status_post = current_status*PV_L_old(1)/PV_L_hold(1)    ! funded ratio, adjusting for anything else

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

        flush(21)
        flush(22)

    end subroutine

    subroutine output_reform()

        implicit none

        integer :: tt

    ! closed reform output

        do tt = 1,JR

            write(500,'(f12.5)') CE_val_pub(tt,:)
            write(501,'(f12.5)') CE_val_priv(tt,:)

        enddo

        flush(500)
        flush(501)

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
                call simulate_normal(pen_return, r_f + port_share*mu_r, port_share**2d0*sigma_vtheta_pub )

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
                call simulate_normal(pen_return, r_f + port_share*mu_r, port_share**2d0*sigma_vtheta_pub )

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

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                                 !
        !   compute pension liabilities under baseline    !
        !                                                 !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

        do tt = 1,JR  ! for each year in the future
            temp_ben_hold = 0d0
            do jjj = 1,80  ! for each year t+j in the future  (everything more than 80 years is not accrued)
                temp_year = 0d0
                do kk= JR, JJ  ! for each retiree age in year t+j

                    if (kk-jjj >= JR) then ! if already retired today, full benefit accrual
                        if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(JR,kk)
                        else
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(tt+jjj,kk)
                        endif
                    else
                        if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                            if ((tt+jjj) > JR) then
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                    age_probs(JR,kk)*wage_share(kk-jjj)
                            else
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                    age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                            endif
                        endif
                    endif
                enddo
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj
			enddo
			! pricing liabilities with state discount factor
            PV_L_hold_simbase(tt) = temp_ben_hold

        enddo

        ! aggregate benefit distributions
        do tt = 1,JR  ! for each future time period
            Agg_B_hold_simbase(tt) = 0d0
            do kk = JR,JJ ! for each retired age group
                Agg_B_hold_simbase(tt) = Agg_B_hold_simbase(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                                prop_pub*age_probs(tt,kk)
            enddo
        enddo

        ! compute normal cost
        do tt=1,JR  ! for each time period
            temp_period = 0d0
            do kk = 1,JR ! for each working cohort
                btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                temp_work = 0d0
                do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                    temp_work = temp_work + (1d0-net_cola_loss)**(jjj-1d0)*btilde*&
                                product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
                enddo
                temp_period = temp_period + total_pop*prop_pub*age_probs(tt,kk)*temp_work
            enddo
            norm_cost_base(tt) = temp_period
        enddo

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                           !
        !   record taxes for paying public wages    !
        !                                           !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! wage tax is the same
        do tt=1,JR  ! for each time period (NOT AGE COHORT)
			tax_base = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) + (1d0-prop_pub)*age_probs(tt,1:JR)*eff_priv(1:JR) )
			tax_recorder_pre(1,tt)  = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )/tax_base
        enddo

        ! DC tax is the same
        tax_recorder_pre(2,:)  = 0d0

        ! bond tax is the same
        tax_recorder_pre(4,:)  = 0d0

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

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                         !
        !   record fiscal aggregates and rates    !
        !                                         !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! total tax (on average)
		do tt=1,JR
            tax_recorder_pre(3,tt)  = tax_recorder_pre(1,tt)  + tax_recorder_pre(2,tt)  + sum(recorder_pre(2,:,tt))/Nsim
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
			avg_ufl_pre(tt)      = sum( recorder_pre(4,:,tt) )/Nsim

		enddo

!        open(34,file="pvl.csv",position='append',status='old')       ! aggregates under the baseline
!        open(38,file="avg_ass.csv",position='append',status='old')
!        open(40,file="total_tax.csv",position='append',status='old')
!        open(41,file="wage_tax.csv",position='append',status='old')
!        open(42,file="pension_tax.csv",position='append',status='old')
!        open(47,file="tax_vol.csv",position='append',status='old')
!        open(50,file="fr.csv",position='append',status='old')
!        open(51,file="fr_vol.csv",position='append',status='old')
!        open(54,file="normcost.csv",position='append',status='old')
!        open(56,file="ufl.csv",position='append',status='old')

        do tt = 1,JR
            ! baseline aggregates
            write(38,*) avg_ass_pre(tt)
            write(40,*) tax_recorder_pre(3,tt)
            write(41,*) tax_recorder_pre(1,tt)
            write(42,*) avg_pen_tax_pre(tt)
            write(47,*) vol_pen_tax_pre(tt)
            write(50,*) avg_fund_pre(tt)
            write(51,*) fund_pre_vol(tt)
            write(34,*) PV_L_hold_simbase(tt)
            write(54,*) norm_cost_base(tt)
            write(56,*) avg_ufl_pre(tt)
        enddo

        flush(34)
        flush(38)
        flush(40)
        flush(41)
        flush(42)
        flush(47)
        flush(50)
        flush(51)
        flush(54)
        flush(56)

    end subroutine

	subroutine simulator_cola()

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

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                                 !
        !   compute pension liabilities under baseline    !
        !                                                 !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

        do tt = 1,JR  ! for each year in the future
            temp_ben_hold = 0d0
            do jjj = 1,80  ! for each year t+j in the future  (everything more than 80 years is not accrued)
                temp_year = 0d0
                do kk= JR, JJ  ! for each retiree age in year t+j

                    if (kk-jjj >= JR) then ! if already retired today, full benefit accrual
                        if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(JR,kk)
                        else
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(tt+jjj,kk)
                        endif
                    else
                        if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                            if ((tt+jjj) > JR) then
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                    age_probs(JR,kk)*wage_share(kk-jjj)
                            else
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                    age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                            endif
                        endif
                    endif
                enddo
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj
			enddo
			! pricing liabilities with state discount factor
            PV_L_hold_simbase(tt) = temp_ben_hold

        enddo

        ! aggregate benefit distributions
        do tt = 1,JR  ! for each future time period
            Agg_B_hold_simbase(tt) = 0d0
            do kk = JR,JJ ! for each retired age group
                Agg_B_hold_simbase(tt) = Agg_B_hold_simbase(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                                prop_pub*age_probs(tt,kk)
            enddo
        enddo

        ! compute normal cost
        do tt=1,JR  ! for each time period
            temp_period = 0d0
            do kk = 1,JR ! for each working cohort
                btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                temp_work = 0d0
                do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                    temp_work = temp_work + (1d0-net_cola_loss)**(jjj-1d0)*btilde*&
                                product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
                enddo
                temp_period = temp_period + total_pop*prop_pub*age_probs(tt,kk)*temp_work
            enddo
            norm_cost_base(tt) = temp_period
        enddo

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                           !
        !   record taxes for paying public wages    !
        !                                           !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! wage tax is the same
        do tt=1,JR  ! for each time period (NOT AGE COHORT)
			tax_base = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) + (1d0-prop_pub)*age_probs(tt,1:JR)*eff_priv(1:JR) )
			tax_recorder_pre(1,tt)  = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )/tax_base
        enddo

        ! DC tax is the same
        tax_recorder_pre(2,:)  = 0d0

        ! bond tax is the same
        tax_recorder_pre(4,:)  = 0d0

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
					temp_c = updated_status_post
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

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                         !
        !   record fiscal aggregates and rates    !
        !                                         !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! total tax (on average)
		do tt=1,JR
            tax_recorder_pre(3,tt)  = tax_recorder_pre(1,tt)  + tax_recorder_pre(2,tt)  + sum(recorder_pre(2,:,tt))/Nsim
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
			avg_ufl_pre(tt)      = sum( recorder_pre(4,:,tt) )/Nsim

		enddo

!        open(34,file="pvl.csv",position='append',status='old')       ! aggregates under the baseline
!        open(38,file="avg_ass.csv",position='append',status='old')
!        open(40,file="total_tax.csv",position='append',status='old')
!        open(41,file="wage_tax.csv",position='append',status='old')
!        open(42,file="pension_tax.csv",position='append',status='old')
!        open(47,file="tax_vol.csv",position='append',status='old')
!        open(50,file="fr.csv",position='append',status='old')
!        open(51,file="fr_vol.csv",position='append',status='old')
!        open(54,file="normcost.csv",position='append',status='old')
!        open(56,file="ufl.csv",position='append',status='old')

        do tt = 1,JR
            ! baseline aggregates
            write(38,*) avg_ass_pre(tt)
            write(40,*) tax_recorder_pre(3,tt)
            write(41,*) tax_recorder_pre(1,tt)
            write(42,*) avg_pen_tax_pre(tt)
            write(47,*) vol_pen_tax_pre(tt)
            write(50,*) avg_fund_pre(tt)
            write(51,*) fund_pre_vol(tt)
            write(34,*) PV_L_hold_simbase(tt)
            write(54,*) norm_cost_base(tt)
            write(56,*) avg_ufl_pre(tt)
        enddo

        flush(34)
        flush(38)
        flush(40)
        flush(41)
        flush(42)
        flush(47)
        flush(50)
        flush(51)
        flush(54)
        flush(56)

    end subroutine


	subroutine simulator_closed()

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

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                                    !
        !   compute pension liabilities under closed plan    !
        !                                                    !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

        do tt = 1,JR  ! for each year in the future
            temp_ben_hold = 0d0
            do jjj = 1,80  ! for each year t+j in the future  (everything more than 80 years is not accrued)
                temp_year = 0d0
                do kk= JR, JJ  ! for each retiree age in year t+j

                    ! Under plan closure, benefits only accrue to those working or retired today (i.e. kk >= tt + jjj)
                    if (kk >= (tt+jjj) ) then

                        if (kk-jjj >= JR) then ! if already retired today, full benefit accrual
                            if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(JR,kk)
                            else
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(tt+jjj,kk)
                            endif
                        else
                            if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                                if ((tt+jjj) > JR) then
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                        age_probs(JR,kk)*wage_share(kk-jjj)
                                else
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                        age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                                endif
                            endif

                        endif
                    endif
                enddo
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj
			enddo
			! pricing liabilities with state discount factor
            PV_L_hold_simbase(tt) = temp_ben_hold

        enddo

        ! aggregate benefit distributions
        do tt = 1,JR  ! for each future time period
            Agg_B_hold_simbase(tt) = 0d0
            do kk = JR,JJ ! for each retired age group
                Agg_B_hold_simbase(tt) = Agg_B_hold_simbase(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                                prop_pub*age_probs(tt,kk)
            enddo
        enddo

        ! compute normal cost
        do tt=1,JR  ! for each time period
            temp_period = 0d0
            do kk = 1,JR ! for each working cohort

                ! normal cost only accrues if agent working at time zero (i.e. kk >= tt)
                if ( kk>= tt ) then

                    btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                    temp_work = 0d0
                    do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                        temp_work = temp_work + (1d0-net_cola_loss)**(jjj-1d0)*btilde*&
                                    product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
                    enddo
                    temp_period = temp_period + total_pop*prop_pub*age_probs(tt,kk)*temp_work

                endif
            enddo
            norm_cost_base(tt) = temp_period
        enddo

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                           !
        !   record taxes for paying public wages    !
        !                                           !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! wage tax is the same
        do tt=1,JR  ! for each time period (NOT AGE COHORT)
			tax_base = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) + (1d0-prop_pub)*age_probs(tt,1:JR)*eff_priv(1:JR) )


            tax_recorder_pre(1,tt) = ( sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )  +&
                                       sum( prop_pub*age_probs(tt,1:tt)*eff_pub(1:tt)*closed_dc_rate ) )/tax_base

        enddo

        ! DC tax is the same
        tax_recorder_pre(2,:)  = 0d0

        ! bond tax is the same
        tax_recorder_pre(4,:)  = 0d0

        !~~~~~~~~~~~~~~~~~~~~~~~!
        !                       !
        !   simulate economy    !
        !                       !
        !~~~~~~~~~~~~~~~~~~~~~~~!

		! pre-reform
		write(*,*) 'Closed Economy Simulation'
		do ii=1,Nsim  ! for each simulation

			! initialize market return shocks
			call simulate_normal(shocks,r_f+mu_r,sigma_vtheta_priv)

			! for each time period
			do tt=1,JR

				! if first period
				if (tt == 1) then

					! current funded ratio
					temp_c = updated_status_post
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

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                         !
        !   record fiscal aggregates and rates    !
        !                                         !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! total tax (on average)
		do tt=1,JR
            tax_recorder_pre(3,tt)  = tax_recorder_pre(1,tt)  + tax_recorder_pre(2,tt)  + sum(recorder_pre(2,:,tt))/Nsim
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
			avg_ufl_pre(tt)      = sum( recorder_pre(4,:,tt) )/Nsim

		enddo

        do tt = 1,JR
            ! baseline aggregates
            write(502,*) avg_ass_pre(tt)
            write(503,*) tax_recorder_pre(3,tt)
            write(504,*) tax_recorder_pre(1,tt)
            write(505,*) avg_pen_tax_pre(tt)
            write(506,*) vol_pen_tax_pre(tt)
            write(507,*) avg_fund_pre(tt)
            write(508,*) fund_pre_vol(tt)
            write(509,*) PV_L_hold_simbase(tt)
        enddo

        flush(502)
        flush(503)
        flush(504)
        flush(505)
        flush(506)
        flush(507)
        flush(508)
        flush(509)

    end subroutine


	subroutine simulator_hybrid()

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

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                                    !
        !   compute pension liabilities under hybrid plan    !
        !                                                    !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        ! aggregate benefit distributions
        do tt = 1,JR  ! for each future time period

            Agg_B_hold_simbase(tt) = 0d0

            if (tt<=5) then    ! if all retirees have original benefit

                do kk = JR,JJ ! for each retired age group
                    ! compute real value of pension distributions
                    Agg_B_hold_simbase(tt) = Agg_B_hold_simbase(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*b*&
                                            prop_pub*age_probs(tt,kk)
                enddo


            else

                do kk = JR,JJ ! for each retired age group
                    ! if 40 or older at time tt
                    if (kk-tt >= 40d0) then

                        Agg_B_hold_simbase(tt) = Agg_B_hold_simbase(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                                prop_pub*age_probs(tt,kk)

                    else
                        Agg_B_hold_simbase(tt) = Agg_B_hold_simbase(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*&
                                                            ben_red(kk-tt)*b*prop_pub*age_probs(tt,kk)

                    endif

                enddo

            endif

        enddo

        ! compute pension liabilities, per period
        do tt = 1,JR  ! for each year tt in the future

            temp_ben_hold = 0d0

            do jjj = 1,80  ! for each year tt+jjj in the future  (everything more than 80 years is not accrued)

                temp_year = 0d0

                do kk= JR, JJ  ! for each retiree age in year tt+jjj (if age is kk in year tt+jjj,
                               !                                    then age is kk-jjj in year tt
                               !                                    current age is kk-jjj-tt    )

                    ! if age is 40 or greater at time zero (i.e. kk-jjj-tt >= 40) receive full pension
                    if (kk-jjj-tt >= 40d0) then

                        if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                            if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(JR,kk)
                            else
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(tt+jjj,kk)
                            endif
                        else

                            if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                                if ((tt+jjj) > JR) then
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                            age_probs(JR,kk)*wage_share(kk-jjj)
                                else
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                        age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                                endif
                            endif

                        endif

                    else  ! receive the reduced benefit

                        ! if fully reduced benefit (i.e. if age is 0 or less at time zero, kk-jjj-tt <0)
                        if (kk-jjj-tt <0d0) then

                            if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                                if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                                            ben_red(1)*b*age_probs(JR,kk)
                                else
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                                            ben_red(1)*b*age_probs(tt+jjj,kk)
                                endif
                            else

                                if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                                    if ((tt+jjj) > JR) then
                                        temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*ben_red(1)*b*&
                                                                                age_probs(JR,kk)*wage_share(kk-jjj)
                                    else
                                        temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*ben_red(1)*b*&
                                                                            age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                                    endif
                                endif

                            endif

                        ! partially reduced benefit (i.e. age between 0 and 40 at time zero)
                        else

                            if (kk-jjj >= JR) then ! if already retired today, full benefit accrual

                                if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                                ben_red(kk-jjj-tt)*b*age_probs(JR,kk)
                                else
                                    temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                                ben_red(kk-jjj-tt)*b*age_probs(tt+jjj,kk)
                                endif
                            else

                                if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                                    if ((tt+jjj) > JR) then
                                        temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                            ben_red(kk-jjj-tt)*b*age_probs(JR,kk)*wage_share(kk-jjj)
                                    else
                                        temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*&
                                                            ben_red(kk-jjj-tt)*b*age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                                    endif
                                endif

                            endif


                        endif

                    endif

                enddo

                ! add up and discount total cash flows for year t+j
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj

			enddo

			! pricing liabilities with state discount rate
            PV_L_hold_simbase(tt) = temp_ben_hold

        enddo

        ! compute normal cost each period: Norm_Cost(tt)
        do tt=1,JR  ! for each time period

            temp_period = 0d0
            do kk = 1,JR ! for each working cohort

                ! normal cost only accrues if agent working at time zero (i.e. kk >= tt)
                if ( kk>= tt ) then

                    ! compute newly accrued benefit

                    if (kk-tt>=40d0) then   ! if 40 or older at time 0, full benefit
                        btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                    elseif (kk-tt<0d0) then  ! if not working at time 0
                        btilde = ben_red(1)*b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                    else
                        btilde = ben_red(kk-tt)*b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                    endif

                    temp_work = 0d0
                    do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                        temp_work = temp_work + (1d0-net_cola_loss)**(jjj-1d0)*btilde*&
                                    product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
                    enddo

                    temp_period = temp_period + total_pop*prop_pub*age_probs(tt,kk)*temp_work

                endif

            enddo

            norm_cost_base(tt) = temp_period
        enddo

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                           !
        !   record taxes for paying public wages    !
        !                                           !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        do tt=1,JR  ! for each time period (NOT AGE COHORT)

			tax_base = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) + (1d0-prop_pub)*age_probs(tt,1:JR)*eff_priv(1:JR) )

            if (tt <= 6d0) then
                tax_recorder_pre(1,tt) = ( sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )  +       &
                                    sum( prop_pub*age_probs(tt,tt:39+tt)*eff_pub(tt:39+tt)*hybrid_dc_rate ) + &
                                    sum( prop_pub*age_probs(tt,1:tt)*eff_pub(1:tt)*hybrid_dc_rate )  &  ! future generations of workers
                                )/tax_base

            else

                tax_recorder_pre(1,tt) = ( sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )  +       &
                                    sum( prop_pub*age_probs(tt,tt:45)*eff_pub(tt:45)*hybrid_dc_rate ) + &
                                    sum( prop_pub*age_probs(tt,1:tt)*eff_pub(1:tt)*hybrid_dc_rate )  &  ! future generations of workers
                                )/tax_base


            endif


        enddo

        ! DC tax is the same
        tax_recorder_pre(2,:)  = 0d0

        ! bond tax is the same
        tax_recorder_pre(4,:)  = 0d0

        !~~~~~~~~~~~~~~~~~~~~~~~!
        !                       !
        !   simulate economy    !
        !                       !
        !~~~~~~~~~~~~~~~~~~~~~~~!

		! pre-reform
		write(*,*) 'Hybrid Economy Simulation'
		do ii=1,Nsim  ! for each simulation

			! initialize market return shocks
			call simulate_normal(shocks,r_f+mu_r,sigma_vtheta_priv)

			! for each time period
			do tt=1,JR

				! if first period
				if (tt == 1) then

					! current funded ratio
					temp_c = updated_status_post
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

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                         !
        !   record fiscal aggregates and rates    !
        !                                         !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! total tax (on average)
		do tt=1,JR
            tax_recorder_pre(3,tt)  = tax_recorder_pre(1,tt)  + tax_recorder_pre(2,tt)  + sum(recorder_pre(2,:,tt))/Nsim
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
			avg_ufl_pre(tt)      = sum( recorder_pre(4,:,tt) )/Nsim

		enddo

        do tt = 1,JR
            ! baseline aggregates
            write(502,*) avg_ass_pre(tt)
            write(503,*) tax_recorder_pre(3,tt)
            write(504,*) tax_recorder_pre(1,tt)
            write(505,*) avg_pen_tax_pre(tt)
            write(506,*) vol_pen_tax_pre(tt)
            write(507,*) avg_fund_pre(tt)
            write(508,*) fund_pre_vol(tt)
            write(509,*) PV_L_hold_simbase(tt)
        enddo

        flush(502)
        flush(503)
        flush(504)
        flush(505)
        flush(506)
        flush(507)
        flush(508)
        flush(509)

    end subroutine


	subroutine cs_simulator(assets,tax,wage_tax,pension_tax,pension_tax_vol,fr,fr_vol,pvl)

		! want to capture (1) average pension tax, (2) volatility pension tax (3) average funded ratio
		!    (a) over time (b) pre-reform and (c) post-reform
		implicit none

        real*8, dimension(JR), intent(out) :: assets, tax, wage_tax, pension_tax, pension_tax_vol, fr, fr_vol, pvl

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

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                                 !
        !   compute pension liabilities under baseline    !
        !                                                 !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

        do tt = 1,JR  ! for each year in the future
            temp_ben_hold = 0d0
            do jjj = 1,80  ! for each year t+j in the future  (everything more than 80 years is not accrued)
                temp_year = 0d0
                do kk= JR, JJ  ! for each retiree age in year t+j

                    if (kk-jjj >= JR) then ! if already retired today, full benefit accrual
                        if ((tt+jjj) > JR) then  ! if outside demographic distribution forecast, use last year (year 45) distribution
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(JR,kk)
                        else
                            temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*age_probs(tt+jjj,kk)
                        endif
                    else
                        if ( (kk-jjj) >= 1 ) then ! if alive/working today, continue
                            if ((tt+jjj) > JR) then
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                    age_probs(JR,kk)*wage_share(kk-jjj)
                            else
                                temp_year = temp_year + total_pop*prop_pub*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                    age_probs(tt+jjj,kk)*wage_share(kk-jjj)
                            endif
                        endif
                    endif
                enddo
                temp_ben_hold = temp_ben_hold + temp_year/(1d0+state_discount)**jjj
			enddo
			! pricing liabilities with state discount factor
            PV_L_hold_simbase(tt) = temp_ben_hold

        enddo

        ! aggregate benefit distributions
        do tt = 1,JR  ! for each future time period
            Agg_B_hold_simbase(tt) = 0d0
            do kk = JR,JJ ! for each retired age group
                Agg_B_hold_simbase(tt) = Agg_B_hold_simbase(tt) + total_pop*(1d0-net_cola_loss)**(kk-JR)*b*&
                                                                                prop_pub*age_probs(tt,kk)
            enddo
        enddo

        ! compute normal cost
        do tt=1,JR  ! for each time period
            temp_period = 0d0
            do kk = 1,JR ! for each working cohort
                btilde = b*eff_pub(kk)/sum(eff_pub(1:(JR-1)) )
                temp_work = 0d0
                do jjj =1,(JJ-JR)  ! for each year of retirement, for respective working cohort
                    temp_work = temp_work + (1d0-net_cola_loss)**(jjj-1d0)*btilde*&
                                product( 1d0-psi(kk:JR+jjj) )/(1d0+alt_r_base)**(JR-kk+jjj)
                enddo
                temp_period = temp_period + total_pop*prop_pub*age_probs(tt,kk)*temp_work
            enddo
            norm_cost_base(tt) = temp_period
        enddo

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                           !
        !   record taxes for paying public wages    !
        !                                           !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! wage tax is the same
        do tt=1,JR  ! for each time period (NOT AGE COHORT)
			tax_base = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) + (1d0-prop_pub)*age_probs(tt,1:JR)*eff_priv(1:JR) )
			tax_recorder_pre(1,tt)  = sum( prop_pub*age_probs(tt,1:JR)*eff_pub(1:JR) )/tax_base
        enddo

        ! DC tax is the same
        tax_recorder_pre(2,:)  = 0d0

        ! bond tax is the same
        tax_recorder_pre(4,:)  = 0d0

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

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        !                                         !
        !   record fiscal aggregates and rates    !
        !                                         !
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

		! total tax (on average)
		do tt=1,JR
            tax_recorder_pre(3,tt)  = tax_recorder_pre(1,tt)  + tax_recorder_pre(2,tt)  + sum(recorder_pre(2,:,tt))/Nsim
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
			avg_ufl_pre(tt)      = sum( recorder_pre(4,:,tt) )/Nsim

		enddo

		! update
        assets          = avg_ass_pre
        tax             = tax_recorder_pre(3,:)
        wage_tax        = tax_recorder_pre(1,:)
        pension_tax     = avg_pen_tax_pre
        pension_tax_vol = vol_pen_tax_pre
        fr              = avg_fund_pre
        fr_vol          = fund_pre_vol
        pvl             = PV_L_hold_simbase


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

!        open(25,file="cohort_pub.csv",position='append',status='old')      ! recording cross-section of agents
!        open(26,file="cohort_priv.csv",position='append',status='old')

        do tt = 1,JR
            write(25,'(f12.5)') cohort_dist_pub(tt,:)
            write(26,'(f12.5)') cohort_dist_priv(tt,:)
        enddo

        flush(25)
        flush(26)

!        close(25)
!        close(26)

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

!        open(27,file="priv_risk.csv",position='append',status='old')
!        open(28,file="pub_risk.csv",position='append',status='old')
!        open(30,file="priv_save_rate.csv",position='append',status='old')
!        open(31,file="priv_save_rate1.csv",position='append',status='old')
!        open(32,file="pub_save_rate.csv",position='append',status='old')
!        open(33,file="pub_save_rate1.csv",position='append',status='old')

        do tt = 1,JJ

            write(27,'(f12.5)') avg_port_priv(tt)
            write(28,'(f12.5)') avg_port_pub(tt)

            write(30,'(f12.5)') avg_save_rate_priv(tt)
            write(31,'(f12.5)') avg_save_rate_priv1(tt)

            write(32,'(f12.5)') avg_save_rate_pub(tt)
            write(33,'(f12.5)') avg_save_rate_pub1(tt)

        enddo

        flush(27)
        flush(28)
        flush(30)
        flush(31)
        flush(32)
        flush(33)

    end subroutine


end program
