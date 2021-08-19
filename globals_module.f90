
include "toolbox.f90"

module globals

    use toolbox

    implicit none

    ! number of years the household retires
    integer, parameter :: JR = 45

    ! number of years the household lives
    integer, parameter :: JJ = 80

    ! number of white noise (zeta) shocks
    integer, parameter :: NW = 7

    ! number of transitory (epsilon) shocks
    integer, parameter :: NS = 7

    ! number of rate of return (vtheta) shocks
    integer, parameter :: NR = 7

    ! number of eps-vtheta shocks
    integer, parameter :: NSR = NS*NR

    ! number of eta shocks
    integer, parameter :: NE = 1000

    ! number of points on the asset grid
    integer, parameter :: NA = 200

    ! number of points on the cash-on-hand-grid
    integer, parameter :: NX = 200

    ! household preference parameters
    real*8, parameter :: gamma = 0.1d0
    real*8, parameter :: egam = 1d0 - 1d0/gamma
    real*8, parameter :: beta = 0.985d0 !0.96d0

    ! risk processes
    real*8, parameter :: sigma_zeta_pub   = 0.0584d0
    real*8, parameter :: sigma_zeta_priv  = 0.0584d0

    real*8, parameter :: sigma_eps_pub    = 0.0152756d0 !0.0169d0
    real*8, parameter :: sigma_eps_priv   = 0.0169d0
!
    real*8, parameter :: sigma_vtheta_priv = 0.157d0**2d0
    real*8, parameter :: sigma_vtheta_pub  = 0.157d0**2d0

    real*8, parameter :: rho_pub  = 0.00d0
    real*8, parameter :: rho_priv = 0.00d0

    ! risk free rate and risk premium
    real*8, parameter :: r_f  = 0.02d0
    real*8, parameter :: mu_r = 0.04d0 !.0676d0

    ! size of the asset grid
    real*8, parameter :: a_l    = 0.0d0
    real*8, parameter :: a_u    = 500d0
    real*8, parameter :: a_grow = 0.04d0

    ! growth of the cash-on-hand grid
    real*8, parameter :: X_grow = 0.04d0

    ! lower and upper bound for X-grid
    real*8, parameter :: X_l_pub  = 10d0
	real*8, parameter :: X_u_pub  = 2000d0
    real*8, parameter :: X_l_priv = 10d0
    real*8, parameter :: X_u_priv = 2000d0

    ! lower and upper bound for the eta-grid
    real*8 :: eta_l_pub(JJ), eta_u_pub(JJ)
    real*8 :: eta_l_priv(JJ), eta_u_priv(JJ)

    ! pension funded status grid
    real*8, parameter          :: surplus_bound = 1.1d0
    real*8, parameter          :: chi_a = .25d0 !.7d0
    real*8, parameter          :: chi_b = surplus_bound
    integer, parameter         :: chi_n = 20
    real*8, parameter          :: chi_grow = 0.01d0
    real*8, dimension(0:chi_n) :: chi_grid

	! labor income tax
	real*8, dimension(JR) :: implied_tax, implied_tax_base, tax_base

    ! discretized shocks
    real*8 :: dist_zeta_pub(NW), zeta_pub(NW), dist_zeta_priv(NW), zeta_priv(NW)
    real*8 :: dist_epsvtheta_pub(NSR), eps_pub(NSR), vtheta_pub(NSR)
    real*8 :: dist_epsvtheta_priv(NSR), eps_priv(NSR), vtheta_priv(NSR)

    ! wages, transfer payments (old-age) and survival probabilities
    real*8 :: eff_pub(JJ), psi(JJ+1)

    real*8 :: eff_priv(JJ), pen_priv(JJ)

    ! individual variables
    real*8 :: X_pub(0:NX), a_pub(0:NA)
    real*8 :: c_pub(JR,JJ, 0:NX,0:chi_n), a_plus_pub(JR,JJ, 0:NX,0:chi_n), V_pub(JR,JJ, 0:NX,0:chi_n) = 0d0  ! value function V(age at date 1,age,wealth,funded ratio)
    real*8 :: omega_plus_pub(JR,JJ, 0:NA,0:chi_n), Q_pub(JR,JJ, 0:NA,0:chi_n)
    real*8 :: phi_X_pub(JR,JJ, 0:NX,0:chi_n), phi_a_pub(JR,JJ, 0:NA,0:chi_n)
    real*8 :: eta_pub(JR,JJ, 0:NE), phi_e_pub(JR,JJ, 0:NE)

    real*8 :: X_priv(0:NX), a_priv(0:NA)
    real*8 :: c_priv(JR,JJ, 0:NX,0:chi_n), a_plus_priv(JR,JJ, 0:NX,0:chi_n), V_priv(JR,JJ, 0:NX,0:chi_n) = 0d0
    real*8 :: omega_plus_priv(JR,JJ, 0:NA,0:chi_n), Q_priv(JR,JJ, 0:NA,0:chi_n)
    real*8 :: phi_X_priv(JR,JJ, 0:NX,0:chi_n), phi_a_priv(JR,JJ, 0:NA,0:chi_n)
    real*8 :: eta_priv(JR,JJ, 0:NE), phi_e_priv(JR,JJ, 0:NE)

    ! numerical variables
    real*8 :: RHS(JJ, 0:NA,0:chi_n)
    integer :: ij_com, ix_com, ia_com, ic_com, tt_com
    real*8 :: cons_com

    ! pension benefits
    real*8, parameter :: ss_benefit_pub  = 16.5d0
    real*8, parameter :: ss_benefit_priv = 18.3d0
    real*8, parameter :: phi_g = 0d0                 ! share of pension surplus rebated to all workers
    real*8, parameter :: pen_split = 1d0     ! share of surplus to public workers, versus private workers

    ! lifecycle earnings equation (college-educated)
    real*8, parameter :: b0             = 2.3831d0 - 4.3148d0
    real*8, parameter :: b1             = .3194d0
    real*8, parameter :: b2             = -.0577d0/10d0
    real*8, parameter :: b3             = .0033d0/100d0
    real*8, parameter :: wage_comp      = .0899d0
    real*8, parameter :: nom_scale_pub  = 1.28d0*(1d0-wage_comp)
    real*8, parameter :: nom_scale_priv = 1.28d0

    ! state demographic info
    real*8                            :: total_pop
	real*8                            :: alt_r_base
	real*8                            :: alt_r_ref
    real*8, parameter                 :: amortization_period = 30d0
    real*8, parameter                 :: pension_split_pub = 0d0  !how the pension insurance tax is split across sectors

    real*8, dimension(JR,JJ)          :: age_probs
	real*8                            :: updated_status_pre, updated_status_post
	real*8                            :: bond_coup
	real*8, dimension(JR)             :: bond_tax
	real*8                            :: arc_rate
    real*8, dimension(JR)             :: Agg_B_hold, Agg_B_hold_base,  PV_L_hold, PV_L_temp, PV_L_hold_base, PV_L_temp1, Norm_Cost
    real*8, dimension(JR,0:chi_n,NSR) :: tax_mat, chi_mat

    ! Rhode Island Reform
    real*8, dimension(JR)    :: dc_con
    real*8, parameter        :: dc_rate = 0.05d0
    real*8, dimension(JR)    :: ben_red
    real*8, dimension(JR,JJ) :: pen_pub

    ! simulation recorder
    integer, parameter            :: nsim = 10000
    real*8, dimension(nsim,JJ,9)  :: recorder_pub, recorder_priv  !cash-in-hand, con, ap, port, death
    real*8, dimension(JJ,0:NX)    :: cohort_dist_pub, cohort_dist_priv
    real*8, dimension(JR,0:NX)    :: CE_val_pub, CE_val_priv

    ! state-specific parameters
    real*8 :: ARC_share                    ! fraction of ARC paid, annually
    real*8 :: state_discount               ! pension liability discount factor
    real*8 :: current_status               ! current pension funded ratio
    real*8 :: b                            ! pension benefit
    real*8 :: prop_pub                     ! size of public sector
    real*8 :: port_share                   ! pension risky portfolio share
    real*8 :: cost_living                  ! cost of living (relative to 100)



contains

    function bond_coupon(x,r,n)

        real*8, intent(in) :: x  ! size of unfunded liability
        real*8, intent(in) :: r  ! municipal debt rate
        real*8, intent(in) :: n  ! maturity (in years)

        real*8             :: bond_coupon

        bond_coupon = x*r*(1d0+r)**n/( (1d0+r)**n -1d0 )

    end function

    function common_earnings_pub(x)

        implicit none

        real*8, intent(in) :: x
        real*8 :: common_earnings_pub

        common_earnings_pub = nom_scale_pub*exp( b0 + b1*x + b2*x**2 + b3*x**3 )

    end function

    function common_earnings_priv(x)

        implicit none

        real*8, intent(in) :: x
        real*8 :: common_earnings_priv

        common_earnings_priv = nom_scale_priv*exp( b0 + b1*x + b2*x**2 + b3*x**3 )

    end function

    ! the first order condition regarding consumption
    function foc_cons_pub(x_in)

        implicit none
        real*8, intent(in) :: x_in
        real*8 :: foc_cons_pub, a_plus, varphi, tomorrow
        integer :: ial, iar

        ! calculate tomorrows assets
        a_plus  = x_in

        ! calculate consumption
        cons_com = X_pub(ix_com) - a_plus

        ! calculate linear interpolation for future part of first order condition
        call linint_Grow(a_plus, a_l, a_u, a_grow, NA, ial, iar, varphi)

        tomorrow = varphi*RHS(ij_com, ial,ic_com) + (1d0-varphi)*RHS(ij_com, iar,ic_com)

        ! calculate first order condition for consumption
        foc_cons_pub = cons_com - tomorrow

    end function

    function foc_cons_priv(x_in)

        implicit none
        real*8, intent(in) :: x_in
        real*8 :: foc_cons_priv, a_plus, varphi, tomorrow
        integer :: ial, iar

        ! calculate tomorrows assets
        a_plus  = x_in

        ! calculate consumption
        cons_com = X_priv(ix_com) - a_plus

        ! calculate linear interpolation for future part of first order condition
        call linint_Grow(a_plus, a_l, a_u, a_grow, NA, ial, iar, varphi)

        tomorrow = varphi*RHS(ij_com, ial,ic_com) + (1d0-varphi)*RHS(ij_com, iar,ic_com)

        ! calculate first order condition for consumption
        foc_cons_priv = cons_com - tomorrow

    end function

    ! the first order condition regarding portfolio choice
    function foc_port_pub(p)

        implicit none
        real*8, intent(in) :: p
        real*8 :: foc_port_pub, omega_p, R_port, X_p, earnings, c_p, varphix, dist, Chi_p, varphiy
        integer :: ixl, ixr, iw, isr, iyl, iyr, ps
        real*8 :: port_return, pre_funded, pension_dist

        ! store portfolio share
        omega_p  = p

        foc_port_pub = 0d0
        if(ij_com+1 >= JR)then     ! if retired
            do isr = 1, NSR

                ! get return on the portfolio
                R_port = 1d0 + r_f + omega_p*(mu_r + vtheta_pub(isr))

                ! get tomorrow's cash-on-hand (epsilon^+ = 0)

                if ( ij_com+1 == JR ) then  ! first year of retirement

                    port_return = port_share*(1d0+vtheta_pub(isr))+(1d0-port_share)*(1d0+r_f)  ! state pension return

                    pre_funded = (port_return*chi_grid(ic_com)*PV_L_hold(ij_com-tt_com) - Agg_B_hold(ij_com-tt_com+1) )/&
                                                                                            PV_L_hold(ij_com-tt_com+1)  ! next-period, pre-contribution funded status

                    if (pre_funded > surplus_bound) then  ! if pre-contribution funded status > upper bound
                        pension_dist = ( pen_split*(1d0-phi_g)*(pre_funded - surplus_bound)*PV_L_hold(ij_com-tt_com+1) )/&
                                                                        (total_pop*age_probs(ij_com-tt_com+1,ij_com+1)*prop_pub)
                    endif

                    X_p = R_port*a_pub(ia_com) + pen_pub(tt_com,ij_com+1) + pension_dist
                else
                    X_p = R_port*a_pub(ia_com) + pen_pub(tt_com,ij_com+1)
                endif

                ! derive interpolation weights
                call linint_Grow(X_p, X_l_pub, X_u_pub, X_grow, NX, ixl, ixr, varphix)

                ! get distributional weight
                dist = dist_epsvtheta_pub(isr)

                ! calculate consumption and FOC
                c_p = varphix      *c_pub(tt_com,ij_com+1, ixl,1) + &   !retirement funded status gridpoint, irrelevant
                      (1d0-varphix)*c_pub(tt_com,ij_com+1, ixr,1)
                c_p = max(c_p, 1d-10)
                foc_port_pub = foc_port_pub + dist*(mu_r + vtheta_pub(isr))*a_pub(ia_com)*margu(c_p)
            enddo
        else                     ! if not retired, subject to labor income risk and pension tax
            do iw = 1, NW
                do isr = 1, NSR

                    ! get return on the portfolio
                    R_port = 1d0 + r_f + omega_p*(mu_r + vtheta_pub(isr))

                    ! get tomorrow's cash on hand             ! ij_com - tt_com = years since time-0
                    !X_p = R_port*(a_pub(ia_com)/eps(isr) + dc_con(tt_com)*eff_pub(ij_com)) + &
                    !			(1d0-tax_mat(ij_com-tt_com+1,ic_com,isr) - &
                    !		(implied_tax(ij_com-tt_com+1)-implied_tax_base(1))/(1d0-implied_tax_base(1)))*eff_pub(ij_com+1)*zeta(iw)

                    X_p = R_port*(a_pub(ia_com)+ dc_con(tt_com)*eff_pub(ij_com))/eps_pub(isr)  + &
                                  (1d0-tax_mat(ij_com-tt_com+1,ic_com,isr) - implied_tax(ij_com-tt_com+1)- &
                                  bond_tax(ij_com-tt_com+1))*eff_pub(ij_com+1)*zeta_pub(iw)

                    ! get tomorrow's funded status
                    Chi_p = Chi_mat(ij_com-tt_com+1,ic_com,isr)

                    ! derive interpolation weights over next-period consumption
                    call linint_Grow(X_p, X_l_pub, X_u_pub, X_grow, NX, ixl, ixr, varphix)
                    call linint_Grow(Chi_p, chi_a, chi_b, chi_grow, chi_n, iyl, iyr, varphiy)

                    ! get distributional weight
                    dist = dist_zeta_pub(iw)*dist_epsvtheta_pub(isr)

                    ! calculate consumption and FOC
                    if (varphix <- varphiy) then  ! bilinear interpolation

                        c_p = varphix*c_pub(tt_com,ij_com+1,ixl,iyl) + &
                              (varphiy-varphix)*c_pub(tt_com,ij_com+1,ixr,iyl) + &
                              (1d0-varphiy)*c_pub(tt_com,ij_com+1,ixr,iyr)

                    else

                        c_p = varphiy*c_pub(tt_com,ij_com+1,ixl,iyl) + &
                              (varphix-varphiy)*c_pub(tt_com,ij_com+1,ixl,iyr) + &
                              (1d0-varphix)*c_pub(tt_com,ij_com+1,ixr,iyr)

                    endif

                    c_p = max(c_p, 1d-10)

                    foc_port_pub = foc_port_pub + dist*(mu_r + vtheta_pub(isr))*a_pub(ia_com)*margu(eps_pub(isr)*c_p)
                enddo
            enddo
        endif

    end function

    function foc_port_priv(p)

        implicit none
        real*8, intent(in) :: p
        real*8 :: foc_port_priv, omega_p, R_port, X_p, earnings, c_p, varphix, dist, Chi_p, varphiy
        integer :: ixl, ixr, iw, isr, iyl, iyr, ps
        real*8 :: pension_dist, pre_funded, port_return

        ! store portfolio share
        omega_p  = p

        foc_port_priv = 0d0
        if(ij_com+1 >= JR)then
            do isr = 1, NSR

                ! get return on the portfolio
                R_port = 1d0 + r_f + omega_p*(mu_r + vtheta_priv(isr))

                ! get tomorrow's cash-on-hand (epsilon^+ = 0)
                if ( ij_com+1 == JR ) then  ! first year of retirement

                    port_return = port_share*(1d0+vtheta_priv(isr))+(1d0-port_share)*(1d0+r_f)  ! state pension return

                    pre_funded = (port_return*chi_grid(ic_com)*PV_L_hold(ij_com-tt_com) - Agg_B_hold(ij_com-tt_com+1) )/&
                                                                                            PV_L_hold(ij_com-tt_com+1)  ! next-period, pre-contribution funded status

                    if (pre_funded > surplus_bound) then  ! if pre-contribution funded status > upper bound
                        pension_dist = ( (1d0-pen_split)*(1d0-phi_g)*(pre_funded - surplus_bound)*PV_L_hold(ij_com-tt_com+1) )/&
                                                        (total_pop*age_probs(ij_com-tt_com+1,ij_com+1)*(1d0-prop_pub))
                    endif

                    X_p = R_port*a_priv(ia_com) + pen_priv(ij_com+1) + pension_dist
                else
                    X_p = R_port*a_priv(ia_com) + pen_priv(ij_com+1)
                endif


                ! derive interpolation weights
                call linint_Grow(X_p, X_l_priv, X_u_priv, X_grow, NX, ixl, ixr, varphix)

                ! get distributional weight
                dist = dist_epsvtheta_priv(isr)

                ! calculate consumption and FOC
                c_p = varphix      *c_priv(tt_com,ij_com+1, ixl,1) + &
                      (1d0-varphix)*c_priv(tt_com,ij_com+1, ixr,1)
                c_p = max(c_p, 1d-10)
                foc_port_priv = foc_port_priv + dist*(mu_r + vtheta_priv(isr))*a_priv(ia_com)*margu(c_p)
            enddo
        else
            do iw = 1, NW
                do isr = 1, NSR

                    ! get return on the portfolio
                    R_port = 1d0 + r_f + omega_p*(mu_r + vtheta_priv(isr))

                    ! get tomorrow's cash on hand
                    !X_p = R_port*a_priv(ia_com)/eps(isr) + &
                    !		(1d0-tax_mat(ij_com-tt_com+1,ic_com,isr) - &
                    !		(implied_tax(ij_com-tt_com+1)-implied_tax_base(1))/(1d0-implied_tax_base(1)))*eff_priv(ij_com+1)*zeta(iw)

                    X_p = R_port*a_priv(ia_com)/eps_priv(isr) + &
                            (1d0-tax_mat(ij_com-tt_com+1,ic_com,isr) - implied_tax(ij_com-tt_com+1) -bond_tax(ij_com-tt_com+1))*&
                                    eff_priv(ij_com+1)*zeta_priv(iw)

                    ! get tomorrow's funded status
                    Chi_p = Chi_mat(ij_com-tt_com+1,ic_com,isr)

                    ! derive interpolation weights
                    call linint_Grow(X_p, X_l_priv, X_u_priv, X_grow, NX, ixl, ixr, varphix)
                    call linint_Grow(Chi_p, chi_a, chi_b, chi_grow, chi_n, iyl, iyr, varphiy)

                    ! get distributional weight
                    dist = dist_zeta_priv(iw)*dist_epsvtheta_priv(isr)

                    ! calculate consumption and FOC
                    if (varphix <- varphiy) then  ! bilinear interpolation

                        c_p = varphix*c_priv(tt_com,ij_com+1,ixl,iyl) + &
                              (varphiy-varphix)*c_priv(tt_com,ij_com+1,ixr,iyl) + &
                              (1d0-varphiy)*c_priv(tt_com,ij_com+1,ixr,iyr)

                    else

                        c_p = varphiy*c_priv(tt_com,ij_com+1,ixl,iyl) + &
                              (varphix-varphiy)*c_priv(tt_com,ij_com+1,ixl,iyr) + &
                              (1d0-varphix)*c_priv(tt_com,ij_com+1,ixr,iyr)
                    endif

                    c_p = max(c_p, 1d-10)

                    foc_port_priv = foc_port_priv + dist*(mu_r + vtheta_priv(isr))*a_priv(ia_com)*margu(eps_priv(isr)*c_p)
                enddo
            enddo
        endif

    end function

    ! calculates marginal utility of consumption
    function margu(cons)

        implicit none
        real*8, intent(in) :: cons
        real*8 :: margu

        margu = max(cons, 1d-10)**(-1d0/gamma)

    end function

    ! calculates the value function
    function valuefunc_pub(a_plus, cons, ij)

        implicit none
        integer, intent(in) :: ij
        real*8, intent(in) :: a_plus, cons
        real*8 :: valuefunc_pub, varphi, c_help
        integer :: ial, iar

        ! check whether consumption or leisure are too small
        c_help = max(cons, 1d-10)

        ! get tomorrows utility
        call linint_Grow(a_plus, a_l, a_u, a_grow, NA, ial, iar, varphi)

        ! calculate tomorrow's part of the value function
        valuefunc_pub = 0d0
        if(ij < JJ)then
            valuefunc_pub = max(varphi*Q_pub(tt_com,ij, ial,ic_com) + &
                               (1d0-varphi)*Q_pub(tt_com,ij, iar,ic_com), 1d-10)**egam/egam
        endif

        ! add todays part and discount
        valuefunc_pub = c_help**egam/egam + beta*(1d0-psi(ij+1))*valuefunc_pub

    end function

    function valuefunc_priv(a_plus, cons, ij)

        implicit none
        integer, intent(in) :: ij
        real*8, intent(in) :: a_plus, cons
        real*8 :: valuefunc_priv, varphi, c_help
        integer :: ial, iar

        ! check whether consumption or leisure are too small
        c_help = max(cons, 1d-10)

        ! get tomorrows utility
        call linint_Grow(a_plus, a_l, a_u, a_grow, NA, ial, iar, varphi)

        ! calculate tomorrow's part of the value function
        valuefunc_priv = 0d0
        if(ij < JJ)then
            valuefunc_priv = max(varphi*Q_priv(tt_com,ij, ial,ic_com) + &
                                (1d0-varphi)*Q_priv(tt_com,ij, iar,ic_com), 1d-10)**egam/egam
        endif

        ! add todays part and discount
        valuefunc_priv = c_help**egam/egam + beta*(1d0-psi(ij+1))*valuefunc_priv

    end function

    ! function to determine annual required contribution (ARC)
    function ARC(chi,L,T)

        implicit none
        real*8,  intent(in) :: chi
        real*8,  intent(in) :: L
        integer, intent(in) :: T
        real*8  :: ARC

        ARC = Norm_Cost(T) + (1d0-chi)*L*arc_rate/(1d0-1d0/(1d0+arc_rate)**amortization_period)

    end function

    ! function to populate tax_matrix
    function tax_populate()

        !returns the pension tax tomorrow, given (1) today's post-contribution funded status Chi
        !   and (2) tomorrow's portfolio return R'
        implicit none

        real*8, dimension(JR,0:chi_n,NSR) :: tax_populate

        ! local variables
        integer :: tt,ii,jj,ps
        real*8 :: port_return, temp_c, req_con, act_con, Agg_B, PV_L, tax_base

        do tt=1,JR  !for current/future year, today

            do ii = 0,chi_n  !for each post-contribution funded status, today

                do jj = 1,NSR  !for each market return, tomorrow

                    ! compute portfolio return, tomorrow
                    port_return = port_share*(1d0+vtheta_priv(jj))+(1d0-port_share)*(1d0+r_f)

                    ! compute next-period, pre-contribution funded status
                    if (tt<JR) then
                        temp_c = ( port_return*chi_grid(ii)*PV_L_hold(tt) - Agg_B_hold(tt+1) )/PV_L_hold(tt+1)
                    else
                        temp_c = ( port_return*chi_grid(ii)*PV_L_hold(tt) - Agg_B_hold(tt) )/PV_L_hold(tt)
                    endif


                    ! compute corresponding ARC
                    if ( temp_c > 1d0) then

                        if ( temp_c >= surplus_bound ) then
                            tax_populate(tt,ii,jj) = 0d0
                        else
                            if (tt<JR) then
                                req_con = Norm_Cost(tt+1)
                                tax_base = total_pop*sum( age_probs(tt+1,1:JR)*( prop_pub*eff_pub(1:JR) &
                                                                             + (1d0 - prop_pub)*eff_priv(1:JR) ))
                            else
                                req_con = Norm_Cost(tt)
                                tax_base = total_pop*sum( age_probs(tt,1:JR)*( prop_pub*eff_pub(1:JR) &
                                                                             + (1d0 - prop_pub)*eff_priv(1:JR) ))
                            endif

                            act_con = ARC_share*req_con
                            tax_populate(tt,ii,jj) = act_con/tax_base
                        endif
                    else
                        if (tt<JR) then
                            req_con = ARC(temp_c,PV_L_hold(tt+1),tt+1)
                            tax_base = total_pop*sum( age_probs(tt+1,1:JR)*( prop_pub*eff_pub(1:JR) &
                                                                             + (1d0 - prop_pub)*eff_priv(1:JR) ))
                        else
                            req_con = ARC(temp_c,PV_L_hold(tt),tt)
                            tax_base = total_pop*sum( age_probs(tt,1:JR)*( prop_pub*eff_pub(1:JR) &
                                             + (1d0 - prop_pub)*eff_priv(1:JR) ))
                        endif

                        act_con = ARC_share*req_con

                        tax_populate(tt,ii,jj) = act_con/tax_base

                    endif

                enddo
            enddo

        enddo

    end function

    function chi_populate()

        !returns the post-contribution pension funded status tomorrow, given (1) today's post-contribution funded status Chi
        !   and (2) tomorrow's portfolio return R'
        implicit none

        real*8, dimension(JR,0:chi_n,NSR) :: chi_populate

        ! local variables
        integer :: tt,ii,jj,ps
        real*8 :: port_return, temp_c, req_con, act_con

        do tt=1,JR  ! for each each point in time

            do ii = 0,chi_n  ! for each current post-contribution funded status

                do jj = 1,NSR  ! for each market return shock tomorrow

                    ! compute return on portfolio
                    port_return = port_share*(1d0+vtheta_priv(jj))+(1d0-port_share)*(1d0+r_f)

                    ! compute pre-contribution funded status, tomorrow
                    if (tt<JR) then
                        temp_c = ( port_return*chi_grid(ii)*PV_L_hold(tt) - Agg_B_hold(tt+1) )/PV_L_hold(tt+1)
                    else
                        temp_c = ( port_return*chi_grid(ii)*PV_L_hold(tt) - Agg_B_hold(tt) )/PV_L_hold(tt)
                    endif

                    ! determine aggregate contributions and post-contribution status
                    if ( temp_c > 1d0) then

                        if ( temp_c > surplus_bound) then
                            chi_populate(tt,ii,jj) = surplus_bound
                        else
                            if (tt<JR) then
                                chi_populate(tt,ii,jj) = ( port_return*chi_grid(ii)*PV_L_hold(tt) - Agg_B_hold(tt+1) +&
                                 ARC_share*Norm_Cost(tt+1))/PV_L_hold(tt+1)
                            else
                                chi_populate(tt,ii,jj) = ( port_return*chi_grid(ii)*PV_L_hold(tt) - Agg_B_hold(tt) +&
                                 ARC_share*Norm_Cost(tt))/PV_L_hold(tt)
                            endif
                        endif
                    else
                        if (tt<JR) then
                            req_con = ARC(temp_c,PV_L_hold(tt+1),tt+1)
                            act_con = ARC_share*req_con

                            chi_populate(tt,ii,jj) = ( port_return*chi_grid(ii)*PV_L_hold(tt) - Agg_B_hold(tt+1) +&
                             act_con)/PV_L_hold(tt+1)
                        else
                            req_con = ARC(temp_c,PV_L_hold(tt),tt)
                            act_con = ARC_share*req_con

                            chi_populate(tt,ii,jj) = ( port_return*chi_grid(ii)*PV_L_hold(tt) - Agg_B_hold(tt) +&
                             act_con)/PV_L_hold(tt)
                        endif

                    endif

                enddo
            enddo
        enddo

    end function

    subroutine CE_compute_pub(updated_status)

        implicit none

        real*8, intent(in) :: updated_status
        integer :: tt, ix, ixl, ixr
        real*8, dimension(0:NX) :: cbar_temp
        real*8, dimension(JR) :: total_discount
        real*8 :: gamma_temp, varphi, vtemp

        gamma_temp = 1d0/gamma

        total_discount = 0d0

        ! compute discount factor for each age cohort
        do tt=1,JR ! for each age cohort
            do ix=tt,JJ
                total_discount(tt) = total_discount(tt) + beta**(ix-tt)*product( 1d0-psi(tt:ix) )
            enddo
        enddo

        call linint_Grow(updated_status,chi_a,chi_b,chi_grow,chi_n,ixl,ixr,varphi)

        ! for each age cohort
        do tt = 1,JR
            ! for each wealth point
            do ix = 0,NX

                ! compute consumption equivalent c(x)
                vtemp = varphi*V_pub(tt,tt,ix,ixl) +  (1d0-varphi)*V_pub(tt,tt,ix,ixr)

                cbar_temp(ix) = ((1d0-gamma_temp)*vtemp/total_discount(tt))**( 1d0/(1d0-gamma_temp) )

            enddo

            ! compute expected value of c(x)
            CE_val_pub(tt,:) = cbar_temp

        enddo

    end subroutine

    subroutine CE_compute_priv(updated_status)

        implicit none

        real*8, intent(in) :: updated_status
        integer :: tt, ix, ixl, ixr
        real*8, dimension(0:NX) :: cbar_temp
        real*8, dimension(JR) :: total_discount
        real*8 :: gamma_temp, varphi, vtemp

        gamma_temp = 1d0/gamma

        total_discount = 0d0

        ! compute discount factor for each age cohort
        do tt=1,JR ! for each age cohort
            do ix=tt,JJ
                total_discount(tt) = total_discount(tt) + beta**(ix-tt)*product( 1d0-psi(tt:ix) )
            enddo
        enddo

        call linint_Grow(updated_status,chi_a,chi_b,chi_grow,chi_n,ixl,ixr,varphi)

        ! for each age cohort
        do tt = 1,JR
            ! for each wealth point
            do ix = 0,NX

                ! compute consumption equivalent c(x)
                vtemp = varphi*V_priv(tt,tt,ix,ixl) +  (1d0-varphi)*V_priv(tt,tt,ix,ixr)

                cbar_temp(ix) = ((1d0-gamma_temp)*vtemp/total_discount(tt))**( 1d0/(1d0-gamma_temp) )

            enddo

            ! compute expected value of c(x)
            CE_val_priv(tt,:) = cbar_temp

        enddo

    end subroutine

    !----------------------------!
    !   Public Worker Problem    !
    !----------------------------!

    ! determines the solution to the household optimization problem
    subroutine solve_household_pub()

        implicit none
        integer :: ij, ix, ia, ic

        ! get decisions in last period of life
        omega_plus_pub(tt_com,JJ, :, :) = 0d0
        do ix = 0, NX
            do ic = 0, chi_n
                a_plus_pub(tt_com,JJ, ix, ic) = 0d0
                c_pub(tt_com,JJ, ix, ic) = X_pub(ix)
                V_pub(tt_com,JJ, ix, ic) = valuefunc_pub(0d0, c_pub(tt_com,JJ, ix,ic), JJ)
            enddo
        enddo

        do ij = JJ-1, tt_com, -1      ! for each age

            do ic = 0, chi_n     ! for each funded status

                ic_com = ic

                ! determine optimal portfolio choice for all others
                do ia = 1, NA
                    call solve_portfolio_pub(ij, ia)
                enddo

                ! set omega for zero savings consistent with next gridpoint
                omega_plus_pub(tt_com,ij, 0, :) = omega_plus_pub(tt_com,ij, 1, :)

                ! interpolate individual RHS and value function
                call interpolate_pub(ij)

                ! determine consumption-savings solution
                do ix = 0, NX
                    call solve_consumption_pub(ij, ix)
                enddo

            enddo
            write(*,'(a,i3,a,i3,a)')'Cohort Date: ',tt_com,' Age: ',ij,' DONE!'
        enddo

    end subroutine

    ! solve the household's portfolio decision
    subroutine solve_portfolio_pub(ij, ia)

        implicit none
        integer, intent(in) :: ij, ia
        real*8 :: x_in, port0, port1, tolerance
        logical :: check

        ! set up communication variables
        ij_com = ij
        ia_com = ia

        ! check for corner solutions
        port0 = foc_port_pub(0d0)
        port1 = foc_port_pub(1d0)

        ! use intermediate value theorem
        if(port0*port1 > 0d0)then
            if(abs(port0) > abs(port1))then
                omega_plus_pub(tt_com,ij, ia,ic_com) = 1d0
            else
                omega_plus_pub(tt_com,ij, ia,ic_com) = 0d0
            endif
            return
        else

            ! get order of magnitude of foc
            !tolerance = 1d-5*abs(port0-port1)
            !tolerance = min(tolerance, 1d-8)
            tolerance = 1d-8
            call settol_root(tolerance)

            ! get best guess for the root of foc_port
            x_in = -port0/(port1-port0)
            check = .false.

            ! solve the household problem using rootfinding
            call fzero(x_in, foc_port_pub, check)

            ! write screen output in case of a problem
            if(check)write(*,'(a, 2i4)')'ERROR IN ROOTFINDING PORT : ', ij, ia

            omega_plus_pub(tt_com,ij, ia,ic_com) = x_in

            ! reset tolerance level to original value
            call settol_root(1d-8)
        endif

    end subroutine

    ! solve the household's consumption-savings decision
    subroutine solve_consumption_pub(ij, ix)

        implicit none
        integer, intent(in) :: ij, ix
        real*8 :: x_in
        logical :: check

        ! determine decision for zero cash-on-hand
        if(X_pub(ix) < 1d-10)then
            a_plus_pub(tt_com,ij, ix,ic_com) = 0d0
            c_pub(tt_com,ij, ix,ic_com) = 0d0
            V_pub(tt_com,ij, ix,ic_com) = valuefunc_pub(0d0, 0d0, ij)
            return
        endif

        ! set up communication variables
        ij_com = ij
        ix_com = ix

        ! get best initial guess from future period
        x_in = a_plus_pub(tt_com,ij+1, ix,ic_com)
        check = .false.

        ! solve the household problem using rootfinding
        call fzero(x_in, foc_cons_pub, check)

        ! write screen output in case of a problem
        if(check)write(*,'(a, 2i4)')'ERROR IN ROOTFINDING CONS : ', ij, ix

        ! check for borrowing constraint
        if(x_in < 0d0)then
            x_in = 0d0
            cons_com = X_pub(ix)
        endif

        ! copy decisions
        a_plus_pub(tt_com,ij, ix,ic_com) = x_in
        c_pub(tt_com,ij, ix,ic_com) = cons_com
        V_pub(tt_com,ij, ix,ic_com) = valuefunc_pub(x_in, cons_com, ij)

    end subroutine

    ! for calculating the rhs of the first order condition at age ij
    subroutine interpolate_pub(ij)

        implicit none
        integer, intent(in) :: ij
        integer :: ia, iw, isr
        real*8 :: X_p, c_p, varphix, dist, EV, R_port, varphiy, Chi_p
        integer :: ixl, ixr, iyl, iyr, ps
        real*8  :: port_return, pre_funded, pension_dist

        RHS(ij, :, :) = 0d0
        Q_pub(tt_com,ij, :, :) = 0d0

        do ia = 0, NA

            ! case agent is retired tomorrow
            if(ij >= JR-1)then

                do isr = 1, NSR

                    ! get return on the portfolio
                    R_port = 1d0 + r_f + omega_plus_pub(tt_com,ij, ia, ic_com)*(mu_r + vtheta_pub(isr))

                    ! get tomorrow's cash-on-hand (epsilon^+ = 0)

                    if ( ij == JR-1 ) then  ! first year of retirement

                        port_return = port_share*(1d0+vtheta_pub(isr))+(1d0-port_share)*(1d0+r_f)  ! state pension return

                        pre_funded = (port_return*chi_grid(ic_com)*PV_L_hold(ij-tt_com) - Agg_B_hold(ij-tt_com+1) )/&
                                                                                                PV_L_hold(ij-tt_com+1)  ! next-period, pre-contribution funded status

                        if (pre_funded > surplus_bound) then  ! if pre-contribution funded status > upper bound
                            pension_dist = ( pen_split*(1d0-phi_g)*(pre_funded - surplus_bound)*PV_L_hold(ij-tt_com+1) )/&
                                                                            (total_pop*age_probs(ij-tt_com+1,ij)*prop_pub)
                        endif

                        X_p = R_port*a_pub(ia) + pen_pub(tt_com,ij+1) + pension_dist
                    else
                        X_p = R_port*a_pub(ia) + pen_pub(tt_com,ij+1)
                    endif

                    ! derive interpolation weights
                    call linint_Grow(X_p, X_l_pub, X_u_pub, X_grow, NX, ixl, ixr, varphix)

                    ! get distributional weight
                    dist = dist_epsvtheta_pub(isr)

                    ! get future consumption value  (value of funded status irrelevant)
                    c_p = max(varphix*c_pub(tt_com,ij+1, ixl,1) + (1d0-varphix)*c_pub(tt_com,ij+1, ixr,1), 1d-10)

                    ! get tomorrow's value function
                    EV = varphix      *(egam*V_pub(tt_com,ij+1, ixl,1))**(1d0/egam) + &
                         (1d0-varphix)*(egam*V_pub(tt_com,ij+1, ixr,1))**(1d0/egam)

                    ! get RHS of foc and Q
                    RHS(ij, ia,ic_com) = RHS(ij, ia,ic_com) + dist*R_port*margu(c_p)
                    Q_pub(tt_com,ij, ia,ic_com)   = Q_pub(tt_com,ij, ia,ic_com) + dist*EV**egam/egam
                enddo

            ! agent is working
            else
                do iw = 1, NW
                    do isr = 1, NSR

                        ! get return on the portfolio
                        R_port = 1d0 + r_f + omega_plus_pub(tt_com,ij, ia, ic_com)*(mu_r + vtheta_pub(isr))

                        ! get tomorrow's cash on hand
                        !X_p   = R_port*(a_pub(ia)/eps(isr)+dc_con(tt_com)*eff_pub(ij)) + &
                        !                (1d0-tax_mat(ij-tt_com+1,ic_com,isr) - &
                        !				(implied_tax(ij-tt_com+1)-implied_tax_base(1))/(1d0-implied_tax_base(1)) )*eff_pub(ij+1)*zeta(iw)

                        X_p   = R_port*(a_pub(ia)+dc_con(tt_com)*eff_pub(ij))/eps_pub(isr) + &
                                (1d0-tax_mat(ij-tt_com+1,ic_com,isr) - implied_tax(ij-tt_com+1) - &
                                bond_tax(ij_com-tt_com+1))*eff_pub(ij+1)*zeta_pub(iw)

                        Chi_p = Chi_mat(ij-tt_com+1,ic_com,isr)

                        ! derive interpolation weights
                        call linint_Grow(X_p, X_l_pub, X_u_pub, X_grow, NX, ixl, ixr, varphix)
                        call linint_Grow(Chi_p, chi_a, chi_b, chi_grow, chi_n, iyl, iyr, varphiy)

                        ! get distributional weight
                        dist = dist_zeta_pub(iw)*dist_epsvtheta_pub(isr)

                        ! get future consumption value
                        if (varphix <- varphiy) then  ! bilinear interpolation

                            c_p = varphix*c_pub(tt_com,ij+1,ixl,iyl) + &
                                  (varphiy-varphix)*c_pub(tt_com,ij+1,ixr,iyl) + &
                                  (1d0-varphiy)*c_pub(tt_com,ij+1,ixr,iyr)

                            EV = varphix*(egam*V_pub(tt_com,ij+1,ixl,iyl))**(1d0/egam) + &
                                 (varphiy-varphix)*(egam*V_pub(tt_com,ij+1,ixr,iyl))**(1d0/egam) + &
                                 (1d0-varphiy)*(egam*V_pub(tt_com,ij+1,ixr,iyr))**(1d0/egam)

                        else

                            c_p = varphiy*c_pub(tt_com,ij+1,ixl,iyl) + &
                                  (varphix-varphiy)*c_pub(tt_com,ij+1,ixl,iyr) + &
                                  (1d0-varphix)*c_pub(tt_com,ij+1,ixr,iyr)

                            EV = varphiy*(egam*V_pub(tt_com,ij+1,ixl,iyl))**(1d0/egam) + &
                                 (varphix-varphiy)*(egam*V_pub(tt_com,ij+1,ixl,iyr))**(1d0/egam) + &
                                 (1d0-varphix)*(egam*V_pub(tt_com,ij+1,ixr,iyr))**(1d0/egam)

                        endif

                        c_p = max(c_p, 1d-10)

                        ! get RHS of foc and Q
                        RHS(ij, ia,ic_com)   = RHS(ij, ia,ic_com)   + dist*R_port*margu(eps_pub(isr)*c_p)
                        Q_pub(tt_com,ij, ia,ic_com) = Q_pub(tt_com,ij, ia,ic_com) + dist*(eps_pub(isr)*EV)**egam/egam
                    enddo
                enddo
            endif

            RHS(ij, ia,ic_com)   = (beta*(1d0-psi(ij+1))*RHS(ij, ia,ic_com))**(-gamma)
            Q_pub(tt_com,ij, ia,ic_com) = (egam*Q_pub(tt_com,ij, ia,ic_com))**(1d0/egam)
        enddo

    end subroutine

    !-----------------------------!
    !   Private Worker Problem    !
    !-----------------------------!
    subroutine solve_household_priv()

        implicit none
        integer :: ij, ix, ia, ic

        ! get decisions in last period of life
        omega_plus_priv(tt_com,JJ, :, :) = 0d0
        do ix = 0, NX
            do ic =0,chi_n
                a_plus_priv(tt_com,JJ, ix,ic) = 0d0
                c_priv(tt_com,JJ, ix,ic) = X_priv(ix)
                V_priv(tt_com,JJ, ix,ic) = valuefunc_priv(0d0, c_priv(tt_com,JJ, ix,ic), JJ)
            enddo
        enddo

        do ij = JJ-1, tt_com, -1

            do ic = 0, chi_n     ! for each funded status

                ic_com = ic

                ! determine optimal portfolio choice for all others
                do ia = 1, NA
                    call solve_portfolio_priv(ij, ia)
                enddo

                ! set omega for zero savings consistent with next gridpoint
                omega_plus_priv(tt_com,ij, 0,:) = omega_plus_priv(tt_com,ij, 1,:)

                ! interpolate individual RHS and value function
                call interpolate_priv(ij)

                ! determine consumption-savings solution
                do ix = 0, NX
                    call solve_consumption_priv(ij, ix)
                enddo

            enddo
            write(*,'(a,i3,a,i3,a)')'Cohort Date: ',tt_com,' Age: ',ij,' DONE!'
        enddo

    end subroutine

    subroutine solve_portfolio_priv(ij, ia)

        implicit none
        integer, intent(in) :: ij, ia
        real*8 :: x_in, port0, port1, tolerance
        logical :: check

        ! set up communication variables
        ij_com = ij
        ia_com = ia

        ! check for corner solutions
        port0 = foc_port_priv(0d0)
        port1 = foc_port_priv(1d0)

        ! use intermediate value theorem
        if(port0*port1 > 0d0)then
            if(abs(port0) > abs(port1))then
                omega_plus_priv(tt_com,ij, ia,ic_com) = 1d0
            else
                omega_plus_priv(tt_com,ij, ia,ic_com) = 0d0
            endif
            return
        else

            ! get order of magnitude of foc
            tolerance = 1d-5*abs(port0-port1)
            tolerance = min(tolerance, 1d-8)
            call settol_root(tolerance)

            ! get best guess for the root of foc_port
            x_in = -port0/(port1-port0)
            check = .false.

            ! solve the household problem using rootfinding
            call fzero(x_in, foc_port_priv, check)

            ! write screen output in case of a problem
            if(check)write(*,'(a, 2i4)')'ERROR IN ROOTFINDING PORT : ', ij, ia

            omega_plus_priv(tt_com,ij, ia,ic_com) = x_in

            ! reset tolerance level to original value
            call settol_root(1d-8)
        endif

    end subroutine

    subroutine solve_consumption_priv(ij, ix)

        implicit none
        integer, intent(in) :: ij, ix
        real*8 :: x_in
        logical :: check

        ! determine decision for zero cash-on-hand
        if(X_priv(ix) < 1d-10)then
            a_plus_priv(tt_com,ij, ix,ic_com) = 0d0
            c_priv(tt_com,ij, ix,ic_com) = 0d0
            V_priv(tt_com,ij, ix,ic_com) = valuefunc_priv(0d0, 0d0, ij)
            return
        endif

        ! set up communication variables
        ij_com = ij
        ix_com = ix

        ! get best initial guess from future period
        x_in = a_plus_priv(tt_com,ij+1, ix,ic_com)
        check = .false.

        ! solve the household problem using rootfinding
        call fzero(x_in, foc_cons_priv, check)

        ! write screen output in case of a problem
        if(check)write(*,'(a, 2i4)')'ERROR IN ROOTFINDING CONS : ', ij, ix

        ! check for borrowing constraint
        if(x_in < 0d0)then
            x_in = 0d0
            cons_com = X_priv(ix)
        endif

        ! copy decisions
        a_plus_priv(tt_com,ij, ix,ic_com) = x_in
        c_priv(tt_com,ij, ix,ic_com) = cons_com
        V_priv(tt_com,ij, ix,ic_com) = valuefunc_priv(x_in, cons_com, ij)

    end subroutine

    subroutine interpolate_priv(ij)

        implicit none
        integer, intent(in) :: ij
        integer :: ia, iw, isr
        real*8 :: X_p, c_p, varphix, dist, EV, R_port, varphiy, Chi_p
        integer :: ixl, ixr, iyl, iyr, ps
        real*8 :: pension_dist, pre_funded, port_return

        RHS(ij, :, :) = 0d0
        Q_priv(tt_com,ij, :, :) = 0d0

        do ia = 0, NA

            ! case agent is retired tomorrow
            if(ij >= JR-1)then

                do isr = 1, NSR

                    ! get return on the portfolio
                    R_port = 1d0 + r_f + omega_plus_priv(tt_com,ij, ia,ic_com)*(mu_r + vtheta_priv(isr))

                    ! get tomorrow's cash-on-hand (epsilon^+ = 0)
                    if ( ij == JR-1 ) then  ! first year of retirement

                        port_return = port_share*(1d0+vtheta_priv(isr))+(1d0-port_share)*(1d0+r_f)  ! state pension return

                        pre_funded = (port_return*chi_grid(ic_com)*PV_L_hold(ij-tt_com) - Agg_B_hold(ij-tt_com+1) )/&
                                                                                                PV_L_hold(ij-tt_com+1)  ! next-period, pre-contribution funded status

                        if (pre_funded > surplus_bound) then  ! if pre-contribution funded status > upper bound
                            pension_dist = ( (1d0-pen_split)*(1d0-phi_g)*(pre_funded - surplus_bound)*PV_L_hold(ij-tt_com+1) )/&
                                                                            (total_pop*age_probs(ij-tt_com+1,ij)*(1d0-prop_pub))
                        endif

                        X_p = R_port*a_priv(ia) + pen_priv(ij+1) + pension_dist
                    else
                        X_p = R_port*a_priv(ia) + pen_priv(ij+1)
                    endif

                    ! derive interpolation weights
                    call linint_Grow(X_p, X_l_priv, X_u_priv, X_grow, NX, ixl, ixr, varphix)

                    ! get distributional weight
                    dist = dist_epsvtheta_priv(isr)

                    ! get future consumption value
                    c_p = max(varphix*c_priv(tt_com,ij+1, ixl,1) + (1d0-varphix)*c_priv(tt_com,ij+1, ixr,1), 1d-10)

                    ! get tomorrow's value function
                    EV = varphix      *(egam*V_priv(tt_com,ij+1, ixl,1))**(1d0/egam) + &
                         (1d0-varphix)*(egam*V_priv(tt_com,ij+1, ixr,1))**(1d0/egam)

                    ! get RHS of foc and Q
                    RHS(ij, ia,ic_com)    = RHS(ij, ia,ic_com) + dist*R_port*margu(c_p)
                    Q_priv(tt_com,ij, ia,ic_com) = Q_priv(tt_com,ij, ia,ic_com) + dist*EV**egam/egam
                enddo

            ! agent is working
            else
                do iw = 1, NW
                    do isr = 1, NSR

                        ! get return on the portfolio
                        R_port = 1d0 + r_f + omega_plus_priv(tt_com,ij, ia,ic_com)*(mu_r + vtheta_priv(isr))

                        ! get tomorrow's cash on hand
                        !X_p = R_port*a_priv(ia)/eps(isr) + (1d0-tax_mat(ij-tt_com+1,ic_com,isr) - &
                        !		 (implied_tax(ij-tt_com+1)-implied_tax_base(1))/(1d0-implied_tax_base(1)) )*eff_priv(ij+1)*zeta(iw)

                        X_p = R_port*a_priv(ia)/eps_priv(isr) + (1d0-tax_mat(ij-tt_com+1,ic_com,isr) - &
                                 implied_tax(ij-tt_com+1) -bond_tax(ij_com-tt_com+1))*eff_priv(ij+1)*zeta_priv(iw)

                        Chi_p = Chi_mat(ij-tt_com+1,ic_com,isr)

                        ! derive interpolation weights
                        call linint_Grow(X_p, X_l_priv, X_u_priv, X_grow, NX, ixl, ixr, varphix)
                        call linint_Grow(Chi_p, chi_a, chi_b, chi_grow, chi_n, iyl, iyr, varphiy)

                        ! get distributional weight
                        dist = dist_zeta_priv(iw)*dist_epsvtheta_priv(isr)

                        ! get future consumption value
                        if (varphix <- varphiy) then  ! bilinear interpolation

                            c_p = varphix*c_priv(tt_com,ij+1,ixl,iyl) + &
                                  (varphiy-varphix)*c_priv(tt_com,ij+1,ixr,iyl) + &
                                  (1d0-varphiy)*c_priv(tt_com,ij+1,ixr,iyr)

                            EV = varphix*(egam*V_priv(tt_com,ij+1,ixl,iyl))**(1d0/egam) + &
                                 (varphiy-varphix)*(egam*V_priv(tt_com,ij+1,ixr,iyl))**(1d0/egam) + &
                                 (1d0-varphiy)*(egam*V_priv(tt_com,ij+1,ixr,iyr))**(1d0/egam)

                        else

                            c_p = varphiy*c_priv(tt_com,ij+1,ixl,iyl) + &
                                  (varphix-varphiy)*c_priv(tt_com,ij+1,ixl,iyr) + &
                                  (1d0-varphix)*c_priv(tt_com,ij+1,ixr,iyr)

                            EV = varphiy*(egam*V_priv(tt_com,ij+1,ixl,iyl))**(1d0/egam) + &
                                 (varphix-varphiy)*(egam*V_priv(tt_com,ij+1,ixl,iyr))**(1d0/egam) + &
                                 (1d0-varphix)*(egam*V_priv(tt_com,ij+1,ixr,iyr))**(1d0/egam)

                        endif

                        c_p = max(c_p, 1d-10)

                        ! get RHS of foc and Q
                        RHS(ij, ia,ic_com)    = RHS(ij, ia,ic_com) + dist*R_port*margu(eps_priv(isr)*c_p)
                        Q_priv(tt_com,ij, ia,ic_com) = Q_priv(tt_com,ij, ia,ic_com) + dist*(eps_priv(isr)*EV)**egam/egam

                    enddo
                enddo
            endif

            RHS(ij, ia,ic_com) = (beta*(1d0-psi(ij+1))*RHS(ij, ia,ic_com))**(-gamma)
            Q_priv(tt_com,ij, ia,ic_com)   = (egam*Q_priv(tt_com,ij, ia,ic_com))**(1d0/egam)
        enddo

    end subroutine

end module
