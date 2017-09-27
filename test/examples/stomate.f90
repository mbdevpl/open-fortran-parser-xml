! =================================================================================================================================
! MODULE       : stomate
!
! CONTACT      : orchidee-help _at_ listes.ipsl.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF       Groups the subroutines that: (1) initialize all variables in 
!! stomate, (2) read and write forcing files of stomate and the soil component,
!! (3) aggregates and convert variables to handle the different time steps 
!! between sechiba and stomate, (4) call subroutines that govern major stomate
!! processes (litter, soil, and vegetation dynamics) and (5) structures these tasks 
!! in stomate_main
!!
!!\n DESCRIPTION : None
!!
!! RECENT CHANGE(S) : None
!!
!! REFERENCE(S)	: None
!!
!! SVN :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/branches/ORCHIDEE-MICT/ORCHIDEE/src_stomate/stomate.f90 $
!! $Date: 2017-07-28 17:48:13 +0200 (ven. 28 juil. 2017) $
!! $Revision: 4542 $
!! \n
!_ ================================================================================================================================

MODULE stomate

  ! Modules used:
  USE netcdf
  USE defprec
  USE grid
  USE constantes
  USE constantes_soil
  USE pft_parameters
  USE stomate_io
  USE stomate_data
  USE stomate_season
  USE stomate_lpj
  USE stomate_litter
  USE stomate_vmax
  USE stomate_soilcarbon
  USE stomate_resp
  USE stomate_permafrost_soilcarbon
  USE sticslai_io
  USE mod_orchidee_para
  USE ioipsl
  USE ioipsl_para 
  USE xios_orchidee
  USE matrix_resolution
  USE utils
  USE stomate_io_carbon_permafrost
  USE stomate_lai
  USE stomate_stics
#ifdef CPP_PARA
  USE mpi
#endif
  
!pss:+
!  USE stomate_cste_wetlands
  USE stomate_wet_ch4_pt_ter_0
  USE stomate_wet_ch4_pt_ter_wet
!pss:-

!! crop
  USE Divers_develop   ! some functions for calculating UDEV, GDH, crfpi(limiting effects of photoperiod), humpotsol
  USE Stics
  USE interpol_help  ! necessary for management map input
!! xuhui


  IMPLICIT NONE

  ! Private & public routines

  PRIVATE
  PUBLIC stomate_main,stomate_clear,init_forcing, stomate_forcing_read, stomate_initialize, stomate_finalize, stomate_veget_update, stomate_init_index

  INTERFACE stomate_accu
     MODULE PROCEDURE stomate_accu_r1d, stomate_accu_r2d, stomate_accu_r3d
  END INTERFACE

  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:,:):: biomass              !! Biomass per ground area @tex $(gC m^{-2})$ @endtex 
!$OMP THREADPRIVATE(biomass)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: veget_cov_max        !! Maximal fractional coverage: maximum share of a pixel
                                                                         !! taken by a PFT 
!$OMP THREADPRIVATE(veget_cov_max)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: carbon_surf          !! carbon pool integrated to over surface soils: active, slow, or passive
!$OMP THREADPRIVATE(carbon_surf)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: ind                  !! Vegetation density, number of individuals per unit 
                                                                         !! ground area @tex $(m^{-2})$ @endtex 
!$OMP THREADPRIVATE(ind)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: age                  !! Age of PFT it normalized by biomass - can increase and
                                                                         !! decrease - (years)
!$OMP THREADPRIVATE(age)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: adapted              !! Winter too cold for PFT to survive (0-1, unitless)
!$OMP THREADPRIVATE(adapted)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: regenerate           !! Winter sufficiently cold to produce viable seeds 
                                                                         !! (0-1, unitless)
!$OMP THREADPRIVATE(regenerate)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: everywhere           !! Is the PFT everywhere in the grid box or very localized 
                                                                         !! (after its intoduction)
!$OMP THREADPRIVATE(everywhere)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: fireindex            !! Probability of fire (unitless)
!$OMP THREADPRIVATE(fireindex)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: veget_lastlight      !! Vegetation fractions (on ground) after last light 
                                                                         !! competition (unitless) 
!$OMP THREADPRIVATE(veget_lastlight)
  REAL(r_std), ALLOCATABLE,SAVE,DIMENSION(:,:)   :: fpc_max              !! "maximal" coverage fraction of a grid box (LAI -> 
                                                                         !! infinity) on ground. [??CHECK??] It's set to zero here, 
                                                                         !! and then is used once in lpj_light.f90 to test if 
                                                                         !! fpc_nat is greater than it. Something seems missing
!$OMP THREADPRIVATE(fpc_max)
  LOGICAL,ALLOCATABLE,SAVE,DIMENSION(:,:)        :: PFTpresent           !! PFT exists (equivalent to veget > 0 for natural PFTs)
!$OMP THREADPRIVATE(PFTpresent)
  LOGICAL,ALLOCATABLE,SAVE,DIMENSION(:,:)        :: senescence           !! The PFT is senescent
!$OMP THREADPRIVATE(senescence)
  LOGICAL,ALLOCATABLE,SAVE,DIMENSION(:,:)        :: begin_leaves         !! Signal to start putting leaves on (true/false)
!$OMP THREADPRIVATE(begin_leaves)
  LOGICAL,ALLOCATABLE,SAVE,DIMENSION(:,:)        :: need_adjacent        !! This PFT needs to be in present in an adjacent gridbox 
                                                                         !! if it is to be introduced in a new gridbox
!$OMP THREADPRIVATE(need_adjacent)
!--
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: humrel_daily         !! Daily plant available water -root profile weighted 
                                                                         !! (0-1, unitless)
!$OMP THREADPRIVATE(humrel_daily)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: humrel_week          !! "Weekly" plant available water -root profile weighted
                                                                         !! (0-1, unitless)
!$OMP THREADPRIVATE(humrel_week)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: humrel_month         !! "Monthly" plant available water -root profile weighted
                                                                         !! (0-1, unitless)
!$OMP THREADPRIVATE(humrel_month)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: maxhumrel_lastyear   !! Last year's max plant available water -root profile 
                                                                         !! weighted (0-1, unitless)
!$OMP THREADPRIVATE(maxhumrel_lastyear)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: maxhumrel_thisyear   !! This year's max plant available water -root profile 
                                                                         !! weighted (0-1, unitless) 
!$OMP THREADPRIVATE(maxhumrel_thisyear)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: minhumrel_lastyear   !! Last year's min plant available water -root profile 
                                                                         !! weighted (0-1, unitless)  
!$OMP THREADPRIVATE(minhumrel_lastyear)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: minhumrel_thisyear   !! This year's minimum plant available water -root profile
                                                                         !! weighted (0-1, unitless)
!$OMP THREADPRIVATE(minhumrel_thisyear)
!---  
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: t2m_daily            !! Daily air temperature at 2 meter (K)
!$OMP THREADPRIVATE(t2m_daily)

  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: Tseason              !! "seasonal" 2 meter temperatures (K)
!$OMP THREADPRIVATE(Tseason)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: Tseason_length       !! temporary variable to calculate Tseason
!$OMP THREADPRIVATE(Tseason_length)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: Tseason_tmp          !! temporary variable to calculate Tseason
!$OMP THREADPRIVATE(Tseason_tmp)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: Tmin_spring_time     !! Number of days after begin_leaves (leaf onset) 
!$OMP THREADPRIVATE(Tmin_spring_time)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: onset_date           !! Date in the year at when the leaves started to grow(begin_leaves), only for diagnostics.
!$OMP THREADPRIVATE(onset_date)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: t2m_week             !! Mean "weekly" (default 7 days) air temperature at 2 
                                                                         !! meter (K)  
!$OMP THREADPRIVATE(t2m_week)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: t2m_month            !! Mean "monthly" (default 20 days) air temperature at 2 
                                                                         !! meter (K)
!$OMP THREADPRIVATE(t2m_month)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: t2m_longterm         !! Mean "Long term" (default 3 years) air temperature at 
                                                                         !! 2 meter (K) 
!$OMP THREADPRIVATE(t2m_longterm)
!pss:+
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: tsurf_year           !! "annual" surface temperatures (K)
!$OMP THREADPRIVATE(tsurf_year)
!pss:-

  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: t2m_min_daily        !! Daily minimum air temperature at 2 meter (K)
!spitfire
  ! daily maximum 2 meter temperatures (K)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: t2m_max_daily
 ! daily wind speed(m/s)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: wspeed_daily
!endspit
!$OMP THREADPRIVATE(t2m_min_daily)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: tsurf_daily          !! Daily surface temperatures (K)
!$OMP THREADPRIVATE(tsurf_daily)
!---
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: precip_daily         !! Daily precipitations sum @tex $(mm day^{-1})$ @endtex
!$OMP THREADPRIVATE(precip_daily)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: precip_lastyear      !! Last year's annual precipitation sum 
                                                                         !! @tex $??(mm year^{-1})$ @endtex
!$OMP THREADPRIVATE(precip_lastyear)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: precip_thisyear      !! This year's annual precipitation sum 
                                                                         !! @tex $??(mm year^{-1})$ @endtex 
!$OMP THREADPRIVATE(precip_thisyear)
!---
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: soilhum_daily        !! Daily soil humidity (0-1, unitless)
!$OMP THREADPRIVATE(soilhum_daily)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: soilhum_month        !! Soil humidity - integrated over a month (0-1, unitless) 
!$OMP THREADPRIVATE(soilhum_month)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: tsoil_daily          !! Daily soil temperatures (K)
!$OMP THREADPRIVATE(tsoil_daily)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: tsoil_month          !! Soil temperatures at each soil layer integrated over a
                                                                         !! month (K) 
!$OMP THREADPRIVATE(tsoil_month)
!--- 
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: litterhum_daily      !! Daily litter humidity (0-1, unitless)
!$OMP THREADPRIVATE(litterhum_daily)
!---
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: control_moist        !! Moisture control of heterotrophic respiration 
                                                                         !! (0-1, unitless)
!$OMP THREADPRIVATE(control_moist)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: control_temp         !! Temperature control of heterotrophic respiration at the
                                                                         !! different soil levels (0-1, unitless)
!$OMP THREADPRIVATE(control_temp)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: control_moist_daily  !! Moisture control of heterotrophic respiration daily 
                                                                         !! (0-1, unitless)
!$OMP THREADPRIVATE(control_moist_daily)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: control_temp_daily   !! Temperature control of heterotrophic respiration, above
                                                                         !! and below daily (0-1, unitless)
!$OMP THREADPRIVATE(control_temp_daily)
!---
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: gdd_init_date        !! inital date for gdd count 
!$OMP THREADPRIVATE(gdd_init_date)

  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: gdd_from_growthinit  !! gdd from beginning of season (C)
!$OMP THREADPRIVATE(gdd_from_growthinit)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: gdd0_lastyear        !! Last year's annual Growing Degree Days,
                                                                         !! threshold 0 deg C (K) 
!$OMP THREADPRIVATE(gdd0_lastyear)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: gdd0_thisyear        !! This year's annual Growing Degree Days,
                                                                         !! threshold 0 deg C (K)
!$OMP THREADPRIVATE(gdd0_thisyear)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: gdd_m5_dormance      !! Growing degree days for onset of growing season, 
                                                                         !! threshold -5 deg C (K)
!$OMP THREADPRIVATE(gdd_m5_dormance)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: gdd_midwinter        !! Growing degree days for onset of growing season, 
                                                                         !! since midwinter (K)
!$OMP THREADPRIVATE(gdd_midwinter)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: ncd_dormance         !! Number of chilling days since leaves were lost (days) 
!$OMP THREADPRIVATE(ncd_dormance)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: ngd_minus5           !! Number of growing days, threshold -5 deg C (days)
!$OMP THREADPRIVATE(ngd_minus5)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: hum_min_dormance     !! Minimum moisture during dormance (0-1, unitless) 
!$OMP THREADPRIVATE(hum_min_dormance)
!---
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: gpp_daily            !! Daily gross primary productivity per ground area 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex
!$OMP THREADPRIVATE(gpp_daily) 
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: gpp_week             !! Mean "weekly" (default 7 days) GPP  
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex
!$OMP THREADPRIVATE(gpp_week)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: maxgppweek_lastyear  !! Last year's maximum "weekly" GPP  
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex 
!$OMP THREADPRIVATE(maxgppweek_lastyear)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: maxgppweek_thisyear  !! This year's maximum "weekly" GPP  
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex  
!$OMP THREADPRIVATE(maxgppweek_thisyear)
!---
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: npp_daily            !! Daily net primary productivity per ground area 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex 
!$OMP THREADPRIVATE(npp_daily)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: npp_longterm         !! "Long term" (default 3 years) net primary productivity 
                                                                         !! per ground area  
                                                                         !! @tex $(gC m^{-2} year^{-1})$ @endtex   
!$OMP THREADPRIVATE(npp_longterm)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: npp_equil            !! Equilibrium NPP written to forcesoil 
                                                                         !! @tex $(gC m^{-2} year^{-1})$ @endtex
!$OMP THREADPRIVATE(npp_equil)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: npp_tot              !! Total NPP written to forcesoil 
                                                                         !! @tex $(gC m^{-2} year^{-1})$ @endtex 
!$OMP THREADPRIVATE(npp_tot)
!---
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: resp_maint_part_radia!! Maintenance respiration of different plant parts per 
                                                                         !! total ground area at Sechiba time step  
                                                                         !! @tex $(gC m^{-2} dt_sechiba^{-1})$ @endtex
!$OMP THREADPRIVATE(resp_maint_part_radia)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: resp_maint_part      !! Maintenance respiration of different plant parts per
                                                                         !! total ground area at Stomate time step 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex
!$OMP THREADPRIVATE(resp_maint_part)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: resp_maint_radia     !! Maintenance respiration per ground area at Sechiba time
                                                                         !! step   
                                                                         !! @tex $(gC m^{-2} dt_sechiba^{-1})$ @endtex
!$OMP THREADPRIVATE(resp_maint_radia)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: resp_maint_d         !! Maintenance respiration per ground area at Stomate time 
                                                                         !! step  
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex
!$OMP THREADPRIVATE(resp_maint_d)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: resp_growth_d        !! Growth respiration per ground area 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex
!$OMP THREADPRIVATE(resp_growth_d)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: resp_hetero_d        !! Heterotrophic respiration per ground area 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex
!$OMP THREADPRIVATE(resp_hetero_d)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: resp_hetero_radia    !! Heterothrophic respiration per ground area at Sechiba
                                                                         !! time step 
                                                                         !! @tex $(gC m^{-2} dt_sechiba^{-1})$ @endtex 
!$OMP THREADPRIVATE(resp_hetero_radia)
!---
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)     :: turnover_time       !! Turnover time of grasses 
                                                                         !! @tex $(dt_stomate^{-1})$ @endtex 
!$OMP THREADPRIVATE(turnover_time)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:,:) :: turnover_daily      !! Senescence-driven turnover (better: mortality) of 
                                                                         !! leaves and roots  
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex
!$OMP THREADPRIVATE(turnover_daily)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:,:) :: turnover_littercalc !! Senescence-driven turnover (better: mortality) of 
                                                                         !! leaves and roots at Sechiba time step 
                                                                         !! @tex $(gC m^{-2} dt_sechiba^{-1})$ @endtex 
!$OMP THREADPRIVATE(turnover_littercalc)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:,:) :: turnover_longterm   !! "Long term" (default 3 years) senescence-driven 
                                                                         !! turnover (better: mortality) of leaves and roots 
                                                                         !! @tex $(gC m^{-2} year^{-1})$ @endtex
!$OMP THREADPRIVATE(turnover_longterm)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:,:) :: bm_to_litter        !! Background (not senescence-driven) mortality of biomass
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex
!$OMP THREADPRIVATE(bm_to_litter)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:,:) :: bm_to_littercalc    !! conversion of biomass to litter per ground area at 
                                                                         !! Sechiba time step 
                                                                         !! @tex $(gC m^{-2} dt_sechiba^{-1})$ @endtex 
!$OMP THREADPRIVATE(bm_to_littercalc)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: dead_leaves          !! Metabolic and structural pools of dead leaves on ground
                                                                         !! per PFT @tex $(gC m^{-2})$ @endtex 
!$OMP THREADPRIVATE(dead_leaves)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:,:,:):: litter             !! Above and below ground metabolic and structural litter 
                                                                         !! per ground area 
                                                                         !! @tex $(gC m^{-2})$ @endtex 
!$OMP THREADPRIVATE(litter)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: litterpart           !! Fraction of litter above the ground belonging to 
                                                                         !! different litter pools (unitless)
!$OMP THREADPRIVATE(litterpart)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: firelitter           !! Total litter above the ground that could potentially 
                                                                         !! burn @tex $(gC m^{-2})$ @endtex 
!$OMP THREADPRIVATE(firelitter)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:,:):: soilcarbon_input     !! Quantity of carbon going into carbon pools from litter
                                                                         !! decomposition per ground area  at Sechiba time step 
                                                                         !! @tex $(gC m^{-2} dt_sechiba^{-1})$ @endtex 
!$OMP THREADPRIVATE(soilcarbon_input)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: soilcarbon_input_daily !! Daily quantity of carbon going into carbon pools from
                                                                           !! litter decomposition per ground area 
                                                                           !! @tex $(gC m^{-2} day^{-1})$ @endtex 
!$OMP THREADPRIVATE(soilcarbon_input_daily)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: carbon               !! Soil carbon pools per ground area: active, slow, or 
                                                                         !! passive, @tex $(gC m^{-2})$ @endtex 
!$OMP THREADPRIVATE(carbon)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: lignin_struc         !! Ratio Lignine/Carbon in structural litter for above and
                                                                         !! below ground compartments (unitless)
!$OMP THREADPRIVATE(lignin_struc)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: lm_lastyearmax       !! Last year's maximum leaf mass per ground area for each
                                                                         !! PFT @tex $(gC m^{-2})$ @endtex  
!$OMP THREADPRIVATE(lm_lastyearmax)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: lm_thisyearmax       !! This year's maximum leaf mass per ground area for each
                                                                         !! PFT @tex $(gC m^{-2})$ @endtex  
!$OMP THREADPRIVATE(lm_thisyearmax)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: maxfpc_lastyear      !! Last year's maximum fpc for each natural PFT, on ground
                                                                         !! [??CHECK] fpc but this ones look ok (computed in 
                                                                         !! season, used in light)?? 
!$OMP THREADPRIVATE(maxfpc_lastyear)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: maxfpc_thisyear      !! This year's maximum fpc for each PFT, on ground (see 
                                                                         !! stomate_season), [??CHECK] fpc but this ones look ok 
                                                                         !! (computed in season, used in light)??
!$OMP THREADPRIVATE(maxfpc_thisyear)
!---
  REAL(r_std), ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: leaf_age             !! Age of different leaf classes (days)
!$OMP THREADPRIVATE(leaf_age)
  REAL(r_std), ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: leaf_frac            !! PFT fraction of leaf mass in leaf age class (0-1, 
                                                                         !! unitless) 
!$OMP THREADPRIVATE(leaf_frac)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: when_growthinit      !! Days since beginning of growing season (days)
!$OMP THREADPRIVATE(when_growthinit)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: herbivores           !! Time constant of probability of a leaf to be eaten by a
                                                                         !! herbivore (days)
!$OMP THREADPRIVATE(herbivores)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: RIP_time             !! How much time ago was the PFT eliminated for the last 
                                                                         !! time (year)
!$OMP THREADPRIVATE(RIP_time)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: time_hum_min         !! Time elapsed since strongest moisture limitation (days) 
!$OMP THREADPRIVATE(time_hum_min)
!---
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: clay_fm              !! Soil clay content (0-1, unitless), parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: clay_fm_g            !! Soil clay content (0-1, unitless), parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: precip_fm            !! Daily precipitations sum @tex $(mm day^{-1})$ @endtex,
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: precip_fm_g          !! Daily precipitations sum @tex $(mm day^{-1})$ @endtex,
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: litterhum_daily_fm   !! Daily relative humidity of litter (0-1, unitless), 
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: litterhum_daily_fm_g !! Daily relative humidity of litter (0-1, unitless), 
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: t2m_daily_fm         !! Daily air temperature at 2 meter (K), parallel 
                                                                         !! computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: t2m_daily_fm_g       !! Daily air temperature at 2 meter (K), parallel 
                                                                         !! computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: t2m_min_daily_fm     !! Daily minimum air temperature at 2 meter (K), 
  !spitfire
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)      :: t2m_max_daily_fm
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)      :: wspeed_daily_fm
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)      :: t2m_max_daily_fm_g
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)      :: wspeed_daily_fm_g
  !endspit
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: t2m_min_daily_fm_g   !! Daily minimum air temperature at 2 meter (K), 
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: tsurf_daily_fm       !! Daily surface temperatures (K), parallel 
                                                                         !! computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)         :: tsurf_daily_fm_g     !! Daily surface temperatures (K), parallel 
                                                                         !! computing 
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: tsoil_daily_fm       !! Daily soil temperatures (K), parallel computing 
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: tsoil_daily_fm_g     !! Daily soil temperatures (K), parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: soilhum_daily_fm     !! Daily soil humidity (0-1, unitless), parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: soilhum_daily_fm_g   !! Daily soil humidity (0-1, unitless), parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: humrel_daily_fm      !! Daily relative humidity of atmosphere (0-1, unitless), 
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: humrel_daily_fm_g    !! Daily relative humidity of atmosphere (0-1, unitless), 
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: gpp_daily_fm         !! Daily gross primary productivity per ground area 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex, 
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: gpp_daily_fm_g       !! Daily gross primary productivity per ground area 
                                                                         !! @tex $(gC m^{-2} day^{-1})$ @endtex, 
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: veget_fm             !! Vegetation coverage taking into account non-biological
                                                                         !! coverage (unitless), parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: veget_fm_g           !! Vegetation coverage taking into account non-biological
                                                                         !! coverage (unitless), parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: veget_max_fm         !! Maximum vegetation coverage taking into account 
                                                                         !! non-biological coverage (unitless), parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: veget_max_fm_g       !! Maximum vegetation coverage taking into account none 
                                                                         !! biological coverage (unitless), parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: lai_fm               !! Leaf area index @tex $@tex $(m^2 m^{-2})$ @endtex$ @endtex, 
                                                                         !! parallel computing
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)       :: lai_fm_g             !! Leaf area index @tex $@tex $(m^2 m^{-2})$ @endtex$ @endtex, 
                                                                         !! parallel computing
!! CROPS
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)       :: swdown_fm              !! downward shortwave radiation
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)       :: swdown_fm_g            !! downward shortwave radiation
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)       :: t2mdiag_fm             !! 2m air temperature (K) of daily maximum 
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)       :: t2mdiag_fm_g           !! 2m air temperature (K) of daily maximum
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)       :: evapot_corr_fm         !! potential evaportranspiration (mm)

  REAL(r_std),ALLOCATABLE,DIMENSION(:,:)       :: evapot_corr_fm_g       !! potential evaportranspiration (mm)
!!! CROPS
!---
  !glcc
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:)                     :: glcc_pft

  !spitfire
  ! Nesterov index accumulated
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:)                     :: ni_acc
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:)                     :: fire_numday
  ! fuel classes (1, 10, 100, 1000 hours)
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: fuel_1hr
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: fuel_10hr
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: fuel_100hr
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: fuel_1000hr
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: def_fuel_1hr_remain
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: def_fuel_10hr_remain
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: def_fuel_100hr_remain
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: def_fuel_1000hr_remain
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:)                    :: lcc
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:)                    :: bafrac_deforest_accu
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: emideforest_litter_accu
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: emideforest_biomass_accu
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:,:)              :: deforest_litter_remain
  REAL(r_std),ALLOCATABLE,SAVE, DIMENSION(:,:,:,:)                :: deforest_biomass_remain
  !endspit
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: co2_fire             !! Carbon emitted to the atmosphere by burning living 
                                                                         !! and dead biomass 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex 
!$OMP THREADPRIVATE(co2_fire)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: co2_to_bm_dgvm       !! Psuedo-photosynthesis,C used to provide seedlings with
                                                                         !! an initial biomass, arbitrarily removed from the 
                                                                         !! atmosphere  
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex 
!$OMP THREADPRIVATE(co2_to_bm_dgvm)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: co2_flux_daily       !! Daily net CO2 flux between atmosphere and biosphere 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex
                                                                         !! [??CHECK] sign convention?
!$OMP THREADPRIVATE(co2_flux_daily)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: co2_flux_monthly     !! Monthly net CO2 flux between atmosphere and biosphere 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex
                                                                         !! [??CHECK] sign convention? 
!$OMP THREADPRIVATE(co2_flux_monthly)

!pss:+
!density flux of methane calculated for entire pixel (gCH4/dt/m**2)
!pour wetland avc Water Table Depth (WTD) = 0
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_tot_0
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_dif_0
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_bub_0
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_pla_0
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)  :: uo_0 !concentration dim = (npts,n)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)  :: uold2_0 !concentration au pas de temps precedent
!pour wetland avc WTD = -x1
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_tot_wet1
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_dif_wet1
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_bub_wet1
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_pla_wet1
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)  :: uo_wet1
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)  :: uold2_wet1
!pour wetland avc WTD = -x2
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_tot_wet2
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_dif_wet2
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_bub_wet2
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_pla_wet2
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)  :: uo_wet2
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)  :: uold2_wet2
!pour wetland avc WTD = -x3
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_tot_wet3
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_dif_wet3
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_bub_wet3
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_pla_wet3
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)  :: uo_wet3
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)  :: uold2_wet3
!pour wetland avc WTD = -x4
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_tot_wet4
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_dif_wet4
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_bub_wet4
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:) :: ch4_flux_density_pla_wet4
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)  :: uo_wet4
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)  :: uold2_wet4

!pss:-

  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)    :: prod10             !! Wood products remaining in the 10 year-turnover pool 
                                                                         !! after the annual release for each compartment 
                                                                         !! @tex $(gC m^{-2})$ @endtex    
                                                                         !! (0:10 input from year of land cover change),
                                                                         !! dimension(#pixels,0:10 years
!$OMP THREADPRIVATE(prod10)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)    :: prod100          !! Wood products remaining in the 100 year-turnover pool
                                                                         !! after the annual release for each compartment
                                                                         !! @tex $(gC m^{-2})$ @endtex  
                                                                         !! (0:100 input from year of land cover change), 
                                                                         !! dimension(#pixels,0:100 years)
!$OMP THREADPRIVATE(prod100)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)    :: flux10           !! Wood decomposition from the 10 year-turnover pool 
                                                                         !! compartments 
                                                                         !! @tex $(gC m^{-2} year^{-1})$ @endtex 
                                                                         !! dimension(#pixels,0:10)  
!$OMP THREADPRIVATE(flux10)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)    :: flux100          !! Wood decomposition from the 100 year-turnover pool 
                                                                         !! compartments 
                                                                         !! @tex $(gC m^{-2} year^{-1})$ @endtex
                                                                         !! dimension(#pixels,0:100)
!$OMP THREADPRIVATE(flux100)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)      :: convflux         !! Release during first year following land cover change 
                                                                         !! (paper, burned, etc...) 
                                                                         !! @tex $(gC m^{-2} year^{-1})$ @endtex  
!$OMP THREADPRIVATE(convflux)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)      :: cflux_prod10     !! Total annual release from the 10 year-turnover pool
                                                                         !! sum of flux10  
                                                                         !! @tex $(gC m^{-2} year^{-1})$ @endtex
!$OMP THREADPRIVATE(cflux_prod10)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)      :: cflux_prod100    !! Total annual release from the 100 year-turnover pool 
                                                                         !! sum of flux100 
                                                                         !! @tex $(gC m^{-2} year^{-1})$ @endtex
!$OMP THREADPRIVATE(cflux_prod100)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: harvest_above        !! Harvest of above ground biomass for agriculture -not 
                                                                         !! just from land use change 
                                                                         !! @tex $(??gC m^{-2} dt_stomate^{-1})$ @endtex
!$OMP THREADPRIVATE(harvest_above)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: carb_mass_total      !! Total on-site and off-site C pool 
                                                                         !! @tex $(??gC m^{-2})$ @endtex                        
!$OMP THREADPRIVATE(carb_mass_total)
  LOGICAL, PARAMETER                                 :: do_daily_permafrost = .TRUE.  !! do we do the permafrost calcs daily (if false then each TS)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: deepC_a          !! deep active carbon profile
!$OMP THREADPRIVATE(deepC_a)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: deepC_s          !! deep slow carbon profile
!$OMP THREADPRIVATE(deepC_s)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: deepC_p          !! deep passive carbon profile
!$OMP THREADPRIVATE(deepC_p)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: O2_soil          !! deep oxygen
!$OMP THREADPRIVATE(O2_soil)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: CH4_soil         !! deep methane
!$OMP THREADPRIVATE(CH4_soil)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: O2_snow          !! snow oxygen
!$OMP THREADPRIVATE(O2_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: CH4_snow         !! snow methane
!$OMP THREADPRIVATE(CH4_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: tdeep_daily      !! daily t profile
!$OMP THREADPRIVATE(tdeep_daily)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: hsdeep_daily     !! daily t profile
!$OMP THREADPRIVATE(hsdeep_daily)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)       :: temp_sol_daily   !! daily soil surface temp
!$OMP THREADPRIVATE(temp_sol_daily)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)       :: pb_pa_daily      !! daily surface pressure [Pa]
!$OMP THREADPRIVATE(pb_pa_daily)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)       :: snow_daily       !! daily snow mass
!$OMP THREADPRIVATE(snow_daily)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: fbact
!$OMP THREADPRIVATE(fbact)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: fbact_daily
!$OMP THREADPRIVATE(fbact_daily)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: prmfrst_soilc_tempctrl
!$OMP THREADPRIVATE(prmfrst_soilc_tempctrl)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: prmfrst_soilc_tempctrl_daily
!$OMP THREADPRIVATE(prmfrst_soilc_tempctrl_daily)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)     :: fixed_cryoturbation_depth  !! depth to hold cryoturbation to for fixed runs
!$OMP THREADPRIVATE(fixed_cryoturbation_depth)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)     :: snowdz_daily       !! daily snow depth profile
!$OMP THREADPRIVATE(snowdz_daily)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)     :: snowrho_daily      !! daily snow density profile
!$OMP THREADPRIVATE(snowrho_daily)
  !below are the arrays that need to be written to the permafrost carbon spinup
  !file
  REAL(r_std),DIMENSION(:,:,:,:),ALLOCATABLE  :: soilcarbon_input_2pfcforcing
!$OMP THREADPRIVATE(soilcarbon_input_2pfcforcing)
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE      :: pb_2pfcforcing
!$OMP THREADPRIVATE(pb_2pfcforcing)
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE      :: snow_2pfcforcing
!$OMP THREADPRIVATE(snow_2pfcforcing)
  REAL(r_std),DIMENSION(:,:,:,:),ALLOCATABLE  :: tprof_2pfcforcing
!$OMP THREADPRIVATE(tprof_2pfcforcing)
  REAL(r_std),DIMENSION(:,:,:,:),ALLOCATABLE  :: fbact_2pfcforcing
!$OMP THREADPRIVATE(fbact_2pfcforcing)
  REAL(r_std),DIMENSION(:,:,:,:),ALLOCATABLE  :: hslong_2pfcforcing
!$OMP THREADPRIVATE(hslong_2pfcforcing)
  REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE    :: veget_max_2pfcforcing
!$OMP THREADPRIVATE(veget_max_2pfcforcing)
  REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE    :: rprof_2pfcforcing
!$OMP THREADPRIVATE(rprof_2pfcforcing)
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE      :: tsurf_2pfcforcing
!$OMP THREADPRIVATE(tsurf_2pfcforcing)
  REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE    :: snowdz_2pfcforcing
!$OMP THREADPRIVATE(snowdz_2pfcforcing)
  REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE    :: snowrho_2pfcforcing
!$OMP THREADPRIVATE(snowrho_2pfcforcing)

!---
  REAL(r_std), SAVE                              :: tau_longterm
!$OMP THREADPRIVATE(tau_longterm)
  REAL(r_std),SAVE                               :: dt_days=zero         !! Time step of STOMATE (days) 
!$OMP THREADPRIVATE(dt_days)
  INTEGER(i_std),SAVE                            :: date=0               !! Date (days) 
!$OMP THREADPRIVATE(date)
  INTEGER(i_std),ALLOCATABLE,SAVE,DIMENSION(:)   :: nforce               !! Number of states calculated for the soil forcing 
                                                                         !! variables (unitless), dimension(::nparan*::nbyear) both 
                                                                         !! given in the run definition file    
!$OMP THREADPRIVATE(nforce)
  INTEGER(i_std),ALLOCATABLE,SAVE,DIMENSION(:)   :: isf                  !! Index for number of time steps that can be stored in 
                                                                         !! memory (unitless), dimension (#nsfm)
!$OMP THREADPRIVATE(isf)
  INTEGER(i_std),ALLOCATABLE,SAVE,DIMENSION(:)   :: nf_cumul             !! Number of years over which the average is calculated in
                                                                         !! forcesoil when cumul flag is set, dimension (#nsft)
                                                                         !! [??CHECK] definition the dimension is number of 
                                                                         !! timesteps in a year?
!$OMP THREADPRIVATE(nf_cumul)
  INTEGER(i_std), SAVE                           :: spinup_period        !! Period of years used to calculate the resolution of the system for spinup analytic. 
                                                                         !! This period correspond in most cases to the period of years of forcing data used
  INTEGER,PARAMETER                              :: r_typ = nf90_real4   !! Specify data format (server dependent)
  LOGICAL,ALLOCATABLE,SAVE,DIMENSION(:)          :: nf_written           !! Flag indicating whether the forcing data have been 
                                                                         !! written
!$OMP THREADPRIVATE(nf_written)
!---
  LOGICAL, SAVE                                  :: do_slow=.FALSE.      !! Flag that determines whether stomate_accu calculates
                                                                         !! the sum(do_slow=.FALSE.) or the mean 
                                                                         !! (do_slow=.TRUE.)
!$OMP THREADPRIVATE(do_slow)
  LOGICAL, SAVE                                  :: EndOfYear=.FALSE.    !! Update annual variables? This variable must be 
                                                                         !! .TRUE. once a year
!$OMP THREADPRIVATE(EndOfYear)
  LOGICAL, SAVE                                  :: EndOfMonth=.FALSE.   !! Update monthly variables? This variable must be 
                                                                         !!.TRUE. once a month 
!$OMP THREADPRIVATE(EndOfMonth)
  LOGICAL, SAVE                                  :: l_first_stomate = .TRUE.!! Is this the first call of stomate?
!$OMP THREADPRIVATE(l_first_stomate)
  LOGICAL, SAVE                                  :: cumul_forcing=.FALSE.!! flag for cumul of forcing if teststomate
!$OMP THREADPRIVATE(cumul_forcing)
  LOGICAL, SAVE                                  :: cumul_Cforcing=.FALSE.  !! Flag, if internal parameter cumul_Cforcing is 
                                                                            !! TRUE then ::nbyear (defined in run definition 
                                                                            !! file will be forced to 1 later in this module. If 
                                                                            !! FALSE the mean over ::nbyear is written in forcesoil
!$OMP THREADPRIVATE(cumul_Cforcing)
!---   
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: harvest_above_monthly   !! [??CHECK] post-processing - should be removed?
!$OMP THREADPRIVATE(harvest_above_monthly)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)      :: cflux_prod_monthly      !! [??CHECK] post-processing - should be removed?
!$OMP THREADPRIVATE(cflux_prod_monthly)
!---
  INTEGER(i_std), SAVE                               :: global_years        !! Global counter of years (year)
!$OMP THREADPRIVATE(global_years)
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:)           :: ok_equilibrium      !! Logical array marking the points where the resolution is ok 
                                                                            !! (true/false)
!$OMP THREADPRIVATE(ok_equilibrium)
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:)           :: carbon_eq           !! Logical array to mark the carbon pools at equilibrium ? 
                                                                            !! If true, the job stops. (true/false)
!$OMP THREADPRIVATE(carbon_eq)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)       :: nbp_accu            !! Accumulated Net Biospheric Production over the year (gC.m^2 )
!$OMP THREADPRIVATE(nbp_accu)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)       :: nbp_flux            !! Net Biospheric Production (gC.m^2.day^{-1})
!$OMP THREADPRIVATE(nbp_flux)
  REAL(r_std), ALLOCATABLE, DIMENSION(:,:,:,:)       :: matrixA             !! Matrix containing the fluxes between the carbon pools
                                                                            !! per sechiba time step 
                                                                            !! @tex $(gC.m^2.day^{-1})$ @endtex
!$OMP THREADPRIVATE(matrixA)
  REAL(r_std), ALLOCATABLE, DIMENSION(:,:,:)         :: vectorB             !! Vector containing the litter increase per sechiba time step
                                                                            !! @tex $(gC m^{-2})$ @endtex
!$OMP THREADPRIVATE(vectorB)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: MatrixV             !! Matrix containing the accumulated values of matrixA 
!$OMP THREADPRIVATE(MatrixV)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: VectorU             !! Matrix containing the accumulated values of VectorB
!$OMP THREADPRIVATE(VectorU)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:,:) :: MatrixW             !! Matrix containing the opposite of matrixA
!$OMP THREADPRIVATE(MatrixW)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: previous_stock      !! Array containing the carbon stock calculated by the analytical
                                                                            !! method in the previous resolution
!$OMP THREADPRIVATE(previous_stock)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: current_stock       !! Array containing the carbon stock calculated by the analytical
                                                                            !! method in the current resolution 
!$OMP THREADPRIVATE(current_stock)
  REAL(r_std), SAVE                                  :: eps_carbon          !! Stopping criterion for carbon pools (unitless,0-1)
!$OMP THREADPRIVATE(eps_carbon)
! gmjc
! for grazing litter
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: litter_avail
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: litter_not_avail
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)  :: litter_avail_frac
!
!  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:) :: resp_hetero_soil_part
!  ! heterotrophic respiration for soil pool (gC/day/m**2 of total ground)
!  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:,:)    :: resp_hetero_soil_d
!  ! heterotrophic respiration for litter (gC/day/m**2 of total ground)
!  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: resp_hetero_litter_d
  ! N fertilization factor controlling Vcmax and SLA
  REAL(r_std), DIMENSION(:,:), ALLOCATABLE, SAVE :: N_limfert
  ! specific leaf area (m2/gC)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)    :: sla_calc
  ! "14days" 2 meter temperatures (K)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)        :: t2m_14
  REAL(r_std ), ALLOCATABLE, SAVE, DIMENSION(:,:)  :: wshtotsum !! yield from autogestion=1 offer import_yield for autogestion=2
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: sr_ugb !!Optimised stocking density (animal m-2)
  REAL(r_std)     , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: compt_ugb !!Counter of grazing days (d)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: nb_ani
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: grazed_frac
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: import_yield
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:,:)      :: sla_age1
  REAL(r_std), DIMENSION(:,:), ALLOCATABLE, SAVE :: when_growthinit_cut
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: nb_grazingdays
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)        :: snowfall_daily
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)        :: snowmass_daily
! top 5 layer grassland soil moisture for grazing
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: tmc_topgrass_daily      !! Daily top 5 layer soil moisture (m^3 m^-3)
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: after_snow
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: after_wet
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: wet1day
  REAL(r_std),ALLOCATABLE,SAVE,DIMENSION(:)      :: wet2day
! end gmjc 
  REAL(r_std),SAVE                                   :: dt_forcesoil        !! Time step of soil forcing file (days)
!$OMP THREADPRIVATE(dt_forcesoil)
  INTEGER(i_std),PARAMETER                           :: nparanmax=366       !! Maximum number of time steps per year for forcesoil
  INTEGER(i_std),SAVE                                :: nparan              !! Number of time steps per year for forcesoil read from run definition (unitless) 
!$OMP THREADPRIVATE(nparan)
  INTEGER(i_std),SAVE                                :: nbyear=1            !! Number of years saved for forcesoil (unitless) 
!$OMP THREADPRIVATE(nbyear)
  INTEGER(i_std),SAVE                                :: iatt                !! Time step of forcing of soil processes (iatt = 1 to ::nparan*::nbyear) 
!$OMP THREADPRIVATE(iatt)
  INTEGER(i_std),SAVE                                :: iatt_old=1          !! Previous ::iatt
!$OMP THREADPRIVATE(iatt_old)
  INTEGER(i_std),SAVE                                :: nsfm                !! Number of time steps that can be stored in memory (unitless) 
!$OMP THREADPRIVATE(nsfm)
  INTEGER(i_std),SAVE                                :: nsft                !! Number of time steps in a year (unitless)
!$OMP THREADPRIVATE(nsft)
  INTEGER(i_std),SAVE                                :: iisf                !! Current pointer for teststomate (unitless)
!$OMP THREADPRIVATE(iisf)
  INTEGER(i_std),SAVE                                :: Cforcing_id         !! File identifer of file 2
!$OMP THREADPRIVATE(Cforcing_id)    
  INTEGER(i_std),PARAMETER                           :: ndm = 10            !! Maximum number of dimensions (unitless)

  CHARACTER(LEN=100), SAVE                           :: Cforcing_permafrost_name !! Name of permafrost forcing file
!$OMP THREADPRIVATE(Cforcing_permafrost_name)

  INTEGER(i_std), SAVE                               :: frozen_respiration_func
!$OMP THREADPRIVATE(frozen_respiration_func)

!!!!! crop

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ! variables used  and should be saved in stomate module, especially for driving STICS LAIdev
  
  ! day_counter for STICS
  
  INTEGER(i_std),SAVE                              :: tday_counter=0       ! count time step each day
  INTEGER(i_std),SAVE                              :: vday_counter=0       ! count time step each day, but we do not reset it to 0 at the end of year, to be used in grain.f90

  ! control of initialization
  ! first call for initialization
  LOGICAL, SAVE       :: f_crop_init = .TRUE.
  ! flag of recycle
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:, :)       :: f_crop_recycle 
  ! identifier for in cycle or not
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:, :)       :: in_cycle 
  ! flag for writing lai at the beginning of SEN stage
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:, :)       :: f_sen_lai 

  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:, :)       :: f_rot_stom
  ! flag for starting the rotation for kjpindex, nvm
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: cyc_num
  ! number of current rotation cycle
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: cyc_num_tot
  ! number of all rotation cycle
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: rot_cmd_store
!  ! the rotation command matrix: xxxyyzz, xxx: % land fraction change, yy:
!  ! source PFT, zz: destination PFT,  dimensions: kjpindex, rot_cmd_max, cyc_rot_max

  !

  ! daily average 2 meter temperatures (degree)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)      :: st2m_daily

  ! daily minimum 2 meter temperatures (not in K but in degree)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)      :: st2m_min_daily
  ! daily maximum 2 meter temperatures (degree)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)      :: st2m_max_daily

!  ! daily maximum 2 meter temperatures (K)
!  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)      :: t2m_max_daily
  ! daily value of soil temperature at the resolution of 1 cm, the second dimension is nvm, the third is  3
  ! the three layers around sowing layer
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :, :)    :: wut_cm_daily
  ! daily mean value of soil relative humidity at the resolution of 1 cm, the second dimension is nvm, the third  is 3
  ! the three layers around sowing layer
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :, :)    :: wus_cm_daily
  ! daily potential evapotranspiration 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)      :: evapot_daily
  ! biomass of previous day, t/ha
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)      :: pdbiomass
  ! aboveground biomass of previous day, t/ha
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)      :: pdmasec
  ! vegetative biomass
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)      :: masecveg   
  ! aboveground dry matter (t ha-1) 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)      :: masec
  ! growth rate of plant, it means the delta total biomass increment (t carbon ha-1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)      :: dltams
  ! daily gdh calculated according to halfhourly temperature // transmitted from stomate.f90 gdh_daily
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)         :: gdh_daily 
  ! Photoperiod // hours
  REAL(r_std),  ALLOCATABLE, SAVE, DIMENSION(:)                             :: phoi 

  !  
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:, :)           :: onarretesomcourdrp 
  !INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:)                           :: codeulaivernal 
!  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)                :: nlevobs    ! the following variables ended with obs are only used for forcing simulation.  
!  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)                :: namfobs    ! the initial value should be always 999
!  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)                :: nfloobs  
!  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)                :: nlanobs  
!  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)                :: nlaxobs  
!  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)                :: nmatobs  
!  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)                :: nrecobs  
!  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)                :: nsenobs  
!  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)                :: ndrpobs  

  ! LAIdev SPECIFIC 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nsendltams
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nsendltai
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nsenpfeuilverte
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nsendurvie
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nsenndurvie
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: densiteequiv
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nplt
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: tursla
  ! sla calculated in STICS
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: ssla          
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: pfeuilverte
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: bsenlai
  
  ! variables are involved in DEVELOPMENT

  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: zrac
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nrec
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nlan
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: tcult
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: udevair
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: udevcult
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: ndrp
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: rfvi
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nlev
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nger
  logical,    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: etatvernal
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: caljvc
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: rfpi
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: upvt
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: utp
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: somcour
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: somcourdrp
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: somcourutp
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: tdevelop
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: somtemp
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: somcourfauche
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: stpltger
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: R_stamflax
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: R_stlaxsen
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: R_stsenlan
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: stlevflo
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nflo
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: R_stlevdrp
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: R_stflodrp
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: R_stdrpmat
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nmat
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nlax
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nrecbutoir
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: group
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: ndebdes
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: R_stdrpdes
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: densite
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: densitelev
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: coeflev
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: densiteger
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: somelong
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: somger
  logical,    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: humectation
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nbjhumec
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: somtemphumec
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: stpltlev
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: namf
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: stmatrec
  
  ! these variables are involved in Lai_calculation
   
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: tustress
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: slai
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: somfeuille
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: pdlai
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nbfeuille
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: reajust
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: ulai
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: pdulai
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: efdensite
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: tempeff
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nstopfeuille
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: deltai
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: svmax
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: nsen
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: laisen
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: pdlaisen
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)        :: dltaisenat
  REAL(r_std),  ALLOCATABLE, SAVE, DIMENSION(:,:,:,:)           :: histgrowthset
  INTEGER(i_std),ALLOCATABLE, SAVE, DIMENSION(:,:)              :: hist_sencourset
  INTEGER(i_std),ALLOCATABLE, SAVE, DIMENSION(:,:)              :: hist_latestset
  INTEGER(i_std),ALLOCATABLE, SAVE, DIMENSION(:,:)              :: doyhiststset
  

  ! these variables are involved in the LAIsenescence

  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: nsencour
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: dltamsen
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: dltaisen
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: fgellev
  logical,    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: gelee
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: fstressgel
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: R_stlevamf
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: dernier_n
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: durvieI
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: durvie
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: ndebsen
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: somsenreste

  ! the compartment senescence module
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: box_ndays
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)      :: box_lai
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)      :: box_lairem
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)      :: box_tdev
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)      :: box_biom
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)      :: box_biomrem
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)      :: box_durage
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)      :: box_somsenbase
  INTEGER(i_std), SAVE                                  :: nboxmax
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)        :: box_ulai ! nvm nobxmax

  ! these variables are involved in STRESS calculation
  
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: shumrel  ! humidity
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: swfac    ! 
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: turfac
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: senfac

  ! these variables are involved in CARBON ALLOCATION PROCESSES
 
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: mafeuiljaune !  Dry matter of yellow leaves // t.ha-1
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: msneojaune  ! ! Newly-formed senescent dry matter  // t.ha-1
 
 
  ! these variables are involved in the CARBON ALLOCATION calculation

  ! grain related   
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :, :)      :: v_dltams
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: fgelflo
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: pdircarb
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: ircarb
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: nbgrains
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: pgrain
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: vitmoy
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: nbgraingel
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: pgraingel
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: dltags
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: ftempremp
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: magrain          ! grain yield from STICS, unit with t/ha
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: pdmagrain
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: nbj0remp 
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: pdsfruittot
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: deltgrain

  ! reprac related

  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: repracmax
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: repracmin
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: kreprac
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: somtemprac
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: urac
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: reprac
  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: nstoprac

  ! carbon calculation from STICS
  
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: c_reserve
!$OMP THREADPRIVATE(c_reserve)
  REAL(r_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: c_leafb
!$OMP THREADPRIVATE(c_leafb)

  ! control the growing season length to avoid a too long growing season 

  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: gslen
!$OMP THREADPRIVATE(gslen)
 
  ! control the drying process of grain 

  INTEGER(i_std),    ALLOCATABLE, SAVE, DIMENSION(:, :)      :: drylen 
!$OMP THREADPRIVATE(drylen)

!! CROP spec, xuhui
  INTEGER(i_std),ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: plantdate !! kjpindex, nvm, cyc_rot_max
!$OMP THREADPRIVATE(plantdate)
  INTEGER(i_std),ALLOCATABLE, SAVE, DIMENSION (:,:)  :: plantdate_now !! kjpindex, nvm, plantdate of current cycle
!$OMP THREADPRIVATE(plantdate_now)
  !LOGICAL,SAVE                                      :: iplt_1d
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:, :)       :: N_add
!$OMP THREADPRIVATE(N_add)
!! overlapping with the definition introuded by GM

!!!! Planting Date of each crop type each site

!!!!! end crop, xuhui

 
PUBLIC clay_fm, humrel_daily_fm, litterhum_daily_fm, t2m_daily_fm, &
   & t2m_min_daily_fm, tsurf_daily_fm, tsoil_daily_fm, soilhum_daily_fm, &
   !spitfire
   & t2m_max_daily_fm, wspeed_daily_fm, &
   !endspit
   & precip_fm, gpp_daily_fm, veget_fm, veget_max_fm, lai_fm
PUBLIC t2mdiag_fm, swdown_fm, evapot_corr_fm
PUBLIC  dt_days, date, do_slow, EndOfYear
PUBLIC isf, nf_written
PUBLIC tday_counter, vday_counter, ssla

CONTAINS
  

!! ================================================================================================================================
!! SUBROUTINE 	: stomate_initialize
!!
!>\BRIEF        Initialization routine for stomate module. 
!!
!! DESCRIPTION  : Initialization routine for stomate module. Read options from parameter file, allocate variables, read variables 
!!                from restart file and initialize variables if necessary. 
!!                
!! \n
!_ ================================================================================================================================

SUBROUTINE stomate_initialize &
        (kjit,           kjpij,             kjpindex,                        &
         rest_id_stom,   hist_id_stom,      hist_id_stom_IPCC,               &
         index1,          lalo,              neighbours,   resolution,        &
         contfrac,       totfrac_nobio,     clay,         t2m,               &
         lai,            veget,             veget_max,                       &
         co2_flux,       fco2_lu,                                            &
         deadleaf_cover, assim_param,       thawed_humidity, depth_organic_soil, &
         soilc_total, heat_Zimov,           temp_growth, altmax)

    IMPLICIT NONE
    !! 0. Variable and parameter declaration
    !! 0.1 Input variables
    INTEGER(i_std),INTENT(in)                       :: kjit              !! Time step number (unitless)
    INTEGER(i_std),INTENT(in)                       :: kjpij             !! Total size of the un-compressed grid (unitless)
    INTEGER(i_std),INTENT(in)                       :: kjpindex          !! Domain size - terrestrial pixels only (unitless)
    INTEGER(i_std),INTENT(in)                       :: rest_id_stom      !! STOMATE's _Restart_ file identifier (unitless)
    INTEGER(i_std),INTENT(in)                       :: hist_id_stom      !! STOMATE's _history_ file identifier (unitless)
    INTEGER(i_std),INTENT(in)                       :: hist_id_stom_IPCC !! STOMATE's IPCC _history_ file identifier(unitless) 
    INTEGER(i_std),DIMENSION(kjpindex),INTENT(in)   :: index1             !! The indices of the terrestrial pixels only (unitless) 
!!!! xuhui: change index to index1 in stomate_initialize because index overwrites the INDEX function of FORTRAN
    REAL(r_std),DIMENSION(kjpindex,2),INTENT(in)    :: lalo              !! Geographical coordinates (latitude,longitude) for pixels (degrees) 
    INTEGER(i_std),DIMENSION(kjpindex,NbNeighb),INTENT(in) :: neighbours !! Neighoring grid points if land for the DGVM (unitless) 
    REAL(r_std),DIMENSION(kjpindex,2),INTENT(in)    :: resolution        !! Size in x an y of the grid (m) - surface area of the gridbox 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)   :: contfrac          !! Fraction of continent in the grid cell (unitless)
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: totfrac_nobio     !! Fraction of grid cell covered by lakes, land ice, cities, ... (unitless) 
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: clay              !! Clay fraction of soil (0-1, unitless)
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: t2m               !! 2 m air temperature (K)
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)  :: lai               !! Leaf area inex @tex $(m^2 m^{-2})$ @endtex
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)  :: veget             !! Fraction of vegetation type including 
                                                                         !! non-biological fraction (unitless) 
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)  :: veget_max         !! Maximum fraction of vegetation type including 
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(out)  :: altmax
                                                                         !! non-biological fraction (unitless) 
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),   INTENT (out)    :: heat_Zimov !! heating associated with decomposition

    !! 0.2 Output variables
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(out) :: co2_flux          !! CO2 flux between atmosphere and biosphere
    REAL(r_std),DIMENSION(kjpindex),INTENT(out)     :: fco2_lu           !! CO2 flux between atmosphere and biosphere from land-use (without forest management)  
                                                                         !! @tex ($gC m^{-2} dt_stomate^{-1}$) @endtex
    REAL(r_std),DIMENSION(kjpindex),INTENT(out)     :: deadleaf_cover    !! Fraction of soil covered by dead leaves (unitless)
    REAL(r_std),DIMENSION(kjpindex,nvm,npco2),INTENT(out) :: assim_param !! min+max+opt temperatures (K) & vmax for photosynthesis  
                                                                         !! @tex $(\mu mol m^{-2}s^{-1})$ @endtex 
    REAL(r_std),DIMENSION(kjpindex),INTENT(out)     :: temp_growth       !! Growth temperature (Â°C)  
                                                                         !! Is equal to t2m_month 
    !! 0.3 Modified variables 
    REAL(r_std), DIMENSION(kjpindex),   INTENT (inout)            :: thawed_humidity    !! specified humidity of thawed soil
    REAL(r_std), DIMENSION(kjpindex),   INTENT (inout)            :: depth_organic_soil !! how deep is the organic soil?
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),   INTENT (inout)  :: soilc_total        !! total soil carbon for use in thermal calcs

    !! 0.4 Local variables
    REAL(r_std)                                   :: dt_days_read             !! STOMATE time step read in restart file (days)
    INTEGER(i_std)                                :: l,k,ji, jv, i, j, m      !! indices    
    INTEGER(i_std)                                :: kc, ip
    REAL(r_std),PARAMETER                         :: max_dt_days = 5.         !! Maximum STOMATE time step (days)
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: rprof                    !! Coefficient of the exponential functions that 
                                                                              !! relates root density to soil depth (unitless) 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: gpp_daily_x              !! "Daily" gpp for teststomate  
                                                                              !! @tex $(??gC m^{-2} dt_stomate^{-1})$ @endtex 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: veget_cov                !! Fractional coverage: actually share of the pixel 
                                                                              !! covered by a PFT (fraction of ground area), 
                                                                              !! taking into account LAI ??(= grid scale fpc)?? 
    INTEGER(i_std)                                :: ier                      !! Check errors in netcdf call (unitless)

    INTEGER(i_std)                                :: max_totsize              !! Memory management - maximum memory size (Mb)
    INTEGER(i_std)                                :: totsize_1step            !! Memory management - memory required to store one 
                                                                              !! time step on one processor (Mb) 
    INTEGER(i_std)                                :: totsize_tmp              !! Memory management - memory required to store one 
                                                                              !! time step on all processors(Mb) 
    INTEGER(i_std)                                :: vid                      !! Variable identifer of netCDF (unitless)
    INTEGER(i_std)                                :: nneigh                   !! Number of neighbouring pixels
    INTEGER(i_std)                                :: direct                   !! 
    INTEGER(i_std)                                :: itemp                   !! 
    INTEGER(i_std),DIMENSION(ndm)                 :: d_id                     !! 
    LOGICAL                                       :: found_restart        !! whether found in restart file
    REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE      :: manage
!    REAL(r_std),DIMENSION(:,:),ALLOCATABLE        :: cyc_num_real
!    REAL(r_std),DIMENSION(:),ALLOCATABLE        :: cyc_num_tot_real
!    REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE      :: rot_cmd_store_real
!    REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE      :: plantdate_real
!    REAL(r_std),DIMENSION(:,:),ALLOCATABLE      :: plantdate_now_real
    CHARACTER(LEN=30), SAVE                       :: temp_str
    !$OMP THREADPRIVATE(temp_str)
!!!    CHARACTER(LEN=30)                             :: var_name
    REAL(r_std), ALLOCATABLE, DIMENSION (:,:,:)           :: Nfert_dat !!temporary nitrogen fertilization data
    CHARACTER(LEN=80)                                     :: var_name !! for restget_p
    INTEGER(i_std)                                        :: yrlen
    CHARACTER(LEN=30)                                     :: strManage
    CHARACTER(LEN=30)                                     :: strVar
    CHARACTER(LEN=30)                                     :: FileN_Nfert  !! file name of nitrogen fertilization
    CHARACTER(LEN=30)                                     :: Var_Nfert !! variable name of fertilization

!_ ================================================================================================================================
    
    !! 1.1 Store current time step in a common variable
    itime = kjit
    
    !![DISPENSABLE] 1.2 Copy the depth of the different soil layers from diaglev specified in slow_proc
    !! 1.3 PFT rooting depth across pixels, humescte is pre-defined 
    ! (constantes_veg.f90). It is defined as the coefficient of an exponential 
    ! function relating root density to depth 
    DO j=1,nvm
       rprof(:,j) = 1./humcste(j)
    ENDDO
    
   !! 1.4.0 Parameters for spinup
   !
   eps_carbon = 0.01
   !Config Key   = EPS_CARBON
   !Config Desc  = Allowed error on carbon stock
   !Config If    = SPINUP_ANALYTIC
   !Config Def   = 0.01
   !Config Help  = 
   !Config Units = [%]   
   CALL getin_p('EPS_CARBON',eps_carbon)       


   !Config Key   = SPINUP_PERIOD
   !Config Desc  = Period to calulcate equilibrium during spinup analytic
   !Config If    = SPINUP_ANALYTIC
   !Config Def   = -1
   !Config Help  = Period corresponds in most cases to the number of years of forcing data used in the spinup.
   !Config Units = [years]   
   spinup_period = -1
   CALL getin_p('SPINUP_PERIOD',spinup_period)       

   ! Check spinup_period values. 
   ! For periods uptil 6 years, to obtain equilibrium, a bigger period have to be used 
   ! and therefore spinup_period is adjusted to 10 years. 
   IF (spinup_analytic) THEN
      IF (spinup_period <= 0) THEN
         WRITE(numout,*) 'Error in parameter spinup_period. This parameter must be > 0 : spinup_period=',spinup_period
         CALL ipslerr_p (3,'stomate_main', &
              'Parameter spinup_period must be set to a positive integer.', &
              'Set this parameter to the number of years of forcing data used for the spinup.', &
              '')
      ELSE IF (spinup_period <= 6) THEN
         ! Adjust to bigger period. The period must be a multiple of the original period.
         WRITE(numout,*) 'Initial spinup_period =',spinup_period,' will be adjusted.'
         spinup_period = spinup_period*(INT(9/spinup_period)+1)
      END IF
      WRITE(numout,*) 'Spinup analytic is activated using eps_carbon=',eps_carbon, ' and spinup_period=',spinup_period
   END IF

   !! 1.4.0 Initialization of PFT specific parameters 
   ! Initialization of PFT specific parameters that have no value
   ! for the bare soil PFT i.e. fire resistance, flamability, maximum lai,
   ! settings for growing degree days (GDD), settings for senescence, 
   ! respiration coefficients, photosynthesis, etc.
   ! [DISPENSABLE]
   
   !! 1.4.1 Allocate memory for all variables in stomate
   ! Allocate memory for all variables in stomate, build new index
   ! tables accounting for the PFTs, read and check flags and set file
   ! identifier for restart and history files.
   CALL stomate_init (kjpij, kjpindex, index1, lalo, &
        rest_id_stom, hist_id_stom, hist_id_stom_IPCC)

   !! 1.4.2 Initialization of PFT specific parameters
   ! Initialization of PFT specific parameters i.e. sla from leaf life, 
   ! sapling characteristics (biomass), migration speed, critical diameter,
   ! coldest tolerable temperature, critical values for phenology, maximum
   ! life time of leaves, respiration coefficients and photosynthesis.
   ! The subroutine also communicates settings read by stomate_constant_init.
   CALL data (kjpindex, lalo)
   !! 1.4.3 Initial conditions
   
   !! 1.4.3.1 Read initial values for STOMATE's variables from the _restart_ file
   ! ??Shouldn't this be included in stomate_init?? Looks like an initialization!
   co2_flux(:,:) = zero
   fco2_lu(:) = zero
   
   ! Get values from _restart_ file. Note that only ::kjpindex, ::index, ::lalo 
   ! and ::resolution are input variables, all others are output variables.
   CALL readstart &
        &        (kjpindex, index1, lalo, resolution, t2m, &
        &         dt_days_read, date, &
        &         ind, adapted, regenerate, &
        &         humrel_daily, gdd_init_date, litterhum_daily, &
        &         t2m_daily, t2m_min_daily, &
        &         t2m_max_daily, wspeed_daily, &
        &         tsurf_daily, tsoil_daily, &
        &         soilhum_daily, precip_daily, &
        &         gpp_daily, npp_daily, turnover_daily, &
        &         humrel_month, humrel_week, &
        &         t2m_longterm, tau_longterm, t2m_month, t2m_week, &
        &         tsoil_month, soilhum_month, fireindex, firelitter, &
        &         maxhumrel_lastyear, maxhumrel_thisyear, &
        &         minhumrel_lastyear, minhumrel_thisyear, &
        &         maxgppweek_lastyear, maxgppweek_thisyear, &
        &         gdd0_lastyear, gdd0_thisyear, &
        &         precip_lastyear, precip_thisyear, &
        &         gdd_m5_dormance,  gdd_from_growthinit, gdd_midwinter, ncd_dormance, ngd_minus5, &
        &         PFTpresent, npp_longterm, lm_lastyearmax, lm_thisyearmax, &
        &         maxfpc_lastyear, maxfpc_thisyear, &
        &         turnover_longterm, gpp_week, biomass, resp_maint_part, &
        &         leaf_age, leaf_frac, &
        &         senescence, when_growthinit, age, &
        &         resp_hetero_d, resp_maint_d, resp_growth_d, co2_fire, co2_to_bm_dgvm, &
        &         veget_lastlight, everywhere, need_adjacent, RIP_time, &
        &         time_hum_min, hum_min_dormance, &
        &         litterpart, litter, dead_leaves, &
        &         carbon, lignin_struc, &
        &         ni_acc,fuel_1hr,fuel_10hr,fuel_100hr,fuel_1000hr, & 
        &         turnover_time, &         
        &         prod10,prod100,flux10, flux100, &
        &         convflux, cflux_prod10, cflux_prod100, bm_to_litter, carb_mass_total, &
        &         Tseason, Tseason_length, Tseason_tmp, &
        &         Tmin_spring_time, begin_leaves, onset_date, &
        &         global_years, ok_equilibrium, nbp_accu, nbp_flux, &
        &         MatrixV, VectorU, previous_stock, current_stock, assim_param, &
        &         deepC_a, deepC_s, deepC_p, O2_soil, CH4_soil, O2_snow, CH4_snow, &
        &         thawed_humidity, depth_organic_soil, altmax,fixed_cryoturbation_depth, & !pss+:wetlabd CH4 emissions
        &         uo_0, uold2_0, uo_wet1, uold2_wet1, uo_wet2, &
        &         uold2_wet2, uo_wet3, uold2_wet3,  uo_wet4, uold2_wet4, tsurf_year, &!) !pss-
!gmjc
        &         wshtotsum, sr_ugb, sla_calc, nb_ani, grazed_frac, &
        &         import_yield, t2m_14, litter_not_avail, nb_grazingdays, &
        &         after_snow, after_wet, wet1day, wet2day )

    IF (ANY(ok_LAIdev)) THEN
        ! CROPS
        CALL sticslai_io_readstart(kjpindex, &
                 f_crop_recycle, in_cycle, f_sen_lai, st2m_max_daily, wut_cm_daily, wus_cm_daily, evapot_daily, pdbiomass, pdmasec, &
                 masecveg, masec, dltams, gdh_daily, phoi, onarretesomcourdrp,  &
                 nsendltams, nsendltai, nsenpfeuilverte, nsendurvie, nsenndurvie, densiteequiv, &
                 nplt, tursla, ssla, pfeuilverte, bsenlai, &
                 zrac, nrec, nlan, tcult, udevair, udevcult, ndrp, rfvi, nlev, nger, etatvernal, &
                 caljvc, rfpi, upvt, utp, somcour, somcourdrp, somcourutp, tdevelop, somtemp, &
                 somcourfauche, stpltger, R_stamflax, R_stlaxsen, R_stsenlan, stlevflo, nflo, &
                 R_stlevdrp, R_stflodrp, R_stdrpmat, nmat, nlax, nrecbutoir, group, ndebdes, R_stdrpdes, densite, &
                 densitelev, coeflev, densiteger, somelong, somger, humectation, nbjhumec, &
                 somtemphumec, stpltlev, namf, stmatrec, tustress, slai, somfeuille, pdlai, &
                 nbfeuille, reajust, ulai, pdulai, efdensite, tempeff, nstopfeuille, deltai, svmax, nsen, &
                 laisen, pdlaisen, dltaisenat, nsencour, dltamsen, dltaisen, fgellev, &
                 gelee, fstressgel, R_stlevamf, dernier_n, durvieI, durvie, ndebsen, somsenreste, &
                 shumrel, swfac, turfac, senfac, mafeuiljaune, msneojaune,  &
                 v_dltams, fgelflo, pdircarb, ircarb, nbgrains, pgrain, vitmoy, nbgraingel, pgraingel, &
                 dltags, ftempremp, magrain, pdmagrain, nbj0remp, pdsfruittot, repracmax, repracmin, &
                 kreprac, somtemprac, urac, reprac, nstoprac, c_reserve, c_leafb, gslen, drylen,    &
                 nboxmax, box_ndays, box_lai, box_lairem, box_tdev, box_biom, box_biomrem, box_durage, box_somsenbase )
    ENDIF

   !some permafrost initializations to hold until the first
   !daily permafrost call
   soilc_total(:,:,:) = deepC_a(:,:,:) + deepC_s(:,:,:) + deepC_p(:,:,:)
   heat_Zimov = zero

!!!!! crop managements, xuhui
   IF (ANY(ok_LAIdev)) THEN    

      rotation_update = 0
      found_restart = .TRUE.
      IF (ok_rotate) THEN
        WRITE(temp_str,'(a)') '0Y'
        CALL getin_p('ROTATION_UPDATE',temp_str)
        itemp=INDEX(TRIM(temp_str),'Y')
        READ(temp_str(1:(itemp-1)),"(I2.2)") rotation_update
        WRITE(numout,*) 'Update frequency for rotation system: ', rotation_update, ' year(s)'

        !!!! No. of cycles for rotation
        cyc_num = val_exp
        CALL restget_p (rest_id_stom, 'cyc_num', nbp_glo, nvm, 1, itime, .TRUE., cyc_num, 'gather', nbp_glo, index_g)
        IF ( ALL(cyc_num(:,:) .EQ. val_exp)) found_restart = .FALSE.

        cyc_num_tot = zero
        CALL restget_p (rest_id_stom, 'cyc_num_tot', nbp_glo, 1,  1, itime, .TRUE., cyc_num_tot, 'gather', nbp_glo, index_g)

        f_rot_stom(:,:) = .FALSE. 
        !!! it did not restart since rotation should always completed before ending of a simulation
        rot_cmd_store(:,:,:) = 0
        IF (found_restart) THEN
            CALL restget_p(rest_id_stom, 'rot_cmd_store', nbp_glo, rot_cmd_max, cyc_rot_max, itime, &
                             .TRUE., rot_cmd_store, 'gather', nbp_glo, index_g)
        ELSE
            CALL stomate_stics_read_cycle(kjpindex,lalo,neighbours,resolution,contfrac, cyc_num, cyc_num_tot)
            CALL stomate_stics_read_rotation(kjpindex,lalo,neighbours,resolution,contfrac, rot_cmd_store)
        ENDIF ! not found_restart

        IF (MOD(rotation_update,cyc_rot_max) .NE. zero) THEN ! coherence check
            WRITE(numout,*) 'cyc_rot_max, rotation_update', cyc_rot_max, rotation_update
            WRITE(numout,*) 'xuhui: rotation cycle is likely interrupted by rotation_update'
        ENDIF

      ENDIF !ok_rotate

      !!! it is unsafe to restart plantdate when rotation system has changed
      !!! as cyc_rot_max may change...
      IF (found_restart) THEN !!! either found rotated variables or NOT ok_rotate
        plantdate(:,:,:) = val_exp
        CALL restget_p(rest_id_stom, 'plantdate', nbp_glo, nvm, cyc_rot_max, itime, &
                      &  .TRUE., plantdate, 'gather', nbp_glo, index_g)
        IF ( ALL(plantdate .EQ. val_exp)) found_restart = .FALSE.

        plantdate_now(:,:) = 0
        CALL restget_p(rest_id_stom, 'plantdate_now', nbp_glo, nvm, 1, itime, &
                      &  .TRUE., plantdate_now, 'gather', nbp_glo, index_g)
        
      ENDIF ! (found_restart) THEN

      IF ( (.NOT. found_restart) .OR. dyn_plntdt )  THEN  
      !! reading plantdate at each initial dynamic plantdate activated but not rotated
          CALL stomate_stics_read_plantdate(kjpindex,lalo,neighbours,resolution,contfrac,cyc_num, &
                                      plantdate, plantdate_now)

      ENDIF ! not (found_restart)

      !!
      !! 5 initialization of nitrogen fertilization
      !!
      !nfert_d = .FALSE.
      CALL getin_p('NITROGEN_USE',nitrogen_use)
      CALL getin_p('FIX_NFERT',fix_nfert)

      N_add = zero
      IF (nitrogen_use) THEN
        ! judge whether we use the fix_N or reading a map 

        IF ( .NOT. fix_nfert) THEN
          FileN_Nfert = "NFERT_FILE"
          Var_Nfert = "NFERT"
          ! now we read the nfert file for crop pft , specifically
          CALL stomate_stics_NfertInput(kjpindex,lalo,neighbours,resolution,contfrac,FileN_Nfert,Var_Nfert,Nfert_dat,yrlen)
    
          DO j = 2,nvm
              IF (ok_LAIdev(j)) THEN
                IF (nvm_nfert) THEN
                    N_add(:,j) = Nfert_dat(:,j,1) ! only use the 1st of the 3rd dimension now
                ELSE
                    N_add(:,j) = Nfert_dat(:,pft_to_mtc(j),1) ! only use the 1st of the 3rd dimension now
                ENDIF
              ENDIF ! ok_LAIdev 
          ENDDO
        ! HERE WE USE THE FIXED NITROGEN FERTILIZATION 
        ELSE

          DO j = 2,nvm
              IF (ok_LAIdev(j)) THEN
                DO ip = 1,kjpindex
                   N_add(ip,j) = SP_avenfert(j) ! only use the 1st of the 3rd dimension
                ENDDO
              ENDIF ! ok_LAIdev 
          ENDDO
        
        ENDIF ! .NOT. fix_nfert) THEN
      ENDIF ! (nitrogen_use) THEN

      CALL stomat_stics_calc_N_limfert(kjpindex, N_add, N_limfert)  
      !! grassland management, if activated, will overwrite N_limfert for PFTs of
      !managed grasslands
      IF (printlev>=4) THEN
        WRITE(numout,*) "Now we finish calculation of  the  N_limfert" 
      ENDIF

    ENDIF ! ANY(ok_LAIdev)
    N_limfert = zero
!!!! end crops, xuhui

   !! 1.4.5 Check time step
   
   !! 1.4.5.1 Allow STOMATE's time step to change although this is dangerous
   IF (dt_days /= dt_days_read) THEN
      WRITE(numout,*) 'slow_processes: STOMATE time step changes:', &
       & dt_days_read,' -> ',dt_days
   ENDIF

   !! 1.4.5.2 Time step has to be a multiple of a full day
   IF ( ( dt_days-REAL(NINT(dt_days),r_std) ) > min_stomate ) THEN
      WRITE(numout,*) 'slow_processes: STOMATE time step is not a mutiple of a full day:', &
       & dt_days,' days.'
      STOP
   ENDIF

   !! 1.4.5.3 upper limit to STOMATE's time step
   IF ( dt_days > max_dt_days ) THEN
      WRITE(numout,*) 'slow_processes: STOMATE time step exceeds the maximum value:', &
       & dt_days,' days > ', max_dt_days, ' days.'  
      STOP
   ENDIF

   !! 1.4.5.4 STOMATE time step must not be less than the forcing time step
   IF ( dt_sechiba > dt_days*one_day ) THEN
      WRITE(numout,*) &
        & 'slow_processes: STOMATE time step ::dt_days smaller than forcing time step ::dt_sechiba'
      STOP
   ENDIF

   !! 1.4.5.6 Final message on time step
   WRITE(numout,*) 'Slow_processes, STOMATE time step (days): ', dt_days

   !! 1.4.6 Write forcing file for teststomate
   IF (ok_co2 .AND. allow_forcing_write) THEN

      !Config Key   = STOMATE_FORCING_NAME
      !Config Desc  = Name of STOMATE's forcing file
      !Config If    = OK_STOMATE
      !Config Def   = NONE
      !Config Help  = Name that will be given
      !Config         to STOMATE's offline forcing file
      !Config         Compatible with Nicolas Viovy's driver
      !Config Units = [FILE]
      CALL getin_p('STOMATE_FORCING_NAME',stomate_forcing_name)

      IF ( TRIM(stomate_forcing_name) /= 'NONE' ) THEN
         
         !! 1.4.6.1 Calculate steps that can be stored in memory
         ! Action for the root processor only (parallel computing)  
         IF (is_root_prc) CALL SYSTEM ('rm -f '//TRIM(stomate_forcing_name))
         WRITE(numout,*) 'writing a forcing file for STOMATE.'

         !Config Key   = STOMATE_FORCING_MEMSIZE
         !Config Desc  = Size of STOMATE forcing data in memory 
         !Config If    = OK_STOMATE
         !Config Def   = 50
         !Config Help  = This variable determines how many
         !Config         forcing states will be kept in memory.
         !Config         Must be a compromise between memory
         !Config         use and frequeny of disk access.
         !Config Units = [MegaBytes]
         max_totsize = 50
         CALL getin_p('STOMATE_FORCING_MEMSIZE', max_totsize)      
         max_totsize = max_totsize*1000000

         totsize_1step = &
              &      SIZE(clay)*KIND(clay) &
              &           +SIZE(humrel_daily)*KIND(humrel_daily) &
              &     +SIZE(litterhum_daily)*KIND(litterhum_daily) &
              &     +SIZE(t2m_daily)*KIND(t2m_daily) &
              &     +SIZE(t2m_min_daily)*KIND(t2m_min_daily) &
              &     +SIZE(tsurf_daily)*KIND(tsurf_daily) &
              &     +SIZE(tsoil_daily)*KIND(tsoil_daily) &
              &     +SIZE(soilhum_daily)*KIND(soilhum_daily) &
              &     +SIZE(precip_daily)*KIND(precip_daily) &
              &     +SIZE(gpp_daily_x)*KIND(gpp_daily_x) &
              &     +SIZE(veget)*KIND(veget) &
              &     +SIZE(veget_max)*KIND(veget_max) &
              &     +SIZE(lai)*KIND(lai)
         
         ! Totsize_1step is the size on a single processor, sum
         ! all processors and send to all processors
         CALL reduce_sum(totsize_1step,totsize_tmp)
         CALL bcast(totsize_tmp)
         totsize_1step=totsize_tmp

         ! Total number of forcing steps
         nsft = INT(one_year/(dt_stomate/one_day))

         ! Number of forcing steps in memory
         nsfm = MIN(nsft, &
              &       MAX(1,NINT( REAL(max_totsize,r_std) &
              &                  /REAL(totsize_1step,r_std))))
        
         
     !! 1.6.4.2 Allocate memory for variables containing forcing data  
         ! and initialize variables (set to zero).
         CALL init_forcing (kjpindex,nsfm,nsft)
         
         ! Indexing for writing forcing file
         isf(:) = (/ (i,i=1,nsfm) /)
         nf_written(:) = .FALSE.
         nf_cumul(:) = 0
         iisf = 0

         !! 1.6.4.3 Create netcdf file
         ! Create, define and populate a netcdf file containing the forcing data.
         ! For the root processor only (parallel computing). NF90_ are functions
         ! from and external library.  
         IF (is_root_prc) THEN

            ! Create new netCDF dataset
            ier = NF90_CREATE (TRIM(stomate_forcing_name),NF90_SHARE,forcing_id)

            ! Add variable attribute
            ! Note ::iim_g and ::jjm_g are dimensions of the global field and 
            ! ::nbp_glo is the number of global continental points
            ier = NF90_PUT_ATT (forcing_id,NF90_GLOBAL,'dt_sechiba',dt_sechiba)
            ier = NF90_PUT_ATT (forcing_id,NF90_GLOBAL,'dt_stomate',dt_stomate)
            ier = NF90_PUT_ATT (forcing_id,NF90_GLOBAL, &
                 & 'nsft',REAL(nsft,r_std))
            ier = NF90_PUT_ATT (forcing_id,NF90_GLOBAL, &
                 & 'kjpij',REAL(iim_g*jjm_g,r_std))
            ier = NF90_PUT_ATT (forcing_id,NF90_GLOBAL, &
                 & 'kjpindex',REAL(nbp_glo,r_std))

            ! Add new dimension
            ier = NF90_DEF_DIM (forcing_id,'points',nbp_glo,d_id(1))
            ier = NF90_DEF_DIM (forcing_id,'layers',nbdl,d_id(2))
            ier = NF90_DEF_DIM (forcing_id,'pft',nvm,d_id(3))
            direct=2
            ier = NF90_DEF_DIM (forcing_id,'direction',direct,d_id(4))
            nneigh=8
            ier = NF90_DEF_DIM (forcing_id,'nneigh',nneigh,d_id(5))
            ier = NF90_DEF_DIM (forcing_id,'time',NF90_UNLIMITED,d_id(6))
            ier = NF90_DEF_DIM (forcing_id,'nbparts',nparts,d_id(7))
            ier = NF90_DEF_DIM (forcing_id,'ngrnd',ngrnd,d_id(8))
            ! Add new variable
            ier = NF90_DEF_VAR (forcing_id,'points',    r_typ,d_id(1),vid)
            ier = NF90_DEF_VAR (forcing_id,'layers',    r_typ,d_id(2),vid)
            ier = NF90_DEF_VAR (forcing_id,'pft',       r_typ,d_id(3),vid)
            ier = NF90_DEF_VAR (forcing_id,'direction', r_typ,d_id(4),vid)
            ier = NF90_DEF_VAR (forcing_id,'nneigh',    r_typ,d_id(5),vid)
            ier = NF90_DEF_VAR (forcing_id,'time',      r_typ,d_id(6),vid)
            ier = NF90_DEF_VAR (forcing_id,'nbparts',   r_typ,d_id(7),vid)
            ier = NF90_DEF_VAR (forcing_id,'ngrnd',     r_typ,d_id(8),vid)
            ier = NF90_DEF_VAR (forcing_id,'index',     r_typ,d_id(1),vid)
            ier = NF90_DEF_VAR (forcing_id,'contfrac',  r_typ,d_id(1),vid) 
            ier = NF90_DEF_VAR (forcing_id,'lalo', &
                 & r_typ,(/ d_id(1),d_id(4) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'neighbours', &
                 & r_typ,(/ d_id(1),d_id(5) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'resolution', &
                 & r_typ,(/ d_id(1),d_id(4) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'clay', &
                 & r_typ,(/ d_id(1),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'humrel', &
                 & r_typ,(/ d_id(1),d_id(3),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'litterhum', &
                 & r_typ,(/ d_id(1),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'t2m', &
                 & r_typ,(/ d_id(1),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'t2m_min', &
                 & r_typ,(/ d_id(1),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'tsurf', &
                 & r_typ,(/ d_id(1),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'tsoil', &
                 & r_typ,(/ d_id(1),d_id(2),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'soilhum', &
                 & r_typ,(/ d_id(1),d_id(2),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'precip', &
                 & r_typ,(/ d_id(1),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'gpp', &
                 & r_typ,(/ d_id(1),d_id(3),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'veget', &
                 & r_typ,(/ d_id(1),d_id(3),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'veget_max', &
                 & r_typ,(/ d_id(1),d_id(3),d_id(6) /),vid)
            ier = NF90_DEF_VAR (forcing_id,'lai', &
                 & r_typ,(/ d_id(1),d_id(3),d_id(6) /),vid)
            ier = NF90_ENDDEF (forcing_id)
            
            ! Given the name of a varaible, nf90_inq_varid finds the variable 
            ! ID (::vid). Put data value(s) into variable ::vid
            ier = NF90_INQ_VARID (forcing_id,'points',vid)
            ier = NF90_PUT_VAR (forcing_id,vid, &
                 & (/(REAL(i,r_std),i=1,nbp_glo) /))
            ier = NF90_INQ_VARID (forcing_id,'layers',vid)
            ier = NF90_PUT_VAR (forcing_id,vid,(/(REAL(i,r_std),i=1,nbdl)/))
            ier = NF90_INQ_VARID (forcing_id,'pft',vid)
            ier = NF90_PUT_VAR (forcing_id,vid,(/(REAL(i,r_std),i=1,nvm)/))
            ier = NF90_INQ_VARID (forcing_id,'direction',vid)
            ier = NF90_PUT_VAR (forcing_id,vid,(/(REAL(i,r_std),i=1,2)/))
            ier = NF90_INQ_VARID (forcing_id,'nneigh',vid)
            ier = NF90_PUT_VAR (forcing_id,vid,(/(REAL(i,r_std),i=1,8)/))
            ier = NF90_INQ_VARID (forcing_id,'time',vid)
            ier = NF90_PUT_VAR (forcing_id,vid,(/(REAL(i,r_std),i=1,nsft)/))
            ier = NF90_INQ_VARID (forcing_id,'nbparts',vid)
            ier = NF90_PUT_VAR (forcing_id,vid,(/(REAL(i,r_std),i=1,nparts)/))
            ier = NF90_INQ_VARID (forcing_id,'index',vid)  
            ier = NF90_PUT_VAR (forcing_id,vid,REAL(index_g,r_std))
            ier = NF90_INQ_VARID (forcing_id,'contfrac',vid)
            ier = NF90_PUT_VAR (forcing_id,vid,REAL(contfrac_g,r_std))
            ier = NF90_INQ_VARID (forcing_id,'lalo',vid)
            ier = NF90_PUT_VAR (forcing_id,vid,lalo_g)
            !ym attention a neighbours, a modifier plus tard      
            ier = NF90_INQ_VARID (forcing_id,'neighbours',vid)
            ier = NF90_PUT_VAR (forcing_id,vid,REAL(neighbours_g,r_std))
            ier = NF90_INQ_VARID (forcing_id,'resolution',vid)
            ier = NF90_PUT_VAR (forcing_id,vid,resolution_g)
         ENDIF ! is_root_prc
      ENDIF ! (stomate_forcing_name) /= 'NONE'
   ENDIF ! ok_co2 =.TRUE.
   
   !! 1.4.7 write forcing file for forcesoil
      
      !! 1.4.7.1 Initialize
      !Config Key   = STOMATE_CFORCING_NAME
      !Config Desc  = Name of STOMATE's carbon forcing file
      !Config If    = OK_STOMATE
      !Config Def   = NONE
      !Config Help  = Name that will be given to STOMATE's carbon
      !Config         offline forcing file
      !Config         Compatible with Nicolas Viovy's driver
      !Config Units = [FILE]
      CALL getin_p('STOMATE_CFORCING_NAME',stomate_Cforcing_name)

      IF ( TRIM(stomate_Cforcing_name) /= 'NONE' ) THEN

         ! Time step of forcesoil
         !Config Key   = FORCESOIL_STEP_PER_YEAR
         !Config Desc  = Number of time steps per year for carbon spinup.
         !Config If    = STOMATE_CFORCING_NAME and OK_STOMATE
         !Config Def   = 365
         !Config Help  = Number of time steps per year for carbon spinup.
         !Config Units = [days, months, year]
         nparan = 365
         CALL getin_p('FORCESOIL_STEP_PER_YEAR', nparan)
         
         ! Correct if setting is out of bounds 
         IF ( nparan < 1 ) nparan = 1

         !Config Key   = FORCESOIL_NB_YEAR
         !Config Desc  = Number of years saved for carbon spinup.
         !Config If    = STOMATE_CFORCING_NAME and OK_STOMATE
         !Config Def   = 1
         !Config Help  = Number of years saved for carbon spinup. If internal parameter cumul_Cforcing is TRUE in stomate.f90
         !Config         Then this parameter is forced to one.
         !Config Units = [years]
         CALL getin_p('FORCESOIL_NB_YEAR', nbyear)
         
         ! Set ::nbyear to 1. if ::cumul_Cforcing=.TRUE.
         IF ( cumul_Cforcing ) THEN
            CALL ipslerr_p (1,'stomate', &
                 &          'Internal parameter cumul_Cforcing is TRUE in stomate.f90', &
                 &          'Parameter FORCESOIL_NB_YEAR is therefore forced to 1.', &
                 &          '::nbyear is thus set to 1.')
            nbyear=1
         ENDIF

         ! Make use of ::nparan to calculate ::dt_forcesoil
         dt_forcesoil = zero
         nparan = nparan+1
         DO WHILE ( dt_forcesoil < dt_stomate/one_day )
            nparan = nparan-1
            IF ( nparan < 1 ) THEN
               STOP 'Problem with number of soil forcing time steps ::nparan < 1.'
            ENDIF
            dt_forcesoil = one_year/REAL(nparan,r_std)
         ENDDO
         IF ( nparan > nparanmax ) THEN
            STOP 'Problem with number of soil forcing time steps ::nparan > ::nparanmax'
         ENDIF
         WRITE(numout,*) 'Time step of soil forcing (d): ',dt_forcesoil

         ! Allocate memory for the forcing variables of soil dynamics
         ALLOCATE( nforce(nparan*nbyear))
         nforce(:) = 0
         ALLOCATE(control_moist(kjpindex,nlevs,nparan*nbyear))
         ALLOCATE(npp_equil(kjpindex,nparan*nbyear))
         ALLOCATE(npp_tot(kjpindex))
         ALLOCATE(control_temp(kjpindex,nlevs,nparan*nbyear))
         ALLOCATE(soilcarbon_input(kjpindex,ncarb,nvm,nparan*nbyear)) 
         
         ! Initialize variables, set to zero
         control_moist(:,:,:) = zero
         npp_equil(:,:) = zero
         npp_tot(:) = zero
         control_temp(:,:,:) = zero
         soilcarbon_input(:,:,:,:) = zero

      ENDIF ! stomate_Cforcing_name) /= 'NONE'

   ! 1.4.7b write forcing file for the soil permafrost module?
      !
      !Config  Key  = STOMATE_CFORCING_PERMAFROST_NAME
      !Config  Desc = Name of STOMATE's carbon forcing file
      !Config  Def  = NONE
      !Config  Help = Name that will be given to STOMATE's carbon
      !Config         offline forcing file
      !-
      Cforcing_permafrost_name = 'NONE'               ! compatibilitïavec driver Nicolas
      CALL getin_p('STOMATE_CFORCING_PF_NM',Cforcing_permafrost_name)
      WRITE(*,*) 'cdk: debug Cforcing_permafrost_name: ',Cforcing_permafrost_name

      IF ( TRIM(Cforcing_permafrost_name) /= 'NONE' ) THEN
         IF (is_root_prc) CALL SYSTEM ('rm -f '//TRIM(Cforcing_permafrost_name))

         ALLOCATE(soilcarbon_input_2pfcforcing(kjpindex,ncarb,nvm,nparan*nbyear))
         ALLOCATE(pb_2pfcforcing(kjpindex,nparan*nbyear))
         ALLOCATE(snow_2pfcforcing(kjpindex,nparan*nbyear))
         ALLOCATE(tprof_2pfcforcing(kjpindex,ndeep,nvm,nparan*nbyear))
         ALLOCATE(fbact_2pfcforcing(kjpindex,ndeep,nvm,nparan*nbyear))
         ALLOCATE(hslong_2pfcforcing(kjpindex,ndeep,nvm,nparan*nbyear))
         ALLOCATE(veget_max_2pfcforcing(kjpindex,nvm,nparan*nbyear))
         ALLOCATE(rprof_2pfcforcing(kjpindex,nvm,nparan*nbyear))
         ALLOCATE(tsurf_2pfcforcing(kjpindex,nparan*nbyear))
         !adding another two snow forcings
         ALLOCATE(snowdz_2pfcforcing(kjpindex,nsnow,nparan*nbyear))
         ALLOCATE(snowrho_2pfcforcing(kjpindex,nsnow,nparan*nbyear))
         soilcarbon_input_2pfcforcing(:,:,:,:) = zero
         pb_2pfcforcing(:,:) = zero
         snow_2pfcforcing(:,:) = zero
         tprof_2pfcforcing(:,:,:,:) = zero
         fbact_2pfcforcing(:,:,:,:) = zero
         hslong_2pfcforcing(:,:,:,:) = zero
         veget_max_2pfcforcing(:,:,:) = zero
         rprof_2pfcforcing(:,:,:) = zero
         tsurf_2pfcforcing(:,:) = zero
         !adding another two snow forcings
         snowdz_2pfcforcing(:,:,:) = zero
         snowrho_2pfcforcing(:,:,:) = zero
      ENDIF

   
   !! 1.4.8 Calculate STOMATE's vegetation fractions from veget, veget_max
   DO j=1,nvm
      WHERE ((1.-totfrac_nobio(:)) > min_sechiba)       
         ! Pixels with vegetation
         veget_cov(:,j) = veget(:,j)/( 1.-totfrac_nobio(:) )
         veget_cov_max(:,j) = veget_max(:,j)/( 1.-totfrac_nobio(:) )
      ELSEWHERE
         ! Pixels without vegetation
         veget_cov(:,j) = zero
         veget_cov_max(:,j) = zero
      ENDWHERE
   ENDDO ! Loop over PFTs
   
   !! 1.4.9 Initialize non-zero variables
      CALL stomate_var_init &
           &         (kjpindex, veget_cov_max, leaf_age, leaf_frac, &
           &          dead_leaves, &
           &          veget, lai, deadleaf_cover, assim_param, &
!gmjc
           &          N_limfert)
!end gmjc          
      ! Initialize land cover change variable
      ! ??Should be integrated in the subroutine?? 
      harvest_above(:) = zero

      ! Initialize temp_growth 
      temp_growth(:)=t2m_month(:)-tp_00
!!BEGINNVADD
!       DO j = 2, nvm
!          DO m = 1, nleafages
!             frac_age(:,j,m) = leaf_frac(:,j,m)
!          END DO
!       ENDDO
!!ENDNVADD
   !  get info for which soil decomposition function to use for the
   !  permafrost calcs
   !  which temperature function of carbon consumption?
   frozen_respiration_func = 1
   CALL getin_p('frozen_respiration_func',frozen_respiration_func)
   WRITE(numout, *)' frozen soil respiration function:  ', frozen_respiration_func
   WRITE(numout, *)' (note the above argument replaces the old C_CONSUM_FUN argument from DMitrys code)'

   !1.4.10 wetland CH4
   !pss:+
   !appel routines pour calcul des densites de flux de CH4

   CH4_calcul  = .FALSE.
   !Config  Key  = CH4_CALCUL
   !Config  Desc = Enable or disable wetlands
   !Config If    = OK_STOMATE
   !Config  Def  = False
   !Config  Help = Enable or disable wetlands
   !               
   !Config Units = y/N
   CALL getin_p('CH4_CALCUL', CH4_calcul)
        
   IF(CH4_calcul) THEN
       !Config  Key  = CH4atm_CONC
       !Config  Desc = 
       !Config If    = CH4_CALCUL
       !Config  Def  = 0.0033
       !Config  Help = 
       !               
       !Config Units = [-]
       CH4atmo_CONC=0.0033
       CALL getin_p('CH4atmo_CONC', CH4atmo_CONC)

       CH4_WTD1  = .TRUE.
       !Config  Key  = CH4_WTD1
       !Config  Desc = 
       !Config If    = CH4_CALCUL
       !Config  Def  = True
       !Config  Help = 
       !               
       !Config Units = Y/n
       CALL getin_p('CH4_WTD1', CH4_WTD1)
       !Config  Key  = CH4_WTD2
       !Config  Desc = 
       !Config If    = CH4_CALCUL
       !Config  Def  = True
       !Config  Help = 
       !               
       !Config Units = Y/n
       CH4_WTD2  = .TRUE.
       CALL getin_p('CH4_WDT2', CH4_WTD2)
       !Config  Key  = CH4_WTD3
       !Config  Desc = 
       !Config If    = CH4_CALCUL
       !Config  Def  = True
       !Config  Help = 
       !               
       !Config Units = Y/n
       CH4_WTD3  = .TRUE.
       CALL getin_p('CH4_WTD3', CH4_WTD3)
       !Config  Key  = CH4_WTD4
       !Config  Desc = 
       !Config If    = CH4_CALCUL
       !Config  Def  = True
       !Config  Help = 
       !               
       !Config Units = Y/n
       CH4_WTD4  = .TRUE.
       CALL getin_p('CH4_WTD4', CH4_WTD4)
    ENDIF

   !! 1.4.11 Update flag
   l_first_stomate = .FALSE.
        
  END SUBROUTINE stomate_initialize
  

!! ================================================================================================================================
!! SUBROUTINE 	: stomate_main
!!
!>\BRIEF        Manages variable initialisation, reading and writing forcing 
!! files, aggregating data at stomate's time step (dt_stomate), aggregating data
!! at longer time scale (i.e. for phenology) and uses these forcing to calculate
!! CO2 fluxes (NPP and respirations) and C-pools (litter, soil, biomass, ...)
!!
!! DESCRIPTION  : The subroutine manages 
!! divers tasks:
!! (1) Initializing all variables of stomate (first call)
!! (2) Reading and writing forcing data (last call)
!! (3) Adding CO2 fluxes to the IPCC history files
!! (4) Converting the time steps of variables to maintain consistency between
!! sechiba and stomate
!! (5) Use these variables to call stomate_lpj, maint_respiration, littercalc,
!! soilcarbon. The called subroutines handle: climate constraints 
!! for PFTs, PFT dynamics, Phenology, Allocation, NPP (based on GPP and
!! authothropic respiration), fire, mortality, vmax, assimilation temperatures,
!! all turnover processes, light competition, sapling establishment, lai,  
!! land cover change and litter and soil dynamics.
!! (6) Use the spin-up method developed by Lardy (2011)(only if SPINUP_ANALYTIC 
!! is set to TRUE).
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): deadleaf_cover, assim_param, lai, height, veget, 
!! veget_max, resp_maint, 
!! resp_hetero,resp_growth, co2_flux, fco2_lu.
!!
!! REFERENCES	: 
!! - Lardy, R, et al., A new method to determine soil organic carbon equilibrium,
!! Environmental Modelling & Software (2011), doi:10.1016|j.envsoft.2011.05.016
!!
!! FLOWCHART    : 
!! \latexonly 
!! \includegraphics[scale=0.5]{stomatemainflow.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================
  
SUBROUTINE stomate_main &
       & (kjit, kjpij, kjpindex, &
       &  index, lalo, neighbours, resolution, contfrac, totfrac_nobio, clay, &
       &  t2m, temp_sol, stempdiag, &
       &  humrel, shumdiag, litterhumdiag, precip_rain, precip_snow, &
       !spitfire
       &  wspeed,lightn, popd, read_observed_ba, observed_ba, humign, &
       &  read_cf_fine,cf_fine,read_cf_coarse,cf_coarse,read_ratio_flag,ratio_flag,read_ratio,ratio,&
       !endspit
       &  gpp, deadleaf_cover, assim_param, &
       &  lai, frac_age, height, veget, veget_max, &
       &  veget_max_new, &
       &  vegetnew_firstday,totfrac_nobio_new, &
       &  glccNetLCC,glccSecondShift,glccPrimaryShift, &
       &  harvest_matrix, bound_spa, &
       &  hist_id, hist2_id, rest_id_stom, hist_id_stom, hist_id_stom_IPCC, &
       &  co2_flux, fco2_lu, resp_maint,resp_hetero,resp_growth,temp_growth,&
       &  swdown, evapot_corr, is_update_rotation_cycle, &   !!! xuhui added for crops
       &  tdeep, hsdeep, snow, heat_Zimov, pb, &
       &  sfluxCH4_deep, sfluxCO2_deep, &
       &  thawed_humidity, depth_organic_soil,&
       &  zz_deep, zz_coef_deep, soilc_total, snowdz, snowrho, &
       &  EndOfYear, f_rot_sech, rot_cmd, &
!gmjc
       &  tmc_topgrass,fc_grazing, humcste_use, altmax)
!end gmjc
    
    IMPLICIT NONE

    
  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    !pss:+ nb de niveau vertical pour le calcul de la concentration de CH4
    INTEGER(i_std),PARAMETER                         :: n = 371
    !pss:-

    INTEGER(i_std),INTENT(in)                       :: kjit              !! Time step number (unitless)
    INTEGER(i_std),INTENT(in)                       :: kjpindex          !! Domain size - terrestrial pixels only (unitless)
    INTEGER(i_std),INTENT(in)                       :: kjpij             !! Total size of the un-compressed grid (unitless)
    INTEGER(i_std),INTENT(in)                       :: rest_id_stom      !! STOMATE's _Restart_ file identifier (unitless)
    INTEGER(i_std),INTENT(in)                       :: hist_id_stom      !! STOMATE's _history_ file identifier (unitless)
    INTEGER(i_std),INTENT(in)                       :: hist_id_stom_IPCC !! STOMATE's IPCC _history_ file identifier 
                                                                         !! (unitless) 
    INTEGER(i_std),DIMENSION(kjpindex),INTENT(in)   :: index             !! Indices of the pixels on the map. Stomate uses a 
                                                                         !! reduced grid excluding oceans. ::index contains 
                                                                         !! the indices of the terrestrial pixels only 
                                                                         !! (unitless) 
    INTEGER(i_std),DIMENSION(kjpindex,NbNeighb),INTENT(in) :: neighbours !! Neighoring grid points if land for the DGVM 
                                                                         !! (unitless) 
    REAL(r_std),DIMENSION(kjpindex,2),INTENT(in)    :: lalo              !! Geographical coordinates (latitude,longitude) 
                                                                         !! for pixels (degrees) 
    REAL(r_std),DIMENSION(kjpindex,2),INTENT(in)    :: resolution        !! Size in x an y of the grid (m) - surface area of 
                                                                         !! the gridbox 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)   :: contfrac          !! Fraction of continent in the grid cell (unitless)
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: totfrac_nobio     !! Fraction of grid cell covered by lakes, land 
                                                                         !! ice, cities, ... (unitless) 
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: clay              !! Clay fraction of soil (0-1, unitless)
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)  :: humrel            !! Relative humidity ("moisture availability") 
                                                                         !! (0-1, unitless) 
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: t2m               !! 2 m air temperature (K)
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: temp_sol          !! Surface temperature (K)
    REAL(r_std),DIMENSION(kjpindex,nbdl),INTENT(in) :: stempdiag         !! Soil temperature (K)
    REAL(r_std),DIMENSION(kjpindex,nbdl),INTENT(in) :: shumdiag          !! Relative soil moisture (0-1, unitless)
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: litterhumdiag     !! Litter humidity (0-1, unitless)
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: precip_rain       !! Rain precipitation  
                                                                         !! @tex $(mm dt_stomate^{-1})$ @endtex 
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: precip_snow       !! Snow precipitation  
                                                                         !! @tex $(mm dt_stomate^{-1})$ @endtex 
    !spitfire
    ! Wind speed
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: wspeed
    ! Lightning input
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: lightn
    ! Flag for read in observed burned area
    LOGICAL, INTENT (in)                                   :: read_observed_ba
    ! Observed burned area
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: observed_ba

    ! Flag for read in observed burned area
    LOGICAL, INTENT (in)                                   :: read_cf_coarse
    ! Observed burned area
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: cf_coarse
    ! Flag for read in observed burned area
    LOGICAL, INTENT (in)                                   :: read_cf_fine
    ! Observed burned area
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: cf_fine
    ! Flag for read in observed burned area
    LOGICAL, INTENT (in)                                   :: read_ratio
    ! Observed burned area
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: ratio
    ! Flag for read in observed burned area
    LOGICAL, INTENT (in)                                   :: read_ratio_flag
    ! Observed burned area
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: ratio_flag

!!!! crops added
    ! downward short-wave radiation, for calculation of the photoperiod 
    REAL(r_std), DIMENSION(kjpindex), INTENT(IN)        :: swdown
    ! potential evapotranspiration 
    REAL(r_std), DIMENSION(kjpindex), INTENT(IN)        :: evapot_corr
    LOGICAL,DIMENSION(kjpindex), INTENT(out)            :: f_rot_sech
    INTEGER(i_std),DIMENSION(kjpindex,rot_cmd_max),INTENT(out) :: rot_cmd
    ! enable the update of rotation cycle for CROP module
    LOGICAL, INTENT(in)                                 :: is_update_rotation_cycle
!!!! crops xuhui

    ! Population density rate
    REAL(r_std), DIMENSION(kjpindex), INTENT(inout)            :: popd !popd declared and allocated and input in slowproc.f90

    ! Human ignitions
    REAL(r_std), DIMENSION(kjpindex), INTENT(inout)            :: humign !humign declared and allocated and input in slowproc.f90
    !endspit

    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)  :: gpp               !! GPP of total ground area  
                                                                         !! @tex $(gC m^{-2} time step^{-1})$ @endtex 
                                                                         !! Calculated in sechiba, account for vegetation 
                                                                         !! cover and effective time step to obtain ::gpp_d 
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)  :: veget_max_new     !! New "maximal" coverage fraction of a PFT (LAI ->  
                                                                         !! infinity) on ground only if EndOfYear is
                                                                         !! activated (unitless)
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(inout)  :: vegetnew_firstday     !! New "maximal" coverage fraction of a PFT (LAI -> 
                                                                         !! infinity) on ground only if EndOfYear is 
    REAL(r_std),DIMENSION(kjpindex,12),INTENT(inout)  :: glccNetLCC      !! New "maximal" coverage fraction of a PFT (LAI -> 
                                                                         !! infinity) on ground only if EndOfYear is 
    REAL(r_std),DIMENSION(kjpindex,12),INTENT(inout)  :: glccSecondShift !! New "maximal" coverage fraction of a PFT (LAI -> 
                                                                         !! infinity) on ground only if EndOfYear is 
    REAL(r_std),DIMENSION(kjpindex,12),INTENT(inout)  :: glccPrimaryShift  !! New "maximal" coverage fraction of a PFT (LAI -> 
                                                                         !! infinity) on ground only if EndOfYear is 
    REAL(r_std),DIMENSION(kjpindex,12),INTENT(inout)  :: harvest_matrix  !! New "maximal" coverage fraction of a PFT (LAI -> 
                                                                         !! infinity) on ground only if EndOfYear is 
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(inout)  :: bound_spa      !! New "maximal" coverage fraction of a PFT (LAI -> 
                                                                         !! infinity) on ground only if EndOfYear is 
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: totfrac_nobio_new !! New fraction of grid cell covered by lakes, land 
                                                                         !! ice, cities, ... (unitless) 
    INTEGER(i_std),INTENT(in)                       :: hist_id           !! ?? [DISPENSABLE] SECHIBA's _history_ file 
                                                                         !! identifier 
    INTEGER(i_std),INTENT(in)                       :: hist2_id          !! ?? [DISPENSABLE] SECHIBA's _history_ file 2 
                                                                         !! identifier 
    LOGICAL, INTENT(in)                             :: EndOfYear       !! Flag set to true for the first sechiba time step on the year.
!gmjc top 5 layer grassland soil moisture for grazing
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)       :: tmc_topgrass
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)       :: fc_grazing
!end gmjc
    !! 0.2 Output variables

    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(out) :: co2_flux          !! CO2 flux between atmosphere and biosphere per 
                                                                         !! average ground area 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex  
                                                                         !! [??CHECK] sign convention? 
    REAL(r_std),DIMENSION(kjpindex),INTENT(out)     :: fco2_lu           !! CO2 flux between atmosphere and biosphere from 
                                                                         !! land-use (without forest management)  
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex  
                                                                         !! [??CHECK] sign convention? 
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(out) :: resp_maint        !! Maitenance component of autotrophic respiration in 
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex 
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(out) :: resp_growth       !! Growth component of autotrophic respiration in 
                                                                         !! @tex ($gC m^{-2} dt_stomate^{-1}$) @endtex
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(out) :: resp_hetero       !! Heterotrophic respiration in  
                                                                         !! @tex $(gC m^{-2} dt_stomate^{-1})$ @endtex  
    REAL(r_std),DIMENSION(kjpindex),INTENT(out)     :: temp_growth       !! Growth temperature (Â°C)  
                                                                         !! Is equal to t2m_month 

    !! 0.3 Modified
   
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(inout)       :: lai            !! Leaf area inex @tex $(m^2 m^{-2})$ @endtex
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)          :: veget          !! Fraction of vegetation type including 
                                                                              !! non-biological fraction (unitless) 
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)  :: humcste_use
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(inout)  :: altmax
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(inout)       :: veget_max      !! Maximum fraction of vegetation type including 
                                                                              !! non-biological fraction (unitless) 
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(inout)       :: height         !! Height of vegetation (m)
    REAL(r_std),DIMENSION(kjpindex,nvm,npco2),INTENT(inout) :: assim_param    !! min+max+opt temperatures (K) & vmax for 
                                                                              !! photosynthesis  
                                                                              !! @tex $(\mu mol m^{-2}s^{-1})$ @endtex  
    REAL(r_std),DIMENSION(kjpindex),INTENT(inout)           :: deadleaf_cover !! Fraction of soil covered by dead leaves 
                                                                              !! (unitless) 
    REAL(r_std),DIMENSION(kjpindex,nvm,nleafages),INTENT(inout):: frac_age    !! Age efficacity from STOMATE 

    !! 0.4 local variables
    
    REAL(r_std)                                   :: dt_days_read             !! STOMATE time step read in restart file (days)
    INTEGER(i_std)                                :: iv,l,k,ji, jv, i, j, m   !! indices    
    REAL(r_std),PARAMETER                         :: max_dt_days = 5.         !! Maximum STOMATE time step (days)
    REAL(r_std)                                   :: hist_days                !! Writing frequency for history file (days)
    REAL(r_std),DIMENSION(0:nbdl)                 :: z_soil                   !! Variable to store depth of the different soil 
                                                                              !! layers (m) 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: rprof                    !! Coefficient of the exponential functions that 
                                                                              !! relates root density to soil depth (unitless) 
    REAL(r_std),DIMENSION(kjpindex)               :: cvegtot                  !! Total "vegetation" cover (unitless)
    REAL(r_std),DIMENSION(kjpindex)               :: precip                   !! Total liquid and solid precipitation  
                                                                              !! @tex $(??mm dt_stomate^{-1})$ @endtex 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: gpp_d                    !! Gross primary productivity per ground area 
                                                                              !! @tex $(??gC m^{-2} dt_stomate^{-1})$ @endtex  
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: gpp_daily_x              !! "Daily" gpp for teststomate  
                                                                              !! @tex $(??gC m^{-2} dt_stomate^{-1})$ @endtex 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: resp_hetero_litter       !! Litter heterotrophic respiration per ground area 
                                                                              !! @tex $(gC m^{-2} day^{-1})$ @endtex  
                                                                              !! ??Same variable is also used to 
                                                                              !! store heterotrophic respiration per ground area 
                                                                              !! over ::dt_sechiba?? 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: resp_hetero_soil         !! soil heterotrophic respiration  
                                                                              !! @tex $(gC m^{-2} day^{-1})$ @endtex
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: veget_cov                !! Fractional coverage: actually share of the pixel 
                                                                              !! covered by a PFT (fraction of ground area), 
                                                                              !! taking into account LAI ??(= grid scale fpc)?? 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: veget_cov_max_new        !! Old value for maximal fractional coverage (unitless)
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: vcmax                    !! Maximum rate of carboxylation
                                                                              !! @tex $(\mumol m^{-2} s^{-1})$ @endtex
    REAL(r_std),DIMENSION(kjpindex,nlevs)         :: control_moist_inst       !! Moisture control of heterotrophic respiration 
                                                                              !! (0-1, unitless) 
    REAL(r_std),DIMENSION(kjpindex,nlevs)         :: control_temp_inst        !! Temperature control of heterotrophic 
                                                                              !! respiration, above and below (0-1, unitless) 
    REAL(r_std),DIMENSION(kjpindex,ncarb,nvm)     :: soilcarbon_input_inst    !! Quantity of carbon going into carbon pools from 
                                                                              !! litter decomposition 
                                                                              !! @tex $(gC m^{-2} day^{-1})$ @endtex 
    
    REAL(r_std), DIMENSION(kjpindex,nsnow), INTENT(in)            :: snowdz   !! snow depth
    REAL(r_std), DIMENSION(kjpindex,nsnow), INTENT(in)            :: snowrho  !! snow density
    !arrays for deep permafrost calculations
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),   INTENT (in)     :: tdeep    !! deep temperature profile
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),   INTENT (in)     :: hsdeep   !! deep long term soil humidity profile
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),   INTENT (out)    :: heat_Zimov !! heating associated with decomposition
    REAL(r_std), DIMENSION(kjpindex),     INTENT (out)            :: sfluxCH4_deep      !! surface flux of CH4 to atmosphere from permafrost
    REAL(r_std), DIMENSION(kjpindex),     INTENT (out)            :: sfluxCO2_deep      !! surface flux of CO2 to atmosphere from permafrost
    REAL(r_std), DIMENSION(kjpindex),   INTENT (inout)            :: thawed_humidity    !! specified humidity of thawed soil
    REAL(r_std), DIMENSION(kjpindex),   INTENT (inout)            :: depth_organic_soil !! how deep is the organic soil?
    REAL(r_std), DIMENSION(kjpindex),         INTENT (in)         :: snow               !! Snow mass [Kg/m^2]
    REAL(r_std), DIMENSION(kjpindex), INTENT (in)                 :: pb                 !! Lowest level pressure = [mb]
    REAL(r_std), DIMENSION(kjpindex)                              :: pb_pa              !! Lowest level pressure = [pa]
    REAL(r_std), DIMENSION(ndeep),   INTENT (in)                  :: zz_deep            !! deep vertical profile
    REAL(r_std), DIMENSION(ndeep),   INTENT (in)                  :: zz_coef_deep       !! deep vertical profile
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),   INTENT (inout)  :: soilc_total        !! total soil carbon for use in thermal calcs

    INTEGER(i_std)                                :: ier                      !! Check errors in netcdf call (unitless)
    REAL(r_std)                                   :: sf_time                  !! Intermediate variable to calculate current time 
                                                                              !! step 
    INTEGER(i_std)                                :: max_totsize              !! Memory management - maximum memory size (Mb)
    INTEGER(i_std)                                :: totsize_1step            !! Memory management - memory required to store one 
                                                                              !! time step on one processor (Mb) 
    INTEGER(i_std)                                :: totsize_tmp              !! Memory management - memory required to store one 
                                                                              !! time step on all processors(Mb) 
    REAL(r_std)                                   :: xn                       !! How many times have we treated in this forcing 
    REAL(r_std), DIMENSION(kjpindex)              :: vartmp                   !! Temporary variable

    INTEGER(i_std),SAVE                           :: Cforcing_id              !! File identifer of file 2
!$OMP THREADPRIVATE(Cforcing_id)   
    INTEGER(i_std),SAVE                           :: Cforcing_permafrost_id   !! Permafrost file identifer 
!$OMP THREADPRIVATE(Cforcing_permafrost_id)
    INTEGER(i_std),PARAMETER                      :: ndm = 10                 !! Maximum number of dimensions (unitless)

    INTEGER(i_std)                                :: vid                      !! Variable identifer of netCDF (unitless)
    INTEGER(i_std)                                :: nneigh                   !! Number of neighbouring pixels
    INTEGER(i_std)                                :: direct                   !! ??
    INTEGER(i_std),DIMENSION(ndm)                 :: d_id                     !! ??
    REAL(r_std)                                   :: net_co2_flux_monthly     !! ??[DISPENSABLE]
    REAL(r_std)                                   :: net_co2_flux_monthly_sum !! ??[DISPENSABLE]
    REAL(r_std),DIMENSION(nbp_glo)                :: clay_g                   !! Clay fraction of soil (0-1, unitless), parallel 
                                                                              !! computing 
    INTEGER(i_std)                                :: iplt                     !! planting date for crops, xuhui
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:,:)    :: soilcarbon_input_g       !! Quantity of carbon going into carbon pools from 
                                                                              !! litter decomposition  
                                                                              !! @tex $(gC m^{-2} dt_sechiba^{-1})$ @endtex, parallel 
                                                                              !! computing 
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)      :: control_moist_g          !! Moisture control of heterotrophic respiration 
                                                                              !! (0-1, unitless), parallel computing 
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)      :: control_temp_g           !! Temperature control of heterotrophic respiration 
                                                                              !! (0-1, unitless), parallel computing 
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:)        :: npp_equil_g              !! Equilibrium NPP written to forcesoil 
                                                                              !! @tex $(gC m^{-2} year^{-1})$ @endtex, parallel 
                                                                              !! computing 

    REAL(r_std)                                   :: net_cflux_prod_monthly_sum    !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std)                                   :: net_cflux_prod_monthly_tot    !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std)                                   :: net_harvest_above_monthly_sum !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std)                                   :: net_harvest_above_monthly_tot !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std)                                   :: net_biosp_prod_monthly_sum    !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std)                                   :: net_biosp_prod_monthly_tot    !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std), DIMENSION(kjpindex,nvm,nbpools)  :: carbon_stock                  !! Array containing the carbon stock for each pool
                                                                                   !! used by ORCHIDEE
    INTEGER(i_std), SAVE                          :: spinup_period                 !! Period of years used to calculate the resolution of the system for the spinup analytic. 
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)    :: tdeep_celsius                 !! deep temperature profile celsius


    ! local declaration for STICS
    ! 
    ! hourly growth unit --- for calculation gdh, in detail see divers_stics module
    REAL(r_std), DIMENSION(kjpindex,nvm)                    :: ugdh 
    ! indicator of a pixel, for loop
    INTEGER                                                 :: pixid
    ! unit of photoperiod, calculated according to swdown, if swdown <= 0 then uphoi = 0, else uphoi = 0.5 (half hour); 
    REAL(r_std), DIMENSION(kjpindex)                        :: uphoi
    ! hourly unit of chilling, calculated according to a step function. 
    ! in detail see Besoins_en_froid.f90
    !REAL(r_std), DIMENSION(kjpindex)                        :: cuh   
    INTEGER(i_std)                                          :: ip

!    ! soil temperature at the resolution of 1 cm, the second dimension is
!    ! determined by the constant of cropsoil, which is the "effective depth of crop soil"
    REAL(r_std), DIMENSION(kjpindex, nvm, 3)           :: stempdiag_cm
!    ! soil relative humidity at the resolution of 1 cm, the second dimension is
!    ! determined by the constant of cropsoil, which is the "effective depth of crop soil"
    REAL(r_std), DIMENSION(kjpindex, nvm, 3)           :: shumdiag_cm
!    ! 2m daily temperature, unit in Celsius degree
    REAL(r_std), DIMENSION(kjpindex)                   :: st2m
   
    REAL(r_std), ALLOCATABLE, DIMENSION (:,:,:)           :: Nfert_dat !!temporary nitrogen fertilization data
    INTEGER(i_std)                                        :: yrlen
    CHARACTER(LEN=30)                                     :: FileN_Nfert      ! file name of the nitrogen fertilization 
    CHARACTER(LEN=30)                                     :: Var_Nfert     ! variable name in the nitrogen fertilization file 


    REAL(r_std)                                           :: innlai, rotprc, temp_prc
    INTEGER(i_std)                                        :: mycmd, stveg, edveg, itemp
    REAL(r_std),DIMENSION(kjpindex, nvm, nvm)             :: matrix_prc_rot
    REAL(r_std),DIMENSION(kjpindex,ndeep,nvm)             :: min_hsdeep_daily

!_ ================================================================================================================================
    
    ! Check that initialization is done
    IF (l_first_stomate) CALL ipslerr_p(3,'stomate_main','Initialization not yet done.','','')
 
    IF (printlev >= 4) THEN
       WRITE(numout,*) 'stomate_main: date=',date,' ymds=', year, month, day, sec, ' itime=', itime, ' do_slow=',do_slow
    ENDIF

    !! 1.1 Store current time step in a common variable
    itime = kjit
    tdeep_celsius(:,:,:) = 0
    
    !![DISPENSABLE] 1.2 Copy the depth of the different soil layers from diaglev specified in slow_proc
    !! 1.3 PFT rooting depth across pixels, humescte is pre-defined 
    ! (constantes_veg.f90). It is defined as the coefficient of an exponential 
    ! function relating root density to depth 
    DO j=1,nvm
       rprof(:,j) = 1./humcste_use(:,j)
    ENDDO

    !! 3. Special treatment for some input arrays.
    
    !! 3.1 Sum of liquid and solid precipitation
    precip(:) = ( precip_rain(:) + precip_snow(:) )*one_day/dt_sechiba
    
    !! 3.2 Calculate STOMATE's vegetation fractions from veget and veget_max
    DO j=1,nvm 
       WHERE ((1.-totfrac_nobio(:)) > min_sechiba)
          ! Pixels with vegetation
          veget_cov(:,j) = veget(:,j)/( 1.-totfrac_nobio(:) )
          veget_cov_max(:,j) = veget_max(:,j)/( 1.-totfrac_nobio(:) )
       ELSEWHERE
          ! Pixels without vegetation
          veget_cov(:,j) = zero
          veget_cov_max(:,j) = zero
       ENDWHERE
    ENDDO

    ! we have to treat similarly for glccNetLCC if gross land cover change
    ! is simulated.
    DO j=1,12
       WHERE ((1.-totfrac_nobio(:)) > min_sechiba)
          ! Pixels with vegetation
          glccNetLCC(:,j)=glccNetLCC(:,j)/( 1.-totfrac_nobio(:) )
       ELSEWHERE
          ! Pixels without vegetation
          glccNetLCC(:,j) = zero
       ENDWHERE
    ENDDO
    ! we have to treat similarly for glccSecondShift if gross land cover change
    ! is simulated.
    DO j=1,12
       WHERE ((1.-totfrac_nobio(:)) > min_sechiba)
          ! Pixels with vegetation
          glccSecondShift(:,j)=glccSecondShift(:,j)/( 1.-totfrac_nobio(:) )
       ELSEWHERE
          ! Pixels without vegetation
          glccSecondShift(:,j) = zero
       ENDWHERE
    ENDDO
    ! we have to treat similarly for glccPrimaryShift if gross land cover change
    ! is simulated.
    DO j=1,12
       WHERE ((1.-totfrac_nobio(:)) > min_sechiba)
          ! Pixels with vegetation
          glccPrimaryShift(:,j)=glccPrimaryShift(:,j)/( 1.-totfrac_nobio(:) )
       ELSEWHERE
          ! Pixels without vegetation
          glccPrimaryShift(:,j) = zero
       ENDWHERE
    ENDDO
    ! we have to treat similarly for harvest_matrix if gross land cover change
    ! is simulated.
    DO j=1,12
       WHERE ((1.-totfrac_nobio(:)) > min_sechiba)
          ! Pixels with vegetation
          harvest_matrix(:,j)=harvest_matrix(:,j)/( 1.-totfrac_nobio(:) )
       ELSEWHERE
          ! Pixels without vegetation
          harvest_matrix(:,j) = zero
       ENDWHERE
    ENDDO

    IF (date ==1) THEN
       DO j=1,nvm
          WHERE ((1.-totfrac_nobio_new(:)) > min_sechiba)       
             ! Pixels with vegetation
             vegetnew_firstday(:,j) = vegetnew_firstday(:,j)/( 1.-totfrac_nobio_new(:) )
          ELSEWHERE
             ! Pixels without vegetation
             vegetnew_firstday(:,j) = zero
          ENDWHERE
       ENDDO ! Loop over PFTs
    END IF
   
    IF ( do_now_stomate_lcchange ) THEN
       DO j=1,nvm
          WHERE ((1.-totfrac_nobio_new(:)) > min_sechiba)
             ! Pixels with vegetation
             veget_cov_max_new(:,j) = veget_max_new(:,j)/( 1.-totfrac_nobio_new(:) )
          ELSEWHERE
             ! Pixels without vegetation
             veget_cov_max_new(:,j) = zero
          ENDWHERE
       ENDDO
    ENDIF

    !! 3.3 Adjust time step of GPP 
    ! No GPP for bare soil
    gpp_d(:,1) = zero
    ! GPP per PFT
    DO j = 2,nvm   
       WHERE (veget_cov_max(:,j) > min_stomate)
          ! The PFT is available on the pixel
          gpp_d(:,j) =  gpp(:,j)/ veget_cov_max(:,j)* one_day/dt_sechiba  
       ELSEWHERE
          ! The PFT is absent on the pixel
          gpp_d(:,j) = zero
       ENDWHERE
    ENDDO
    !permafrost:  get the residence time for soil carbon
    IF ( printlev>=3 ) WRITE(*,*) 'cdk debug stomate: prep to calc fbact'
    tdeep_celsius = tdeep - ZeroCelsius
    fbact = microactem(tdeep_celsius, frozen_respiration_func, hsdeep, kjpindex, ndeep, nvm)
    prmfrst_soilc_tempctrl = 1./fbact

    !! 3.4 The first time step of the first day of the month  
    ! implies that the month is over
    IF ( day == 1 .AND. sec .LT. dt_sechiba ) THEN
       EndOfMonth=.TRUE.
    ELSE
       EndOfMonth=.FALSE.
    ENDIF
    

  !! 4. Calculate variables for dt_stomate (i.e. "daily")

    ! 4.0 calculating "daily" variables for crops
    ! 4.0.1 calculation of halfhourly udh according to halfhourly temperature t2ma
    ! this feature enable the model to account diurnal temperature effects, xuhui 

    ! air temperature from Kelvin to Celsius
    st2m(:) = t2m(:) - 273.15   
 
    DO jv = 2, nvm 
       IF (ok_LAIdev(jv)) THEN
           ugdh(:,jv) = (st2m(:) - SP_tdmin(jv) )
           DO pixid = 1, kjpindex
              IF (st2m(pixid) .LT. SP_tdmin(jv)) ugdh(pixid,jv) = 0.0
              !  the equation dtradia/one_hour is to calculate the corresponding udh in one day 
              IF (st2m(pixid) .GT. SP_tdmax(jv)) ugdh(pixid,jv) = (SP_tdmax(jv) - SP_tdmin(jv)) 
              ! comments see above
           ENDDO
        ENDIF
    ENDDO
 
   
    ! 4.0.2 judgement of the photoperiod accodring to down-ward short-wave radiation
    !       calculation of the uphoi according to swdown 

    DO pixid = 1, kjpindex
       IF (swdown(pixid) <=  0.0) THEN
          uphoi(pixid) = 0.0 
       ELSE
          uphoi(pixid) = 1.  ! photoperiod for each sechiba time step, unit in hours  
       ENDIF       
    ENDDO ! finish the calculation of photoperiod for each sechiba time step
    
    ! 4.0.3 calculation of daily accumulation of chilling units--see Besoins_en_froid.f90
    
!    DO pixid = 1, kjpindex
!       IF (t2m(pixid) .LE. 1.4) cuh = 0.0 * dtradia/3600
!       IF ((t2m(pixid) .GT. 1.4) .AND. (t2m(pixid) .LE. 2.4)) cuh = 0.5 * dtradia/3600
!       IF ((t2m(pixid) .GT. 2.4) .AND. (t2m(pixid) .LE. 9.1)) cuh = 1.0 * dtradia/3600
!       IF ((t2m(pixid) .GT. 9.1) .AND. (t2m(pixid) .LE. 12.4)) cuh = 0.5 * dtradia/3600
!       IF ((t2m(pixid) .GT. 12.4) .AND. (t2m(pixid) .LE. 15.9)) cuh = 0.0 * dtradia/3600
!       IF ((t2m(pixid) .GT. 15.9) .AND. (t2m(pixid) .LE. 17.5)) cuh = -0.5 * dtradia/3600
!       IF (t2m(pixid) .GT. 17.5) cuh = -1.0 * dtradia/3600
!    ENDDO ! finish the calculation of cuh in each time step of sechiba's loop


    ! Note: If dt_days /= 1, then variables 'xx_daily' (eg. half-daily or bi-daily) are by definition
    ! not expressed on a daily basis. This is not a problem but could be
    ! confusing


    !!!!! daily variable accumulation for crops
    ! calculation of the daily gdh according to halfhourly udh (see above)
    CALL stomate_accu( do_slow, ugdh, gdh_daily)
    ! calculation of the daily  photoperiod 
    CALL stomate_accu( do_slow, uphoi, phoi)  ! finish of the calculation of photoperiod, phoi, in hours
    ! calculation of the daily accumulated potential evapotranspiration
    CALL stomate_accu( do_slow, evapot_corr, evapot_daily)
    !!!!! end for crops, xuhui

    !! 4.1 Accumulate instantaneous variables (do_slow=.FALSE.) 
    ! Accumulate instantaneous variables (do_slow=.FALSE.) and eventually 
    ! calculate daily mean value (do_slow=.TRUE.) 
    CALL stomate_accu ( do_slow, humrel,        humrel_daily)
    CALL stomate_accu ( do_slow, litterhumdiag, litterhum_daily)
    CALL stomate_accu ( do_slow, t2m,           t2m_daily)
    CALL stomate_accu ( do_slow, temp_sol,      tsurf_daily)
    CALL stomate_accu ( do_slow, stempdiag,     tsoil_daily)
    CALL stomate_accu ( do_slow, shumdiag,      soilhum_daily)
    CALL stomate_accu ( do_slow, precip,        precip_daily)
    CALL stomate_accu ( do_slow, gpp_d,         gpp_daily)
!gmjc # trunk do not introduce snow in stomate
    CALL stomate_accu ( do_slow, precip_snow, snowfall_daily)
    CALL stomate_accu ( do_slow, snow,        snowmass_daily)
    CALL stomate_accu ( do_slow, tmc_topgrass, tmc_topgrass_daily)
!end gmjc

    !ditto for permafrost variables
    IF ( do_daily_permafrost ) THEN
       CALL stomate_accu( do_slow, tdeep, tdeep_daily)
       CALL stomate_accu( do_slow, hsdeep, hsdeep_daily)
       !           CALL stomate_accu( do_slow, fbact(:,:,iv), fbact_daily(:,:,iv))
       CALL stomate_accu( do_slow, prmfrst_soilc_tempctrl,prmfrst_soilc_tempctrl_daily)
       CALL stomate_accu( do_slow, snow, snow_daily)
       CALL stomate_accu( do_slow, pb * 100., pb_pa_daily)
       CALL stomate_accu( do_slow, temp_sol, temp_sol_daily)
       !adding two forcings
       CALL stomate_accu( do_slow, snowdz, snowdz_daily)
       CALL stomate_accu( do_slow, snowrho, snowrho_daily)
    ENDIF
 
    !! 4.2 Daily minimum temperature
    t2m_min_daily(:) = MIN( t2m(:), t2m_min_daily(:) )
    !! Daily maximum temperature
    t2m_max_daily(:) = MAX( t2m(:), t2m_max_daily(:) )
    !spitfire
    CALL stomate_accu ( do_slow, wspeed, wspeed_daily)
!    t2m_max_daily(:) = MAX( t2m(:), t2m_max_daily(:) )
    !endspit

    !! 4.3 Calculate maintenance respiration
    ! Note: lai is passed as output argument to overcome previous problems with 
    ! natural and agricultural vegetation types. 
!    WRITE(numout,*) 'lai before stomate_resp:',lai(1,12:14)
    CALL maint_respiration &
         & (kjpindex,lai,t2m,t2m_longterm,stempdiag,height,veget_cov_max, &
         & rprof,biomass,resp_maint_part_radia, &
!gmjc
         & sla_calc)
!end gmjc    
    ! Aggregate maintenance respiration across the different plant parts 
    resp_maint_radia(:,:) = zero
    DO j=2,nvm
       DO k= 1, nparts
          resp_maint_radia(:,j) = resp_maint_radia(:,j) &
               & + resp_maint_part_radia(:,j,k)
       ENDDO
    ENDDO
    
    ! Maintenance respiration separated by plant parts
    resp_maint_part(:,:,:) = resp_maint_part(:,:,:) &
         & + resp_maint_part_radia(:,:,:)
    
    !! 4.4 Litter dynamics and litter heterothropic respiration 
    ! Including: litter update, lignin content, PFT parts, litter decay,
    ! litter heterotrophic respiration, dead leaf soil cover.
    ! Note: there is no vertical discretisation in the soil for litter decay.
    IF (printlev >= 5) THEN
        DO jv = 2,nvm
            IF (ok_LAIdev(jv)) THEN
                WRITE(numout,*) 'jv, turnover_daily(1,jv,:,icarbon)', jv,  turnover_daily(1,jv,:,icarbon)
            ENDIF
        ENDDO
    ENDIF
    turnover_littercalc(:,:,:,:) = turnover_daily(:,:,:,:) * dt_sechiba/one_day
    bm_to_littercalc(:,:,:,:)    = bm_to_litter(:,:,:,:) * dt_sechiba/one_day       
    CALL littercalc (kjpindex, &
         turnover_littercalc, bm_to_littercalc, &
         veget_cov_max, temp_sol, stempdiag, shumdiag, litterhumdiag, &
         litterpart, litter, litter_avail, litter_not_avail, litter_avail_frac, &
         !spitfire
         fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr, &
         !endspit
         dead_leaves, lignin_struc, &
         deadleaf_cover, resp_hetero_litter, &
         soilcarbon_input_inst, control_temp_inst, control_moist_inst, &
         matrixA, vectorB, &!)
!!!!!crops
         stempdiag_cm, shumdiag_cm,&
!!!!!xuhui
!gmjc
         sla_calc,do_slow)
!end gmjc
    
    ! Heterothropic litter respiration during time step ::dt_sechiba @tex $(gC m^{-2})$ @endtex
    resp_hetero_litter(:,:) = resp_hetero_litter(:,:) * dt_sechiba/one_day
    IF ( ok_pc ) THEN
       IF ( .NOT. do_daily_permafrost ) THEN
          pb_pa = pb * 100.
          ! should input daily-averaged values here
          !temp_sol -> tsurf daily, tdeep, hsdeep, stempdiag, shumdiag,
          !profil_froz_diag, snow, pb_pa...

          CALL deep_carbcycle(kjpindex, index, itime, dt_sechiba, lalo, clay, &
               temp_sol, tdeep, hsdeep, snow, heat_Zimov, pb_pa, &  !cdk++
               sfluxCH4_deep, sfluxCO2_deep, &
               deepC_a, deepC_s, deepC_p, O2_soil, CH4_soil, O2_snow, CH4_snow,&
               zz_deep, zz_coef_deep, depth_organic_soil,soilcarbon_input_inst,&
               veget_max, rprof, altmax, carbon, carbon_surf,resp_hetero_soil, fbact, fixed_cryoturbation_depth,snowdz,snowrho)

          resp_hetero_soil(:,:) = resp_hetero_soil(:,:) * dt_sechiba/one_day
          ! Total heterothrophic respiration during time step ::dt_sechiba @tex
          ! $(gC
          ! m^{-2})$ @endtex
          resp_hetero_radia(:,:) = resp_hetero_litter(:,:) + resp_hetero_soil(:,:)
          resp_hetero_d(:,:) = resp_hetero_d(:,:) + resp_hetero_radia(:,:)

          soilc_total(:,:,:) = deepC_a(:,:,:) + deepC_s(:,:,:) + deepC_p(:,:,:)


          ! separate resp_hetero_litter and resp_hetero_soil for history file
            CALL histwrite_p (hist_id_stomate, 'resp_hetero_soil', itime, &
                            resp_hetero_soil(:,:), kjpindex*nvm, horipft_index)
            CALL histwrite_p (hist_id_stomate, 'resp_hetero_litter', itime, &
                            resp_hetero_litter(:,:), kjpindex*nvm, horipft_index)
       ELSE

          CALL stomate_accu( do_slow, soilcarbon_input_inst, soilcarbon_input_daily)
          resp_hetero_radia(:,:) = resp_hetero_litter(:,:)
          resp_hetero_d(:,:) = resp_hetero_d(:,:) + resp_hetero_radia(:,:)
            CALL histwrite_p (hist_id_stomate, 'resp_hetero_litter', itime, &
                            resp_hetero_litter(:,:), kjpindex*nvm,horipft_index)
       ENDIF
    ELSE
 
    !! 4.5 Soil carbon dynamics and soil heterotrophic respiration
    ! Note: there is no vertical discretisation in the soil for litter decay.
    CALL soilcarbon (kjpindex, dt_sechiba/one_day, clay, &
         soilcarbon_input_inst, control_temp_inst, control_moist_inst, &
         carbon, resp_hetero_soil, matrixA)
!gmjc
!         resp_hetero_soil_part)
!end gmjc

    !Permafrost carbon
    carbon_surf(:,:,:) = carbon(:,:,:)
    soilc_total(:,:,:) = zero
    heat_Zimov = zero    
    ! Heterothropic soil respiration during time step ::dt_sechiba @tex $(gC m^{-2})$ @endtex 
    resp_hetero_soil(:,:) = resp_hetero_soil(:,:) * dt_sechiba/one_day
! gmjc resp_hetero_soil for each carbon pool
!    resp_hetero_soil_part(:,:,:)=resp_hetero_soil_part(:,:,:)*dt_sechiba/one_day
!    resp_hetero_soil_d(:,:,:)=resp_hetero_soil_d(:,:,:)+resp_hetero_soil_part(:,:,:)
!    WRITE (numout,*) 'active het resp daily',resp_hetero_soil_d(:,iactive,:)
! end gmjc
    ! Total heterothrophic respiration during time step ::dt_sechiba @tex $(gC m^{-2})$ @endtex
    resp_hetero_radia(:,:) = resp_hetero_litter(:,:) + resp_hetero_soil(:,:)
    resp_hetero_d(:,:) = resp_hetero_d(:,:) + resp_hetero_radia(:,:)
    
    !! 4.6 Accumulate instantaneous variables (do_slow=.FALSE.) 
    ! Accumulate instantaneous variables (do_slow=.FALSE.) and eventually 
    ! calculate daily mean value (do_slow=.TRUE.) 
    CALL stomate_accu ( do_slow, control_moist_inst, control_moist_daily)
    CALL stomate_accu ( do_slow, control_temp_inst,  control_temp_daily)

    CALL stomate_accu ( do_slow, soilcarbon_input_inst, soilcarbon_input_daily)
   ENDIF 

!!!!! crops
    ! calculate daily mean values for soil temperature for root zone
    CALL stomate_accu(do_slow, stempdiag_cm, wut_cm_daily)

    ! calculate daily mean values for soil moisture for root zone
    CALL stomate_accu(do_slow, shumdiag_cm, wus_cm_daily)
!!!!! xuhui


!! 5. Daily processes - performed at the end of the day
    
    IF (do_slow) THEN
       WRITE(numout,*) "Number of days: ", date

       !5.0 permafrost
       IF ( ok_pc ) THEN
          IF ( do_daily_permafrost ) THEN
             !++cdk permafrost carbon calcs next:
             fbact_daily = un/prmfrst_soilc_tempctrl_daily
             min_hsdeep_daily = MIN(hsdeep_daily, un)

             CALL deep_carbcycle(kjpindex, index, INT(itime*dt_sechiba/dt_stomate), dt_stomate, lalo, clay, &
                  temp_sol_daily, tdeep_daily, min_hsdeep_daily, snow_daily, heat_Zimov, pb_pa_daily, &  !cdk++
                  sfluxCH4_deep, sfluxCO2_deep, &
                  deepC_a, deepC_s, deepC_p, O2_soil, CH4_soil,O2_snow,CH4_snow, &
                  zz_deep, zz_coef_deep,depth_organic_soil,soilcarbon_input_daily, &
                  veget_max, rprof, altmax, carbon,carbon_surf,resp_hetero_soil, fbact_daily,fixed_cryoturbation_depth,snowdz_daily,snowrho_daily)

             resp_hetero_radia =  resp_hetero_soil
             resp_hetero_d = resp_hetero_d + resp_hetero_radia

             soilc_total(:,:,:) = deepC_a(:,:,:) + deepC_s(:,:,:) + deepC_p(:,:,:)
             ! separate resp_hetero_litter and resp_hetero_soil for history
             CALL histwrite_p (hist_id_stomate, 'resp_hetero_soil', itime, &
                  resp_hetero_soil(:,:), kjpindex*nvm, horipft_index)

          ENDIF
       ENDIF

       !5.1.0 wetland CH4
       !pss:+
       !appel routines pour calcul des densites de flux de CH4

        IF (CH4_calcul) THEN
          
          !routine pour densite de flux d un wetland ou WTD = 0
          CALL ch4_wet_flux_density_0 (kjpindex,stempdiag,tsurf_daily,tsurf_year,veget_cov_max,veget,&
               & carbon,lai,uo_0,uold2_0, ch4_flux_density_tot_0, ch4_flux_density_dif_0,&
               & ch4_flux_density_bub_0,ch4_flux_density_pla_0, CH4atmo_CONC)  

          IF (CH4_WTD1) THEN
             !routine calcule densite de flux d un wetland ou WTD = pwater_wet1 (cf.stomate_cste_wetlands.f90) 
             CALL ch4_wet_flux_density_wet (kjpindex,stempdiag,tsurf_daily,tsurf_year,veget_cov_max,veget,&
                  & carbon_surf,lai,uo_wet1,uold2_wet1,ch4_flux_density_tot_wet1, ch4_flux_density_dif_wet1, &
                  & ch4_flux_density_bub_wet1,ch4_flux_density_pla_wet1, CH4atmo_CONC, pwater_wet1)  
          ELSE
             ch4_flux_density_tot_wet1=0.0
             ch4_flux_density_dif_wet1=0.0
             ch4_flux_density_bub_wet1=0.0
             ch4_flux_density_pla_wet1=0.0
          ENDIF
             
          IF (CH4_WTD2) THEN
             !routine calcule densite de flux d un wetland ou WTD = pwater_wet2 (cf.stomate_cste_wetlands.f90) 
             CALL ch4_wet_flux_density_wet (kjpindex,stempdiag,tsurf_daily,tsurf_year,veget_cov_max,veget,&
                  & carbon_surf,lai,uo_wet2,uold2_wet2,ch4_flux_density_tot_wet2, ch4_flux_density_dif_wet2, &
                  & ch4_flux_density_bub_wet2,ch4_flux_density_pla_wet2, CH4atmo_CONC, pwater_wet2)  
          ELSE
             ch4_flux_density_tot_wet2=0.0
             ch4_flux_density_dif_wet2=0.0
             ch4_flux_density_bub_wet2=0.0
             ch4_flux_density_pla_wet2=0.0
          ENDIF

          IF (CH4_WTD3) THEN
             !routine calcule densite de flux d un wetland ou WTD = pwater_wet3 (cf.stomate_cste_wetlands.f90) 
             CALL ch4_wet_flux_density_wet (kjpindex,stempdiag,tsurf_daily,tsurf_year,veget_cov_max,veget,&
                  & carbon_surf,lai,uo_wet3,uold2_wet3,ch4_flux_density_tot_wet3, ch4_flux_density_dif_wet3, &
                  & ch4_flux_density_bub_wet3,ch4_flux_density_pla_wet3, CH4atmo_CONC, pwater_wet3)  
          ELSE
             ch4_flux_density_tot_wet3=0.0
             ch4_flux_density_dif_wet3=0.0
             ch4_flux_density_bub_wet3=0.0
             ch4_flux_density_pla_wet3=0.0
          ENDIF

          IF (CH4_WTD4) THEN
             !routine calcule densite de flux d un wetland ou WTD = pwater_wet4 (cf.stomate_cste_wetlands.f90) 
             CALL ch4_wet_flux_density_wet (kjpindex,stempdiag,tsurf_daily,tsurf_year,veget_cov_max,veget,&
                & carbon_surf,lai,uo_wet4,uold2_wet4,ch4_flux_density_tot_wet4, ch4_flux_density_dif_wet4, &
                & ch4_flux_density_bub_wet4,ch4_flux_density_pla_wet4, CH4atmo_CONC, pwater_wet4)  
          ELSE
             ch4_flux_density_tot_wet4=0.0
             ch4_flux_density_dif_wet4=0.0
             ch4_flux_density_bub_wet4=0.0
             ch4_flux_density_pla_wet4=0.0
          ENDIF
          
       ELSE
          
          ch4_flux_density_tot_wet1=0.0
          ch4_flux_density_dif_wet1=0.0
          ch4_flux_density_bub_wet1=0.0
          ch4_flux_density_pla_wet1=0.0
          
          ch4_flux_density_tot_wet2=0.0
          ch4_flux_density_dif_wet2=0.0
          ch4_flux_density_bub_wet2=0.0
          ch4_flux_density_pla_wet2=0.0
          
          ch4_flux_density_tot_wet3=0.0
          ch4_flux_density_dif_wet3=0.0
          ch4_flux_density_bub_wet3=0.0
          ch4_flux_density_pla_wet3=0.0
          
          ch4_flux_density_tot_wet4=0.0
          ch4_flux_density_dif_wet4=0.0
          ch4_flux_density_bub_wet4=0.0
          ch4_flux_density_pla_wet4=0.0
          
          ch4_flux_density_tot_0=0.0
          ch4_flux_density_dif_0=0.0
          ch4_flux_density_bub_0=0.0
          ch4_flux_density_pla_0=0.0
          
       ENDIF
!pss:-        
!!!!! crop variables 
       tday_counter = tday_counter + dt_days  ! julian day, ranging [1 365]
       vday_counter = vday_counter + dt_days

       ! for CROP module, decision taken in slowproc
       IF (is_update_rotation_cycle) THEN
            !!!! re-reading the rotation maps to overwrite the restarted variables
            CALL stomate_stics_read_cycle(kjpindex,lalo,neighbours,resolution,contfrac, &
                                    cyc_num, cyc_num_tot)
            !!!! rotation command
            CALL stomate_stics_read_rotation(kjpindex,lalo,neighbours,resolution,contfrac, &
                                    rot_cmd_store)
            !!! read planting dates for the new rotation system
            CALL stomate_stics_read_plantdate(kjpindex,lalo,neighbours,resolution,contfrac, &
                                    cyc_num, plantdate, plantdate_now)
       ENDIF
!!!!! end crop variables, xuhui

       !! 5.1 Update lai
       ! Use lai from stomate
       ! ?? check if this is the only time ok_pheno is used??
       ! ?? Looks like it is the only time. But this variables probably is defined 
       ! in stomate_constants or something, in which case, it is difficult to track.
       IF (ok_pheno) THEN
          !! 5.1.1 Update LAI 
          ! Set lai of bare soil to zero
          lai(:,ibare_sechiba) = zero
!!!!! crops

          ! CROP step: Reinitialization of recycle
          ! We reinitialize for each PFT-pixel, because the crop cycle for each pixel and each PFT is quite different
          ! ! We control this process by a flag  f_crop_init
          
          CALL Stics_init(&
               kjpindex                ,&   
               !nvm                     ,&   
               f_crop_init             ,&   
               f_crop_recycle          ,&   
               in_cycle                ,&   
               f_sen_lai                ,&   
               onarretesomcourdrp      ,&   
!               nlevobs                 ,&
!               namfobs                 ,&
!               nfloobs                 ,&
!               nlanobs                 ,&
!               nlaxobs                 ,&
!               nmatobs                 ,&
!               nrecobs                 ,&
!               nsenobs                 ,&
!               ndrpobs                 ,&
               nsendltams              ,&
               nsendltai               ,&
               nsenpfeuilverte         ,&
               nsendurvie              ,&
               nsenndurvie             ,&
               densiteequiv            ,&
               nplt                    ,&
               tursla                  ,&
               ssla                     ,&
               pfeuilverte             ,&
               bsenlai                 ,&
               zrac                    ,&
               nrec                    ,& 
               nlan                    ,&
               tcult                   ,&
               udevair                 ,&
               udevcult                ,&
               ndrp                    ,&
               rfvi                    ,&
               nlev                    ,&
               nger                    ,&
               etatvernal              ,&
               caljvc                  ,&
               rfpi                    ,&
               upvt                    ,&
               utp                     ,&
               somcour                 ,&
               somcourdrp              ,&
               somcourutp              ,&
               tdevelop                ,&
               somtemp                 ,&
               somcourfauche           ,&
               stpltger                ,&
               R_stamflax              ,&
               R_stlaxsen              ,&
               R_stsenlan              ,&
               stlevflo                ,&
               nflo                    ,&
               R_stlevdrp              ,&
               R_stflodrp              ,&
               R_stdrpmat              ,&
               nmat                    ,&
               nlax                    ,&
               nrecbutoir              ,&
               group                   ,&
               ndebdes                 ,&
               R_stdrpdes              ,&
               densite                 ,&
               densitelev              ,&
               coeflev                 ,&
               densiteger              ,&
               somelong                 ,&
               somger                  ,&
               humectation             ,&
               nbjhumec                ,&
               somtemphumec            ,&
               stpltlev                ,&
               namf                    ,&
               stmatrec                ,&
               tustress                ,&
               slai                     ,&
               somfeuille              ,&
               pdlai                   ,&
               nbfeuille               ,&
               reajust                 ,&
               ulai                    ,&
               pdulai                  ,&
               efdensite               ,&
               tempeff                 ,&
               nstopfeuille            ,&
               deltai                  ,&
               svmax                    ,&
               nsen                    ,&
               laisen                  ,&
               pdlaisen                ,&
               dltaisenat              ,&
               nsencour                ,&
               dltamsen                ,&
               dltaisen                ,&
               fgellev                 ,&
               gelee                   ,&
               fstressgel              ,&
               R_stlevamf              ,&
               dernier_n               ,&
               durvieI                 ,&
               durvie                  ,&
               ndebsen                 ,&
               somsenreste             ,&
               shumrel                  ,&
               swfac                   ,&
               turfac                  ,&
               senfac                  ,&
               mafeuiljaune            ,&
               msneojaune              ,&
               v_dltams                ,&
               fgelflo                 ,&
               pdircarb                ,&
               ircarb                  ,&
               nbgrains                ,&
               pgrain                  ,&
               vitmoy                  ,&
               nbgraingel              ,&
               pgraingel               ,&
               dltags                  ,&
               ftempremp               ,&
               magrain                 ,&
               pdmagrain               ,&
               nbj0remp                ,&
               pdsfruittot             ,&
               repracmax               ,&
               repracmin               ,&
               kreprac                 ,&
               somtemprac              ,&
               urac                    ,&
               reprac                  ,&
               nstoprac                ,&
               c_reserve               ,&
               c_leafb                 ,&
               !biomass,                 &
               deltgrain               ,&
               gslen                   ,&
               drylen, &
               histgrowthset, &
               hist_sencourset, &
               hist_latestset, &
               doyhiststset, &
               nboxmax, box_ulai, box_ndays, box_lai, box_lairem, box_tdev, box_biom, box_biomrem, box_durage, box_somsenbase)                
!               write(*,*) 'xuhui: box_ulai after init, ',box_ulai(11,:)
               ! adjust the unit to Celsius   
               st2m_daily(:) = t2m_daily(:) - 273.15
               st2m_min_daily(:) = t2m_min_daily(:) - 273.15
               st2m_max_daily(:) = t2m_max_daily(:) - 273.15
               wut_cm_daily(:,:,:) = wut_cm_daily(:,:,:) - 273.15

!!!!! end crops, xuhui
          ! lai for all PFTs
          DO j = 2, nvm

             IF (ok_LAIdev(j)) THEN ! crops
        ! loop for each pixel, because the LAIdev module is build for pixel scale
        ! daily loop, so, at first, initialize some variables. After that, we should close the initialization flag.          
              IF (printlev>=5) THEN
                   WRITE(numout,*) 'before enter driver_stics'
               ENDIF
               DO ip = 1, kjpindex 
                 IF (veget_max(ip,j) .GT. 0.0) THEN !!! added xuhui to save computing time
  !                 CALL getin_p('IMPOSE_IPLT',iplt_1d)
                   IF (ok_rotate) THEN
                       iplt = plantdate_now(ip,j)
                   ELSE
                       iplt = plantdate_now(ip,j)
                   ENDIF 
                   
                   ! HERE WE JUDGE WHETHER WE ADOPT A DYNAMIC NITROGEN STRATEGY, BUT NOT NOW.
                   IF (SP_DY_INN(j)) THEN
                      ! here is the process for dynamic nitrogen limitation 
                      ! to be completed when full nitrogen cycle becomes
                      ! available, xuhui 
                   ELSE
                      innlai = SP_innlai(j)
                   ENDIF  
                   IF (printlev>=4) THEN
                      WRITE(numout,*) 'j, wut_cm_daily(ip, j, :)', j, wut_cm_daily(ip,j,:)
                      WRITE(numout,*) 'j, wus_cm_daily(ip, j, :)', j, wus_cm_daily(ip,j,:)
                   ENDIF
  !                 IF (printlev>=4) THEN
  !                     write(*,*) 'xhui: boxulai in stomate: ',box_ulai(j,:)
  !                 ENDIF
                   CALL driver_stics(&
                                     tday_counter,                               &  ! IN, julian date
                                     in_cycle(ip, j),                               &  ! INout
                                     f_crop_recycle(ip, j),                              &  ! INOUT
                                     f_sen_lai(ip, j),                               &  ! INout
                                     st2m_daily(ip),                          &  ! IN
                                     st2m_min_daily(ip),                          &  ! IN  
                                     gdh_daily(ip, j),                       &  ! IN
                                     phoi(ip),                               &  ! IN
                                     onarretesomcourdrp(ip, j),              &  ! IN                          
                                     wut_cm_daily(ip, j, :),              &  ! IN
                                     wus_cm_daily(ip, j, :),                 &  ! IN
  !                                   nlevobs(ip, j),                         &  ! IN
  !                                   namfobs(ip, j),                         &  ! IN
  !                                   nfloobs(ip, j),                         &  ! IN
  !                                   nlanobs(ip, j),                         &  ! IN
  !                                   nlaxobs(ip, j),                         &  ! IN
  !                                   nmatobs(ip, j),                         &  ! IN
  !                                   nrecobs(ip, j),                         &  ! IN
  !                                   nsenobs(ip, j),                         &  ! IN
  !                                   ndrpobs(ip, j),                         &  ! IN
                                     dltams(ip, j),                          &  ! IN
                                     evapot_daily(ip),                             &  ! IN
                                     masec(ip, j),                           &  ! IN
                                     masecveg(ip, j),                        &  ! IN
                                     nsendltams(ip, j),                      &  ! INOUT
                                     nsendltai(ip, j),                       &
                                     nsenpfeuilverte(ip, j),                 &
                                     nsendurvie(ip, j),                      &
                                     nsenndurvie(ip, j),                     &
                                     densiteequiv(ip, j),                    &
                                     nplt(ip, j),                            &
                                     tursla(ip, j),                          &
                                     ssla(ip, j),                             &
                                     pfeuilverte(ip, j),                     &
                                     bsenlai(ip, j),                         &
                                     zrac(ip, j),                            &
                                     nrec(ip, j),                            &
                                     nlan(ip, j),                            &
                                     tcult(ip, j),                           &
                                     udevair(ip, j),                         &
                                     udevcult(ip, j),                        &
                                     ndrp(ip, j),                            &
                                     rfvi(ip, j),                            &
                                     nlev(ip, j),                            &
                                     nger(ip, j),                            &
                                     etatvernal(ip, j),                      &
                                     caljvc(ip, j),                          &
                                     rfpi(ip, j),                            &
                                     upvt(ip, j),                            &
                                     utp(ip, j),                             &
                                     somcour(ip, j),                         &
                                     somcourdrp(ip, j),                      &
                                     somcourutp(ip, j),                      &
                                     tdevelop(ip, j),                        &
                                     somtemp(ip, j),                         &
                                     somcourfauche(ip, j),                   &
                                     stpltger(ip, j),                        &
                                     R_stamflax(ip, j),                      &
                                     R_stlaxsen(ip, j),                      &
                                     R_stsenlan(ip, j),                      &
                                     stlevflo(ip, j),                        &
                                     nflo(ip, j),                            &
                                     R_stlevdrp(ip, j),                      &
                                     R_stflodrp(ip, j),                      &
                                     R_stdrpmat(ip, j),                      &
                                     nmat(ip, j),                            &
                                     nlax(ip, j),                            &
                                     nrecbutoir(ip, j),                      &
                                     group(ip, j),                           &
                                     ndebdes(ip, j),                         &
                                     R_stdrpdes(ip, j),                      &
                                     densite(ip, j),                         &
                                     densitelev(ip, j),                      &
                                     coeflev(ip, j),                         &
                                     densiteger(ip, j),                      &
                                     somelong(ip, j),                        &
                                     somger(ip, j),                          &
                                     humectation(ip, j),                     &
                                     nbjhumec(ip, j),                        &
                                     somtemphumec(ip, j),                    &
                                     stpltlev(ip, j),                        &
                                     namf(ip, j),                            &
                                     stmatrec(ip, j),                        &
                                     tustress(ip, j),                        &
                                     slai(ip, j),                             &
                                     somfeuille(ip, j),                      &
                                     pdlai(ip, j),                           &
                                     nbfeuille(ip, j),                       &
                                     reajust(ip, j),                         &
                                     ulai(ip, j),                            &
                                     pdulai(ip, j),                          &
                                     efdensite(ip, j),                       &
                                     tempeff(ip, j),                         &
                                     nstopfeuille(ip, j),                    &
                                     deltai(ip, j),                          &
                                     svmax(ip, j),                            &
                                     nsen(ip, j),                            &
                                     laisen(ip, j),                          &
                                     dltaisenat(ip, j),                      &
                                     nsencour(ip, j),                        &
                                     dltamsen(ip, j),                        &
                                     dltaisen(ip, j),                        &
                                     fgellev(ip, j),                         &
                                     gelee(ip, j),                           &
                                     fstressgel(ip, j),                      &
                                     pdlaisen(ip, j),                        &
                                     R_stlevamf(ip, j),                      &
                                     dernier_n(ip, j),                       &
                                     durvieI(ip, j),                         &
                                     durvie(ip, j),                          &
                                     ndebsen(ip, j),                         &
                                     somsenreste(ip, j),                     &
                                     shumrel(ip, j),                          &
                                     humrel(ip, j),                          &
                                     swfac(ip, j),                           &
                                     turfac(ip, j),                          &
                                     senfac(ip, j),                          &
                                     mafeuiljaune(ip, j),                    &
                                     msneojaune(ip, j),                      &
                                     gslen(ip, j),                           &
                                     drylen(ip, j),                           &
                                     humrel_daily(ip, j),                    &
                                     !! parameters
                                     SP_codeplante(j),                        &
                                     SP_stade0(j),                            &
                                     ! SP_iplt0(j),                             &
                                     iplt,                             &
                                     SP_iwater(j),                            &
                                     SP_codesimul(j),                         &
                                     SP_codelaitr(j),                         &
                                     SP_slamax(j),                            &
                                     SP_slamin(j),                            &
                                     SP_codeperenne(j),                       &
                                     SP_codcueille(j),                        &
                                     SP_codegdh(j),                           &
                                     SP_codetemp(j),                          &
                                     SP_coderetflo(j),                        &
                                     SP_codeinnact(j),                        &
                                     SP_codeh2oact(j),                        &
                                     SP_stressdev(j),                         &
                                     !SP_innlai(j),                         &
                                     innlai,                                &
                                     SP_innsenes(j),                         &
                                     SP_codebfroid(j),                        &
                                     SP_codephot(j),                          &
                                     SP_codedormance(j),                      &
                                     SP_codefauche(j),                        &
                                     SP_codetempfauche(j),                    &
                                     SP_codlainet(j),                         &
                                     SP_codeindetermin(j),                    &
                                     SP_codeinitprec(j),                      &
                                     SP_culturean(j),                          &
                                     SP_jvc(j),                               &
                                     SP_tfroid(j),                            &
                                     SP_ampfroid(j),                          &
                                     SP_jvcmini(j),                           &
                                     SP_tgmin(j)   ,                          &                               
                                     SP_stpltger(j),                          &
                                     SP_profsem(j),                           &
                                     SP_propjgermin(j),                       &
                                     SP_tdmax(j),                             &
                                     SP_nbjgerlim(j),                         &
                                     SP_densitesem(j),                        &
                                     SP_vigueurbat(j),                        &
                                     SP_codepluiepoquet(j),                   &
                                     SP_codehypo(j),                          &
                                     SP_elmax(j),                             &
                                     SP_belong(j),                            &
                                     SP_celong(j),                            &
                                     SP_nlevlim1(j) ,                         &
                                     SP_nlevlim2(j),                          &
                                     SP_codrecolte(j),                        &
                                     SP_variete(j),                           &
                                     SP_codegermin(j),                        &
                                     S_codeulaivernal(j),                        &
                                     SP_swfacmin(j),                        &
                                     SP_laiplantule(j),                       &
                                     SP_vlaimax(j),                           &
                                     SP_stlevamf(j),                          &
                                     SP_stamflax(j),                          &
                                     SP_udlaimax(j),                          &
                                     SP_laicomp(j),                           &
                                     SP_adens(j),                             &
                                     SP_bdens(j),                             &
                                     SP_tcxstop(j),                           &
                                     SP_tcmax(j),                             &
                                     SP_tcmin(j),                             &
                                     SP_dlaimax(j),                           &
                                     SP_dlaimin(j),                           &
                                     SP_pentlaimax(j),                        &
                                     SP_tigefeuil(j),                         &
                                     SP_stlaxsen(j),                         &
                                     SP_stsenlan(j),                         &
                                     SP_stlevdrp(j),                         &
                                     SP_stflodrp(j),                         &
                                     SP_stdrpmat(j),                         &
                                     SP_stdrpdes(j),                         &
                                     SP_phyllotherme(j),                         &
                                     SP_lai0(j),                              &
                                     SP_tustressmin(j),                       &
                                     ! STICS:: SENESCENCE
                                     SP_nbfgellev(j),                         &
                                     SP_maxgs(j),                         &
                                     SP_ratiodurvieI(j),                      &
                                     SP_durvieF(j),                           &
                                     SP_ratiosen(j),                          &
                                     SP_tdmin(j),                             &
                                     ! STICS:: F_humerac
                                     SP_sensrsec(j),                             &
                                     ! STICS:: GEL
                                     SP_codgellev(j),                             &
                                     SP_codgeljuv(j),                             &
                                     SP_codgelveg(j),                             &
                                     SP_tletale(j),                             &
                                     SP_tdebgel(j),                             &
                                     SP_tgellev10(j),                             &
                                     SP_tgellev90(j),                             &
                                     SP_tgeljuv10(j),                             &
                                     SP_tgeljuv90(j),                             &
                                     SP_tgelveg10(j),                             &
                                     SP_tgelveg90(j),                             &
                                     ! STICS:: Photoperiod
                                     SP_sensiphot(j),                             &
                                     SP_phosat(j),                             &
                                     SP_phobase(j),                            &
                                     histgrowthset(ip,j,:,:),                  &
                                     hist_sencourset(ip,j),                    &
                                     hist_latestset(ip,j),                     &
                                     doyhiststset(ip,j),                       &
                                     SP_nbox(j), box_ulai(j,:), box_ndays(ip,j,:), box_lai(ip,j,:), &
                                     box_lairem(ip,j,:), box_tdev(ip,j,:), box_biom(ip,j,:), &
                                     box_biomrem(ip,j,:), box_durage(ip,j,:), box_somsenbase(ip,j,:), &
                                     codesla)
   
                   lai(ip, j) = slai(ip, j)
                 ENDIF ! veget_max(ip,j)>0
                 
               ENDDO ! loop for npts
             
             ELSE ! non-crop calculation
                IF (printlev>=5 .AND. (j .EQ. 10) ) THEN 
                    WRITE(numout,*) 'biomass(:,10,ileaf,icarbon)', biomass(:,10,ileaf,icarbon)
                    WRITE(numout,*) 'sla_calc(:,10)', sla_calc(:,10)
                    WRITE(numout,*) 'lai(:,10)', lai(:,10)
                 ENDIF
                 lai(:,j) = biomass(:,j,ileaf,icarbon)*sla_calc(:,j)
             ENDIF
          ENDDO ! loop for PFTs
          frac_age(:,:,:) = leaf_frac(:,:,:)
              
!          WRITE (numout,*) 'LAI before alloc: ',lai(:,12:14)
       ELSE 
          ! 5.1.2 Use a prescribed lai
          ! WARNING: code in setlai is identical to the lines above
          ! Update subroutine if LAI has to be forced 
          CALL  setlai(biomass,sla_calc,lai) 
          frac_age(:,:,:) = zero
       ENDIF
       !! 5.2 Calculate long-term "meteorological" and biological parameters
       ! mainly in support of calculating phenology. If ::EndOfYear=.TRUE.
       ! annual values are update (i.e. xx_lastyear).
       CALL season &
            &          (kjpindex, dt_days, EndOfYear, &
            &           veget_cov, veget_cov_max, &
            &           humrel_daily, t2m_daily, &
            &           tsurf_daily, & !pss+-
            &           tsoil_daily, soilhum_daily, lalo, &
            &           precip_daily, npp_daily, biomass, &
            &           turnover_daily, gpp_daily, when_growthinit, &
            &           maxhumrel_lastyear, maxhumrel_thisyear, &
            &           minhumrel_lastyear, minhumrel_thisyear, &
            &           maxgppweek_lastyear, maxgppweek_thisyear, &
            &           gdd0_lastyear, gdd0_thisyear, &
            &           precip_lastyear, precip_thisyear, &
            &           lm_lastyearmax, lm_thisyearmax, &
            &           maxfpc_lastyear, maxfpc_thisyear, &
            &           humrel_month, humrel_week, t2m_longterm, &
            &           tau_longterm, t2m_month, t2m_week, &
            &           tsurf_year, & !pss+-
            &           tsoil_month, soilhum_month, &
            &           npp_longterm, turnover_longterm, gpp_week, &
            &           gdd_m5_dormance, gdd_midwinter, ncd_dormance, ngd_minus5, &
            &           time_hum_min, hum_min_dormance, gdd_init_date, &
            &           gdd_from_growthinit, herbivores, &
            &           Tseason, Tseason_length, Tseason_tmp, &
            &           Tmin_spring_time, t2m_min_daily, begin_leaves, onset_date, &!)
!gmjc
            &           t2m_14, sla_calc)
!end gmjc
       
       !! 5.3 Use all processes included in stomate

          ! 5.3.0 get the biomass of previous day for crop STICS (unit in t dry matter/ha) 

          DO j = 2, nvm
             IF (ok_LAIdev(j)) THEN
                 DO ip = 1, kjpindex
                        
                    pdbiomass(ip, j) = (SUM(biomass(ip, j, :, icarbon)))/100.0/0.48  ! gC/m2 --> t/ha dry biomass
                    ! total aboveground biomass of previous day
                    pdmasec(ip, j) = (biomass(ip, j, ileaf, icarbon) + biomass(ip, j, isapabove, icarbon)&
                         + biomass(ip, j, icarbres, icarbon) + biomass(ip, j, ifruit, icarbon))/100.0/0.48 
                 ENDDO
              ENDIF
          ENDDO
          
!          ! 5.3.0.1  check before calling stomateLpj
!          IF (printlev>=4) THEN
!              print *, 'date in stomate is', tday_counter 
!              print *, 'nger in stomate is', nger(:, 11:13) 
!              print *, 'nlev in stomate is', nlev(:, 11:13)
!              print *, 'ndrp in stomate is', ndrp(:, 11:13)
!              print *, 'nmat in stomate is', nmat(:, 11:13)
!              print *, 'reprac in stomate is', reprac(:, 11:13)
!              print *, 'biomass reserve before phenology in stomate is', biomass(:, 11:13, icarbres, icarbon)
!          ENDIF

          !! 5.3.1  Activate stomate processes 
          ! Activate stomate processes (the complete list of processes depends 
          ! on whether the DGVM is used or not). Processes include: climate constraints 
          ! for PFTs, PFT dynamics, Phenology, Allocation, NPP (based on GPP and
          ! authothropic respiration), fire, mortality, vmax, assimilation temperatures,
          ! all turnover processes, light competition, sapling establishment, lai and 
          ! land cover change.
          IF (printlev>=4) THEN
              WRITE(numout,*) 'tday_counter before Lpj: ',tday_counter
!              WRITE(numout,*) 'lai(:,11): ',lai(:,11)
!              WRITE(numout,*) 'in_cycle(:,11): ',in_cycle(:,11)
          ENDIF
          CALL StomateLpj &
               &            (kjpindex, dt_days, &
               &             lalo, neighbours, resolution, contfrac, &
               &             clay, herbivores, &
               &             tsurf_daily, tsoil_daily, t2m_daily, t2m_min_daily, &
               !spitfire
               &             t2m_max_daily, precip_daily, wspeed_daily, lightn, popd, humign, &
               &             read_observed_ba, observed_ba, &
               &             read_cf_fine,cf_fine,read_cf_coarse,cf_coarse,read_ratio_flag,ratio_flag,read_ratio,ratio, date, &
               !endspit
               &             litterhum_daily, soilhum_daily, &
               &             maxhumrel_lastyear, minhumrel_lastyear, &
               &             gdd0_lastyear, precip_lastyear, &
               &             humrel_month, humrel_week, t2m_longterm, t2m_month, t2m_week, &
               &             tsoil_month, soilhum_month, &
               &             gdd_m5_dormance,  gdd_from_growthinit, gdd_midwinter, ncd_dormance, ngd_minus5, &
               &             turnover_longterm, gpp_daily, gpp_week, &
               &             time_hum_min, maxfpc_lastyear, resp_maint_part,&
               &             PFTpresent, age, fireindex, firelitter, &
               &             leaf_age, leaf_frac, biomass, ind, adapted, regenerate, &
               &             senescence, when_growthinit, litterpart, litter, &
               &             litter_avail, litter_not_avail, litter_avail_frac, &
               &             dead_leaves, carbon,carbon_surf, lignin_struc, &
               !spitfire
               &             ni_acc,fire_numday,fuel_1hr,fuel_10hr,fuel_100hr,fuel_1000hr, & 
               &             lcc,bafrac_deforest_accu,emideforest_litter_accu,emideforest_biomass_accu,& 
               &             deforest_litter_remain,deforest_biomass_remain, &
               &             def_fuel_1hr_remain,def_fuel_10hr_remain,&
               &             def_fuel_100hr_remain,def_fuel_1000hr_remain,&
               !endspit
               &             veget_cov_max, veget_cov_max_new, npp_longterm, lm_lastyearmax, &
               &             veget_lastlight, everywhere, need_adjacent, RIP_time, &
               &             lai, rprof,npp_daily, turnover_daily, turnover_time,&
               &             control_moist_inst, control_temp_inst, soilcarbon_input_inst, &
               &             co2_to_bm_dgvm, co2_fire, &
               &             resp_hetero_d, resp_maint_d, resp_growth_d, &
               &             height, deadleaf_cover, vcmax, &
               &             bm_to_litter,&
               &             prod10, prod100, flux10, flux100, vegetnew_firstday,&
               &             glccNetLCC, &
               &             glccSecondShift,glccPrimaryShift, &
               &             harvest_matrix, bound_spa, glcc_pft, &
               &             convflux, cflux_prod10, cflux_prod100, harvest_above, carb_mass_total, &
               &             fpc_max, Tseason, Tseason_length, Tseason_tmp, &
               &             Tmin_spring_time, begin_leaves, onset_date, &
               &             matrixA,&
!!!!! for crops
               &             pdlai, slai, & 
                             ! for crop allocation
               &             in_cycle, deltai, dltaisen, ssla, pgrain, deltgrain, reprac, &
               &             nger, nlev, ndrp,  nlax, &
               &             c_reserve, c_leafb, nmat, nrec, N_limfert,tday_counter, &
!!!!! end for crops
               &             zz_coef_deep, deepC_a, deepC_s, deepC_p, & !pss:+
               &             ch4_flux_density_tot_0, ch4_flux_density_dif_0, ch4_flux_density_bub_0, &
               &             ch4_flux_density_pla_0, ch4_flux_density_tot_wet1,ch4_flux_density_dif_wet1, &
               &             ch4_flux_density_bub_wet1,ch4_flux_density_pla_wet1,ch4_flux_density_tot_wet2, &
               &             ch4_flux_density_dif_wet2,ch4_flux_density_bub_wet2,ch4_flux_density_pla_wet2, &
               &             ch4_flux_density_tot_wet3,ch4_flux_density_dif_wet3,ch4_flux_density_bub_wet3,&
               &             ch4_flux_density_pla_wet3, ch4_flux_density_tot_wet4,ch4_flux_density_dif_wet4,&
               &             ch4_flux_density_bub_wet4,ch4_flux_density_pla_wet4,tsurf_year, &!) !pss:-
!gmjc
               &             wshtotsum, sr_ugb, compt_ugb, nb_ani, grazed_frac, &
               &             import_yield, sla_age1, t2m_14, sla_calc,snowfall_daily,MODULO(date,365),&
               &             when_growthinit_cut, nb_grazingdays, &
               &             EndOfYear, &          !! Flag set to true for the first sechiba
!gmjc top 5 layer grassland soil moisture for grazing
               &             humrel_daily,tmc_topgrass_daily,fc_grazing,snowmass_daily, &
               &             after_snow, after_wet, wet1day, wet2day)
!end gmjc


          !! 5.3.3 Call STICS components for crop development
!          WRITE(numout,*) 'slai after StomateLpj: ',slai(1,12:14)
!          WRITE(numout,*) 'LAI after alloc: ',lai(1,12:14)
          matrix_prc_rot(:,:,:) = 0.0
          DO j = 2, nvm
!              IF (ok_LAIdev(j)) THEN
              IF (ok_LAIdev(j)) THEN
                  lai(:,j) = slai(:,j) ! allocation may have changed lai, update it
                  DO i = 1, kjpindex
                      
                      ! vegetative biomass
                      masecveg(i, j) = (biomass(i, j, ileaf, icarbon)&          ! leaf
                                     + biomass(i, j, isapabove, icarbon) &   ! above sapwood
                                     + biomass(i, j, icarbres, icarbon))/100.0/0.48     ! carbon reserve
                      
                      ! total aboveground biomass , unit in t/ha
                      masec(i, j) = (biomass(i, j, ileaf, icarbon) + biomass(i, j, isapabove, icarbon)&
                                     + biomass(i, j, icarbres, icarbon) + biomass(i, j, ifruit, icarbon))/100.0/0.48 ! aboveground biomass, 7 is fruit, aboveground dry matter
                      
                      ! biomass increment
                      dltams(i, j) = SUM(biomass(i, j, :, icarbon))/100.0/0.48 - pdbiomass(i, j)   ! unit with t/ha 

                      ! for each crop PFT and each pixel, we call the gain and calculate the crop yield 
                      call grain(tday_counter,vday_counter, dltams(i, j), nflo(i, j), ndrp(i, j),nrec(i, j),nlev(i, j),nrecbutoir(i, j), & ! IN
                                 somcourdrp(i, j), nmat(i, j), masec(i, j), pdmasec(i, j), st2m_min_daily(i), st2m_max_daily(i),  & ! IN  t2m_min_daily and t2m_max_daily should be tcultmin, tcultmax
                                 v_dltams(i, j, :), fgelflo(i, j), pdircarb(i, j), ircarb(i, j), nbgrains(i, j), pgrain(i, j), vitmoy(i, j),nbgraingel(i, j),pgraingel(i, j),  & ! INOUT
                                 dltags(i, j),ftempremp(i, j),magrain(i, j), pdmagrain(i, j), nbj0remp(i, j),pdsfruittot(i, j), deltgrain(i, j), &
                                 SP_nbjgrain(j), SP_codeplante(j), SP_codgelflo(j), SP_tgelflo10(j), SP_tgelflo90(j), SP_cgrain(j), SP_cgrainv0(j), &
                                 SP_nbgrmax(j), SP_nbgrmin(j), SP_codazofruit(j), SP_codeinnact(j), SP_codeir(j), SP_vitircarb(j), SP_irmax(j), SP_vitircarbT(j), &
                                 SP_codetremp(j), SP_tminremp(j), SP_tmaxremp(j), SP_pgrainmaxi(j))                 ! INOUT

                      ! calculate root biomass ratio
                      call reprac_calc(tday_counter, nsen(i, j), nlax(i, j), nflo(i, j), nmat(i, j), nrec(i, j), nplt(i, j), in_cycle(i, j), nger(i, j), st2m_daily(i), nlev(i, j), &      ! IN
                                  zrac(i, j), repracmax(i, j), repracmin(i, j), kreprac(i, j), somtemprac(i, j), urac(i, j), reprac(i, j), nstoprac(i, j),     &
                                  SP_stoprac(j), SP_codeperenne(j), SP_codehypo(j), SP_codegermin(j),SP_zracplantule(j), SP_profsem(j), SP_codtrophrac(j),  &
                                  SP_repracpermax(j), SP_repracpermin(j), SP_krepracperm(j), SP_repracseumax(j), SP_repracseumin(j), SP_krepracseu(j),  &
                                  SP_codetemprac(j), SP_tcmin(j), SP_tcmax(j), SP_codedyntalle(j), SP_tcxstop(j), SP_stlevamf(j), SP_stamflax(j))     ! parameter
   
                      ! after this subroutine, we get the reprac variable for each crop PFT and each pixel

                      IF (.NOT. ok_rotate) THEN
                          ! Check status of crop development
                          IF (nrec(i, j) >  0 ) THEN
                             f_crop_recycle(i, j) = .TRUE. ! control the initialization for this grid
                             in_cycle(i, j) = .FALSE.  ! identifier of the cycle completeness
                             f_sen_lai(i, j) = .TRUE.
                          ENDIF
                      ELSE ! ok_rotate 
                        !!!! invoking crop rotation module here
                        IF ( (nrec(i,j) > 0) .AND. (cyc_num(i,j) < cyc_num_tot(i)) ) THEN
                            f_rot_stom(i,j) = .TRUE.
                        ELSEIF ( (nrec(i,j)>0) .AND. (cyc_num(i,j) .EQ. cyc_num_tot(i)) .AND. (cyc_num_tot(i) .GT. 1) ) THEN
                            f_rot_stom(i,j) = .TRUE.
!                        ELSEIF () !!! when the emergence or maturity fails
!                        ELSEIF ( (nrec(i,j) .EQ. 0) .AND. (nmat(i,j) .GT. 0) .AND.   ) THEN 
!                        ! when maturity is later than next planting date
!                            f_rot_stom(i,j) = .TRUE.
                        ENDIF

                        IF (f_rot_stom(i,j)) THEN ! start rotation commands
                            temp_prc = 0.0
                            DO k = 1,rot_cmd_max
                                mycmd = rot_cmd_store(i,k,cyc_num(i,j))
                                IF (mycmd  .GT. 0) THEN
                                !!!! valid command
                                    CALL stomate_stics_get_cmd(mycmd,stveg,edveg,rotprc)
                                    IF ( (stveg .EQ. j) .AND. (veget_max(i,j) .GT. zero) )  THEN !! find the rotation command(s)
                                        !!! record for rotate_stomate
                                        IF ( (.NOT. ok_LAIdev(edveg)) .AND. (edveg .NE. 1) ) THEN
                                            WRITE(numout,*) 'edveg, mycmd', edveg, mycmd 
                                            STOP 'stomate_main: rotation target not a cropland/bare soil'
                                        ENDIF
                                        matrix_prc_rot(i,stveg,edveg) = rotprc
                                        !!! keep record for rotate_sechiba
                                        IF (.NOT. f_rot_sech(i)) THEN ! the first rotation command
                                            f_rot_sech(i) = .TRUE.
                                            rot_cmd(i,1) = mycmd
                                        ELSE ! some command(s) already 
                                            ! f_rot_sech(i) is already true
                                            itemp = 1
                                            DO WHILE ( (rot_cmd(i,itemp) .GT. 0) .AND. (itemp .LE. SIZE(rot_cmd,2,i_std)) )
                                                itemp = itemp + 1
                                            ENDDO
                                            IF (itemp .GT. SIZE(rot_cmd,2,i_std)) THEN
                                                WRITE(numout,*) 'rotation command more than storage dimension:'
                                                WRITE(numout,*) 'rot_cmd(i,:)', rot_cmd(i,:)
                                                WRITE(numout,*) 'itemp, mycmd', itemp, mycmd
                                                STOP 'increase rot_cmd_max to restart '
                                            ELSEIF (rot_cmd(i,itemp) .LE. 0) THEN
                                                rot_cmd(i,itemp) = mycmd
                                            ELSE  ! it should never goes here...
                                                WRITE(numout,*) 'rot_cmd(i,:)', rot_cmd(i,:)
                                                WRITE(numout,*) 'itemp, SIZE(rot_cmd,2,i_std)',itemp, SIZE(rot_cmd,2,i_std)
                                                STOP 'stomate_main: a logical error in rotation'
                                            ENDIF
                                        ENDIF
                                        temp_prc = temp_prc + rotprc
                                    ENDIF
                                ENDIF
                            ENDDO
                            !!! check if more than 100% of stveg is being converted...
                            IF (temp_prc .GT. 1.0+min_stomate) THEN
                                WRITE(numout,*) 'rotation command error, more than 100% rotated'
                                WRITE(numout,*) 'temp_prc, ', temp_prc
                                STOP 'stomate_main: rotation command'
                            ELSEIF (temp_prc .LT. 1.0-min_stomate) THEN
                                WRITE(numout,*) 'rotation command error, some land disappeared'
                                WRITE(numout,*) 'temp_prc, ', temp_prc
                                STOP 'stomate_main: rotation command'
                            ENDIF

                            f_crop_recycle(i, j) = .TRUE. ! control the initialization for this grid
                            in_cycle(i, j) = .FALSE.  ! identifier of the cycle completeness
                            f_sen_lai(i, j) = .TRUE.
                        ELSE !!! rotation not started, normal end of crop cycle
                          IF (nrec(i, j) >  0 ) THEN
                             f_crop_recycle(i, j) = .TRUE. ! control the initialization for this grid
                             in_cycle(i, j) = .FALSE.  ! identifier of the cycle completeness
                             f_sen_lai(i, j) = .TRUE.
                             IF ( (printlev >= 3) .AND. (veget_max(i,j) .GT. 0) ) THEN
                                WRITE(numout,*) 'rotation not started after harvest of i, j', i, j
                             ENDIF
                          ENDIF                         
                        ENDIF
                        !!!! end rotation, xuhui
                      ENDIF ! ok_rotate
                  ENDDO !kpjindex
              ENDIF !ok_LAIdev
          ENDDO ! PFT loop
!          WRITE(numout,*) 'lai after grain: ',lai(1,12:14)
          IF (ok_rotate) THEN
              DO i = 1, kjpindex
!                 IF ( SUM(SUM(matrix_prc_rot(i,:,:))) .GT. min_stomate) THEN
                 IF ( (ANY(f_rot_stom(i,:))) .AND. (SUM(matrix_prc_rot(i,:,:)) .GT. min_stomate) ) THEN
                    !!! activate the stomate rotation for this point
                    IF (printlev>=3) THEN
                        WRITE(numout,*) 'before stomate_stics_rotation:' 
                        WRITE(numout,*) 'point i',i
                        WRITE(numout,*) 'rot_cmd', rot_cmd
                        WRITE(numout,*) 'plantdate_now(i,:)', plantdate_now(i,:)
                        WRITE(numout,*) 'cyc_num(i,:) ', cyc_num(i,:)
                        DO jv = 1,nvm
                            IF (ok_LAIdev(jv)) WRITE(numout,*) 'jv, turnover(i,jv,:,icarbon)', jv, turnover_daily(i,jv,:,icarbon)
                        ENDDO
                    ENDIF
                    CALL stomate_stics_rotation(kjpindex, i, matrix_prc_rot(i,:,:), veget_max(i,:), &
                                        in_cycle, cyc_num_tot, plantdate, nrec, nlev, &
                                        plantdate_now, &
                                        soilc_total(i,:,:), cyc_num, litter, turnover_daily,&
                                        deepC_a, deepC_s, deepC_p, carbon, &
                                        age, ind, PFTpresent, senescence, &
                                        when_growthinit, everywhere, leaf_frac)
                    f_rot_stom(i,:) = .FALSE.
                    IF (printlev>=3) THEN
                        WRITE(numout,*) 'after stomate_stics_rotation:'
                        WRITE(numout,*) 'plantdate_now(i,:)', plantdate_now(i,:)
                        WRITE(numout,*) 'cyc_num(i,:) ', cyc_num(i,:)
                        DO jv = 1,nvm
                            IF (ok_LAIdev(jv)) WRITE(numout,*) 'jv, turnover(i,jv,:,icarbon)', jv, turnover_daily(i,jv,:,icarbon)
                        ENDDO
                    ENDIF
                 ENDIF
              ENDDO
          ENDIF
          IF (EndOfYear) THEN  ! update crop calendar
              tday_counter = 0
              !!! update plantdate_cyc
              DO j = 2,nvm
                IF (ok_LAIdev(j)) THEN
                    DO i = 1,kjpindex
                        IF (plantdate_now(i,j) .GT. 365) THEN 
                        !!! considering the next year planting date
                            plantdate_now(i,j) = plantdate_now(i,j) - 365
                        ENDIF
                    ENDDO
                ENDIF
              ENDDO
              IF (printlev>=3 .AND. ok_rotate) THEN
                WRITE(numout,*) 'after EndOfYear:'
                WRITE(numout,*) 'plantdate_now(1,:)', plantdate_now(1,:)
              ENDIF
          ENDIF
          !! 5.3.2 Calculate the total CO2 flux from land use change
          fco2_lu(:) = SUM(convflux(:,:) &
               &             + cflux_prod10(:,:)  &
               &             + cflux_prod100(:,:),DIM=2) &
               &             + harvest_above(:)

          !! 5.4 Calculate veget and veget_max
          veget_max(:,:) = zero 
          DO j = 1, nvm
             veget_max(:,j) = veget_max(:,j) + &
                  & veget_cov_max(:,j) * ( 1.-totfrac_nobio(:) )
          ENDDO
          
          !! 5.5 Photosynthesis parameters
          assim_param(:,:,ivcmax) = zero
          DO j = 2,nvm
             assim_param(:,j,ivcmax) = vcmax(:,j)
          ENDDO

          !! 5.6 Update forcing variables for soil carbon
          IF (TRIM(stomate_Cforcing_name) /= 'NONE') THEN
             npp_tot(:) = 0
             DO j=2,nvm
                npp_tot(:) = npp_tot(:) + npp_daily(:,j)
             ENDDO
             ! ::nbyear Number of years saved for carbon spinup
             sf_time = MODULO(REAL(date,r_std)-1,one_year*REAL(nbyear,r_std))
             iatt=FLOOR(sf_time/dt_forcesoil) + 1
             IF (iatt == 0) iatt = iatt_old + 1
             IF ((iatt<iatt_old) .and. (.not. cumul_Cforcing)) THEN
                nforce(:)=0
                soilcarbon_input(:,:,:,:) = zero
                control_moist(:,:,:) = zero
                control_temp(:,:,:) = zero
                npp_equil(:,:) = zero
             ENDIF
             iatt_old = iatt
             ! Update forcing
             nforce(iatt) = nforce(iatt)+1
             soilcarbon_input(:,:,:,iatt) = soilcarbon_input(:,:,:,iatt) + soilcarbon_input_daily(:,:,:)
             control_moist(:,:,iatt) = control_moist(:,:,iatt) + control_moist_daily(:,:)
             control_temp(:,:,iatt) = control_temp(:,:,iatt) + control_temp_daily(:,:)
             npp_equil(:,iatt) = npp_equil(:,iatt) + npp_tot(:)
          ENDIF
          ! 5.6b update forcing variables for permafrost soil carbon
          !
          IF (TRIM(Cforcing_permafrost_name) /= 'NONE') THEN
             npp_tot(:)=0

             DO j=2,nvm
                npp_tot(:)=npp_tot(:) + npp_daily(:,j)
             ENDDO
             sf_time = MODULO(REAL(date,r_std)-1,one_year*REAL(nbyear,r_std))
             iatt=FLOOR(sf_time/dt_forcesoil)+1
             IF ((iatt < 1) .OR. (iatt > nparan*nbyear)) THEN
                WRITE(numout,*) 'Error with iatt=',iatt
                CALL ipslerr_p (3,'stomate', &
                     &          'Error with iatt.', '', &
                     &          '(Problem with dt_forcesoil ?)')
             ENDIF

             IF ((iatt<iatt_old) .and. (.not. cumul_Cforcing)) THEN
                soilcarbon_input_2pfcforcing(:,:,:,:) = 0
                pb_2pfcforcing(:,:) = 0
                snow_2pfcforcing(:,:) = 0
                tprof_2pfcforcing(:,:,:,:) = 0
!!!cdk treat fbact differently so that we take the mean rate, not the mean
!residence time
                fbact_2pfcforcing(:,:,:,:) = 0
                hslong_2pfcforcing(:,:,:,:) = 0
                veget_max_2pfcforcing(:,:,:) = 0
                rprof_2pfcforcing(:,:,:) = 0
                tsurf_2pfcforcing(:,:) = 0
                !adding two snow forcings
                snowdz_2pfcforcing(:,:,:) = 0
                snowrho_2pfcforcing(:,:,:) = 0
             ENDIF
             iatt_old=iatt

                soilcarbon_input_2pfcforcing(:,:,:,iatt) = soilcarbon_input_2pfcforcing(:,:,:,iatt) + soilcarbon_input_daily(:,:,:)
                pb_2pfcforcing(:,iatt) = pb_2pfcforcing(:,iatt) + pb_pa_daily(:)
                snow_2pfcforcing(:,iatt) = snow_2pfcforcing(:,iatt) + snow_daily(:)
                tprof_2pfcforcing(:,:,:,iatt) = tprof_2pfcforcing(:,:,:,iatt) + tdeep_daily(:,:,:)
!!!cdk treat fbact differently so that we take the mean rate, not the mean
!residence time
                fbact_2pfcforcing(:,:,:,iatt) = fbact_2pfcforcing(:,:,:,iatt) + prmfrst_soilc_tempctrl_daily(:,:,:)
                hslong_2pfcforcing(:,:,:,iatt) = hslong_2pfcforcing(:,:,:,iatt) + hsdeep_daily(:,:,:)
                veget_max_2pfcforcing(:,:,iatt) = veget_max_2pfcforcing(:,:,iatt) + veget_max(:,:)
                rprof_2pfcforcing(:,:,iatt) = rprof_2pfcforcing(:,:,iatt) + rprof(:,:)
                tsurf_2pfcforcing(:,iatt) = tsurf_2pfcforcing(:,iatt) + temp_sol_daily(:)
                !adding two snow forcings
                snowdz_2pfcforcing(:,:,iatt) = snowdz_2pfcforcing(:,:,iatt) + snowdz_daily(:,:)
                snowrho_2pfcforcing(:,:,iatt) = snowrho_2pfcforcing(:,:,iatt) + snowrho_daily(:,:)
          ENDIF
       
       !! 5.8 Write forcing file if ::ok_co2=.TRUE.
       ! Note: if STOMATE is run in coupled mode the forcing file is written
       ! If run in stand-alone mode, the forcing file is read!
       IF ( ok_co2 .AND. TRIM(stomate_forcing_name) /= 'NONE' ) THEN
          
          !! 5.8.1 Convert GPP to sechiba time steps
          ! GPP is multiplied by coverage to obtain forcing @tex $(gC m^{-2} dt_stomate^{-1})$\f \end@tex $(m^2 m^{-2})$ @endtexonly
          ! @tex$ m^{-2}$ @endtex remains in the units because ::veget_cov_max is a fraction, not a 
          ! surface area. In sechiba values are ponderated by surface and frac_no_bio. 
          ! At the beginning of stomate, the units are converted. 
          ! When we use forcesoil we call sechiba_main and so we need the have the same units as in sechiba.
          gpp_daily_x(:,:) = zero
          DO j = 2, nvm             
             gpp_daily_x(:,j) = gpp_daily_x(:,j) + &
              & gpp_daily(:,j) * dt_stomate / one_day * veget_cov_max(:,j)
          ENDDO
          
          ! Bare soil moisture availability has not been treated
          ! in STOMATE, update it here
          humrel_daily(:,ibare_sechiba) = humrel(:,ibare_sechiba)   

          ! Update index to store the next forcing step in memory
          iisf = iisf+1

          ! How many times have we treated this forcing state
          xn = REAL(nf_cumul(isf(iisf)),r_std)
          
          !! 5.8.2 Cumulate forcing variables
          ! Cumulate forcing variables (calculate average)
          ! Note: precipitation is multiplied by dt_stomate/one_day to be consistent with 
          ! the units in sechiba
          IF (cumul_forcing) THEN
             clay_fm(:,iisf) = (xn*clay_fm(:,iisf)+clay(:))/(xn+1.)
             humrel_daily_fm(:,:,iisf) = &
                  & (xn*humrel_daily_fm(:,:,iisf) + humrel_daily(:,:))/(xn+1.)
             litterhum_daily_fm(:,iisf) = &
                  & (xn*litterhum_daily_fm(:,iisf)+litterhum_daily(:))/(xn+1.)
             t2m_daily_fm(:,iisf) = &
                  & (xn*t2m_daily_fm(:,iisf)+t2m_daily(:))/(xn+1.)
             t2m_min_daily_fm(:,iisf) = &
                  & (xn*t2m_min_daily_fm(:,iisf)+t2m_min_daily(:))/(xn+1.)
             !spitfire
             t2m_max_daily_fm(:,iisf) = &
                           ( xn*t2m_max_daily_fm(:,iisf) + t2m_max_daily(:) ) / (xn+1.)
             wspeed_daily_fm(:,iisf) = &
                           ( xn*wspeed_daily_fm(:,iisf) + wspeed_daily(:) ) / (xn+1.)
             !endspit
             tsurf_daily_fm(:,iisf) = &
                  & (xn*tsurf_daily_fm(:,iisf)+tsurf_daily(:))/(xn+1.)
             tsoil_daily_fm(:,:,iisf) = &
                  & (xn*tsoil_daily_fm(:,:,iisf)+tsoil_daily(:,:))/(xn+1.)
             soilhum_daily_fm(:,:,iisf) = &
                  & (xn*soilhum_daily_fm(:,:,iisf)+soilhum_daily(:,:))/(xn+1.)
             precip_fm(:,iisf) = &
                  & (xn*precip_fm(:,iisf)+precip_daily(:)*dt_stomate/one_day)/(xn+1.)
             gpp_daily_fm(:,:,iisf) = &
                  & (xn*gpp_daily_fm(:,:,iisf) + gpp_daily_x(:,:))/(xn+1.)
             veget_fm(:,:,iisf) = &
                  & (xn*veget_fm(:,:,iisf) + veget(:,:) )/(xn+1.)
             veget_max_fm(:,:,iisf) = &
                  & (xn*veget_max_fm(:,:,iisf) + veget_max(:,:) )/(xn+1.)
             lai_fm(:,:,iisf) = &
                  & (xn*lai_fm(:,:,iisf) + lai(:,:) )/(xn+1.)
             t2mdiag_fm(:,iisf) = &
                  & (xn*t2mdiag_fm(:,iisf)+t2m(:))/(xn+1.)
             swdown_fm(:,iisf) = &
                  & (xn*swdown_fm(:,iisf)+swdown(:))/(xn+1.)
             evapot_corr_fm(:,iisf) = &
                  & (xn*evapot_corr_fm(:,iisf)+evapot_corr(:))/(xn+1.)
          ELSE
             ! Here we just calculate the values
             clay_fm(:,iisf) = clay(:)
             humrel_daily_fm(:,:,iisf) = humrel_daily(:,:)
             litterhum_daily_fm(:,iisf) = litterhum_daily(:)
             t2m_daily_fm(:,iisf) = t2m_daily(:)
             t2m_min_daily_fm(:,iisf) =t2m_min_daily(:)
             !spitfire
             t2m_max_daily_fm(:,iisf) =t2m_max_daily(:)
             wspeed_daily_fm(:,iisf) = wspeed_daily(:)
             !endspit
             tsurf_daily_fm(:,iisf) = tsurf_daily(:)
             tsoil_daily_fm(:,:,iisf) =tsoil_daily(:,:)
             soilhum_daily_fm(:,:,iisf) =soilhum_daily(:,:)
             precip_fm(:,iisf) = precip_daily(:)
             gpp_daily_fm(:,:,iisf) =gpp_daily_x(:,:)
             veget_fm(:,:,iisf) = veget(:,:)
             veget_max_fm(:,:,iisf) =veget_max(:,:)
             lai_fm(:,:,iisf) =lai(:,:)
             t2mdiag_fm(:,iisf) =t2m(:)
             swdown_fm(:,iisf) =swdown(:)
             evapot_corr_fm(:,iisf) =evapot_corr(:)
          ENDIF
          nf_cumul(isf(iisf)) = nf_cumul(isf(iisf))+1

          ! 5.8.3 Do we have to write the forcing states?
          IF (iisf == nsfm) THEN

             !! 5.8.3.1 Write these forcing states
             CALL forcing_write(forcing_id,1,nsfm)
             ! determine which forcing states must be read
             isf(1) = isf(nsfm)+1
             IF ( isf(1) > nsft ) isf(1) = 1
             DO iisf = 2, nsfm
                isf(iisf) = isf(iisf-1)+1
                IF (isf(iisf) > nsft)  isf(iisf) = 1
             ENDDO

             ! Read forcing variables - for debug use only
             ! CALL forcing_read(forcing_id,nsfm)
             iisf = 0

          ENDIF

       ENDIF


       !! 5.9 Compute daily CO2 flux (AR5 output - not essential)
       ! CO2 flux in @tex $gC m^{-2} s^{-1}$ @endtex (positive towards the atmosphere) is sum of:
       ! (1) heterotrophic respiration from ground + (2) maintenance respiration 
       ! from the plants + (3) growth respiration from the plants + (4) co2 
       ! emissions from fire - (5) co2 taken up in the DGVM to establish 
       ! saplings - (6) co2 taken up by photosyntyhesis
       co2_flux_daily(:,:)=   &
            & resp_maint_d(:,:) + resp_growth_d(:,:) + resp_hetero_d(:,:) + &
            & co2_fire(:,:) - co2_to_bm_dgvm(:,:) - gpp_daily(:,:)

     CALL xios_orchidee_send_field("nep",SUM(co2_flux_daily*veget_cov_max,dim=2)/1e3/one_day)

       IF ( hist_id_stom_IPCC > 0 ) THEN
          vartmp(:) = SUM(co2_flux_daily*veget_cov_max,dim=2)/1e3/one_day*contfrac
          CALL histwrite_p (hist_id_stom_IPCC, "nep", itime, &
               vartmp, kjpindex, hori_index)
       ENDIF

       ! See 5.9 for details on NEP + fire. At the monthly time step also 
       ! harvest and land use change are calculated
       co2_flux_monthly(:,:) = co2_flux_monthly(:,:) + co2_flux_daily(:,:)
       harvest_above_monthly(:) = harvest_above_monthly(:) + harvest_above(:)
       cflux_prod_monthly(:,:) = cflux_prod_monthly(:,:) + convflux(:,:) + & 
        & cflux_prod10(:,:) + cflux_prod100(:,:)
      
       !! 5.10 Compute monthly CO2 fluxes 
       IF ( EndOfMonth ) THEN
          !! 5.10.1 Write history file for monthly fluxes
          CALL histwrite_p (hist_id_stomate, 'CO2FLUX', itime, &
               co2_flux_monthly, kjpindex*nvm, horipft_index)
          
             CALL histwrite_p (hist_id_stomate, "NONBIOFRAC", itime,&
                  totfrac_nobio, kjpindex, hori_index)
          !?? I (=VB) translated the French, but the whole stuff does not make sense to me.
          ! If one deletes the montly cumulation,
          ! one should not forget this change in resolution(:,1)*resolution(:,2)*contfrac(:)
          ! Si on supprimer le cumul par mois, 
          ! il ne faut pas oublier cette modif resolution(:,1)*resolution(:,2)*contfrac(:)
          ! Should be supressed, this is post-processing 
          DO j=2, nvm
             co2_flux_monthly(:,j) = co2_flux_monthly(:,j)* &
                  resolution(:,1)*resolution(:,2)*contfrac(:)
          ENDDO

          ! Should be supressed, this is post-processing
          ! ?? How does it differ from co2_flux_monthly??
          net_co2_flux_monthly = zero
          DO ji=1,kjpindex
             DO j=2,nvm
                net_co2_flux_monthly = net_co2_flux_monthly + &
                     &  co2_flux_monthly(ji,j)*veget_cov_max(ji,j)
             ENDDO
          ENDDO

     
          !! 5.10.2 Cumulative fluxes of land use cover change, harvest and net biosphere production
          ! Parallel processing, gather the information from different processors. first argument is the lo
          ! local variable, the second argument is the global variable. bcast send it to all processors.
          net_cflux_prod_monthly_sum = &
              &  SUM(SUM(cflux_prod_monthly(:,:),DIM=2)*resolution(:,1)*resolution(:,2)*contfrac(:))*1e-15
          CALL reduce_sum(net_cflux_prod_monthly_sum,net_cflux_prod_monthly_tot)
          CALL bcast(net_cflux_prod_monthly_tot)
          net_harvest_above_monthly_sum = &
             &   SUM(harvest_above_monthly(:)*resolution(:,1)*resolution(:,2)*contfrac(:))*1e-15
          CALL reduce_sum(net_harvest_above_monthly_sum,net_harvest_above_monthly_tot)
          CALL bcast(net_harvest_above_monthly_tot)
          net_co2_flux_monthly = net_co2_flux_monthly*1e-15
          CALL reduce_sum(net_co2_flux_monthly,net_co2_flux_monthly_sum)
          CALL bcast(net_co2_flux_monthly_sum)
          net_biosp_prod_monthly_tot =  &
             & ( net_co2_flux_monthly_sum + net_cflux_prod_monthly_tot + &
             & net_harvest_above_monthly_tot )
          
          WRITE(numout,9010) 'GLOBAL net_cflux_prod_monthly    (Peta gC/month)  = ',net_cflux_prod_monthly_tot
          WRITE(numout,9010) 'GLOBAL net_harvest_above_monthly (Peta gC/month)  = ',net_harvest_above_monthly_tot
          WRITE(numout,9010) 'GLOBAL net_co2_flux_monthly      (Peta gC/month)  = ',net_co2_flux_monthly_sum
          WRITE(numout,9010) 'GLOBAL net_biosp_prod_monthly    (Peta gC/month)  = ',net_biosp_prod_monthly_tot

9010  FORMAT(A52,F17.14)

          ! Reset Monthly values
          co2_flux_monthly(:,:) = zero
          harvest_above_monthly(:) = zero
          cflux_prod_monthly(:,:)    = zero

       ENDIF ! Monthly processes - at the end of the month
       
       IF (spinup_analytic) THEN
          nbp_accu(:) = nbp_accu(:) + (-SUM(co2_flux_daily(:,:) * veget_max(:,:),dim=2) - SUM(convflux(:,:) + cflux_prod10(:,:) + &
                    cflux_prod100(:,:),DIM=2)  - harvest_above(:))/1e3 
       ENDIF

       !! 5.11 Reset daily variables
       humrel_daily(:,:) = zero
       litterhum_daily(:) = zero
       t2m_daily(:) = zero
       t2m_min_daily(:) = large_value
       !spitfire
       t2m_max_daily(:) = (-1)*large_value
       wspeed_daily(:) = zero
       !endspit
       tsurf_daily(:) = zero
       tsoil_daily(:,:) = zero
       soilhum_daily(:,:) = zero
       precip_daily(:) = zero
       gpp_daily(:,:) = zero
       resp_maint_part(:,:,:)=zero
       resp_hetero_d=zero
       IF (printlev >= 3) THEN
          WRITE(numout,*) 'stomate_main: daily processes done'
       ENDIF

    ENDIF  ! Daily processes - at the end of the day
    
  !! 6. Outputs from Stomate

    ! co2_flux receives a value from STOMATE only if STOMATE is activated.
    ! Otherwise, the calling hydrological module must do this itself.

    !! 6.1 Respiration and fluxes
    resp_maint(:,:) = resp_maint_radia(:,:)*veget_cov_max(:,:)
    resp_maint(:,ibare_sechiba) = zero
    resp_growth(:,:)= resp_growth_d(:,:)*veget_cov_max(:,:)*dt_sechiba/one_day
    resp_hetero(:,:) = resp_hetero_radia(:,:)*veget_cov_max(:,:)
    
    !! 6.2 Derived CO2 fluxes
    ! CO2 flux in gC m^{-2} s^{-1} (positive towards the atmosphere) is sum of:
    ! (1) heterotrophic respiration from ground + (2) maintenance respiration 
    ! from the plants + (3) growth respiration from the plants + (4) co2 
    ! emissions from fire - (5) co2 taken up in the DGVM to establish 
    ! saplings - (6) co2 taken up by photosyntyhesis
    co2_flux(:,:) = resp_hetero(:,:) + resp_maint(:,:) + resp_growth(:,:) &
         & + (co2_fire(:,:)-co2_to_bm_dgvm(:,:))*veget_cov_max(:,:)/one_day &
         & - gpp(:,:)
    
    temp_growth(:)=t2m_month(:)-tp_00 


    ! 6.3 output of crop variables, xuhui

    IF (ANY(ok_LAIdev)) THEN
!        CALL histwrite_p (hist_id_stomate, 'TCULT', itime, &
!                       tcult, kjpindex*nvm, horipft_index)
!        CALL histwrite_p (hist_id_stomate, 'UDEVAIR', itime, &
!                       udevair, kjpindex*nvm, horipft_index)
!        CALL histwrite_p (hist_id_stomate, 'UDEVCULT', itime, &
!                       udevcult, kjpindex*nvm, horipft_index)
!        CALL histwrite_p (hist_id_stomate, 'SHUMREL', itime, &
!                       shumrel, kjpindex*nvm, horipft_index)
!        CALL histwrite_p (hist_id_stomate, 'TURFAC', itime, &
!                       turfac, kjpindex*nvm, horipft_index)
!        CALL histwrite_p (hist_id_stomate, 'TURSLA', itime, &
!                       tursla, kjpindex*nvm, horipft_index)
!        CALL histwrite_p (hist_id_stomate, 'SWFAC', itime, &
!                       swfac, kjpindex*nvm, horipft_index)
!        CALL histwrite_p (hist_id_stomate, 'SENFAC', itime, &
!                       senfac, kjpindex*nvm, horipft_index)a

        CALL xios_orchidee_send_field('NLEV', real(nlev))
        CALL xios_orchidee_send_field('NFLO', real(nflo))
        CALL xios_orchidee_send_field('NDRP', real(ndrp))
        CALL xios_orchidee_send_field('NMAT', real(nmat))
        CALL xios_orchidee_send_field('NREC', real(nrec))
        CALL xios_orchidee_send_field('REPRAC',reprac)
        CALL xios_orchidee_send_field('DLTLAI',deltai)
        CALL xios_orchidee_send_field('DLTLAISEN', dltaisen)
        CALL xios_orchidee_send_field('IRCARB', ircarb)
        CALL xios_orchidee_send_field('PlantDate', float(plantdate(:,:,1)))

        CALL histwrite_p (hist_id_stomate, 'NLEV', itime, &
                       real(nlev), kjpindex*nvm, horipft_index)
        CALL histwrite_p (hist_id_stomate, 'NFLO', itime, &
                       real(nflo), kjpindex*nvm, horipft_index)
        CALL histwrite_p (hist_id_stomate, 'NDRP', itime, &
                       real(ndrp), kjpindex*nvm, horipft_index)
        CALL histwrite_p (hist_id_stomate, 'NREC', itime, &
                       real(nrec), kjpindex*nvm, horipft_index)
        CALL histwrite_p (hist_id_stomate, 'NMAT', itime, &
                       real(nmat), kjpindex*nvm, horipft_index)
        CALL histwrite_p (hist_id_stomate, 'REPRAC', itime, &
                       reprac, kjpindex*nvm, horipft_index)
        CALL histwrite_p (hist_id_stomate, 'DLTLAI', itime, &
                       deltai, kjpindex*nvm, horipft_index)
        CALL histwrite_p (hist_id_stomate, 'DLTLAISEN', itime, &
                       dltaisen, kjpindex*nvm, horipft_index)
        CALL histwrite_p (hist_id_stomate, 'IRCARB', itime, &
                       ircarb, kjpindex*nvm, horipft_index)
    
         ! for planting date and nitrogen fertilization
!         IF ( .NOT. iplt_1d) THEN
             CALL histwrite_p (hist_id_stomate, 'PlantDate', itime, &
                 float(plantdate(:,:,1)), kjpindex*nvm, horipft_index)
    !        CALL histwrite_p (hist_id_stomate, 'PLNTDT', itime, &
    !                    real(plantdate), kjpindex*nvm, horipft_index)
!         ENDIF
         IF ( nitrogen_use) THEN
             CALL histwrite_p (hist_id_stomate, 'N_add', itime, &
                       N_add(:,:), kjpindex*nvm, horipft_index)
    !        CALL histwrite_p (hist_id_stomate, 'N_ADD', itime, &
    !                    N_add, kjpindex*nvm, horipft_index)
    !        CALL histwrite_p (hist_id_stomate, 'N_LIMFERT', itime, &
    !                    N_limfert, kjpindex*nvm, horipft_index)
             CALL xios_orchidee_send_field('N_add', N_add)
         ENDIF
     
    
     ENDIF
   
  !! 7. Analytical spinup

    IF (spinup_analytic) THEN

       !! 7.1. Update V and U at sechiba time step
       DO m = 2,nvm
          DO j = 1,kjpindex 
             ! V <- A * V
             MatrixV(j,m,:,:) = MATMUL(matrixA(j,m,:,:),MatrixV(j,m,:,:))
             ! U <- A*U + B
             VectorU(j,m,:) = MATMUL(matrixA(j,m,:,:),VectorU(j,m,:)) + vectorB(j,m,:)
          ENDDO ! loop pixels
       ENDDO ! loop PFTS


       !! 7.2. What happened at the end of the year ?
       IF (EndOfYear) THEN

          !
          ! 7.2.1 Increase the years counter every EndOfyear
          !
          global_years = global_years + 1 


          !
          ! 7.2.3 Is global_years is a multiple of the period time ?
          !

          !
          ! 3.2.1 When global_years is a multiple of the spinup_period, we calculate :
          !       1) the mean nbp flux over the period. This value is restarted
          !       2) we solve the matrix system by Gauss Jordan method
          !       3) We test if a point is at equilibrium : if yes, we mark the point (ok_equilibrium array)
          !       4) Then we reset the matrix 
          !       5) We erase the carbon_stock calculated by ORCHIDEE by the one found by the method
          IF( MOD(global_years, spinup_period) == 0 ) THEN
             WRITE(numout,*) 'Spinup analytic : Calculate if system is in equlibrium. global_years=',global_years
             ! The number total of days during the forcing period is given by :
             !    spinup_period*365 (we consider only the noleap calendar)
             nbp_flux(:) = nbp_accu(:) / ( spinup_period * 365.)
             ! Reset the values
             nbp_accu(:) = zero

             carbon_stock(:,ibare_sechiba,:) = zero
             ! Prepare the matrix for the resolution
             ! Add a temporary matrix W which contains I-MatrixV
             ! we should take the opposite of matrixV and add the identitiy : we solve (I-MatrixV)*C = VectorU
             MatrixW(:,:,:,:) = moins_un * MatrixV(:,:,:,:)
             DO jv = 1,nbpools
                MatrixW(:,:,jv,jv) =  MatrixW(:,:,jv,jv) + un
             ENDDO
             carbon_stock(:,:,:) = VectorU(:,:,:)

             !
             !  Solve the linear system
             !
             DO m = 2,nvm
                DO j = 1,kjpindex
                   ! the solution will be stored in VectorU : so it should be restarted before
                   ! loop over npts and nvm, so we solved npts*(nvm-1) (7,7) linear systems
                   CALL gauss_jordan_method(nbpools,MatrixW(j,m,:,:),carbon_stock(j,m,:))
                ENDDO ! loop pixels
             ENDDO ! loop PFTS

             ! Reset temporary matrixW
             MatrixW(:,:,:,:) = zero 


             previous_stock(:,:,:) = current_stock(:,:,:)
             current_stock(:,:,:) = carbon_stock(:,:,:)  
             ! The relative error is calculated over the passive carbon pool (sum over the pfts) over the pixel.
             CALL error_L1_passive(kjpindex,nvm, nbpools, current_stock, previous_stock, veget_max, &
                  &                eps_carbon, carbon_eq)   

             !! ok_equilibrium is saved,
             WHERE( carbon_eq(:) .AND. .NOT.(ok_equilibrium(:)) )
                ok_equilibrium(:) = .TRUE.  
             ENDWHERE

             ! Reset matrixV for the pixel to the identity matrix and vectorU to zero
             MatrixV(:,:,:,:) = zero
             VectorU(:,:,:) = zero
             DO jv = 1,nbpools
                MatrixV(:,:,jv,jv) = un
             END DO
             WRITE(numout,*) 'Reset for matrixV and VectorU done'    

             !! Write the values found in the standard outputs of ORCHIDEE
             litter(:,istructural,:,iabove,icarbon) = carbon_stock(:,:,istructural_above)
             litter(:,istructural,:,ibelow,icarbon) = carbon_stock(:,:,istructural_below)
             litter(:,imetabolic,:,iabove,icarbon) = carbon_stock(:,:,imetabolic_above)
             litter(:,imetabolic,:,ibelow,icarbon) = carbon_stock(:,:,imetabolic_below)
             carbon(:,iactive,:) = carbon_stock(:,:,iactive_pool)
             carbon(:,islow,:) = carbon_stock(:,:,islow_pool)
             carbon(:,ipassive,:) = carbon_stock(:,:,ipassive_pool) 

             ! Final step, test if all points at the local domain are at equilibrium
             ! The simulation can be stopped when all local domains have reached the equilibrium
             IF(ALL(ok_equilibrium)) THEN
                WRITE(numout,*) 'Spinup analytic : Equilibrium for carbon pools is reached for current local domain'
             ELSE
                WRITE(numout,*) 'Spinup analytic : Equilibrium for carbon pools is not yet reached for current local domain'
             END IF
          ENDIF ! ( MOD(global_years,spinup_period) == 0)
       ENDIF ! (EndOfYear)

    ENDIF !(spinup_analytic)
   
    IF (printlev >= 4) WRITE(numout,*) 'Leaving stomate_main'

  END SUBROUTINE stomate_main

!! ================================================================================================================================
!! SUBROUTINE 	: stomate_finalize
!!
!>\BRIEF        Write variables to restart file
!!
!! DESCRIPTION  : Write variables to restart file
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): 
!!
!! REFERENCES	: 
!!
!! \n
!_ ================================================================================================================================

  SUBROUTINE stomate_finalize (kjit, kjpindex, index, clay, &
                               zz_deep, zz_coef_deep, thawed_humidity, depth_organic_soil, &
                               assim_param, altmax ) 
    
    IMPLICIT NONE
    
    !! 0. Variable and parameter declaration
    !! 0.1 Input variables
    INTEGER(i_std),INTENT(in)                       :: kjit              !! Time step number (unitless)
    INTEGER(i_std),INTENT(in)                       :: kjpindex          !! Domain size - terrestrial pixels only (unitless)
    INTEGER(i_std),DIMENSION(kjpindex),INTENT(in)   :: index             !! Indices of the terrestrial pixels only (unitless)
    REAL(r_std),DIMENSION(kjpindex),INTENT(in)      :: clay              !! Clay fraction of soil (0-1, unitless)
    REAL(r_std), DIMENSION(ndeep),   INTENT (in)    :: zz_deep           !! deep vertical profile
    REAL(r_std), DIMENSION(ndeep),   INTENT (in)    :: zz_coef_deep      !! deep vertical profile

    REAL(r_std),DIMENSION(kjpindex,nvm,npco2),INTENT(in) :: assim_param    !! min+max+opt temperatures (K) & vmax for 
                                                                            !! photosynthesis  
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(inout)  :: altmax
    !! 0.2 Modified variables
    REAL(r_std), DIMENSION(kjpindex),   INTENT (inout)            :: thawed_humidity    !! specified humidity of thawed soil
    REAL(r_std), DIMENSION(kjpindex),   INTENT (inout)            :: depth_organic_soil !! how deep is the organic soil?

    !! 0.4 Local variables
    REAL(r_std)                                   :: dt_days_read             !! STOMATE time step read in restart file (days)
    INTEGER(i_std)                                :: l,k,ji, jv, i, j, m      !! indices    
    REAL(r_std),PARAMETER                         :: max_dt_days = 5.         !! Maximum STOMATE time step (days)
    REAL(r_std)                                   :: hist_days                !! Writing frequency for history file (days)
    REAL(r_std),DIMENSION(0:nbdl)                 :: z_soil                   !! Variable to store depth of the different soil layers (m)
    REAL(r_std),DIMENSION(kjpindex)               :: cvegtot                  !! Total "vegetation" cover (unitless)
    REAL(r_std),DIMENSION(kjpindex)               :: precip                   !! Total liquid and solid precipitation  
                                                                              !! @tex $(??mm dt_stomate^{-1})$ @endtex 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: gpp_d                    !! Gross primary productivity per ground area 
                                                                              !! @tex $(??gC m^{-2} dt_stomate^{-1})$ @endtex  
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: gpp_daily_x              !! "Daily" gpp for teststomate  
                                                                              !! @tex $(??gC m^{-2} dt_stomate^{-1})$ @endtex 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: resp_hetero_litter       !! Litter heterotrophic respiration per ground area 
                                                                              !! @tex $(gC m^{-2} day^{-1})$ @endtex  
                                                                              !! ??Same variable is also used to 
                                                                              !! store heterotrophic respiration per ground area 
                                                                              !! over ::dt_sechiba?? 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: resp_hetero_soil         !! soil heterotrophic respiration  
                                                                              !! @tex $(gC m^{-2} day^{-1})$ @endtex
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: veget_cov                !! Fractional coverage: actually share of the pixel 
                                                                              !! covered by a PFT (fraction of ground area), 
                                                                              !! taking into account LAI ??(= grid scale fpc)?? 
    REAL(r_std),DIMENSION(kjpindex,nvm)           :: vcmax                    !! Maximum rate of carboxylation
                                                                              !! @tex $(\mumol m^{-2} s^{-1})$ @endtex
    REAL(r_std),DIMENSION(kjpindex,nlevs)         :: control_moist_inst       !! Moisture control of heterotrophic respiration 
                                                                              !! (0-1, unitless) 
    REAL(r_std),DIMENSION(kjpindex,nlevs)         :: control_temp_inst        !! Temperature control of heterotrophic 
                                                                              !! respiration, above and below (0-1, unitless) 
    REAL(r_std),DIMENSION(kjpindex,ncarb,nvm)     :: soilcarbon_input_inst    !! Quantity of carbon going into carbon pools from 
                                                                              !! litter decomposition 
                                                                              !! @tex $(gC m^{-2} day^{-1})$ @endtex 
    
    INTEGER(i_std)                                :: ier                      !! Check errors in netcdf call (unitless)
    REAL(r_std)                                   :: sf_time                  !! Intermediate variable to calculate current time 
                                                                              !! step 
    INTEGER(i_std)                                :: max_totsize              !! Memory management - maximum memory size (Mb)
    INTEGER(i_std)                                :: totsize_1step            !! Memory management - memory required to store one 
                                                                              !! time step on one processor (Mb) 
    INTEGER(i_std)                                :: totsize_tmp              !! Memory management - memory required to store one 
                                                                              !! time step on all processors(Mb) 
    REAL(r_std)                                   :: xn                       !! How many times have we treated in this forcing 
    REAL(r_std), DIMENSION(kjpindex)              :: vartmp                   !! Temporary variable
    INTEGER(i_std)                                :: vid                      !! Variable identifer of netCDF (unitless)
    INTEGER(i_std)                                :: nneigh                   !! Number of neighbouring pixels
    INTEGER(i_std)                                :: direct                   !! ??
    INTEGER(i_std),DIMENSION(ndm)                 :: d_id                     !! ??
    REAL(r_std),DIMENSION(nbp_glo)                :: clay_g                   !! Clay fraction of soil (0-1, unitless), parallel 
                                                                              !! computing 
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:,:)    :: soilcarbon_input_g       !! Quantity of carbon going into carbon pools from 
                                                                              !! litter decomposition  
                                                                              !! @tex $(gC m^{-2} dt_sechiba^{-1})$ @endtex, parallel 
                                                                              !! computing 
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)      :: control_moist_g          !! Moisture control of heterotrophic respiration 
                                                                              !! (0-1, unitless), parallel computing 
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:,:)      :: control_temp_g           !! Temperature control of heterotrophic respiration 
                                                                              !! (0-1, unitless), parallel computing 
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:)        :: npp_equil_g              !! Equilibrium NPP written to forcesoil 
                                                                              !! @tex $(gC m^{-2} year^{-1})$ @endtex, parallel 
                                                                              !! computing 

    REAL(r_std)                                   :: net_cflux_prod_monthly_sum    !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std)                                   :: net_cflux_prod_monthly_tot    !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std)                                   :: net_harvest_above_monthly_sum !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std)                                   :: net_harvest_above_monthly_tot !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std)                                   :: net_biosp_prod_monthly_sum    !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std)                                   :: net_biosp_prod_monthly_tot    !! AR5 output?? gC m2 month-1 (one variable for 
                                                                                   !! reduce_sum and one for bcast??), parallel 
                                                                                   !! computing 
    REAL(r_std), DIMENSION(kjpindex,nvm,nbpools)  :: carbon_stock                  !! Array containing the carbon stock for each pool
                                                                                   !! used by ORCHIDEE


!_ ================================================================================================================================
    
    !! 1. Write restart file for stomate
    IF (printlev>=3) WRITE (numout,*) 'Write restart file for STOMATE'
             
    CALL writerestart &
        &         (kjpindex, index, &
        &          dt_days, date, &
        &          ind, adapted, regenerate, &
        &          humrel_daily, gdd_init_date, litterhum_daily, &
        &          t2m_daily, t2m_min_daily, & 
        &          t2m_max_daily, wspeed_daily, &
        &          tsurf_daily, tsoil_daily, &
        &          soilhum_daily, precip_daily, &
        &          gpp_daily, npp_daily, turnover_daily, &
        &          humrel_month, humrel_week, &
        &          t2m_longterm, tau_longterm, t2m_month, t2m_week, &
        &          tsoil_month, soilhum_month, fireindex, firelitter, &
        &          maxhumrel_lastyear, maxhumrel_thisyear, &
        &          minhumrel_lastyear, minhumrel_thisyear, &
        &          maxgppweek_lastyear, maxgppweek_thisyear, &
        &          gdd0_lastyear, gdd0_thisyear, &
        &          precip_lastyear, precip_thisyear, &
        &          gdd_m5_dormance, gdd_from_growthinit, gdd_midwinter, ncd_dormance, ngd_minus5, &
        &          PFTpresent, npp_longterm, lm_lastyearmax, lm_thisyearmax, &
        &          maxfpc_lastyear, maxfpc_thisyear, &
        &          turnover_longterm, gpp_week, biomass, resp_maint_part, &
        &          leaf_age, leaf_frac, &
        &          senescence, when_growthinit, age, &
        &          resp_hetero_d, resp_maint_d, resp_growth_d, co2_fire, co2_to_bm_dgvm, &
        &          veget_lastlight, everywhere, need_adjacent, &
        &          RIP_time, &
        &          time_hum_min, hum_min_dormance, &
        &          litterpart, litter, dead_leaves, &
        &          carbon, lignin_struc, &
        &          ni_acc,fuel_1hr,fuel_10hr,fuel_100hr,fuel_1000hr, &
        &          turnover_time, &
        &          prod10,prod100,flux10, flux100, &
        &          convflux, cflux_prod10, cflux_prod100, bm_to_litter, carb_mass_total, &
        &          Tseason, Tseason_length, Tseason_tmp, &
        &          Tmin_spring_time, begin_leaves, onset_date, &
        &          global_years, ok_equilibrium, nbp_accu, nbp_flux, &
        &          MatrixV, VectorU, previous_stock, current_stock, assim_param, &
        &          deepC_a, deepC_s, deepC_p, O2_soil, CH4_soil, O2_snow,CH4_snow, &
        &          thawed_humidity, depth_organic_soil, altmax,fixed_cryoturbation_depth, & !pss+:wetlabd CH4 emissions
        &         uo_0, uold2_0, uo_wet1, uold2_wet1, uo_wet2, &
        &         uold2_wet2, uo_wet3, uold2_wet3,  uo_wet4, uold2_wet4, tsurf_year, &!) !pss- 
    !gmjc
        &         wshtotsum, sr_ugb, sla_calc, nb_ani, grazed_frac, &
        &         import_yield, t2m_14, litter_not_avail, nb_grazingdays, &
        &         after_snow, after_wet, wet1day, wet2day)
    !end gmjc

    IF (ANY(ok_LAIdev)) THEN
!  writerestart for crop variables, xuhui
       CALL sticslai_io_writestart( kjpindex, f_crop_recycle, in_cycle, f_sen_lai, st2m_max_daily, &
        &          wut_cm_daily, wus_cm_daily, evapot_daily, pdbiomass, pdmasec,  &
        &          masecveg, masec, dltams, gdh_daily, phoi, onarretesomcourdrp,  &
        &          nsendltams, nsendltai, nsenpfeuilverte, nsendurvie, nsenndurvie, densiteequiv, &
        &          nplt, tursla, ssla, pfeuilverte, bsenlai, &
        &          zrac, nrec, nlan, tcult, udevair, udevcult, ndrp, rfvi, nlev, nger, etatvernal, &
        &          caljvc, rfpi, upvt, utp, somcour, somcourdrp, somcourutp, tdevelop, somtemp, &
        &          somcourfauche, stpltger, R_stamflax, R_stlaxsen, R_stsenlan, stlevflo, nflo, &
        &          R_stlevdrp, R_stflodrp, R_stdrpmat, nmat, nlax, nrecbutoir, group, ndebdes, R_stdrpdes, densite, &
        &          densitelev, coeflev, densiteger, somelong, somger, humectation, nbjhumec, &
        &          somtemphumec, stpltlev, namf, stmatrec, tustress, slai, somfeuille, pdlai, &
        &          nbfeuille, reajust, ulai, pdulai, efdensite, tempeff, nstopfeuille, deltai, svmax, nsen, &
        &          laisen, pdlaisen, dltaisenat, nsencour, dltamsen, dltaisen, fgellev, &
        &          gelee, fstressgel, R_stlevamf, dernier_n, durvieI, durvie, ndebsen, somsenreste, &
        &          shumrel, swfac, turfac, senfac, mafeuiljaune, msneojaune, &
        &          v_dltams, fgelflo, pdircarb, ircarb, nbgrains, pgrain, vitmoy, nbgraingel, pgraingel, &
        &          dltags, ftempremp, magrain, pdmagrain, nbj0remp, pdsfruittot, repracmax, repracmin, &
        &          kreprac, somtemprac, urac, reprac,  nstoprac, c_reserve, c_leafb, gslen, drylen, &
        &          nboxmax, box_ndays, box_lai, box_lairem, box_tdev, box_biom, box_biomrem, box_durage, box_somsenbase, &
        &          cyc_num, cyc_num_tot, rot_cmd_store, plantdate, plantdate_now )
! end crop variables, xuhui

    ENDIF

    !! 2.2 Write file with variables that force general processes in stomate
    IF (ok_co2 .AND. allow_forcing_write ) THEN
      IF ( TRIM(stomate_forcing_name) /= 'NONE' ) THEN  
         CALL forcing_write(forcing_id,1,iisf)
         ! Close forcing file
         IF (is_root_prc) ier = NF90_CLOSE (forcing_id)
         forcing_id=-1
      END IF
    END IF

    !! 2.3 Collect variables that force the soil processes in stomate
    IF (TRIM(stomate_Cforcing_name) /= 'NONE' ) THEN 
      
      !! 2.3.1 Collet variables 
      WRITE(numout,*) &
           &      'stomate: writing the forcing file for carbon spinup'
      DO iatt = 1, nparan*nbyear
         IF ( nforce(iatt) > 0 ) THEN
            soilcarbon_input(:,:,:,iatt) = &
                 & soilcarbon_input(:,:,:,iatt)/REAL(nforce(iatt),r_std)
            control_moist(:,:,iatt) = &
                 & control_moist(:,:,iatt)/REAL(nforce(iatt),r_std)
            control_temp(:,:,iatt) = &
                 & control_temp(:,:,iatt)/REAL(nforce(iatt),r_std)
            npp_equil(:,iatt) = &
                 & npp_equil(:,iatt)/REAL(nforce(iatt),r_std)
         ELSE
            WRITE(numout,*) &
                 &         'We have no soil carbon forcing data for this time step:', &
                 &         iatt
            WRITE(numout,*) ' -> we set them to zero'
            soilcarbon_input(:,:,:,iatt) = zero
            control_moist(:,:,iatt) = zero
            control_temp(:,:,iatt) = zero
            npp_equil(:,iatt) = zero
         ENDIF
      ENDDO

      ! Allocate memory for parallel computing
      IF (is_root_prc) THEN
         ALLOCATE(soilcarbon_input_g(nbp_glo,ncarb,nvm,nparan*nbyear))
         ALLOCATE(control_moist_g(nbp_glo,nlevs,nparan*nbyear))
         ALLOCATE(control_temp_g(nbp_glo,nlevs,nparan*nbyear))
         ALLOCATE(npp_equil_g(nbp_glo,nparan*nbyear))
      ENDIF
      
      ! Gather distributed variables
      CALL gather(clay,clay_g)
      CALL gather(soilcarbon_input,soilcarbon_input_g)
      CALL gather(control_moist,control_moist_g)
      CALL gather(control_temp,control_temp_g)
      CALL gather(npp_equil,npp_equil_g)
      
      !! 2.3.2 Create netcdf
      ! Create, define and populate a netcdf file containing the forcing data.
      ! For the root processor only (parallel computing). NF90_ are functions
      ! from and external library.  
      IF (is_root_prc) THEN
         WRITE (numout,*) 'Create Cforcing file : ',TRIM(stomate_Cforcing_name)
         ! Create new netCDF dataset
         ier = NF90_CREATE (TRIM(stomate_Cforcing_name),NF90_64BIT_OFFSET ,Cforcing_id)
         IF (ier /= NF90_NOERR) THEN
            WRITE (numout,*) 'Error in creating Cforcing file : ',TRIM(stomate_Cforcing_name)
            CALL ipslerr_p (3,'stomate_main', &
                 &        'PROBLEM creating Cforcing file', &
                 &        NF90_STRERROR(ier),'')
         END IF

         ! Add variable attribute
         ! Note ::nbp_glo is the number of global continental points
         ier = NF90_PUT_ATT (Cforcing_id,NF90_GLOBAL, &
              &                        'kjpindex',REAL(nbp_glo,r_std))
         ier = NF90_PUT_ATT (Cforcing_id,NF90_GLOBAL, &
              &                        'nparan',REAL(nparan,r_std))
         ier = NF90_PUT_ATT (Cforcing_id,NF90_GLOBAL, &
              &                        'nbyear',REAL(nbyear,r_std))
         
         ! Add new dimension
         ier = NF90_DEF_DIM (Cforcing_id,'points',nbp_glo,d_id(1))
         ier = NF90_DEF_DIM (Cforcing_id,'carbtype',ncarb,d_id(2))
         ier = NF90_DEF_DIM (Cforcing_id,'vegtype',nvm,d_id(3))
         ier = NF90_DEF_DIM (Cforcing_id,'level',nlevs,d_id(4))
         ier = NF90_DEF_DIM (Cforcing_id,'time_step',NF90_UNLIMITED,d_id(5))
         
         ! Add new variable
         ier = NF90_DEF_VAR (Cforcing_id,'points',    r_typ,d_id(1),vid)
         ier = NF90_DEF_VAR (Cforcing_id,'carbtype',  r_typ,d_id(2),vid)
         ier = NF90_DEF_VAR (Cforcing_id,'vegtype',   r_typ,d_id(3),vid)
         ier = NF90_DEF_VAR (Cforcing_id,'level',     r_typ,d_id(4),vid)
         ier = NF90_DEF_VAR (Cforcing_id,'time_step', r_typ,d_id(5),vid)
         ier = NF90_DEF_VAR (Cforcing_id,'index',     r_typ,d_id(1),vid)
         ier = NF90_DEF_VAR (Cforcing_id,'clay',      r_typ,d_id(1),vid)
         ier = NF90_DEF_VAR (Cforcing_id,'soilcarbon_input',r_typ, &
              &                        (/ d_id(1),d_id(2),d_id(3),d_id(5) /),vid)
         ier = NF90_DEF_VAR (Cforcing_id,'control_moist',r_typ, &
              &                        (/ d_id(1),d_id(4),d_id(5) /),vid)
         ier = NF90_DEF_VAR (Cforcing_id,'control_temp',r_typ, &
              &                        (/ d_id(1),d_id(4),d_id(5) /),vid)
         ier = NF90_DEF_VAR (Cforcing_id,'npp_equil',r_typ, &
              &                        (/ d_id(1),d_id(5) /),vid)
         ier = NF90_ENDDEF (Cforcing_id)
         
         ! Given the name of a varaible, nf90_inq_varid finds the variable 
         ! ID (::vid). Put data value(s) into variable ::vid 
         ier = NF90_INQ_VARID (Cforcing_id,'points',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, &
              &                          (/(REAL(i,r_std),i=1,nbp_glo)/))
         ier = NF90_INQ_VARID (Cforcing_id,'carbtype',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, &
              &                        (/(REAL(i,r_std),i=1,ncarb)/))
         ier = NF90_INQ_VARID (Cforcing_id,'vegtype',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, &
              &                            (/(REAL(i,r_std),i=1,nvm)/))
         ier = NF90_INQ_VARID (Cforcing_id,'level',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, &
              &                          (/(REAL(i,r_std),i=1,nlevs)/))
         ier = NF90_INQ_VARID (Cforcing_id,'time_step',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, &
              &                          (/(REAL(i,r_std),i=1,nparan*nbyear)/))
         ier = NF90_INQ_VARID (Cforcing_id,'index',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, REAL(index_g,r_std) )
         ier = NF90_INQ_VARID (Cforcing_id,'clay',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, clay_g )
         ier = NF90_INQ_VARID (Cforcing_id,'soilcarbon_input',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, soilcarbon_input_g )
         ier = NF90_INQ_VARID (Cforcing_id,'control_moist',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, control_moist_g )
         ier = NF90_INQ_VARID (Cforcing_id,'control_temp',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, control_temp_g )
         ier = NF90_INQ_VARID (Cforcing_id,'npp_equil',vid)
         ier = NF90_PUT_VAR (Cforcing_id,vid, npp_equil_g )
         
         ! Close netCDF
         ier = NF90_CLOSE (Cforcing_id)
         IF (ier /= NF90_NOERR) THEN
            CALL ipslerr_p (3,'stomate_main', &
                 &        'PROBLEM in closing Cforcing file', &
                 &        NF90_STRERROR(ier),'')
         END IF

         Cforcing_id = -1
      ENDIF

      ! Clear memory
      IF (is_root_prc) THEN
         DEALLOCATE(soilcarbon_input_g)
         DEALLOCATE(control_moist_g)
         DEALLOCATE(control_temp_g)
         DEALLOCATE(npp_equil_g)
      ENDIF

    ENDIF ! TRIM(stomate_Cforcing_name) /= 'NONE'
    !! write the permafrost carbon forcing file
    IF (TRIM(Cforcing_permafrost_name) /= 'NONE' ) THEN
      WRITE(numout,*) &
           'stomate: writing the forcing file for permafrost carbon spinup'
      !
      DO iatt = 1, nparan*nbyear
         IF ( nforce(iatt) > 0 ) THEN
            soilcarbon_input_2pfcforcing(:,:,:,iatt) = &
                 soilcarbon_input_2pfcforcing(:,:,:,iatt)/REAL(nforce(iatt),r_std)
            pb_2pfcforcing(:,iatt) = &
                 pb_2pfcforcing(:,iatt)/REAL(nforce(iatt),r_std)
            snow_2pfcforcing(:,iatt) = &
                 snow_2pfcforcing(:,iatt)/REAL(nforce(iatt),r_std)
            tprof_2pfcforcing(:,:,:,iatt) = &
                 tprof_2pfcforcing(:,:,:,iatt)/REAL(nforce(iatt),r_std)
            fbact_2pfcforcing(:,:,:,iatt) = &
                 1./(fbact_2pfcforcing(:,:,:,iatt)/REAL(nforce(iatt),r_std))
    !!!cdk invert this so we take the mean decomposition rate rather than the mean
    !residence time
            hslong_2pfcforcing(:,:,:,iatt) = &
                 hslong_2pfcforcing(:,:,:,iatt)/REAL(nforce(iatt),r_std)
            veget_max_2pfcforcing(:,:,iatt) = &
                 veget_max_2pfcforcing(:,:,iatt)/REAL(nforce(iatt),r_std)
            rprof_2pfcforcing(:,:,iatt) = &
                 rprof_2pfcforcing(:,:,iatt)/REAL(nforce(iatt),r_std)
            tsurf_2pfcforcing(:,iatt) = &
                 tsurf_2pfcforcing(:,iatt)/REAL(nforce(iatt),r_std)
            ! Adding another two snow forcing
            snowdz_2pfcforcing(:,:,iatt) = &
                 snowdz_2pfcforcing(:,:,iatt)/REAL(nforce(iatt),r_std)
            snowrho_2pfcforcing(:,:,iatt) = &
                 snowrho_2pfcforcing(:,:,iatt)/REAL(nforce(iatt),r_std)
         ELSE
            WRITE(numout,*) &
                 &         'We have no soil carbon forcing data for this time step:', &
                 &         iatt
            WRITE(numout,*) ' -> we set them to zero'
            !soilcarbon_input(:,:,:,iatt) = zero
            !control_moist(:,:,iatt) = zero
            !control_temp(:,:,iatt) = zero
            !npp_equil(:,iatt) = zero
            soilcarbon_input_2pfcforcing(:,:,:,iatt) = zero
            pb_2pfcforcing(:,iatt) = zero
            snow_2pfcforcing(:,iatt) = zero
            tprof_2pfcforcing(:,:,:,iatt) = zero
            fbact_2pfcforcing(:,:,:,iatt) = zero
            hslong_2pfcforcing(:,:,:,iatt) = zero
            veget_max_2pfcforcing(:,:,iatt) = zero
            rprof_2pfcforcing(:,:,iatt) = zero
            tsurf_2pfcforcing(:,iatt) = zero
            snowdz_2pfcforcing(:,:,iatt) = zero
            snowrho_2pfcforcing(:,:,iatt) = zero
         ENDIF
      ENDDO
      !-
      WRITE (numout,*) 'Create Cforcing file : ',TRIM(Cforcing_permafrost_name)
      CALL stomate_io_carbon_permafrost_write( Cforcing_permafrost_name,                 &
                nbp_glo,            nbp_mpi_para_begin(mpi_rank),   nbp_mpi_para(mpi_rank),     nparan,         &
                nbyear,             index_g,                        zz_deep,                    zz_coef_deep,   &
                clay,               depth_organic_soil,             lalo,                                       &
                snowdz_2pfcforcing, snowrho_2pfcforcing,            soilcarbon_input_2pfcforcing,               &
                tsurf_2pfcforcing,  pb_2pfcforcing,                 snow_2pfcforcing,                           &
                tprof_2pfcforcing,  fbact_2pfcforcing,              veget_max_2pfcforcing,                      &
                rprof_2pfcforcing,  hslong_2pfcforcing )
    
    ENDIF
  
  END SUBROUTINE stomate_finalize

!! ================================================================================================================================
!! SUBROUTINE 	: stomate_init
!!
!>\BRIEF        The routine is called only at the first simulation. At that 
!! time settings and flags are read and checked for internal consistency and 
!! memory is allocated for the variables in stomate.
!!
!! DESCRIPTION  : The routine reads the 
!! following flags from the run definition file:
!! -ipd (index of grid point for online diagnostics)\n
!! -ok_herbivores (flag to activate herbivores)\n
!! -treat_expansion (flag to activate PFT expansion across a pixel\n
!! -harvest_agri (flag to harvest aboveground biomass from agricultural PFTs)\n
!! \n
!! Check for inconsistent setting between the following flags:
!! -ok_stomate\n
!! -ok_dgvm\n
!! -ok_co2\n
!! \n
!! Memory is allocated for all the variables of stomate and new indexing tables 
!! are build. New indexing tables are needed because a single pixel can conatin 
!! several PFTs. The new indexing tables have separate indices for the different 
!! PFTs. Similar index tables are build for land use cover change.\n
!! \n
!! Several global variables and land cover change variables are initialized to 
!! zero.\n
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): Strictly speaking the subroutine has no output 
!! variables. However, the routine allocates memory and builds new indexing 
!! variables for later use.\n 
!!
!! REFERENCE(S)	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE stomate_init &
       &  (kjpij, kjpindex, index, lalo, &
       &   rest_id_stom, hist_id_stom, hist_id_stom_IPCC)

  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    !pss:+, nb of vertical layers for CH4 diffussion
    INTEGER(i_std),PARAMETER                         :: n = 371
    !pss:-

    INTEGER(i_std),INTENT(in)                    :: kjpij             !! Total size of the un-compressed grid, including 
                                                                      !! oceans (unitless) 
    INTEGER(i_std),INTENT(in)                    :: kjpindex          !! Domain size - number of terrestrial pixels 
                                                                      !! (unitless) 
    INTEGER(i_std),INTENT(in)                    :: rest_id_stom      !! STOMATE's _Restart_ file identifier
    INTEGER(i_std),INTENT(in)                    :: hist_id_stom      !! STOMATE's _history_ file identifier
    INTEGER(i_std),INTENT(in)                    :: hist_id_stom_IPCC !! STOMATE's IPCC _history_ file identifier 
    INTEGER(i_std),DIMENSION(kjpindex),INTENT(in):: index             !! Indices of the terrestrial pixels on the global 
                                                                      !! map 
    REAL(r_std),DIMENSION(kjpindex,2),INTENT(in) :: lalo              !! Geogr. coordinates (latitude,longitude) (degrees)
   
    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    LOGICAL                                      :: l_error           !! Check errors in netcdf call
    INTEGER(i_std)                               :: ier               !! Check errors in netcdf call
    INTEGER(i_std)                               :: ji,j,ipd,l,iv,ip,kc     !! Indices

    ! CROP spec
    REAL(r_std), ALLOCATABLE, DIMENSION (:,:,:)           :: manage !!temporary management data
    REAL(r_std), ALLOCATABLE, DIMENSION (:,:,:)           :: Nfert_dat !!temporary nitrogen fertilization data
    INTEGER(i_std),DIMENSION(rot_cmd_max)                 :: temp_cmd  !! temporary command variable for rot_1d only
    CHARACTER(LEN=30)                                     :: temp_varname !! temporary variable string for rot_1d only
    INTEGER(i_std)                                        :: yrlen
    CHARACTER(LEN=30)                                     :: strManage
    CHARACTER(LEN=30)                                     :: strVar
    CHARACTER(LEN=30)                                     :: FileN_Nfert  !! file name of nitrogen fertilization
    CHARACTER(LEN=30)                                     :: Var_Nfert !! variable name of fertilization
    ! END CROP spec, xuhui
!_ ================================================================================================================================
    
  !! 1. Online diagnostics

    IF ( kjpindex > 0 ) THEN
       !Config  Key  = STOMATE_DIAGPT
       !Config  Desc = Index of grid point for online diagnostics
       !Config If    = OK_STOMATE
       !Config  Def  = 1
       !Config  Help = This is the index of the grid point which
       !               will be used for online diagnostics.
       !Config Units = [-]
       ! By default ::ipd is set to 1
       ipd = 1
       ! Get ::ipd from run definition file
       CALL getin_p('STOMATE_DIAGPT',ipd)
       ipd = MIN( ipd, kjpindex )
       WRITE(numout,*) 'Stomate: '
       WRITE(numout,*) '  Index of grid point for online diagnostics: ',ipd
       WRITE(numout,*) '  Lon, lat:',lalo(ipd,2),lalo(ipd,1)
       WRITE(numout,*) '  Index of this point on GCM grid: ',index(ipd)
       !
    ENDIF
    
  !! 2. Check consistency of flags

    IF ( ( .NOT. ok_stomate ) .AND. ok_dgvm ) THEN
       WRITE(numout,*) 'Cannot do dynamical vegetation without STOMATE.'
       WRITE(numout,*) 'Inconsistency between ::ok_stomate and ::ok_dgvm'
       WRITE(numout,*) 'Stop: fatal error'
       STOP
    ENDIF

    IF ((.NOT.ok_co2).AND.ok_stomate) THEN
       WRITE(numout,*) 'Cannot call STOMATE without GPP.'
       WRITE(numout,*) 'Inconsistency between ::ok_stomate and ::ok_co2'
       WRITE(numout,*) 'Stop: fatal error'
       STOP
    ENDIF

  !! 3. Communicate settings
    
    WRITE(numout,*) 'stomate first call - overview of the activated flags:'
    WRITE(numout,*) '  Photosynthesis: ', ok_co2
    WRITE(numout,*) '  STOMATE: ', ok_stomate
    WRITE(numout,*) '  LPJ: ', ok_dgvm
    
  !! 4. Allocate memory for STOMATE's variables

    l_error = .FALSE.


    ALLOCATE(veget_cov_max(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for veget_cov_max. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(ind(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for ind. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(adapted(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for adapted. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(regenerate(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for regenerate. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(humrel_daily(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for humrel_daily. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(litterhum_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for litterhum_daily. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(t2m_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for t2m_daily. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(t2m_min_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for t2m_min_daily. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    !spitfire
    ALLOCATE(t2m_max_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for t2m_max_daily. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(wspeed_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for wspeed_daily. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF
    !endspit

    ALLOCATE(tsurf_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for tsurf_daily. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(tsoil_daily(kjpindex,nbdl),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for tsoil_daily. We stop. We need kjpindex*nbdl words',kjpindex,nbdl
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(soilhum_daily(kjpindex,nbdl),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for soilhum_daily. We stop. We need kjpindex*nbdl words',kjpindex,nbdl
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(precip_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for precip_daily. We stop. We need kjpindex words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(gpp_daily(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for gpp_daily. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(npp_daily(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for npp_daily. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(turnover_daily(kjpindex,nvm,nparts,nelements),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for turnover_daily. We stop. We need kjpindex*nvm*nparts*nelements words', &
       &   kjpindex,nvm,nparts,nelements
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(turnover_littercalc(kjpindex,nvm,nparts,nelements),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for turnover_littercalc. We stop. We need kjpindex*nvm*nparts*nelements words', & 
        &  kjpindex,nvm,nparts,nelements
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(humrel_month(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for humrel_month. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(humrel_week(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for humrel_week. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(t2m_longterm(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for t2m_longterm. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(t2m_month(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for t2m_month. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(Tseason(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for Tseason. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(Tseason_length(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for Tseason_length. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(Tseason_tmp(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for Tseason_tmp. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(Tmin_spring_time(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for Tmin_spring_time. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(onset_date(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for onset_date. We stop. We need kjpindex*nvm*nparts words',kjpindex,nvm,2
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(t2m_week(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for t2m_week. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

!pss:+    
    ALLOCATE(tsurf_year(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for turf_year. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF
!pss:-

    ALLOCATE(tsoil_month(kjpindex,nbdl),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for tsoil_month. We stop. We need kjpindex*nbdl words',kjpindex,nbdl
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(soilhum_month(kjpindex,nbdl),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for soilhum_month. We stop. We need kjpindex*nbdl words',kjpindex,nbdl
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(fireindex(kjpindex,nvm),stat=ier) 
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for fireindex. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(firelitter(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for firelitter. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(maxhumrel_lastyear(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for maxhumrel_lastyear. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(maxhumrel_thisyear(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for maxhumrel_thisyear. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(minhumrel_lastyear(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for minhumrel_lastyear. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(minhumrel_thisyear(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for minhumrel_thisyear. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(maxgppweek_lastyear(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for maxgppweek_lastyear. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(maxgppweek_thisyear(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for maxgppweek_thisyear. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(gdd0_lastyear(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for gdd0_lastyear. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(gdd0_thisyear(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for gdd0_thisyear. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(gdd_init_date(kjpindex,2),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for gdd_init_date. We stop. We need kjpindex*2 words',kjpindex,2
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(gdd_from_growthinit(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for gdd_from_growthinit. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(precip_lastyear(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for precip_lastyear. We stop. We need kjpindex*nvm words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(precip_thisyear(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for precip_thisyear. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(gdd_m5_dormance(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for gdd_m5_dormance. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(gdd_midwinter(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for gdd_midwinter. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(ncd_dormance(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for ncd_dormance. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(ngd_minus5(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for ngd_minus5. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(PFTpresent(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for PFTpresent. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(npp_longterm(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for npp_longterm. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(lm_lastyearmax(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for lm_lastyearmax. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(lm_thisyearmax(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for lm_thisyearmax. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(maxfpc_lastyear(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for maxfpc_lastyear. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(maxfpc_thisyear(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for maxfpc_thisyear. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(turnover_longterm(kjpindex,nvm,nparts,nelements),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for turnover_longterm. We stop. We need kjpindex*nvm*nparts*nelements words', & 
       &    kjpindex,nvm,nparts,nelements
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(gpp_week(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for gpp_week. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(biomass(kjpindex,nvm,nparts,nelements),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for biomass. We stop. We need kjpindex*nvm*nparts*nelements words', &
       &    kjpindex,nvm,nparts,nelements
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(senescence(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for senescence. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(begin_leaves(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for begin_leaves. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(when_growthinit(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for when_growthinit. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(age(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for age. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(resp_hetero_d(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for resp_hetero_d. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(resp_hetero_radia(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for resp_hetero_radia. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(resp_maint_d(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for resp_maint_d. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(resp_growth_d(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for resp_growth_d. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

!!!! variables for wetland methane emissions
    !pss:+
    ALLOCATE(ch4_flux_density_tot_0(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)

    ALLOCATE(ch4_flux_density_dif_0(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_bub_0(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_pla_0(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    
    ALLOCATE(ch4_flux_density_tot_wet1(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_dif_wet1(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_bub_wet1(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_pla_wet1(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)

    ALLOCATE(ch4_flux_density_tot_wet2(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_dif_wet2(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_bub_wet2(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_pla_wet2(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
   
    ALLOCATE(ch4_flux_density_tot_wet3(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_dif_wet3(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_bub_wet3(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_pla_wet3(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    
    ALLOCATE(ch4_flux_density_tot_wet4(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_dif_wet4(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_bub_wet4(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ch4_flux_density_pla_wet4(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)

    ALLOCATE(uo_0(kjpindex,n),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(uold2_0(kjpindex,n),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(uo_wet1(kjpindex,n),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(uold2_wet1(kjpindex,n),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(uo_wet2(kjpindex,n),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(uold2_wet2(kjpindex,n),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(uo_wet3(kjpindex,n),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(uold2_wet3(kjpindex,n),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(uo_wet4(kjpindex,n),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(uold2_wet4(kjpindex,n),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    !pss:-


    ALLOCATE(co2_fire(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for co2_fire. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(co2_to_bm_dgvm(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for co2_to_bm_dgvm. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(veget_lastlight(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for veget_lastlight. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(everywhere(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for everywhere. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(need_adjacent(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for need_adjacent. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(leaf_age(kjpindex,nvm,nleafages),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for leaf_age. We stop. We need kjpindex*nvm*nleafages words', & 
       &      kjpindex,nvm,nleafages
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(leaf_frac(kjpindex,nvm,nleafages),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for leaf_frac. We stop. We need kjpindex*nvm*nleafages words', & 
       &      kjpindex,nvm,nleafages
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(RIP_time(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for RIP_time. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(time_hum_min(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for time_hum_min. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(hum_min_dormance(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for hum_min_dormance. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(litterpart(kjpindex,nvm,nlitt),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for litterpart. We stop. We need kjpindex*nvm*nlitt words',  &
       &  kjpindex,nvm,nlitt
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(litter(kjpindex,nlitt,nvm,nlevs,nelements),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for litter. We stop. We need kjpindex*nlitt*nvm*nlevs*nelements words', & 
       &    kjpindex,nlitt,nvm,nlevs,nelements
       STOP 'stomate_init'
    ENDIF

    !glcc
    ALLOCATE (glcc_pft(kjpindex,nvm), stat=ier) ; l_error = l_error .OR. (ier.NE.0)

    !spitfire
    ALLOCATE(ni_acc(kjpindex),stat=ier) ; l_error = l_error .OR. (ier /= 0)
    ALLOCATE(fire_numday(kjpindex),stat=ier) ; l_error = l_error .OR. (ier /= 0)
    ALLOCATE (fuel_1hr(kjpindex,nvm,nlitt,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (fuel_10hr(kjpindex,nvm,nlitt,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (fuel_100hr(kjpindex,nvm,nlitt,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (fuel_1000hr(kjpindex,nvm,nlitt,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (def_fuel_1hr_remain(kjpindex,nvm,nlitt,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (def_fuel_10hr_remain(kjpindex,nvm,nlitt,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (def_fuel_100hr_remain(kjpindex,nvm,nlitt,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (def_fuel_1000hr_remain(kjpindex,nvm,nlitt,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (lcc(kjpindex,nvm), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    lcc(:,:)=zero
    ALLOCATE (bafrac_deforest_accu(kjpindex,nvm), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (emideforest_litter_accu(kjpindex,nvm,nlitt,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (emideforest_biomass_accu(kjpindex,nvm,nparts,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (deforest_litter_remain(kjpindex,nlitt,nvm,nlevs,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    ALLOCATE (deforest_biomass_remain(kjpindex,nvm,nparts,nelements), stat=ier) ; l_error = l_error .OR. (ier.NE.0)
    !endspit

    ALLOCATE(dead_leaves(kjpindex,nvm,nlitt),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for dead_leaves. We stop. We need kjpindex*nvm*nlitt words', & 
       &   kjpindex,nvm,nlitt
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(carbon(kjpindex,ncarb,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for carbon. We stop. We need kjpindex*ncarb*nvm words',kjpindex,ncarb,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(carbon_surf(kjpindex,ncarb,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for carbon_surf. We stop. We need kjpindex*ncarb*nvm words',kjpindex,ncarb,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(lignin_struc(kjpindex,nvm,nlevs),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for lignin_struc. We stop. We need kjpindex*nvm*nlevs words',kjpindex,nvm,nlevs
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(turnover_time(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for turnover_time. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(co2_flux_daily(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for co2_flux_daily. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(co2_flux_monthly(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for co2_flux_monthly. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (cflux_prod_monthly(kjpindex,nwp), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for cflux_prod_monthly. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF
 
    ALLOCATE (harvest_above_monthly(kjpindex), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for harvest_above_monthly. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(bm_to_litter(kjpindex,nvm,nparts,nelements),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for bm_to_litter. We stop. We need kjpindex*nvm*nparts*nelements words', & 
       &    kjpindex,nvm,nparts,nelements
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(bm_to_littercalc(kjpindex,nvm,nparts,nelements),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for bm_to_littercalc. We stop. We need kjpindex*nvm*nparts*nelements words', &
       &   kjpindex,nvm,nparts,nelements
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(herbivores(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for herbivores. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(resp_maint_part_radia(kjpindex,nvm,nparts),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for resp_maint_part_radia. We stop. We need kjpindex*nvm*nparts words', &
       &  kjpindex,nvm,nparts
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(resp_maint_radia(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for resp_maint_radia. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(resp_maint_part(kjpindex,nvm,nparts),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for resp_maint_part. We stop. We need kjpindex*nvm*nparts words', &
       &    kjpindex,nvm,nparts
       STOP 'stomate_init'
    ENDIF
    resp_maint_part(:,:,:) = zero

    ALLOCATE (prod10(kjpindex,0:10,nwp), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for prod10. We stop. We need kjpindex*11 words',kjpindex,11
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (prod100(kjpindex,0:100,nwp), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for prod100. We stop. We need kjpindex*101 words',kjpindex,101
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (flux10(kjpindex,10,nwp), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for flux10. We stop. We need kjpindex*10 words',kjpindex,10
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (flux100(kjpindex,100,nwp), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for flux100. We stop. We need kjpindex*100 words',kjpindex,100
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (convflux(kjpindex,nwp), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for convflux. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (cflux_prod10(kjpindex,nwp), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for cflux_prod10. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (cflux_prod100(kjpindex,nwp), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for cflux_prod100. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (harvest_above(kjpindex), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for harvest_above. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (carb_mass_total(kjpindex), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for carb_mass_total. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (soilcarbon_input_daily(kjpindex,ncarb,nvm), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for soilcarbon_input_daily. We stop. We need kjpindex*ncarb*nvm words', & 
       &    kjpindex,ncarb,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (control_temp_daily(kjpindex,nlevs), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for control_temp_daily. We stop. We need kjpindex*nlevs words',kjpindex,nlevs
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (control_moist_daily(kjpindex,nlevs), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for control_moist_daily. We stop. We need kjpindex*nlevs words',kjpindex,nlevs
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (fpc_max(kjpindex,nvm), stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for fpc_max. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(ok_equilibrium(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0) 
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for ok_equilibrium. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(carbon_eq(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for carbon_eq. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(nbp_accu(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for nbp_accu. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(nbp_flux(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for nbp_flux. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(matrixA(kjpindex,nvm,nbpools,nbpools),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for matrixA. We stop. We need kjpindex*nvm*nbpools*nbpools words',  & 
       &     kjpindex, nvm, nbpools, nbpools
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(vectorB(kjpindex,nvm,nbpools),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for vectorB. We stop. We need kjpindex*nvm*nbpools words',  & 
       &     kjpindex, nvm, nbpools
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(VectorU(kjpindex,nvm,nbpools),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for VectorU. We stop. We need kjpindex*nvm*nbpools words',  & 
       &     kjpindex, nvm, nbpools
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(MatrixV(kjpindex,nvm,nbpools,nbpools),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for MatrixV. We stop. We need kjpindex*nvm*nbpools*nbpools words',  & 
       &     kjpindex, nvm, nbpools, nbpools
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(MatrixW(kjpindex,nvm,nbpools,nbpools),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for MatrixW. We stop. We need kjpindex*nvm*nbpools*nbpools words',  & 
       &     kjpindex, nvm, nbpools, nbpools
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(previous_stock(kjpindex,nvm,nbpools),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for previous_stock. We stop. We need kjpindex*nvm*nbpools words',  & 
       &     kjpindex, nvm, nbpools
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(current_stock(kjpindex,nvm,nbpools),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for current_stock. We stop. We need kjpindex*nvm*nbpools words',  & 
       &     kjpindex, nvm, nbpools
       STOP 'stomate_init'
    ENDIF
!!!!! crops

    IF (ANY(ok_LAIdev)) THEN
      ALLOCATE(cyc_num_tot(kjpindex),stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'stomate_initialize', 'There is an allocation variable error: ', 'cyc_num_tot', '')
      ALLOCATE(cyc_num(kjpindex,nvm),stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'stomate_initialize', 'There is an allocation variable error: ', 'cyc_num', '')
      ALLOCATE(rot_cmd_store(kjpindex,rot_cmd_max,cyc_rot_max),stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'stomate_initialize', 'There is an allocation variable error: ', 'rot_cmd_store', '')
      ALLOCATE(plantdate(kjpindex,nvm,cyc_rot_max),stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'stomate_initialize', 'There is an allocation variable error: ', 'plantdate', '')
      ALLOCATE(plantdate_now(kjpindex,nvm),stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'stomate_initialize', 'There is an allocation variable error: ', 'plantdate_now', '')
      ALLOCATE(f_rot_stom(kjpindex, nvm), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'stomate_initialize', 'There is an allocation variable error: ', 'f_rot_stom', '')

      ALLOCATE(N_limfert(kjpindex,nvm),stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'stomate_initialize', 'There is an allocation variable error: ', 'N_limfert', '')

      ALLOCATE(N_add(kjpindex,nvm),stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'stomate_init', 'Memory allocation error', 'N_add', '')
      N_add = zero
    ELSE
      ALLOCATE(N_limfert(1,1),stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'stomate_initialize', 'There is an allocation variable error: ', 'N_limfert', '')
    ENDIF
    

    ! allocation of STICS variables

    ALLOCATE(f_crop_recycle(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(in_cycle(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(f_sen_lai(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)

    ALLOCATE(st2m_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(st2m_min_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(st2m_max_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)



!    ALLOCATE(t2m_max_daily(kjpindex),stat=ier)  !already defined previously
!    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(wut_cm_daily(kjpindex, nvm, 3),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(wus_cm_daily(kjpindex, nvm, 3),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(evapot_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(pdbiomass(kjpindex, nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(pdmasec(kjpindex, nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(masecveg(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(masec(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(dltams(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(gdh_daily(kjpindex,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    ALLOCATE(phoi(kjpindex),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(onarretesomcourdrp(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    !ALLOCATE(codeulaivernal(nvm),stat=ier)   ! sensitive or not to vernalbility is only related to PFTS, how to initialize it
    !    l_error = l_error .OR. (ier /= 0)


!    ALLOCATE(nlevobs(kjpindex,nvm),stat=ier)
!        l_error = l_error .OR. (ier /= 0)
!    ALLOCATE(namfobs(kjpindex,nvm),stat=ier)
!        l_error = l_error .OR. (ier /= 0)
!    ALLOCATE(nfloobs(kjpindex,nvm),stat=ier)
!        l_error = l_error .OR. (ier /= 0)
!    ALLOCATE(nlanobs(kjpindex,nvm),stat=ier)
!        l_error = l_error .OR. (ier /= 0)
!    ALLOCATE(nlaxobs(kjpindex,nvm),stat=ier)
!        l_error = l_error .OR. (ier /= 0)
!    ALLOCATE(nmatobs(kjpindex,nvm),stat=ier)
!        l_error = l_error .OR. (ier /= 0)
!    ALLOCATE(nrecobs(kjpindex,nvm),stat=ier)
!        l_error = l_error .OR. (ier /= 0)
!    ALLOCATE(nsenobs(kjpindex,nvm),stat=ier)
!        l_error = l_error .OR. (ier /= 0)
!    ALLOCATE(ndrpobs(kjpindex,nvm),stat=ier)
!        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nsendltams(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nsendltai(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nsenpfeuilverte(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nsendurvie(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nsenndurvie(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(densiteequiv(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nplt(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(tursla(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ssla(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(pfeuilverte(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(bsenlai(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
     
    ! Development of STICS
    ALLOCATE(zrac(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nrec(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nlan(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(tcult(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(udevair(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(udevcult(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ndrp(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(rfvi(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nlev(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nger(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(etatvernal(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(caljvc(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(rfpi(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(upvt(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(utp(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somcour(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somcourdrp(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somcourutp(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(tdevelop(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somtemp(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somcourfauche(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(stpltger(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(R_stamflax(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(R_stlaxsen(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(R_stsenlan(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(stlevflo(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nflo(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(R_stlevdrp(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(R_stflodrp(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(R_stdrpmat(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nmat(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nlax(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nrecbutoir(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(group(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ndebdes(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(R_stdrpdes(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(densite(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(densitelev(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(coeflev(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(densiteger(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somelong(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somger(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(humectation(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nbjhumec(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somtemphumec(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(stpltlev(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(namf(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(stmatrec(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    
    
    ! LAI calculation
    ALLOCATE(tustress(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(slai(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somfeuille(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(pdlai(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nbfeuille(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(reajust(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ulai(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(pdulai(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)



    ALLOCATE(efdensite(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(tempeff(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nstopfeuille(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(deltai(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(svmax(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)

    ALLOCATE(nsen(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)

    ALLOCATE(pdlaisen(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(dltaisenat(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    

    ! LAIsenescence
    ALLOCATE(nsencour(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(dltamsen(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
 
    ALLOCATE(dltaisen(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(fgellev(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(gelee(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(fstressgel(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
   
    ALLOCATE(laisen(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(R_stlevamf(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(dernier_n(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(durvieI(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(durvie(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ndebsen(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somsenreste(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    WRITE(numout,*) 'stomate_init: before box ulai'
    
!    IF ( any(ok_LAIdev(:)) .AND. any(SP_codlainet(:) .eq. 2) ) THEN
        ALLOCATE(histgrowthset(kjpindex,nvm,300,5),stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(hist_sencourset(kjpindex,nvm),stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(hist_latestset(kjpindex,nvm),stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(doyhiststset(kjpindex,nvm),stat=ier)
            l_error = l_error .OR. (ier /= 0)
!    ELSE IF ( ANY(ok_LAIdev(:)) .AND. ANY(SP_codlainet(:) .eq. 3) ) THEN
        nboxmax = maxval(SP_nbox(:))
        write(*,*) 'xuhui: nobxmax: ',nboxmax
        ALLOCATE(box_ulai(nvm,nboxmax), stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(box_ndays(kjpindex, nvm, nboxmax), stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(box_lai(kjpindex, nvm, nboxmax), stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(box_lairem(kjpindex, nvm, nboxmax), stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(box_tdev(kjpindex, nvm, nboxmax), stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(box_biom(kjpindex, nvm, nboxmax), stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(box_biomrem(kjpindex, nvm, nboxmax), stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(box_durage(kjpindex, nvm, nboxmax), stat=ier)
            l_error = l_error .OR. (ier /= 0)
        ALLOCATE(box_somsenbase(kjpindex, nvm, nboxmax), stat=ier)
            l_error = l_error .OR. (ier /= 0)
        write(*,*) 'xuhui: box module allocated'                
!    ENDIF
    
    IF (l_error) THEN
       STOP 'stomate_init: error in memory allocation for box lai'
    ENDIF 

    ! STRESS STICS
    ALLOCATE(shumrel(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(swfac(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(turfac(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(senfac(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
 

    ALLOCATE(mafeuiljaune(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(msneojaune(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)



 
    ! STICS: CARBON POOLS

    ALLOCATE(v_dltams(kjpindex,nvm, vlength),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(fgelflo(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(pdircarb(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ircarb(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nbgrains(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(pgrain(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(vitmoy(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nbgraingel(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(pgraingel(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(dltags(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(ftempremp(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(magrain(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(pdmagrain(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nbj0remp(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(pdsfruittot(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(repracmax(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(repracmin(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(kreprac(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(somtemprac(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(urac(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(reprac(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(nstoprac(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)

    ALLOCATE(c_reserve(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    ALLOCATE(c_leafb(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)

    ALLOCATE(deltgrain(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    
    ALLOCATE(gslen(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)
    
    ALLOCATE(drylen(kjpindex,nvm),stat=ier)
        l_error = l_error .OR. (ier /= 0)

    IF (l_error) THEN
       STOP 'stomate_init: error in memory allocation for crops'
    ENDIF 



!!!!! end crops, xuhui
    
   !allocate stomate permafrost variables
   ALLOCATE (deepC_a(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (deepC_s(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (deepC_p(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (O2_soil(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (CH4_soil(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (O2_snow(kjpindex, nsnow,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (CH4_snow(kjpindex, nsnow,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (tdeep_daily(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (fbact(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (fbact_daily(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (prmfrst_soilc_tempctrl(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (prmfrst_soilc_tempctrl_daily(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (hsdeep_daily(kjpindex, ndeep,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
!   ALLOCATE (soilcarbon_input_daily(kjpindex,ncarb,nvm), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (temp_sol_daily(kjpindex), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (snow_daily(kjpindex), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (pb_pa_daily(kjpindex), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE(fixed_cryoturbation_depth(kjpindex,nvm),stat=ier )
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (snowdz_daily(kjpindex,nsnow), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
   ALLOCATE (snowrho_daily(kjpindex,nsnow), stat=ier)
   l_error = l_error .OR. (ier.NE.0)
    IF (l_error) THEN
       STOP 'stomate_init: error in memory allocation'
    ENDIF 

!gmjc
!    ALLOCATE(resp_hetero_litter_d(kjpindex,nvm),stat=ier)
!    l_error = l_error .OR. (ier /= 0)
!    IF (l_error) THEN
!       WRITE(numout,*) 'Memory allocation error for resp_hetero_litter_d. We stop. We need kjpindex*nvm words',kjpindex,nvm
!       STOP 'stomate_init'
!    ENDIF
!
!    ALLOCATE(resp_hetero_soil_d(kjpindex,ncarb,nvm),stat=ier)
!    l_error = l_error .OR. (ier /= 0)
!    IF (l_error) THEN
!       WRITE(numout,*) 'Memory allocation error for resp_hetero_soil_d. We stop. We need kjpindex*ncarb*nvm words',kjpindex,ncarb,nvm
!       STOP 'stomate_init'
!    ENDIF
!
!    ALLOCATE(resp_hetero_soil_part(kjpindex,ncarb,nvm),stat=ier)
!    l_error = l_error .OR. (ier /= 0)
!    IF (l_error) THEN
!       WRITE(numout,*) 'Memory allocation error for resp_hetero_soil_part. We stop. We need kjpindex*ncarb*nvm words',kjpindex,ncarb,nvm
!       STOP 'stomate_init'
!    ENDIF
    ALLOCATE(litter_avail(kjpindex,nlitt,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for litter_avail. We stop. We need kjpindex*nlitt*nvm words',  &
       &  kjpindex,nlitt,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(litter_not_avail(kjpindex,nlitt,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for litter_not_avail. We stop. We need kjpindex*nlitt*nvm words',  &
       &  kjpindex,nlitt,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(litter_avail_frac(kjpindex,nlitt,nvm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for litter_avail_frac. We stop. We need kjpindex*nlitt*nvm words',  &
       &  kjpindex,nlitt,nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(sla_calc(kjpindex,nvm),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for sla_calc. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

   ALLOCATE(wshtotsum(kjpindex,nvm),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for wshtotsum. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

   ALLOCATE(sr_ugb(kjpindex,nvm),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for sr_ugb. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

   ALLOCATE(compt_ugb(kjpindex,nvm),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for compt_ugb. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

   ALLOCATE(nb_ani(kjpindex,nvm),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for nb_ani. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

   ALLOCATE(grazed_frac(kjpindex,nvm),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for grazed_frac. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

   ALLOCATE(import_yield(kjpindex,nvm),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for import_yield. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

   ALLOCATE(t2m_14(kjpindex),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for t2m_14. We stop. We need kjpindex*nvm words',kjpindex
       STOP 'stomate_init'
    ENDIF

   ALLOCATE (sla_age1(kjpindex,nvm), stat=ier)
    l_error = l_error .OR. (ier.NE.0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for sla_age1. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

   ALLOCATE (when_growthinit_cut(kjpindex,nvm), stat=ier)
    l_error = l_error .OR. (ier.NE.0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for when_growthinit_cut. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF
   ALLOCATE(nb_grazingdays(kjpindex,nvm),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for nb_grazingdays. We stop. We need kjpindex*nvm words',kjpindex,nvm
       STOP 'stomate_init'
    ENDIF

   ALLOCATE(snowfall_daily(kjpindex),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for snowfall_daily. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

   ALLOCATE(snowmass_daily(kjpindex),stat=ier)
   l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for snowmass_daily. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF
! top 5 layer grassland soil moisture for grazing
    ALLOCATE(tmc_topgrass_daily(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for tmc_topgrass_daily. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF
    ALLOCATE(after_snow(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for after_snow. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF
    ALLOCATE(after_wet(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for after_wet. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF
    ALLOCATE(wet1day(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for wet1day. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF
    ALLOCATE(wet2day(kjpindex),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Memory allocation error for wet2day. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF
!end gmjc
  !! 5. File definitions

    ! Store history and restart files in common variables
    hist_id_stomate = hist_id_stom
    hist_id_stomate_IPCC = hist_id_stom_IPCC
    rest_id_stomate = rest_id_stom

  !! 6. Initialization of global and land cover change variables. 

    ! All variables are cumulative variables. bm_to_litter is not and is therefore
    ! excluded
    !   bm_to_litter(:,:,:) = zero
    turnover_daily(:,:,:,:) = zero
    resp_hetero_d(:,:) = zero
    co2_flux_daily(:,:) = zero
    co2_flux_monthly(:,:) = zero
    cflux_prod_monthly(:,:) = zero
    harvest_above_monthly(:) = zero
    control_moist_daily(:,:) = zero
    control_temp_daily(:,:) = zero
    soilcarbon_input_daily(:,:,:) = zero
    ! Land cover change variables
    prod10(:,:,:)  = zero
    prod100(:,:,:) = zero
    flux10(:,:,:)  = zero
    flux100(:,:,:) = zero
    convflux(:,:)  = zero
    cflux_prod10(:,:) = zero
    cflux_prod100(:,:) = zero
    fpc_max(:,:)=zero
   !permafrost   
    tdeep_daily(:,:,:) = zero
    fbact(:,:,:) = zero
    fbact_daily(:,:,:) = zero
    prmfrst_soilc_tempctrl(:,:,:) = zero
    prmfrst_soilc_tempctrl_daily(:,:,:) = zero
    hsdeep_daily(:,:,:) = zero
    soilcarbon_input_daily(:,:,:) = zero
    temp_sol_daily(:) = zero
    snow_daily(:) = zero
    pb_pa_daily(:) = zero
    snowdz_daily(:,:) = zero
    snowrho_daily(:,:) = zero
!gmjc
   DO j = 1,nvm
     sla_calc(:,j) = sla(j)
     sla_age1(:,j) = sla_max(j)
   ENDDO
   wshtotsum(:,:)=zero
   sr_ugb(:,:)=zero
   compt_ugb(:,:)=zero
   nb_ani(:,:)=zero
   grazed_frac(:,:)=zero
   import_yield(:,:)=zero
   litter_not_avail = 0.0
   litter_avail = 0.0
   litter_avail_frac = 1.0
   when_growthinit_cut(:,:)=20.0
   nb_grazingdays(:,:) = zero
   snowfall_daily(:) = zero
   snowmass_daily(:) = zero
!top 5 layer grassland soil moisture for grazing
   tmc_topgrass_daily(:) = zero
   after_snow(:) = zero
   after_wet(:) = zero
   wet1day(:) = 6
   wet2day(:) = 6
!end gmjc 

   ! 6 Initialize index histwrite related variables
   CALL stomate_init_index(kjpij, kjpindex, index)

    ! 8 initialization of crop module
    ! 
    ! initialization of the flag for each PFT and each pixel

    WRITE(numout,*) 'start STICS init'
        
    IF (ANY(ok_LAIdev)) THEN
       IF (f_crop_init) THEN
          f_crop_recycle(:, :) = .TRUE.
          CALL Stics_init(&
               kjpindex                ,&   
               !nvm                     ,&   
               f_crop_init             ,&   
               f_crop_recycle          ,&   
               in_cycle          ,&   
               f_sen_lai         ,&   
               onarretesomcourdrp      ,&   
!               nlevobs                 ,&
!               namfobs                 ,&
!               nfloobs                 ,&
!               nlanobs                 ,&
!               nlaxobs                 ,&
!               nmatobs                 ,&
!               nrecobs                 ,&
!               nsenobs                 ,&
!               ndrpobs                 ,&
               nsendltams              ,&
               nsendltai               ,&
               nsenpfeuilverte         ,&
               nsendurvie              ,&
               nsenndurvie             ,&
               densiteequiv            ,&
               nplt                    ,&
               tursla                  ,&
               ssla                     ,&
               pfeuilverte             ,&
               bsenlai                 ,&
               zrac                    ,&
               nrec                    ,& 
               nlan                    ,&
               tcult                   ,&
               udevair                 ,&
               udevcult                ,&
               ndrp                    ,&
               rfvi                    ,&
               nlev                    ,&
               nger                    ,&
               etatvernal              ,&
               caljvc                  ,&
               rfpi                    ,&
               upvt                    ,&
               utp                     ,&
               somcour                 ,&
               somcourdrp              ,&
               somcourutp              ,&
               tdevelop                ,&
               somtemp                 ,&
               somcourfauche           ,&
               stpltger                ,&
               R_stamflax              ,&
               R_stlaxsen              ,&
               R_stsenlan              ,&
               stlevflo                ,&
               nflo                    ,&
               R_stlevdrp              ,&
               R_stflodrp              ,&
               R_stdrpmat              ,&
               nmat                    ,&
               nlax                    ,&
               nrecbutoir              ,&
               group                   ,&
               ndebdes                 ,&
               R_stdrpdes              ,&
               densite                 ,&
               densitelev              ,&
               coeflev                 ,&
               densiteger              ,&
               somelong                 ,&
               somger                  ,&
               humectation             ,&
               nbjhumec                ,&
               somtemphumec            ,&
               stpltlev                ,&
               namf                    ,&
               stmatrec                ,&
               tustress                ,&
               slai                     ,&
               somfeuille              ,&
               pdlai                   ,&
               nbfeuille               ,&
               reajust                 ,&
               ulai                    ,&
               pdulai                  ,&
               efdensite               ,&
               tempeff                 ,&
               nstopfeuille            ,&
               deltai                  ,&
               svmax                    ,&
               nsen                    ,&
               laisen                  ,&
               pdlaisen                ,&
               dltaisenat              ,&
               nsencour                ,&
               dltamsen                ,&
               dltaisen                ,&
               fgellev                 ,&
               gelee                   ,&
               fstressgel              ,&
               R_stlevamf              ,&
               dernier_n               ,&
               durvieI                 ,&
               durvie                  ,&
               ndebsen                 ,&
               somsenreste             ,&
               shumrel                  ,&
               swfac                   ,&
               turfac                  ,&
               senfac                  ,&               
               mafeuiljaune            ,&
               msneojaune              ,&
               v_dltams                ,&
               fgelflo                 ,&
               pdircarb                ,&
               ircarb                  ,&
               nbgrains                ,&
               pgrain                  ,&
               vitmoy                  ,&
               nbgraingel              ,&
               pgraingel               ,&
               dltags                  ,&
               ftempremp               ,&
               magrain                 ,&
               pdmagrain               ,&
               nbj0remp                ,&
               pdsfruittot             ,&
               repracmax               ,&
               repracmin               ,&
               kreprac                 ,&
               somtemprac              ,&
               urac                    ,&
               reprac                  ,&
               nstoprac                ,&
               c_reserve               ,&
               c_leafb                 ,&
               !biomass                 ,&
               deltgrain               ,&
               gslen                   ,&
               drylen, &
               histgrowthset, &
               hist_sencourset, &
               hist_latestset, &
               doyhiststset, &
               nboxmax, box_ulai, box_ndays, box_lai, box_lairem, box_tdev, box_biom, box_biomrem, box_durage, box_somsenbase)                

          f_crop_recycle(:, :) = .FALSE.
          f_crop_init = .FALSE.
       ENDIF
    ENDIF
    WRITE(numout,*) 'end stomate_init'
  END SUBROUTINE stomate_init


!! ================================================================================================================================
!! SUBROUTINE 	: stomate_init_index
!!
!>\BRIEF         Initialize all indices related variables for histwrite_p
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCES	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  SUBROUTINE stomate_init_index(kjpij, kjpindex, index_g)
    INTEGER(i_std), INTENT(in)                      :: kjpij          !! Total size of the un-compressed grid, including 
                                                                      !! oceans (unitless) 
    INTEGER(i_std), INTENT(in)                      :: kjpindex       !! Domain size - number of terrestrial pixels 
                                                                      !! (unitless) 
    INTEGER(i_std), DIMENSION(kjpindex), INTENT(in) :: index_g        !! Indices of the terrestrial pixels on the global 
                                                                      !! map 

    INTEGER(i_std) :: ier, i, j, ji


    ALLOCATE(hori_index(kjpindex),stat=ier)
    IF (ier /=0) THEN
       WRITE(numout,*) 'Memory allocation error for hori_index. We stop. We need kjpindex words',kjpindex
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(horipft_index(kjpindex*nvm),stat=ier)
    IF (ier /= 0) THEN
       WRITE(numout,*) 'Memory allocation error for horipft_index. We stop. We need kjpindex*nvm words',kjpindex*nvm
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(horideep_index(kjpindex*ndeep),stat=ier)
    IF (ier /= 0) THEN
       WRITE(numout,*) 'Memory allocation error for horideep_index. We stop. We need kjpindex*nvm words',kjpindex*ndeep
       STOP 'stomate_init'
    ENDIF

    ALLOCATE(horisnow_index(kjpindex*nsnow),stat=ier)
    IF (ier /= 0) THEN
       WRITE(numout,*) 'Memory allocation error for horipft_index. We stop. We need kjpindex*nsnow words',kjpindex*nsnow
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (horip10_index(kjpindex*10), stat=ier)
    IF (ier /= 0) THEN
       WRITE(numout,*) 'Memory allocation error for horip10_index. We stop. We need kjpindex*10 words',kjpindex,10
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (horip100_index(kjpindex*100), stat=ier)
    IF (ier /= 0) THEN
       WRITE(numout,*) 'Memory allocation error for horip100_index. We stop. We need kjpindex*100 words',kjpindex,100
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (horip11_index(kjpindex*11), stat=ier)
    IF (ier /= 0) THEN
       WRITE(numout,*) 'Memory allocation error for horip11_index. We stop. We need kjpindex*11 words',kjpindex,11
       STOP 'stomate_init'
    ENDIF

    ALLOCATE (horip101_index(kjpindex*101), stat=ier)
    IF (ier /= 0) THEN
       WRITE(numout,*) 'Memory allocation error for horip101_index. We stop. We need kjpindex*101 words',kjpindex,101
       STOP 'stomate_init'
    ENDIF

    ! In STOMATE reduced grids are used containing only terrestrial pixels.
    ! Build a new indexing table for the vegetation fields separating 
    ! between the different PFTs. Note that ::index has dimension (kjpindex) 
    ! wheras ::indexpft has dimension (kjpindex*nvm). 

    DO j = 1, ndeep
       DO ji = 1, kjpindex
          horideep_index((j-1)*kjpindex+ji) = index_g(ji)+(j-1)*kjpij + offset_omp - offset_mpi
       ENDDO
    ENDDO

    DO j = 1, nsnow
       DO ji = 1, kjpindex
          horisnow_index((j-1)*kjpindex+ji) = index_g(ji)+(j-1)*kjpij  + offset_omp - offset_mpi
       ENDDO
    ENDDO

    hori_index(:) = index_g(:)

    DO j = 1, nvm
       DO ji = 1, kjpindex
          horipft_index((j-1)*kjpindex+ji) = index_g(ji)+(j-1)*kjpij + offset_omp - offset_mpi
       ENDDO
    ENDDO

    ! Similar index tables are build for the land cover change variables
    DO j = 1, 10
       DO ji = 1, kjpindex
          horip10_index((j-1)*kjpindex+ji) = index_g(ji)+(j-1)*kjpij + offset_omp - offset_mpi
       ENDDO
    ENDDO

    DO j = 1, 100
       DO ji = 1, kjpindex
          horip100_index((j-1)*kjpindex+ji) = index_g(ji)+(j-1)*kjpij + offset_omp - offset_mpi
       ENDDO
    ENDDO

    DO j = 1, 11
       DO ji = 1, kjpindex
          horip11_index((j-1)*kjpindex+ji) = index_g(ji)+(j-1)*kjpij + offset_omp - offset_mpi
       ENDDO
    ENDDO

    DO j = 1, 101
       DO ji = 1, kjpindex
          horip101_index((j-1)*kjpindex+ji) = index_g(ji)+(j-1)*kjpij + offset_omp - offset_mpi
       ENDDO
    ENDDO
  END SUBROUTINE stomate_init_index

!! ================================================================================================================================
!! SUBROUTINE 	: stomate_clear
!!
!>\BRIEF        Deallocate memory of the stomate variables.
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCES	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  
  SUBROUTINE stomate_clear

  !! 1. Deallocate all dynamics variables

    IF (ALLOCATED(veget_cov_max)) DEALLOCATE(veget_cov_max)
    IF (ALLOCATED(ind)) DEALLOCATE(ind)
    IF (ALLOCATED(adapted)) DEALLOCATE(adapted)
    IF (ALLOCATED(regenerate)) DEALLOCATE(regenerate)
    IF (ALLOCATED(humrel_daily)) DEALLOCATE(humrel_daily)
    IF (ALLOCATED(gdd_init_date)) DEALLOCATE(gdd_init_date)
    IF (ALLOCATED(litterhum_daily)) DEALLOCATE(litterhum_daily)
    IF (ALLOCATED(t2m_daily))  DEALLOCATE(t2m_daily)
    IF (ALLOCATED(t2m_min_daily))  DEALLOCATE(t2m_min_daily)
    !spitfire
    IF (ALLOCATED(t2m_max_daily))  DEALLOCATE(t2m_max_daily)
    IF (ALLOCATED(wspeed_daily))  DEALLOCATE(wspeed_daily)
    !endspit
    IF (ALLOCATED(tsurf_daily))  DEALLOCATE(tsurf_daily)
    IF (ALLOCATED(tsoil_daily)) DEALLOCATE(tsoil_daily)
    IF (ALLOCATED(soilhum_daily)) DEALLOCATE(soilhum_daily)
    IF (ALLOCATED(precip_daily)) DEALLOCATE(precip_daily)
    IF (ALLOCATED(gpp_daily)) DEALLOCATE(gpp_daily)
    IF (ALLOCATED(npp_daily)) DEALLOCATE(npp_daily)
    IF (ALLOCATED(turnover_daily)) DEALLOCATE(turnover_daily)
    IF (ALLOCATED(turnover_littercalc)) DEALLOCATE(turnover_littercalc)
    IF (ALLOCATED(humrel_month)) DEALLOCATE(humrel_month)
    IF (ALLOCATED(humrel_week)) DEALLOCATE(humrel_week)
    IF (ALLOCATED(t2m_longterm)) DEALLOCATE(t2m_longterm)
    IF (ALLOCATED(t2m_month)) DEALLOCATE(t2m_month)
    IF (ALLOCATED(Tseason)) DEALLOCATE(Tseason)
    IF (ALLOCATED(Tseason_length)) DEALLOCATE(Tseason_length)
    IF (ALLOCATED(Tseason_tmp)) DEALLOCATE(Tseason_tmp)
    IF (ALLOCATED(Tmin_spring_time)) DEALLOCATE(Tmin_spring_time)
    IF (ALLOCATED(onset_date)) DEALLOCATE(onset_date)
    IF (ALLOCATED(t2m_week)) DEALLOCATE(t2m_week)
    IF (ALLOCATED(tsoil_month)) DEALLOCATE(tsoil_month)
    IF (ALLOCATED(soilhum_month)) DEALLOCATE(soilhum_month)
    IF (ALLOCATED(fireindex)) DEALLOCATE(fireindex)
    IF (ALLOCATED(firelitter)) DEALLOCATE(firelitter)
    IF (ALLOCATED(maxhumrel_lastyear)) DEALLOCATE(maxhumrel_lastyear)
    IF (ALLOCATED(maxhumrel_thisyear)) DEALLOCATE(maxhumrel_thisyear)
    IF (ALLOCATED(minhumrel_lastyear)) DEALLOCATE(minhumrel_lastyear)
    IF (ALLOCATED(minhumrel_thisyear)) DEALLOCATE(minhumrel_thisyear)
    IF (ALLOCATED(maxgppweek_lastyear)) DEALLOCATE(maxgppweek_lastyear)
    IF (ALLOCATED(maxgppweek_thisyear)) DEALLOCATE(maxgppweek_thisyear)
    IF (ALLOCATED(gdd0_lastyear)) DEALLOCATE(gdd0_lastyear)
    IF (ALLOCATED(gdd0_thisyear)) DEALLOCATE(gdd0_thisyear)
    IF (ALLOCATED(precip_lastyear)) DEALLOCATE(precip_lastyear)
    IF (ALLOCATED(precip_thisyear)) DEALLOCATE(precip_thisyear)
    IF (ALLOCATED(gdd_m5_dormance)) DEALLOCATE(gdd_m5_dormance)
    IF (ALLOCATED(gdd_from_growthinit)) DEALLOCATE(gdd_from_growthinit)
    IF (ALLOCATED(gdd_midwinter)) DEALLOCATE(gdd_midwinter)
    IF (ALLOCATED(ncd_dormance)) DEALLOCATE(ncd_dormance)
    IF (ALLOCATED(ngd_minus5))  DEALLOCATE(ngd_minus5)
    IF (ALLOCATED(PFTpresent)) DEALLOCATE(PFTpresent)
    IF (ALLOCATED(npp_longterm)) DEALLOCATE(npp_longterm)
    IF (ALLOCATED(lm_lastyearmax)) DEALLOCATE(lm_lastyearmax)
    IF (ALLOCATED(lm_thisyearmax)) DEALLOCATE(lm_thisyearmax)
    IF (ALLOCATED(maxfpc_lastyear)) DEALLOCATE(maxfpc_lastyear)
    IF (ALLOCATED(maxfpc_thisyear)) DEALLOCATE(maxfpc_thisyear)
    IF (ALLOCATED(turnover_longterm)) DEALLOCATE(turnover_longterm)
    IF (ALLOCATED(gpp_week)) DEALLOCATE(gpp_week)
    IF (ALLOCATED(biomass)) DEALLOCATE(biomass)
    IF (ALLOCATED(senescence)) DEALLOCATE(senescence)
    IF (ALLOCATED(begin_leaves)) DEALLOCATE(begin_leaves)
    IF (ALLOCATED(when_growthinit)) DEALLOCATE(when_growthinit)
    IF (ALLOCATED(age))  DEALLOCATE(age)
    IF (ALLOCATED(resp_hetero_d)) DEALLOCATE(resp_hetero_d)
    IF (ALLOCATED(resp_hetero_radia)) DEALLOCATE(resp_hetero_radia)
    IF (ALLOCATED(resp_maint_d)) DEALLOCATE(resp_maint_d)
    IF (ALLOCATED(resp_growth_d)) DEALLOCATE(resp_growth_d)
    IF (ALLOCATED(co2_fire)) DEALLOCATE(co2_fire)
    IF (ALLOCATED(co2_to_bm_dgvm)) DEALLOCATE(co2_to_bm_dgvm)
    IF (ALLOCATED(veget_lastlight)) DEALLOCATE(veget_lastlight)
    IF (ALLOCATED(everywhere)) DEALLOCATE(everywhere)
    IF (ALLOCATED(need_adjacent)) DEALLOCATE(need_adjacent)
    IF (ALLOCATED(leaf_age)) DEALLOCATE(leaf_age)
    IF (ALLOCATED(leaf_frac)) DEALLOCATE(leaf_frac)
    IF (ALLOCATED(RIP_time)) DEALLOCATE(RIP_time)
    IF (ALLOCATED(time_hum_min)) DEALLOCATE(time_hum_min)
    IF (ALLOCATED(hum_min_dormance)) DEALLOCATE(hum_min_dormance)
    IF (ALLOCATED(litterpart)) DEALLOCATE(litterpart)
    IF (ALLOCATED(litter)) DEALLOCATE(litter)

    !glcc
    IF ( ALLOCATED (glcc_pft)) DEALLOCATE (glcc_pft)

    !spitfire
    IF ( ALLOCATED (ni_acc)) DEALLOCATE (ni_acc)
    IF ( ALLOCATED (fire_numday)) DEALLOCATE (fire_numday)
    IF ( ALLOCATED (fuel_1hr)) DEALLOCATE (fuel_1hr)
    IF ( ALLOCATED (fuel_10hr)) DEALLOCATE (fuel_10hr)
    IF ( ALLOCATED (fuel_100hr)) DEALLOCATE (fuel_100hr)
    IF ( ALLOCATED (fuel_1000hr)) DEALLOCATE (fuel_1000hr)
    IF ( ALLOCATED (def_fuel_1hr_remain)) DEALLOCATE (def_fuel_1hr_remain)
    IF ( ALLOCATED (def_fuel_10hr_remain)) DEALLOCATE (def_fuel_10hr_remain)
    IF ( ALLOCATED (def_fuel_100hr_remain)) DEALLOCATE (def_fuel_100hr_remain)
    IF ( ALLOCATED (def_fuel_1000hr_remain)) DEALLOCATE (def_fuel_1000hr_remain)
    IF ( ALLOCATED (lcc)) DEALLOCATE (lcc)
    IF ( ALLOCATED (bafrac_deforest_accu)) DEALLOCATE (bafrac_deforest_accu)
    IF ( ALLOCATED (emideforest_litter_accu)) DEALLOCATE (emideforest_litter_accu)
    IF ( ALLOCATED (emideforest_biomass_accu)) DEALLOCATE (emideforest_biomass_accu)
    IF ( ALLOCATED (deforest_litter_remain)) DEALLOCATE (deforest_litter_remain)
    IF ( ALLOCATED (deforest_biomass_remain)) DEALLOCATE (deforest_biomass_remain)
    !endspit
    IF (ALLOCATED(dead_leaves)) DEALLOCATE(dead_leaves)
    IF (ALLOCATED(carbon)) DEALLOCATE(carbon)
    IF (ALLOCATED(carbon_surf)) DEALLOCATE(carbon_surf)
    IF (ALLOCATED(lignin_struc)) DEALLOCATE(lignin_struc)
    IF (ALLOCATED(turnover_time)) DEALLOCATE(turnover_time)
    IF (ALLOCATED(co2_flux_daily)) DEALLOCATE(co2_flux_daily)
    IF (ALLOCATED(co2_flux_monthly)) DEALLOCATE(co2_flux_monthly)
    IF (ALLOCATED(harvest_above_monthly)) DEALLOCATE (harvest_above_monthly)
    IF (ALLOCATED(cflux_prod_monthly)) DEALLOCATE (cflux_prod_monthly)
    IF (ALLOCATED(bm_to_litter)) DEALLOCATE(bm_to_litter)
    IF (ALLOCATED(bm_to_littercalc)) DEALLOCATE(bm_to_littercalc)
    IF (ALLOCATED(herbivores)) DEALLOCATE(herbivores)
    IF (ALLOCATED(resp_maint_part_radia)) DEALLOCATE(resp_maint_part_radia)
    IF (ALLOCATED(resp_maint_radia)) DEALLOCATE(resp_maint_radia)
    IF (ALLOCATED(resp_maint_part)) DEALLOCATE(resp_maint_part)

    !pss:+
    IF (ALLOCATED(uo_0)) DEALLOCATE(uo_0)
    IF (ALLOCATED(uold2_0)) DEALLOCATE(uold2_0)
    IF (ALLOCATED(uo_wet1)) DEALLOCATE(uo_wet1)
    IF (ALLOCATED(uold2_wet1)) DEALLOCATE(uold2_wet1)
    IF (ALLOCATED(uo_wet2)) DEALLOCATE(uo_wet2)
    IF (ALLOCATED(uold2_wet2)) DEALLOCATE(uold2_wet2)
    IF (ALLOCATED(uo_wet3)) DEALLOCATE(uo_wet3)
    IF (ALLOCATED(uold2_wet3)) DEALLOCATE(uold2_wet3)
    IF (ALLOCATED(uo_wet4)) DEALLOCATE(uo_wet4)
    IF (ALLOCATED(uold2_wet4)) DEALLOCATE(uold2_wet4)

    IF (ALLOCATED(tsurf_year)) DEALLOCATE(tsurf_year)
    IF (ALLOCATED(ch4_flux_density_tot_0)) DEALLOCATE(ch4_flux_density_tot_0)
    IF (ALLOCATED(ch4_flux_density_dif_0)) DEALLOCATE(ch4_flux_density_dif_0)
    IF (ALLOCATED(ch4_flux_density_bub_0)) DEALLOCATE(ch4_flux_density_bub_0)
    IF (ALLOCATED(ch4_flux_density_pla_0)) DEALLOCATE(ch4_flux_density_pla_0)
    IF (ALLOCATED(ch4_flux_density_tot_wet1)) DEALLOCATE(ch4_flux_density_tot_wet1)
    IF (ALLOCATED(ch4_flux_density_dif_wet1)) DEALLOCATE(ch4_flux_density_dif_wet1)
    IF (ALLOCATED(ch4_flux_density_bub_wet1)) DEALLOCATE(ch4_flux_density_bub_wet1)
    IF (ALLOCATED(ch4_flux_density_pla_wet1)) DEALLOCATE(ch4_flux_density_pla_wet1)
    IF (ALLOCATED(ch4_flux_density_tot_wet2)) DEALLOCATE(ch4_flux_density_tot_wet2)
    IF (ALLOCATED(ch4_flux_density_dif_wet2)) DEALLOCATE(ch4_flux_density_dif_wet2)
    IF (ALLOCATED(ch4_flux_density_bub_wet2)) DEALLOCATE(ch4_flux_density_bub_wet2)
    IF (ALLOCATED(ch4_flux_density_pla_wet2)) DEALLOCATE(ch4_flux_density_pla_wet2)
    IF (ALLOCATED(ch4_flux_density_tot_wet3)) DEALLOCATE(ch4_flux_density_tot_wet3)
    IF (ALLOCATED(ch4_flux_density_dif_wet3)) DEALLOCATE(ch4_flux_density_dif_wet3)
    IF (ALLOCATED(ch4_flux_density_bub_wet3)) DEALLOCATE(ch4_flux_density_bub_wet3)
    IF (ALLOCATED(ch4_flux_density_pla_wet3)) DEALLOCATE(ch4_flux_density_pla_wet3)
    IF (ALLOCATED(ch4_flux_density_tot_wet4)) DEALLOCATE(ch4_flux_density_tot_wet4)
    IF (ALLOCATED(ch4_flux_density_dif_wet4)) DEALLOCATE(ch4_flux_density_dif_wet4)
    IF (ALLOCATED(ch4_flux_density_bub_wet4)) DEALLOCATE(ch4_flux_density_bub_wet4)
    IF (ALLOCATED(ch4_flux_density_pla_wet4)) DEALLOCATE(ch4_flux_density_pla_wet4)
    !pss:-

    IF (ALLOCATED(hori_index)) DEALLOCATE(hori_index)
    IF (ALLOCATED(horipft_index)) DEALLOCATE(horipft_index)
    IF (ALLOCATED(clay_fm)) DEALLOCATE(clay_fm)
    IF (ALLOCATED(humrel_daily_fm)) DEALLOCATE(humrel_daily_fm)
    IF (ALLOCATED(litterhum_daily_fm))  DEALLOCATE(litterhum_daily_fm)
    IF (ALLOCATED(t2m_daily_fm))  DEALLOCATE(t2m_daily_fm)
    IF (ALLOCATED(t2m_min_daily_fm))  DEALLOCATE(t2m_min_daily_fm)
    !spitfire
    IF (ALLOCATED(t2m_max_daily_fm))  DEALLOCATE (t2m_max_daily_fm)
    IF (ALLOCATED(wspeed_daily_fm))  DEALLOCATE (wspeed_daily_fm)
    !endspit
    IF (ALLOCATED(tsurf_daily_fm)) DEALLOCATE(tsurf_daily_fm)
    IF (ALLOCATED(tsoil_daily_fm)) DEALLOCATE(tsoil_daily_fm)
    IF (ALLOCATED(soilhum_daily_fm))  DEALLOCATE(soilhum_daily_fm)
    IF (ALLOCATED(precip_fm)) DEALLOCATE(precip_fm)
    IF (ALLOCATED(gpp_daily_fm))  DEALLOCATE(gpp_daily_fm)
    IF (ALLOCATED(veget_fm)) DEALLOCATE(veget_fm)
    IF (ALLOCATED(veget_max_fm)) DEALLOCATE(veget_max_fm)
    IF (ALLOCATED(lai_fm))  DEALLOCATE(lai_fm)
    IF (ALLOCATED(t2mdiag_fm))  DEALLOCATE(t2mdiag_fm)
    IF (ALLOCATED(swdown_fm))  DEALLOCATE(swdown_fm)
    IF (ALLOCATED(evapot_corr_fm))  DEALLOCATE(evapot_corr_fm)
    !
    IF (ALLOCATED(ok_equilibrium)) DEALLOCATE(ok_equilibrium)
    IF (ALLOCATED(carbon_eq)) DEALLOCATE(carbon_eq)
    IF (ALLOCATED(matrixA)) DEALLOCATE(matrixA)
    IF (ALLOCATED(vectorB)) DEALLOCATE(vectorB)
    IF (ALLOCATED(MatrixV)) DEALLOCATE(MatrixV)
    IF (ALLOCATED(VectorU)) DEALLOCATE(VectorU)
    IF (ALLOCATED(MatrixW)) DEALLOCATE(MatrixW)
    IF (ALLOCATED(previous_stock)) DEALLOCATE(previous_stock)
    IF (ALLOCATED(current_stock)) DEALLOCATE(current_stock) 
    IF (ALLOCATED(nbp_accu)) DEALLOCATE(nbp_accu)
    IF (ALLOCATED(nbp_flux)) DEALLOCATE(nbp_flux)

!!!!! crop variables

    IF (ALLOCATED(cyc_num_tot)) DEALLOCATE(cyc_num_tot)
    IF (ALLOCATED(cyc_num)) DEALLOCATE(cyc_num)
    IF (ALLOCATED(rot_cmd_store)) DEALLOCATE(rot_cmd_store)
    IF (ALLOCATED(plantdate)) DEALLOCATE(plantdate)
    IF (ALLOCATED(plantdate_now)) DEALLOCATE(plantdate_now)
    IF (ALLOCATED(f_rot_stom)) DEALLOCATE(f_rot_stom)

! Deallocate the STICS dynamic variables
    WRITE(numout,*) 'before clear stics variables'
    IF (ALLOCATED(in_cycle)) DEALLOCATE(in_cycle)
    IF (ALLOCATED(f_sen_lai)) DEALLOCATE(f_sen_lai)


    IF (ALLOCATED(st2m_daily)) DEALLOCATE(st2m_daily)
    IF (ALLOCATED(st2m_min_daily)) DEALLOCATE(st2m_min_daily)
    IF (ALLOCATED(st2m_max_daily)) DEALLOCATE(st2m_max_daily)

    IF (ALLOCATED(t2m_max_daily)) DEALLOCATE(t2m_max_daily)
    IF (ALLOCATED(wut_cm_daily)) DEALLOCATE(wut_cm_daily)
    IF (ALLOCATED(wus_cm_daily)) DEALLOCATE(wus_cm_daily)
    IF (ALLOCATED(evapot_daily))   DEALLOCATE(evapot_daily)
    IF (ALLOCATED(pdbiomass))      DEALLOCATE(pdbiomass)
    IF (ALLOCATED(pdmasec))      DEALLOCATE(pdmasec)
    IF (ALLOCATED(masecveg))       DEALLOCATE(masecveg)
    IF (ALLOCATED(masec))          DEALLOCATE(masec)
    IF (ALLOCATED(dltams))         DEALLOCATE(dltams)
    IF (ALLOCATED(gdh_daily)) DEALLOCATE(gdh_daily)
    IF (ALLOCATED(phoi))          DEALLOCATE(phoi)
    IF (ALLOCATED(onarretesomcourdrp))          DEALLOCATE(onarretesomcourdrp)
    !IF (ALLOCATED(codeulaivernal))          DEALLOCATE(codeulaivernal)
!    IF (ALLOCATED(nlevobs))          DEALLOCATE(nlevobs)
!    IF (ALLOCATED(namfobs))          DEALLOCATE(namfobs)
!    IF (ALLOCATED(nfloobs))          DEALLOCATE(nfloobs)
!    IF (ALLOCATED(nlanobs))          DEALLOCATE(nlanobs)
!    IF (ALLOCATED(nlaxobs))          DEALLOCATE(nlaxobs)
!    IF (ALLOCATED(nmatobs))          DEALLOCATE(nmatobs)
!    IF (ALLOCATED(nrecobs))          DEALLOCATE(nrecobs)
!    IF (ALLOCATED(nsenobs))          DEALLOCATE(nsenobs)
!    IF (ALLOCATED(ndrpobs))          DEALLOCATE(ndrpobs)
    IF (ALLOCATED(nsendltams))          DEALLOCATE(nsendltams)
    IF (ALLOCATED(nsendltai))          DEALLOCATE(nsendltai)
    IF (ALLOCATED(nsenpfeuilverte))          DEALLOCATE(nsenpfeuilverte)
    IF (ALLOCATED(nsendurvie))          DEALLOCATE(nsendurvie)
    IF (ALLOCATED(nsenndurvie))          DEALLOCATE(nsenndurvie)
    IF (ALLOCATED(densiteequiv))          DEALLOCATE(densiteequiv)
    IF (ALLOCATED(nplt))          DEALLOCATE(nplt)
    IF (ALLOCATED(tursla))          DEALLOCATE(tursla)
    IF (ALLOCATED(ssla))          DEALLOCATE(ssla)
    IF (ALLOCATED(pfeuilverte))          DEALLOCATE(pfeuilverte)
    IF (ALLOCATED(bsenlai))          DEALLOCATE(bsenlai)
    IF (ALLOCATED(zrac))          DEALLOCATE(zrac)
    IF (ALLOCATED(nrec))          DEALLOCATE(nrec)
    IF (ALLOCATED(nlan))          DEALLOCATE(nlan)
    IF (ALLOCATED(tcult))          DEALLOCATE(tcult)
    IF (ALLOCATED(udevair))          DEALLOCATE(udevair)
    IF (ALLOCATED(udevcult))          DEALLOCATE(udevcult)
    IF (ALLOCATED(ndrp))          DEALLOCATE(ndrp)
    IF (ALLOCATED(rfvi))          DEALLOCATE(rfvi)
    IF (ALLOCATED(nlev))          DEALLOCATE(nlev)
    IF (ALLOCATED(nger))          DEALLOCATE(nger)
    IF (ALLOCATED(etatvernal))          DEALLOCATE(etatvernal)
    IF (ALLOCATED(caljvc))          DEALLOCATE(caljvc)
    IF (ALLOCATED(rfpi))          DEALLOCATE(rfpi)
    IF (ALLOCATED(upvt))          DEALLOCATE(upvt)
    IF (ALLOCATED(utp))          DEALLOCATE(utp)
    IF (ALLOCATED(somcour))          DEALLOCATE(somcour)
    IF (ALLOCATED(somcourdrp))          DEALLOCATE(somcourdrp)
    IF (ALLOCATED(somcourutp))          DEALLOCATE(somcourutp)
    IF (ALLOCATED(tdevelop))          DEALLOCATE(tdevelop)
    IF (ALLOCATED(somtemp))          DEALLOCATE(somtemp)
    IF (ALLOCATED(somcourfauche))          DEALLOCATE(somcourfauche)
    IF (ALLOCATED(stpltger))          DEALLOCATE(stpltger)
    IF (ALLOCATED(R_stamflax))          DEALLOCATE(R_stamflax)
    IF (ALLOCATED(R_stlaxsen))          DEALLOCATE(R_stlaxsen)
    IF (ALLOCATED(R_stsenlan))          DEALLOCATE(R_stsenlan)
    IF (ALLOCATED(stlevflo))          DEALLOCATE(stlevflo)
    IF (ALLOCATED(nflo))          DEALLOCATE(nflo)
    IF (ALLOCATED(R_stlevdrp))          DEALLOCATE(R_stlevdrp)
    IF (ALLOCATED(R_stflodrp))          DEALLOCATE(R_stflodrp)
    IF (ALLOCATED(R_stdrpmat))          DEALLOCATE(R_stdrpmat)
    IF (ALLOCATED(nmat)) DEALLOCATE(nmat)
    IF (ALLOCATED(nlax)) DEALLOCATE(nlax)
    IF (ALLOCATED(nrecbutoir)) DEALLOCATE(nrecbutoir)
    IF (ALLOCATED(group)) DEALLOCATE(group)
    IF (ALLOCATED(ndebdes)) DEALLOCATE(ndebdes)
    IF (ALLOCATED(R_stdrpdes)) DEALLOCATE(R_stdrpdes)
    IF (ALLOCATED(densite)) DEALLOCATE(densite)
    IF (ALLOCATED(densitelev)) DEALLOCATE(densitelev)
    IF (ALLOCATED(coeflev)) DEALLOCATE(coeflev)
    IF (ALLOCATED(densiteger)) DEALLOCATE(densiteger)
    IF (ALLOCATED(somelong)) DEALLOCATE(somelong)
    IF (ALLOCATED(somger)) DEALLOCATE(somger)
    IF (ALLOCATED(humectation)) DEALLOCATE(humectation)
    IF (ALLOCATED(nbjhumec)) DEALLOCATE(nbjhumec)
    IF (ALLOCATED(somtemphumec)) DEALLOCATE(somtemphumec)
    IF (ALLOCATED(stpltlev)) DEALLOCATE(stpltlev)
    IF (ALLOCATED(namf)) DEALLOCATE(namf)
    IF (ALLOCATED(stmatrec)) DEALLOCATE(stmatrec)
    IF (ALLOCATED(tustress)) DEALLOCATE(tustress)
    IF (ALLOCATED(slai)) DEALLOCATE(slai)
    IF (ALLOCATED(somfeuille)) DEALLOCATE(somfeuille)
    IF (ALLOCATED(pdlai)) DEALLOCATE(pdlai)
    IF (ALLOCATED(nbfeuille)) DEALLOCATE(nbfeuille)
    IF (ALLOCATED(reajust)) DEALLOCATE(reajust)
    IF (ALLOCATED(ulai)) DEALLOCATE(ulai)
    IF (ALLOCATED(pdulai)) DEALLOCATE(pdulai)
    IF (ALLOCATED(efdensite)) DEALLOCATE(efdensite)
    IF (ALLOCATED(tempeff)) DEALLOCATE(tempeff)
    IF (ALLOCATED(nstopfeuille)) DEALLOCATE(nstopfeuille)
    IF (ALLOCATED(deltai)) DEALLOCATE(deltai)
    IF (ALLOCATED(svmax)) DEALLOCATE(svmax)
    IF (ALLOCATED(nsen)) DEALLOCATE(nsen)
    IF (ALLOCATED(laisen)) DEALLOCATE(laisen)
    IF (ALLOCATED(pdlaisen)) DEALLOCATE(pdlaisen)
    IF (ALLOCATED(dltaisenat)) DEALLOCATE(dltaisenat)
    IF (ALLOCATED(nsencour)) DEALLOCATE(nsencour)
    IF (ALLOCATED(dltamsen)) DEALLOCATE(dltamsen)
    IF (ALLOCATED(dltaisen)) DEALLOCATE(dltaisen)
    IF (ALLOCATED(fgellev)) DEALLOCATE(fgellev)
    IF (ALLOCATED(gelee)) DEALLOCATE(gelee)
    IF (ALLOCATED(fstressgel)) DEALLOCATE(fstressgel)
    IF (ALLOCATED(R_stlevamf)) DEALLOCATE(R_stlevamf)
    IF (ALLOCATED(dernier_n)) DEALLOCATE(dernier_n)
    IF (ALLOCATED(durvieI)) DEALLOCATE(durvieI)
    IF (ALLOCATED(durvie)) DEALLOCATE(durvie)
    IF (ALLOCATED(ndebsen)) DEALLOCATE(ndebsen)
    IF (ALLOCATED(somsenreste)) DEALLOCATE(somsenreste)
    IF (ALLOCATED(shumrel)) DEALLOCATE(shumrel)
    IF (ALLOCATED(swfac)) DEALLOCATE(swfac)
    IF (ALLOCATED(turfac)) DEALLOCATE(turfac)
    IF (ALLOCATED(senfac)) DEALLOCATE(senfac)
    
    IF (ALLOCATED(mafeuiljaune)) DEALLOCATE(mafeuiljaune)
    IF (ALLOCATED(msneojaune)) DEALLOCATE(msneojaune)
    WRITE(numout,*) 'before clear box lai variables'
    IF (ALLOCATED(histgrowthset)) DEALLOCATE(histgrowthset)
    IF (ALLOCATED(hist_sencourset)) DEALLOCATE(hist_sencourset)
    IF (ALLOCATED(hist_latestset)) DEALLOCATE(hist_latestset)
    IF (ALLOCATED(doyhiststset)) DEALLOCATE(doyhiststset)
    ! STICS:: CARBON ALLOCATION

    IF (ALLOCATED(v_dltams)) DEALLOCATE(v_dltams)
    IF (ALLOCATED(fgelflo)) DEALLOCATE(fgelflo)
    IF (ALLOCATED(pdircarb)) DEALLOCATE(pdircarb)
    IF (ALLOCATED(ircarb)) DEALLOCATE(ircarb)
    IF (ALLOCATED(nbgrains)) DEALLOCATE(nbgrains)
    IF (ALLOCATED(pgrain)) DEALLOCATE(pgrain)
    IF (ALLOCATED(vitmoy)) DEALLOCATE(vitmoy)
    IF (ALLOCATED(nbgraingel)) DEALLOCATE(nbgraingel)
    IF (ALLOCATED(pgraingel)) DEALLOCATE(pgraingel)
    IF (ALLOCATED(dltags)) DEALLOCATE(dltags)
    IF (ALLOCATED(ftempremp)) DEALLOCATE(ftempremp)
    IF (ALLOCATED(magrain)) DEALLOCATE(magrain)
    IF (ALLOCATED(pdmagrain)) DEALLOCATE(pdmagrain)
    IF (ALLOCATED(nbj0remp)) DEALLOCATE(nbj0remp)
    IF (ALLOCATED(pdsfruittot)) DEALLOCATE(pdsfruittot)
    IF (ALLOCATED(repracmax)) DEALLOCATE(repracmax)
    IF (ALLOCATED(repracmin)) DEALLOCATE(repracmin)
    IF (ALLOCATED(kreprac)) DEALLOCATE(kreprac)
    IF (ALLOCATED(somtemprac)) DEALLOCATE(somtemprac)
    IF (ALLOCATED(urac)) DEALLOCATE(urac)
    IF (ALLOCATED(reprac)) DEALLOCATE(reprac)
    IF (ALLOCATED(nstoprac)) DEALLOCATE(nstoprac)
    IF (ALLOCATED(c_reserve)) DEALLOCATE(c_reserve)
    IF (ALLOCATED(c_leafb)) DEALLOCATE(c_leafb)
    IF (ALLOCATED(deltgrain)) DEALLOCATE(deltgrain)
    IF (ALLOCATED(gslen)) DEALLOCATE(gslen)
    IF (ALLOCATED(drylen)) DEALLOCATE(drylen)

    IF (ALLOCATED(plantdate)) DEALLOCATE(plantdate)
    IF (ALLOCATED(N_add)) DEALLOCATE(N_add)

    IF (ALLOCATED(box_ndays)) DEALLOCATE(box_ndays)
    IF (ALLOCATED(box_lai)) DEALLOCATE(box_lai)
    IF (ALLOCATED(box_lairem)) DEALLOCATE(box_lairem)
    IF (ALLOCATED(box_tdev)) DEALLOCATE(box_tdev)
    IF (ALLOCATED(box_biom)) DEALLOCATE(box_biom)
    IF (ALLOCATED(box_biomrem)) DEALLOCATE(box_biomrem)
    IF (ALLOCATED(box_durage)) DEALLOCATE(box_durage)
    IF (ALLOCATED(box_somsenbase)) DEALLOCATE(box_somsenbase)

    IF (ALLOCATED(rot_cmd_store)) DEALLOCATE(rot_cmd_store)
    IF (ALLOCATED(cyc_num_tot)) DEALLOCATE(cyc_num_tot)
    IF (ALLOCATED(cyc_num)) DEALLOCATE(cyc_num)
!!!!! end crop variables, xuhui

    IF (is_root_prc) THEN
       IF (ALLOCATED(clay_fm_g)) DEALLOCATE(clay_fm_g)
       IF (ALLOCATED(humrel_daily_fm_g)) DEALLOCATE(humrel_daily_fm_g)
       IF (ALLOCATED(litterhum_daily_fm_g))  DEALLOCATE(litterhum_daily_fm_g)
       IF (ALLOCATED(t2m_daily_fm_g))  DEALLOCATE(t2m_daily_fm_g)
       IF (ALLOCATED(t2m_min_daily_fm_g))  DEALLOCATE(t2m_min_daily_fm_g)
       !spitfire
       IF (ALLOCATED(t2m_max_daily_fm_g))  DEALLOCATE (t2m_max_daily_fm_g)
       IF (ALLOCATED(wspeed_daily_fm_g))  DEALLOCATE (wspeed_daily_fm_g)
       !endspit
       IF (ALLOCATED(tsurf_daily_fm_g)) DEALLOCATE(tsurf_daily_fm_g)
       IF (ALLOCATED(tsoil_daily_fm_g)) DEALLOCATE(tsoil_daily_fm_g)
       IF (ALLOCATED(soilhum_daily_fm_g))  DEALLOCATE(soilhum_daily_fm_g)
       IF (ALLOCATED(precip_fm_g)) DEALLOCATE(precip_fm_g)
       IF (ALLOCATED(gpp_daily_fm_g))  DEALLOCATE(gpp_daily_fm_g)
       IF (ALLOCATED(veget_fm_g)) DEALLOCATE(veget_fm_g)
       IF (ALLOCATED(veget_max_fm_g)) DEALLOCATE(veget_max_fm_g)
       IF (ALLOCATED(lai_fm_g))  DEALLOCATE(lai_fm_g)
       IF (ALLOCATED(t2mdiag_fm_g))  DEALLOCATE(t2mdiag_fm_g)
       IF (ALLOCATED(swdown_fm_g))  DEALLOCATE(swdown_fm_g)
       IF (ALLOCATED(evapot_corr_fm_g))  DEALLOCATE(evapot_corr_fm_g)
    ENDIF

    IF (ALLOCATED(isf)) DEALLOCATE(isf)
    IF (ALLOCATED(nf_written)) DEALLOCATE(nf_written)
    IF (ALLOCATED(nf_cumul)) DEALLOCATE(nf_cumul)
    IF (ALLOCATED(nforce)) DEALLOCATE(nforce)
    IF (ALLOCATED(control_moist)) DEALLOCATE(control_moist)
    IF (ALLOCATED(control_temp)) DEALLOCATE(control_temp)
    IF (ALLOCATED(soilcarbon_input)) DEALLOCATE(soilcarbon_input)
    IF ( ALLOCATED (horip10_index)) DEALLOCATE (horip10_index)
    IF ( ALLOCATED (horip100_index)) DEALLOCATE (horip100_index)
    IF ( ALLOCATED (horip11_index)) DEALLOCATE (horip11_index)
    IF ( ALLOCATED (horip101_index)) DEALLOCATE (horip101_index)
    IF ( ALLOCATED (prod10)) DEALLOCATE (prod10)
    IF ( ALLOCATED (prod100)) DEALLOCATE (prod100)
    IF ( ALLOCATED (flux10)) DEALLOCATE (flux10)
    IF ( ALLOCATED (flux100)) DEALLOCATE (flux100)
    IF ( ALLOCATED (convflux)) DEALLOCATE (convflux)
    IF ( ALLOCATED (cflux_prod10)) DEALLOCATE (cflux_prod10)
    IF ( ALLOCATED (cflux_prod100)) DEALLOCATE (cflux_prod100)
    IF ( ALLOCATED (harvest_above)) DEALLOCATE (harvest_above)
    IF ( ALLOCATED (soilcarbon_input_daily)) DEALLOCATE (soilcarbon_input_daily)
    IF ( ALLOCATED (control_temp_daily)) DEALLOCATE (control_temp_daily)
    IF ( ALLOCATED (control_moist_daily)) DEALLOCATE (control_moist_daily)

    IF ( ALLOCATED (fpc_max)) DEALLOCATE (fpc_max)
!gmjc
    IF (ALLOCATED(litter_avail)) DEALLOCATE(litter_avail)
    IF (ALLOCATED(litter_not_avail)) DEALLOCATE(litter_not_avail)
    IF (ALLOCATED(litter_avail_frac)) DEALLOCATE(litter_avail_frac)
!    IF (ALLOCATED(resp_hetero_litter_d)) DEALLOCATE(resp_hetero_litter_d)
!    IF (ALLOCATED(resp_hetero_soil_d)) DEALLOCATE(resp_hetero_soil_d)
!    IF (ALLOCATED(resp_hetero_soil_part)) DEALLOCATE(resp_hetero_soil_part)
    IF (ALLOCATED(sla_calc)) DEALLOCATE(sla_calc)
    IF (ALLOCATED(wshtotsum)) DEALLOCATE(wshtotsum)
    IF (ALLOCATED(sr_ugb)) DEALLOCATE(sr_ugb)
    IF (ALLOCATED(compt_ugb)) DEALLOCATE(compt_ugb)
    IF (ALLOCATED(nb_ani)) DEALLOCATE(nb_ani)
    IF (ALLOCATED(grazed_frac)) DEALLOCATE(grazed_frac)
    IF (ALLOCATED(import_yield)) DEALLOCATE(import_yield)
    IF (ALLOCATED(t2m_14)) DEALLOCATE(t2m_14)
    IF (ALLOCATED(sla_age1)) DEALLOCATE(sla_age1)
    IF (ALLOCATED(N_limfert)) DEALLOCATE(N_limfert)
    IF (ALLOCATED(when_growthinit_cut)) DEALLOCATE(when_growthinit_cut)
    IF (ALLOCATED(nb_grazingdays)) DEALLOCATE(nb_grazingdays)
    IF (ALLOCATED(snowfall_daily)) DEALLOCATE(snowfall_daily)
    IF (ALLOCATED(snowmass_daily)) DEALLOCATE(snowmass_daily)
!top 5 layer grassland soil moisture for grazing
    IF (ALLOCATED(tmc_topgrass_daily)) DEALLOCATE(tmc_topgrass_daily)
    IF (ALLOCATED(after_snow)) DEALLOCATE(after_snow)
    IF (ALLOCATED(after_wet)) DEALLOCATE(after_wet)
    IF (ALLOCATED(wet1day)) DEALLOCATE(wet1day)
    IF (ALLOCATED(wet2day)) DEALLOCATE(wet2day)
!end gmjc
 !! 2. reset l_first

    l_first_stomate=.TRUE.

 !! 3. call to clear functions

    CALL season_clear
    CALL stomatelpj_clear
    CALL littercalc_clear
    CALL vmax_clear
    CALL permafrost_carbon_clear
    IF ( ALLOCATED (deepC_a)) DEALLOCATE(deepC_a)
    IF ( ALLOCATED (deepC_s)) DEALLOCATE(deepC_s)
    IF ( ALLOCATED (deepC_p)) DEALLOCATE(deepC_p)
    IF ( ALLOCATED (O2_soil)) DEALLOCATE(O2_soil)
    IF ( ALLOCATED (CH4_soil)) DEALLOCATE(CH4_soil)
    IF ( ALLOCATED (O2_snow)) DEALLOCATE(O2_snow)
    IF ( ALLOCATED (CH4_snow)) DEALLOCATE(CH4_snow)
    IF ( ALLOCATED (tdeep_daily)) DEALLOCATE(tdeep_daily)
    IF ( ALLOCATED (fbact)) DEALLOCATE(fbact)
    IF ( ALLOCATED (fbact_daily)) DEALLOCATE(fbact_daily)
    IF ( ALLOCATED (prmfrst_soilc_tempctrl)) DEALLOCATE(prmfrst_soilc_tempctrl)
    IF ( ALLOCATED (prmfrst_soilc_tempctrl_daily)) DEALLOCATE(prmfrst_soilc_tempctrl_daily)
    IF ( ALLOCATED (hsdeep_daily)) DEALLOCATE(hsdeep_daily)
    IF ( ALLOCATED (temp_sol_daily)) DEALLOCATE(temp_sol_daily)
    ! IF ( ALLOCATED (soilcarbon_input_daily))
    ! DEALLOCATE(soilcarbon_input_daily)
    IF ( ALLOCATED (pb_pa_daily)) DEALLOCATE(pb_pa_daily)
    IF ( ALLOCATED (snow_daily)) DEALLOCATE(snow_daily)
    IF ( ALLOCATED (fixed_cryoturbation_depth)) DEALLOCATE(fixed_cryoturbation_depth)
    IF ( ALLOCATED (snowdz_daily)) DEALLOCATE(snowdz_daily)
    IF ( ALLOCATED (snowrho_daily)) DEALLOCATE(snowrho_daily)
 
  END SUBROUTINE stomate_clear


!! ================================================================================================================================
!! SUBROUTINE 	: stomate_var_init
!!
!>\BRIEF        Initialize variables of stomate with a none-zero initial value.
!! Subroutine is called only if ::ok_stomate = .TRUE. STOMATE diagnoses some 
!! variables for SECHIBA : assim_param, deadleaf_cover, etc. These variables can 
!! be recalculated from STOMATE's prognostic variables. Note that height is
!! saved in SECHIBA.
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): leaf age (::leaf_age) and fraction of leaves in leaf 
!! age class (::leaf_frac). The maximum water on vegetation available for 
!! interception, fraction of soil covered by dead leaves
!! (::deadleaf_cover) and assimilation parameters (:: assim_param).
!!
!! REFERENCE(S)	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  
  SUBROUTINE stomate_var_init &
       &  (kjpindex, veget_cov_max, leaf_age, leaf_frac, &
       &   dead_leaves, &
       &   veget, lai, deadleaf_cover, assim_param, &!)
!gmjc
       &    N_limfert)
!end gmjc

  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std),INTENT(in)                             :: kjpindex        !! Domain size - terrestrial pixels only
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)        :: veget           !! Fraction of pixel covered by PFT. Fraction 
                                                                             !! accounts for none-biological land covers 
                                                                             !! (unitless) 
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)        :: veget_cov_max   !! Fractional coverage: maximum share of the pixel 
                                                                             !! covered by a PFT (unitless) 
    REAL(r_std),DIMENSION(kjpindex,nvm,nlitt),INTENT(in)  :: dead_leaves     !! Metabolic and structural fraction of dead leaves 
                                                                             !! per ground area 
                                                                             !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)        :: lai             !! Leaf area index 
                                                                             !! @tex $(m^2 m{-2})$ @endtex 
    REAL(r_std),DIMENSION(kjpindex,nvm,nleafages),INTENT(in) :: leaf_age     !! Age of different leaf classes per PFT (days)
    REAL(r_std),DIMENSION(kjpindex,nvm,nleafages),INTENT(in) :: leaf_frac    !! Fraction of leaves in leaf age class per PFT 
                                                                             !! (unitless; 1)     

    !! 0.2 Modified variables
    REAL(r_std),DIMENSION(kjpindex,nvm,npco2),INTENT(inout) :: assim_param   !! min+max+opt temperatures (K) & vmax for 
                                                                             !! photosynthesis  
    
    !! 0.3 Output variables

    REAL(r_std),DIMENSION(kjpindex), INTENT (out)         :: deadleaf_cover  !! Fraction of soil covered by dead leaves 
                                                                             !! (unitless) 


!gmjc
    ! N fertilization factor on Vcmax and SLA
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)       :: N_limfert
!end gmjc
    ! 0.4 Local variables
   
    REAL(r_std),PARAMETER                                 :: dt_0 = zero     !! Dummy time step, must be zero
    REAL(r_std),DIMENSION(kjpindex,nvm)                   :: vcmax           !! Dummy vcmax 
                                                                             !! @tex $(\mu mol m^{-2} s^{-1})$ @endtex
    REAL(r_std),DIMENSION(kjpindex,nvm,nleafages)         :: leaf_age_tmp    !! Temporary variable
    REAL(r_std),DIMENSION(kjpindex,nvm,nleafages)         :: leaf_frac_tmp   !! Temporary variable
                                                                             !! (unitless; 1)     
    INTEGER(i_std)                                        :: j               !! Index (untiless)
    
!_ ================================================================================================================================   

    ! Calculate assim_param if it was not found in the restart file
    IF (ALL(assim_param(:,:,:)==val_exp)) THEN
       ! Use temporary leaf_age_tmp and leaf_frac_tmp to preserve the input variables from being modified by the subroutine vmax.
       leaf_age_tmp(:,:,:)=leaf_age(:,:,:)
       leaf_frac_tmp(:,:,:)=leaf_frac(:,:,:)

       !! 1.1 Calculate a temporary vcmax (stomate_vmax.f90)
       CALL vmax (kjpindex, dt_0, leaf_age_tmp, leaf_frac_tmp, vcmax, N_limfert)

       !! 1.2 transform into nvm vegetation types
       assim_param(:,:,ivcmax) = zero
       DO j = 2, nvm
          assim_param(:,j,ivcmax)=vcmax(:,j)
       ENDDO
    END IF
    
    !! 2. Dead leaf cover (stomate_litter.f90)
    CALL deadleaf (kjpindex, veget_cov_max, dead_leaves, deadleaf_cover, sla_calc)     
    
  END SUBROUTINE stomate_var_init


!! ================================================================================================================================
!! INTERFACE 	: stomate_accu
!!
!>\BRIEF        Accumulate a variable for the time period specified by 
!! dt_sechiba or calculate the mean value over the period of dt_stomate
!! 
!! DESCRIPTION : Accumulate a variable for the time period specified by 
!! dt_sechiba or calculate the mean value over the period of dt_stomate.
!! stomate_accu interface can be used for variables having 1, 2 or 3 dimensions.
!! The corresponding subruoutine stomate_accu_r1d, stomate_accu_r2d or
!! stomate_accu_r3d will be selected through the interface depending on the number of dimensions.
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): accumulated or mean variable ::field_out:: 
!!
!! REFERENCE(S)	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  SUBROUTINE stomate_accu_r1d (ldmean, field_in, field_out)
    
  !! 0. Variable and parameter declaration

    !! 0.1 Input variables
    LOGICAL,INTENT(in)                               :: ldmean    !! Flag to calculate the mean over
    REAL(r_std),DIMENSION(:),INTENT(in)    :: field_in  !! Field that needs to be accumulated
    
    !! 0.2 Modified variables
    REAL(r_std),DIMENSION(:),INTENT(inout) :: field_out !! Accumulated or mean field

!_ ================================================================================================================================

  !! 1. Accumulate field
    field_out(:) = field_out(:)+field_in(:)*dt_sechiba
   
  !! 2. Mean fields

    IF (ldmean) THEN
       field_out(:) = field_out(:)/dt_stomate
    ENDIF

  END SUBROUTINE stomate_accu_r1d
  
  SUBROUTINE stomate_accu_r2d (ldmean, field_in, field_out)
    
  !! 0. Variable and parameter declaration

    !! 0.1 Input variables
    LOGICAL,INTENT(in)                               :: ldmean    !! Flag to calculate the mean over
    REAL(r_std),DIMENSION(:,:),INTENT(in)    :: field_in  !! Field that needs to be accumulated
    
    !! 0.2 Modified variables
    REAL(r_std),DIMENSION(:,:),INTENT(inout) :: field_out !! Accumulated or mean field

!_ ================================================================================================================================

  !! 1. Accumulate field
    field_out(:,:) = field_out(:,:)+field_in(:,:)*dt_sechiba
   
  !! 2. Mean fields

    IF (ldmean) THEN
       field_out(:,:) = field_out(:,:)/dt_stomate
    ENDIF

  END SUBROUTINE stomate_accu_r2d

  SUBROUTINE stomate_accu_r3d (ldmean, field_in, field_out)
    
  !! 0. Variable and parameter declaration

    !! 0.1 Input variables
    LOGICAL,INTENT(in)                                   :: ldmean    !! Flag to calculate the mean over
    REAL(r_std),DIMENSION(:,:,:),INTENT(in)    :: field_in  !! Field that needs to be accumulated
    
    !! 0.2 Modified variables
    REAL(r_std),DIMENSION(:,:,:),INTENT(inout) :: field_out !! Accumulated or mean field

!_ ================================================================================================================================

  !! 1. Accumulate field
    field_out(:,:,:) = field_out(:,:,:)+field_in(:,:,:)*dt_sechiba
   
  !! 2. Mean fields

    IF (ldmean) THEN
       field_out(:,:,:) = field_out(:,:,:)/dt_stomate
    ENDIF

  END SUBROUTINE stomate_accu_r3d

!! ================================================================================================================================
!! SUBROUTINE 	: stomate_veget_update
!!
!>\BRIEF        After the vegetation has been updated some variables needs to be reinitialized for too small fractions
!! 
!! DESCRIPTION :  After the vegetation has been updated some variables needs to be reinitialized for too small fractions.
!!                This subroutine is called from slowproc_veget after removing to small fractions.
!!
!! RECENT CHANGE(S) : These calculations were previously done in stomate_lcchange
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S)	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE stomate_veget_update(kjpindex, veget_max, f_rot_sech)

    !! 0. Variable and parameter declaration
    !! 0.1 Input variables
    INTEGER(i_std),INTENT(in)                       :: kjpindex          !! Local domain size - terrestrial pixels only (unitless)
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)  :: veget_max         !! Maximum fraction of vegetation type including 
                                                                         !! non-biological fraction (unitless) 
    !! 0.2 Local variables
    INTEGER(i_std)                                  :: i, j              !! Indices
    LOGICAL,DIMENSION(kjpindex), INTENT(in)         :: f_rot_sech        !! whether a grid point is under rotation

    ! Only proceed if the initalization of stomate has been done
    IF (.NOT. l_first_stomate) THEN

       ! Loop over all points on all pft's
       ! If the fraction is too small, reset the depending variables
       ! veget_max will be changed in slowproc_veget after this subroutine has been done. 
       DO i = 1, kjpindex
          IF (.NOT. f_rot_sech(i)) THEN
              DO j=2, nvm
                 IF ( veget_max(i,j) < min_vegfrac ) THEN 
                    ind(i,j) = zero
                    biomass(i,j,:,:) = zero
                    PFTpresent(i,j) = .FALSE.
                    senescence(i,j) = .FALSE.
                    age(i,j) = zero
                    when_growthinit(i,j) = large_value
                    everywhere(i,j) = zero
                    carbon(i,:,j) = zero
                    litter(i,:,j,:,:) = zero
                    bm_to_litter(i,j,:,:) = zero
                    turnover_daily(i,j,:,:) = zero
                 ENDIF
              END DO
          ENDIF
       END DO
    END IF

  END SUBROUTINE stomate_veget_update

!! ================================================================================================================================
!! SUBROUTINE 	: init_forcing
!!
!>\BRIEF        Allocate memory for the variables containing the forcing data.
!! The maximum size of the allocated memory is specified in run definition file
!! (::max_totsize) and needs to be a compromise between charging the memory and 
!! accessing disks to get the forcing data.
!!
!! DESCRIPTION : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): Strictly speaking the subroutine has no output 
!! variables. However, the routine allocates memory for later use. 
!!
!! REFERENCE(S)	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  
  SUBROUTINE init_forcing (kjpindex,nsfm,nsft_loc)
    
  !! 0. Variable and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std),INTENT(in) :: kjpindex !! Domain size - terrestrial pixels only (unitless)
    INTEGER(i_std),INTENT(in) :: nsfm     !! Number of time steps that can be stored in memory (unitless)
    INTEGER(i_std),INTENT(in) :: nsft_loc !! Number of time steps in a year (unitless)

   !! 0.2 Output variables

   !! 0.3 Modified variables

   !! 0.4 Local variables

    LOGICAL                   :: l_error  !! Check errors in netcdf call
    INTEGER(i_std)            :: ier      !! Check errors in netcdf call
!_ ================================================================================================================================
    
  !! 1. Allocate memory

    ! Note ::nvm is number of PFTs and ::nbdl is number of soil layers
    l_error = .FALSE.
    ALLOCATE(clay_fm(kjpindex,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables clay_fm ',kjpindex,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(humrel_daily_fm(kjpindex,nvm,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables humrel_daily_fm ',kjpindex,nvm,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(litterhum_daily_fm(kjpindex,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables litterhum_daily_fm ',kjpindex,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(t2m_daily_fm(kjpindex,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables t2m_daily_fm ',kjpindex,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(t2m_min_daily_fm(kjpindex,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables t2m_min_daily_fm ',kjpindex,nsfm
       STOP 'init_forcing'
    ENDIF
    !spitfire
    ALLOCATE(t2m_max_daily_fm(kjpindex,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables t2m_max_daily_fm ',kjpindex,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(wspeed_daily_fm(kjpindex,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables wspeed_daily_fm ',kjpindex,nsfm
       STOP 'init_forcing'
    ENDIF
    !endspit
    ALLOCATE(tsurf_daily_fm(kjpindex,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables tsurf_daily_fm ',kjpindex,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(tsoil_daily_fm(kjpindex,nbdl,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables tsoil_daily_fm ',kjpindex,nbdl,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(soilhum_daily_fm(kjpindex,nbdl,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables soilhum_daily_fm ',kjpindex,nbdl,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(precip_fm(kjpindex,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables precip_fm ',kjpindex,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(gpp_daily_fm(kjpindex,nvm,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables gpp_daily_fm ',kjpindex,nvm,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(veget_fm(kjpindex,nvm,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables veget_fm ',kjpindex,nvm,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(veget_max_fm(kjpindex,nvm,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables veget_max_fm ',kjpindex,nvm,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(lai_fm(kjpindex,nvm,nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables lai_fm ',kjpindex,nvm,nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(isf(nsfm),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables isf ',nsfm
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(nf_written(nsft_loc),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables nf_written ',nsft_loc
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(nf_cumul(nsft_loc),stat=ier)
    l_error = l_error .OR. (ier /= 0)
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables nf_cumul ',nsft_loc
       STOP 'init_forcing'
    ENDIF
    ALLOCATE(t2mdiag_fm(kjpindex,nsfm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3, 'forcing_zero', 'Problem with memory allocation: forcing variable', 't2mdiag_fm', '')
    ALLOCATE(swdown_fm(kjpindex,nsfm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3, 'forcing_zero', 'Problem with memory allocation: forcing variable', 'swdown_fm', '')
    ALLOCATE(evapot_corr_fm(kjpindex,nsfm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3, 'forcing_zero', 'Problem with memory allocation: forcing variable', 'evapot_corr_fm', '')
    
  !! 2. Allocate memory for the root processor only (parallel computing)

    ! Where, ::nbp_glo is the number of global continental points
    IF (is_root_prc) THEN
       ALLOCATE(clay_fm_g(nbp_glo,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables clay_fm_g ',nbp_glo,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(humrel_daily_fm_g(nbp_glo,nvm,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables humrel_daily_fm_g ',nbp_glo,nvm,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(litterhum_daily_fm_g(nbp_glo,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables litterhum_daily_fm_g ',nbp_glo,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(t2m_daily_fm_g(nbp_glo,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables t2m_daily_fm_g ',nbp_glo,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(t2m_min_daily_fm_g(nbp_glo,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables t2m_min_daily_fm_g ',nbp_glo,nsfm
          STOP 'init_forcing'
       ENDIF
       !spitfire
       ALLOCATE(t2m_max_daily_fm_g(nbp_glo,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables t2m_max_daily_fm_g ',nbp_glo,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(wspeed_daily_fm_g(nbp_glo,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables wspeed_daily_fm_g ',nbp_glo,nsfm
          STOP 'init_forcing'
       ENDIF
       !endspit
       ALLOCATE(tsurf_daily_fm_g(nbp_glo,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables tsurf_daily_fm_g ',nbp_glo,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(tsoil_daily_fm_g(nbp_glo,nbdl,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables tsoil_daily_fm_g ',nbp_glo,nbdl,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(soilhum_daily_fm_g(nbp_glo,nbdl,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables soilhum_daily_fm_g ',nbp_glo,nbdl,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(precip_fm_g(nbp_glo,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables precip_fm_g ',nbp_glo,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(gpp_daily_fm_g(nbp_glo,nvm,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables gpp_daily_fm_g ',nbp_glo,nvm,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(veget_fm_g(nbp_glo,nvm,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables veget_fm_g ',nbp_glo,nvm,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(veget_max_fm_g(nbp_glo,nvm,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables veget_max_fm_g ',nbp_glo,nvm,nsfm
          STOP 'init_forcing'
       ENDIF
       ALLOCATE(lai_fm_g(nbp_glo,nvm,nsfm),stat=ier)
       l_error = l_error .OR. (ier /= 0)
       IF (l_error) THEN
          WRITE(numout,*) 'Problem with memory allocation: forcing variables lai_fm_g ',nbp_glo,nvm,nsfm
          STOP 'init_forcing'
       ENDIF

       ALLOCATE(t2mdiag_fm_g(nbp_glo,nsfm),stat=ier)
       IF (ier /= 0) CALL ipslerr(3, 'Problem with memory allocation: forcing variables,','t2mdiag_fm_g', '', '')
       ALLOCATE(swdown_fm_g(nbp_glo,nsfm),stat=ier)
       IF (ier /= 0) CALL ipslerr(3, 'Problem with memory allocation: forcing variables,','swdown_fm_g', '', '')
       ALLOCATE(evapot_corr_fm_g(nbp_glo,nsfm),stat=ier)
       IF (ier /= 0) CALL ipslerr(3, 'Problem with memory allocation: forcing variables,','evapot_corr_fm_g', '', '')
    ELSE
       ! Allocate memory for co-processors
       ALLOCATE(clay_fm_g(0,nsfm),stat=ier)
       ALLOCATE(humrel_daily_fm_g(0,nvm,nsfm),stat=ier)
       ALLOCATE(litterhum_daily_fm_g(0,nsfm),stat=ier)
       ALLOCATE(t2m_daily_fm_g(0,nsfm),stat=ier)
       ALLOCATE(t2m_min_daily_fm_g(0,nsfm),stat=ier)
       !spitfire
       ALLOCATE(t2m_max_daily_fm_g(0,nsfm),stat=ier)
       ALLOCATE(wspeed_daily_fm_g(0,nsfm),stat=ier)
       !endspit
       ALLOCATE(tsurf_daily_fm_g(0,nsfm),stat=ier)
       ALLOCATE(tsoil_daily_fm_g(0,nbdl,nsfm),stat=ier)
       ALLOCATE(soilhum_daily_fm_g(0,nbdl,nsfm),stat=ier)
       ALLOCATE(precip_fm_g(0,nsfm),stat=ier)
       ALLOCATE(gpp_daily_fm_g(0,nvm,nsfm),stat=ier)
       ALLOCATE(veget_fm_g(0,nvm,nsfm),stat=ier)
       ALLOCATE(veget_max_fm_g(0,nvm,nsfm),stat=ier)
       ALLOCATE(lai_fm_g(0,nvm,nsfm),stat=ier)
       ALLOCATE(t2mdiag_fm_g(0,nsfm),stat=ier)
       ALLOCATE(swdown_fm_g(0,nsfm),stat=ier)
       ALLOCATE(evapot_corr_fm_g(0,nsfm),stat=ier)
    ENDIF ! is_root_proc
    
    IF (l_error) THEN
       WRITE(numout,*) 'Problem with memory allocation: forcing variables'
       STOP 'init_forcing'
    ENDIF

  !! 3. Initilaize variables

    CALL forcing_zero
    
  END SUBROUTINE init_forcing


!! ================================================================================================================================
!! SUBROUTINE 	: forcing_zero
!!
!>\BRIEF        Initialize variables containing the forcing data; variables are 
!! set to zero.
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCES	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  
  SUBROUTINE forcing_zero
    
    clay_fm(:,:) = zero
    humrel_daily_fm(:,:,:) = zero
    litterhum_daily_fm(:,:) = zero
    t2m_daily_fm(:,:) = zero
    t2m_min_daily_fm(:,:) = zero
    !spitfire
    t2m_max_daily_fm(:,:) = zero
    wspeed_daily_fm(:,:) = zero
    !endspit
    tsurf_daily_fm(:,:) = zero
    tsoil_daily_fm(:,:,:) = zero
    soilhum_daily_fm(:,:,:) = zero
    precip_fm(:,:) = zero
    gpp_daily_fm(:,:,:) = zero
    veget_fm(:,:,:) = zero
    veget_max_fm(:,:,:) = zero
    lai_fm(:,:,:) = zero
    t2mdiag_fm(:,:) = zero
    swdown_fm(:,:) = zero
    evapot_corr_fm(:,:) = zero
    
  END SUBROUTINE forcing_zero


!! ================================================================================================================================
!! SUBROUTINE 	: forcing_write
!!
!>\BRIEF        Appends data values to a netCDF file containing the forcing 
!! variables of the general processes in stomate.
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): netCDF file
!!
!! REFERENCES	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  
  SUBROUTINE forcing_write(forcing_id,ibeg,iend)
    
  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std),INTENT(in)      :: forcing_id  !! File identifer of forcing file, assigned when netcdf is created
    INTEGER(i_std),INTENT(in)      :: ibeg, iend  !! First and last time step to be written

    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                 :: ii          !! Index of isf where isf is the number of time steps that can be 
                                                  !! stored in memory 
    INTEGER(i_std)                 :: iblocks     !! Index of block that is written
    INTEGER(i_std)                 :: nblocks     !! Number of blocks that needs to be written
    INTEGER(i_std)                 :: ier         !! Check errors in netcdf call
    INTEGER(i_std),DIMENSION(0:2)  :: ifirst      !! First block in memory - changes with iblocks
    INTEGER(i_std),DIMENSION(0:2)  :: ilast       !! Last block in memory - changes with iblocks
    INTEGER(i_std),PARAMETER       :: ndm = 10    !! Maximum number of dimensions
    INTEGER(i_std),DIMENSION(ndm)  :: start       !! First block to write
    INTEGER(i_std)                 :: ndim        !! Dimensions of forcing to be added to the netCDF
    INTEGER(i_std),DIMENSION(ndm)  :: count_force !! Number of elements in each dimension  
    INTEGER(i_std)                 :: vid         !! Variable identifer of netCDF
!_ ================================================================================================================================
    
  !! 1. Determine number of blocks of forcing variables that are stored in memory

    nblocks = 0
    ifirst(:) = 1
    ilast(:) = 1
    DO ii = ibeg, iend
       IF (     (nblocks /= 0) &
            &      .AND.(isf(ii) == isf(ilast(nblocks))+1)) THEN
          ! Last block found
          ilast(nblocks) = ii
       ELSE
          ! First block found
          nblocks = nblocks+1
          IF (nblocks > 2)  STOP 'Problem in forcing_write'
          ifirst(nblocks) = ii
          ilast(nblocks) = ii
       ENDIF
    ENDDO

  !! 2. Gather distributed variables (parallel computing)

    CALL gather(clay_fm,clay_fm_g)
    CALL gather(humrel_daily_fm,humrel_daily_fm_g)
    CALL gather(litterhum_daily_fm,litterhum_daily_fm_g)
    CALL gather(t2m_daily_fm,t2m_daily_fm_g)
    CALL gather(t2m_min_daily_fm,t2m_min_daily_fm_g)
    !spitfire
    CALL gather(t2m_max_daily_fm,t2m_max_daily_fm_g)
    CALL gather(wspeed_daily_fm,wspeed_daily_fm_g)
    !endspit
    CALL gather(tsurf_daily_fm,tsurf_daily_fm_g)
    CALL gather(tsoil_daily_fm,tsoil_daily_fm_g)
    CALL gather(soilhum_daily_fm,soilhum_daily_fm_g)
    CALL gather(precip_fm,precip_fm_g)
    CALL gather(gpp_daily_fm,gpp_daily_fm_g)
    CALL gather(veget_fm,veget_fm_g)
    CALL gather(veget_max_fm,veget_max_fm_g)
    CALL gather(lai_fm,lai_fm_g)
    CALL gather(t2mdiag_fm,t2mdiag_fm_g)
    CALL gather(swdown_fm,swdown_fm_g)
    CALL gather(evapot_corr_fm,evapot_corr_fm_g)
 
 !! 3. Append data to netCDF file
   
    IF (is_root_prc) THEN
       ! The netCDF file has been created earlier in this module, a file ID is available 
       ! and variables and dimensions have already been defined
       DO iblocks = 1, nblocks
          IF (ifirst(iblocks) /= ilast(iblocks)) THEN
             ndim = 2
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(clay_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'clay',vid)
             ier = NF90_PUT_VAR (forcing_id,vid, &
                  &              clay_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(humrel_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'humrel',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            humrel_daily_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(litterhum_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'litterhum',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            litterhum_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(t2m_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'t2m',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            t2m_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(t2m_min_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'t2m_min',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            t2m_min_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             !spitfire
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(t2m_max_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'t2m_min',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            t2m_max_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(wspeed_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'t2m_min',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            wspeed_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             !endspit
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(tsurf_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'tsurf',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            tsurf_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(tsoil_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'tsoil',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            tsoil_daily_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(soilhum_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'soilhum',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            soilhum_daily_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(precip_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'precip',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            precip_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(gpp_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'gpp',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            gpp_daily_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(veget_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'veget',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            veget_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(veget_max_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'veget_max',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            veget_max_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(lai_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'lai',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            lai_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(t2mdiag_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'t2mdiag',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            t2mdiag_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(swdown_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'tsurf',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            swdown_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(evapot_corr_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'tsurf',vid)
             ier = NF90_PUT_VAR (forcing_id, vid, &
                  &            evapot_corr_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  & start=start(1:ndim), count=count_force(1:ndim))
          ENDIF
       ENDDO
    ENDIF
    
  !! 4. Adjust flag of forcing file
    nf_written(isf(:)) = .TRUE.

  END SUBROUTINE forcing_write

  
!! ================================================================================================================================
!! SUBROUTINE 	: stomate_forcing_read
!!
!>\BRIEF        Read forcing file.
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): None 
!!
!! REFERENCES	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  
  SUBROUTINE stomate_forcing_read(forcing_id,nsfm)
   
  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std),INTENT(in)  :: forcing_id           !! File identifer of forcing file, assigned when netcdf is created
    INTEGER(i_std),INTENT(in)  :: nsfm                 !! Number of time steps stored in memory        
    
    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                 :: ii                !! Index of isf where isf is the number of time steps that can be stored in 
                                                        !! memory 
    INTEGER(i_std)                 :: iblocks           !! Index of block that is written
    INTEGER(i_std)                 :: nblocks           !! Number of blocks that needs to be written
    INTEGER(i_std)                 :: ier               !! Check error of netcdf call
    INTEGER(i_std),DIMENSION(0:2)  :: ifirst            !! First block in memory - changes with iblocks
    INTEGER(i_std),DIMENSION(0:2)  :: ilast             !! Last block in memory - changes with iblocks
    INTEGER(i_std),PARAMETER       :: ndm = 10          !! Maximum number of dimensions
    INTEGER(i_std),DIMENSION(ndm)  :: start             !! First block to write
    INTEGER(i_std)                 :: ndim              !! Dimensions of forcing to be added to the netCDF
    INTEGER(i_std),DIMENSION(ndm)  :: count_force       !! Number of elements in each dimension
    INTEGER(i_std)                 :: vid               !! Variable identifer of netCDF
    LOGICAL, PARAMETER             :: check=.FALSE.     !! Flag for debugging 
    LOGICAL                        :: a_er=.FALSE.      !! Error catching from netcdf file
!_ ================================================================================================================================

    IF (check) WRITE(numout,*) "stomate_forcing_read "
    
  !! 1. Set to zero if the corresponding forcing state

    ! has not yet been written into the file  
    DO iisf = 1, nsfm
       IF (.NOT.nf_written(isf(iisf))) THEN
          clay_fm(:,iisf) = zero
          humrel_daily_fm(:,:,iisf) = zero
          litterhum_daily_fm(:,iisf) = zero
          t2m_daily_fm(:,iisf) = zero
          t2m_min_daily_fm(:,iisf) = zero
          !spitfire
          t2m_max_daily_fm(:,iisf) = zero
          wspeed_daily_fm(:,iisf) = zero
          !endspit
          tsurf_daily_fm(:,iisf) = zero
          tsoil_daily_fm(:,:,iisf) = zero
          soilhum_daily_fm(:,:,iisf) = zero
          precip_fm(:,iisf) = zero
          gpp_daily_fm(:,:,iisf) = zero
          veget_fm(:,:,iisf) = zero
          veget_max_fm(:,:,iisf) = zero
          lai_fm(:,:,iisf) = zero
          t2mdiag_fm(:,iisf) = zero
          swdown_fm(:,iisf) = zero
          evapot_corr_fm(:,iisf) = zero
       ENDIF
    ENDDO
    
  !! 2. determine blocks of forcing states that are contiguous in memory

    nblocks = 0
    ifirst(:) = 1
    ilast(:) = 1
    
    DO ii = 1, nsfm
       IF (nf_written(isf(ii))) THEN
          IF (     (nblocks /= 0) &
               &        .AND.(isf(ii) == isf(ilast(nblocks))+1)) THEN

             ! element is contiguous with last element found
             ilast(nblocks) = ii
          ELSE

             ! found first element of new block
             nblocks = nblocks+1
             IF (nblocks > 2)  STOP 'Problem in stomate_forcing_read'
             
             ifirst(nblocks) = ii
             ilast(nblocks) = ii
          ENDIF
       ENDIF
    ENDDO
    IF (check) WRITE(numout,*) "stomate_forcing_read nblocks, ifirst, ilast",nblocks, ifirst, ilast
    
  !! 3. Read variable values

    IF (is_root_prc) THEN
       DO iblocks = 1, nblocks
          IF (check) WRITE(numout,*) "stomate_forcing_read iblocks, ifirst(iblocks), ilast(iblocks)",iblocks, &
               ifirst(iblocks), ilast(iblocks)
          IF (ifirst(iblocks) /= ilast(iblocks)) THEN
             a_er=.FALSE.
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(clay_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'clay',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &            clay_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(humrel_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'humrel',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &            humrel_daily_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(litterhum_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'litterhum',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              litterhum_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(t2m_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'t2m',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              t2m_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(t2m_min_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'t2m_min',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              t2m_min_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             !spitfire
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(t2m_max_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'t2m_min',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              t2m_max_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(wspeed_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'t2m_min',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              wspeed_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)
             !endspit
             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(tsurf_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'tsurf',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              tsurf_daily_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(tsoil_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'tsoil',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              tsoil_daily_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(soilhum_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'soilhum',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              soilhum_daily_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(precip_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'precip',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              precip_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(gpp_daily_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'gpp',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &            gpp_daily_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(veget_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'veget',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &            veget_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(veget_max_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'veget_max',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &            veget_max_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 3;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(lai_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'lai',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &            lai_fm_g(:,:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(t2mdiag_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'t2mdiag',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              t2mdiag_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(swdown_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'swdown',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &             swdown_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)

             ndim = 2;
             start(1:ndim) = 1; start(ndim) = isf(ifirst(iblocks));
             count_force(1:ndim) = SHAPE(evapot_corr_fm_g)
             count_force(ndim) = isf(ilast(iblocks))-isf(ifirst(iblocks))+1
             ier = NF90_INQ_VARID (forcing_id,'evapot_corr',vid)
             a_er = a_er.OR.(ier /= 0)
             ier = NF90_GET_VAR (forcing_id, vid, &
                  &              evapot_corr_fm_g(:,ifirst(iblocks):ilast(iblocks)), &
                  &            start=start(1:ndim), count=count_force(1:ndim))
             a_er = a_er.OR.(ier /= 0)
             IF (a_er) THEN
                CALL ipslerr_p (3,'stomate_forcing_read', &
                     &        'PROBLEM when read forcing file', &
                     &        '','')
             ENDIF

          ENDIF ! (ifirst(iblocks) /= ilast(iblocks))
       ENDDO ! iblocks
    ENDIF ! is_root_prc

  !! 4. Distribute the variable over several processors

    CALL scatter(clay_fm_g,clay_fm)
    CALL scatter(humrel_daily_fm_g,humrel_daily_fm)
    CALL scatter(litterhum_daily_fm_g,litterhum_daily_fm)
    CALL scatter(t2m_daily_fm_g,t2m_daily_fm)
    CALL scatter(t2m_min_daily_fm_g,t2m_min_daily_fm)
    !spitfire
    CALL scatter(t2m_max_daily_fm_g,t2m_max_daily_fm)
    CALL scatter(wspeed_daily_fm_g,wspeed_daily_fm)
    !endspit
    CALL scatter(tsurf_daily_fm_g,tsurf_daily_fm)
    CALL scatter(tsoil_daily_fm_g,tsoil_daily_fm)
    CALL scatter(soilhum_daily_fm_g,soilhum_daily_fm)
    CALL scatter(precip_fm_g,precip_fm)
    CALL scatter(gpp_daily_fm_g,gpp_daily_fm)
    CALL scatter(veget_fm_g,veget_fm)
    CALL scatter(veget_max_fm_g,veget_max_fm)
    CALL scatter(lai_fm_g,lai_fm)
    CALL scatter(t2mdiag_fm_g,t2mdiag_fm)
    CALL scatter(swdown_fm_g,swdown_fm)
    CALL scatter(evapot_corr_fm_g,evapot_corr_fm)
  
  END SUBROUTINE stomate_forcing_read

END MODULE stomate
