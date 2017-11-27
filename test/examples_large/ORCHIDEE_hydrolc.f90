! =================================================================================================================================
! MODULE          : hydrolc
!
! CONTACT         : orchidee-help _at_ listes.ipsl.fr
!
! LICENCE         : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF          This module computes the water budget of 
!! land surfaces. It distinguishes three main reservoirs with separate water budgets : 
!! the canopy interception reservoir, the snow pack, and the soil, described here using two
!! soil layers, following the so-called Choisnel scheme. The module contains the  
!! following subroutines: hydrolc_main, hydrolc_init, hydrolc_clear, hydrolc_var_init, 
!! hydrolc_snow, hydrolc_vegupd, hydrolc_canop, hydrolc_soil, hydrolc_waterbal, hydrolc_alma, 
!! hydrolc_hdiff.  
!!
!! \n DESCRIPTION : The aim of this module is to update the different water prognostic variables 
!! (within canopy, snow and soil) and to compute the related liquid water fluxes (e.g. 
!! throughfall, runoff, snow melt) and diagnostic variables used in other modules (e.g. 
!! humrel and rsol to compute latent heat fluxes ; shumdiag for soil thermodynamics; snow mass 
!! and snow age for snow albedo). 
!! 
!! The main input variables are precipitation (liquid and solid) and the different terms of 
!! evapotranspiration (transpiration, bare soil evaporation, interception loss, sublimation). 
!! The module organizes the required initialisation of water prognostic variables, their
!! integration at each timestep given the above forcings, and the required output (writing 
!! of restart file, updated prognostic variables, diagnostic variables). 
!! 
!! The specificity of hydrolc in ORCHIDEE is to describe the soil using two soil layers, 
!! following the so-called Choisnel scheme. The upper layer has a variable depth
!! and can disappear after dry spells (Ducoudré et al., 1993). Soil moisture stress on 
!! transpiration and bare soil evaporation acts via the height of dry soil at the top of soil.
!!
!! Runoff is produced when the entire soil column is saturated, as in the bucket scheme 
!! of Manabe (1969). Drainage and surface runoff are only diagnosed,  mostly for use in 
!! the routing module, using a constant 95% - 5% redistribution (Ngo-Duc et al., 2005).
!! Irrigation can interact with the soil columns (de Rosnay et al., 2003), as well as 
!! river discharge in floodplains (Ngo-Duc et al., 2005).
!!
!! The dynamic of the one-layer snow pack results from accumulation, sublimation and 
!! snowmelt. Snow ageing is accounted for but it does not influence snow density.  
!!
!! The subgrid variability of soil follows the one of vegetation, by introducing as many 
!! soil columns as PFTs (de Rosnay & Polcher, 1998). The water budget is performed 
!! independently in each PFT/soil column, but water can be exchanged laterally. 
!! The soil moisture of the bottom layer is homogenized between the soil columns at 
!! each timestep, and a diffusion between the top soil layers can be allowed if the
!! flag ok_hdiff is true (the default value is false).
!!
!! RECENT CHANGE(S) : None 
!!
!! REFERENCE(S)     : 
!!  - Ducoudré, N, Laval, K & Perrier, A, 1993. SECHIBA, a new set of parameterisations
!! of the hydrologic exchanges at the land-atmosphere interface within the LMD Atmospheric General
!! Circulation Model. Journal of Climate, 6, pp. 248-273. 
!!  - Ducharne, A. Laval, K. and Polcher, J. (1998) Sensitivity of the hydrological cycle to the 
!! parameterization of soil hydrology in a GCM. Climate Dynamics, 14:307-327. 
!!  - de Rosnay, P. and Polcher J. (1998) Modeling root water uptake in a complex land surface scheme
!!  coupled to a GCM. Hydrology and Earth System Sciences, 2(2-3):239-256. 
!!  - de Rosnay, P., Polcher, J., Laval, K. et Sabre, M. (2003). Integrated parameterization of 
!! irrigation in the land surface model ORCHIDEE. Validation over Indian Peninsula. Geophys. Res.
!! Lett, 30(19):HLS2-1 -
!! HLS2-4. 
!!  - Krinner, G., Viovy, N., de Noblet-Ducoudré, N., Ogee, J., Polcher, J., Friedlingstein, P., 
!! Ciais, P., Sitch, S. et Prentice, I. (2005). A dynamic global vegetation model for studies of the 
!! coupled atmosphere-biosphere system. Global Biogeochem. Cycles, 19(1).
!!  - Ngo-Duc, T., Laval, K., Ramillien, G., Polcher, J. et Cazenave, A. (2007). Validation of the land
!!  water storage simulated by Organising Carbon and Hydrology in Dynamic Ecosystems (ORCHIDEE) with 
!! Gravity Recovery and Climate Experiment (GRACE) data. Water Resour. Res, 43:W04427. 
!!  - ALMA : http://www.lmd.jussieu.fr/~polcher/ALMA/
!!  - Ducoudré, N. (1990). Sensibilite du climat simule a la parametrisation des echanges de vapeur 
!! d'eau entre la biosphere et l'atmosphere. These de Doctorat, Université Paris 6. 
!!  - Ducharne A (1997). Le cycle de l'eau : modelisation de l'hydrologie continentale, etude de ses 
!! interactions avec le climat, These de Doctorat, Université Paris 6. 
!!  - de Rosnay, P. (1999). Representation des interactions sol-plante-atmosphere dans le modele de 
!! circulation generale du LMD. These de doctorat, Université Paris 6. 
!!  - Guimberteau, M. (2010). Modelisation de l'hydrologie continentale et influences de l'irrigation 
!! sur le cycle de l'eau, these de doctorat, Université Paris 6. 
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/branches/ORCHIDEE-MICT/ORCHIDEE/src_sechiba/hydrolc.f90 $
!! $Date: 2017-07-17 12:59:54 +0200 (lun. 17 juil. 2017) $
!! $Revision: 4507 $
!! \n
!_ ================================================================================================================================
 
MODULE hydrolc
  
  ! modules used
  USE ioipsl
  USE xios_orchidee
  USE ioipsl_para 
  USE constantes        ! contains technical constants
  USE constantes_soil   ! soil properties and geometry (underground)
  USE pft_parameters    ! surface and vegetation properties
  USE sechiba_io_p        ! for inpout/output
  USE grid              ! for grid, masks and calendar
  USE mod_orchidee_para ! for variable and subroutines realted to the parallelism of orchidee
  USE explicitsnow

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: hydrolc_main, hydrolc_initialize, hydrolc_finalize, hydrolc_clear 


  LOGICAL, SAVE                                   :: ok_hdiff        !! To do horizontal diffusion between soil columns
!$OMP THREADPRIVATE(ok_hdiff)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: bqsb            !! Water content in the bottom layer  
                                                                     !! @tex ($kg m^{-2}$) @endtex 
!$OMP THREADPRIVATE(bqsb)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: gqsb            !! Water content in the top layer @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(gqsb)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: dsg             !! Depth of the top layer (m) 
!$OMP THREADPRIVATE(dsg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: dsp             !! Depth of dry soil in the bottom layer plus depth of top 
                                                                     !! layer (m)
!$OMP THREADPRIVATE(dsp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: irrig_fin       !! final application of irrigation water for each pft
!$OMP THREADPRIVATE(irrig_fin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: mean_bqsb       !! Mean water content in the bottom layer, averaged over the 
                                                                     !! soil columns @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(mean_bqsb)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: mean_gqsb       !! Mean water content in the top layer, averaged over the   
                                                                     !! soil columns @tex ($kg m^{-2}$) @endtex 
!$OMP THREADPRIVATE(mean_gqsb)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: tot_flux        !! Total water flux
!$OMP THREADPRIVATE(tot_flux)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: tot_water_beg   !! Total amount of water at start of time step 
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(tot_water_beg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: tot_water_end   !! Total amount of water at end of time step  
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(tot_water_end)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: tot_watveg_beg  !! Total amount of intercepted water on vegetation at start of
                                                                     !! time step  @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(tot_watveg_beg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: tot_watveg_end  !! Total amount of intercepted water on vegetation at end of 
                                                                     !! time step  @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(tot_watveg_end)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: tot_watsoil_beg !! Total amount of water in the soil at start of time step  
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(tot_watsoil_beg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: tot_watsoil_end !! Total amount of water in the soil at end of time step  
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(tot_watsoil_end)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: snow_beg        !! Total snow water equivalent at start of time step  
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(snow_beg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: snow_end        !! Total snow water equivalent at end of time step 
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(snow_end)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: delsoilmoist    !! Change in soil moisture during time step 
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(delsoilmoist)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: delintercept    !! Change in interception storage during time step 
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(delintercept)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: delswe          !! Change in snow water equivalent during time step 
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(delswe)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: dss             !! Depth of dry soil at the top, whether in the top or bottom 
                                                                     !! layer (m) 
!$OMP THREADPRIVATE(dss)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: hdry            !! Mean top dry soil height (m) version beton
!$OMP THREADPRIVATE(hdry)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: precisol        !! Throughfall @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(precisol)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: subsnowveg      !! Sublimation of snow on vegetation 
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(subsnowveg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: subsnownobio    !! Sublimation of snow on other surface types (ice, lakes,...)
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(subsnownobio)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: snowmelt        !! Snow melt @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(snowmelt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: icemelt         !! Ice melt @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(icemelt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: gdrainage       !! Drainage between the two soil layers 
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(gdrainage)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: vegtot          !! Total fraction of grid-cell covered by PFTs (bare soil + 
                                                                     !! vegetation) (0-1, unitless)
!$OMP THREADPRIVATE(vegtot)  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: resdist         !! Previous values of "veget" (0-1, unitless) ! Last map of 
                                                                     !! PFT fractions used to define the soil columns 
!$OMP THREADPRIVATE(resdist)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: mx_eau_var      !! Maximum water content of the soil 
                                                                     !! @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(mx_eau_var)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: ruu_ch          !! Volumetric soil water capacity @tex ($kg m^{-3}$) @endtex
!$OMP THREADPRIVATE(ruu_ch)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: runoff          !! Total runoff @tex ($kg m^{-2}$) @endtex
!$OMP THREADPRIVATE(runoff)
  REAL(r_std), PARAMETER                          :: dsg_min = 0.001 !! Reference depth to define subgrid variability of saturated
                                                                     !! top soil layer (m) 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: subsinksoil     !! Excess of sublimation as a sink for the soil
!$OMP THREADPRIVATE(subsinksoil)

!_ ================================================================================================================================      
                                                               
CONTAINS

!! ================================================================================================================================
!! SUBROUTINE 	: hydrolc_initialize
!!
!>\BRIEF         Allocate module variables, read from restart file or initialize with default values
!!
!! DESCRIPTION :
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrolc_initialize( kjit,      kjpindex,     index,          rest_id, &
                                 veget,     veget_max,    tot_bare_soil,           &
                                 rsol,      drysoil_frac, snow,                    &
                                 snow_age,  snow_nobio,   snow_nobio_age, humrel,  &
                                 vegstress, qsintveg,     shumdiag,       snowrho, &
                                 snowtemp,  snowgrain,    snowdz,         snowheat )


  !! 0. Variable and parameter declaration

    !! 0.1  Input variables
    INTEGER(i_std), INTENT(in)                            :: kjit             !! Current time step number (unitless)
    INTEGER(i_std), INTENT(in)                            :: kjpindex         !! Domain size (number of grid cells) (unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)      :: index            !! Indices of the land grid points on the map 
    INTEGER(i_std),INTENT (in)                            :: rest_id          !! Restart file identifier 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)     :: veget            !! Grid-cell fraction effectively covered by 
                                                                              !! vegetation for each PFT, except for soil 
                                                                              !! (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)     :: veget_max        !! PFT fractions within grid-cells: max value of 
                                                                              !! veget for vegetation PFTs and min value for 
                                                                              !! bare soil (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)          :: tot_bare_soil    !! Total evaporating bare soil fraction 

    !! 0.2 Output variables
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)        :: rsol             !! Resistance to bare soil evaporation 
                                                                              !! @tex ($s m^{-1}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)        :: drysoil_frac     !! Fraction of visibly dry soil, for bare soil 
                                                                              !! albedo calculation in condveg.f90 
                                                                              !! (0-1, unitless) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)        :: snow             !! Snow water equivalent @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)        :: snow_age         !! Snow age (days)
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (out) :: snow_nobio       !! Snow water equivalent on nobio areas  
                                                                              !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (out) :: snow_nobio_age   !! Snow age on ice, lakes, ...  (days)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)    :: humrel           !! Soil moisture stress factor on transpiration and
                                   
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)    :: vegstress        !! Vegetation moisture stress (only for vegetation 
                                                                              !! growth) (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)    :: qsintveg         !! Amount of water in the canopy interception 
                                                                              !! reservoir @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (out)   :: shumdiag         !! Mean relative soil moisture in the different
                                                                              !! levels used by thermosoil.f90 (0-1, unitless)
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out)  :: snowrho          !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out)  :: snowtemp         !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out)  :: snowgrain        !! Snow grainsize
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out)  :: snowdz           !! Snow layer thickness
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out)  :: snowheat         !! Snow heat content
 
    !! 0.4 Local variables

!_ ================================================================================================================================
    
  !! 1.  Initialisation
    CALL hydrolc_init (kjit, kjpindex, index, rest_id, &
         veget, tot_bare_soil, &
         humrel, vegstress, shumdiag, &
         snow, snow_age, snow_nobio, snow_nobio_age, qsintveg, &
         snowdz,snowgrain,snowrho,snowtemp,snowheat, &
         drysoil_frac, rsol)

    
    CALL hydrolc_var_init (kjpindex, veget, veget_max, tot_bare_soil, &
         rsol, drysoil_frac, mx_eau_var, ruu_ch, shumdiag)
    
    ! If we check the water balance, we first save the total amount of water
    IF (check_waterbal) CALL hydrolc_waterbal_init (kjpindex, qsintveg, snow, snow_nobio)
    
    IF (almaoutput) THEN
       CALL hydrolc_alma_init(kjpindex, index, qsintveg, snow, snow_nobio)
    ENDIF
    
  END SUBROUTINE hydrolc_initialize


!! ================================================================================================================================
!! SUBROUTINE            : hydrolc_main 
!!
!>\BRIEF                 Main routine for water budget calculation on land surfaces. 
!!
!! DESCRIPTION           : This routine drives all the calls pertaining 
!! to the land-surface water budget. It is called once during the initialisation of 
!! ORCHIDEE, and then once for each time step, to drive the water budget calculations, 
!! and the link to history files processing (histwrite). It is called one more time 
!! at the last time step, for the writing of the restart file. 
!!
!! The initialisation step calls hydrolc_init and hydrolc_var_init, plus hydrolc_waterbal 
!! if we choose to check the water balance. Writing is performed to the restart file, 
!! before the water budget calculations, at a timestep that is controlled by the flag
!! "ldrestart_write".
!!
!! The water budget calculations are separated into four subroutines related to the 
!! main three water reservoirs over land grid-cells, and called at each time step : \n
!!  - hydrolc_snow for snow process (including age of snow)
!!  - hydrolc_vegupd to manage the required redistribution of water between reservoirs 
!! when the vegetation fraction of PFTs has changed
!!  - hydrolc_canop for canopy process (interception)
!!  - hydrolc_soil for soil hydrological process (soil moisture and runoff), followed by 
!! hydrolc_hdiff if we choose to account for horizontal diffusion between the soil columns.
!! If we choose to check the water balance, this is done over each timestep dt_sechiba, 
!! by hydrolc_waterbal. 
!!
!! The link to "hist" files processing has two kinds of controls: 
!! - the standard output ncdf file corresponds to hist_id ; if hist2_id positive, a second  
!! ncdf file is output, with a different frequency
!! - almaoutput defines the nature/name of the written variables, whether following the 
!! ALMA norm or not
!! Note that histwrite does different actions depending on the current time step : 
!! writing on "hist" files at a selected frequency ; summing of the output variables 
!! in between in view of averaging.
!! The subroutine also handles some "CMIP5" output (mrro, mrros and prveg).
!!
!! We consider that any water on ice (nobio) is snow and we only peform a water balance to have consistency. 
!! The water balance is limited to + or - \f$10^6\f$ so that accumulation is not endless
!!
!! IMPORTANT NOTE : The water fluxes are used in their integrated form, over the time step 
!! dt_sechiba, with a unit of \f$kg.m^{-2}\f$.
!!
!! RECENT CHANGE(S) : None
!! 
!! MAIN OUTPUT VARIABLE(S) : snow, snow_age, snow_nobio, snow_nobio_age, tot_melt, 
!! returnflow, irrigation, humrel, vegstress, rsol, drysoil_frac, shumdiag, litterhumdiag 
!! + variables declared in the module
!! + variables sent for ouput by histwrite
!!
!! REFERENCE(S) : Same as for module hydrolc
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrolc_main (kjit, kjpindex, index, indexveg, &
     & temp_sol_new, floodout, run_off_tot, drainage, frac_nobio, totfrac_nobio, vevapwet, veget, veget_max,&
     & qsintmax, qsintveg, vevapnu, vevapsno, vevapflo, snow, snow_age, snow_nobio, snow_nobio_age, tot_melt, transpir, &
     & precip_rain, precip_snow, returnflow, reinfiltration, irrigation, vegstress_old, transpot, humrel, vegstress, rsol, drysoil_frac, &!added vegestress_old & transpot for crop irrigation, xuhui
     & evapot, evapot_corr, flood_frac, flood_res, shumdiag, litterhumdiag, soilcap, rest_id, hist_id, hist2_id, soil_deficit, & !add soil_deficit for crop irrigation, xuhui
     & temp_air, pb, u, v, pgflux, &
     & snowrho,snowtemp, snowgrain,snowdz,snowheat,snowliq,&
     & grndflux,gtemp, tot_bare_soil, &
     & soilflxresid, &
     & lambda_snow,cgrnd_snow,dgrnd_snow,temp_sol_add)

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables
 
    INTEGER(i_std), INTENT(in)                              :: kjit             !! Current time step number (unitless)
    INTEGER(i_std), INTENT(in)                              :: kjpindex         !! Domain size (number of grid cells) (unitless)
    INTEGER(i_std),INTENT (in)                              :: rest_id,hist_id  !! _Restart_ file and _history_ file identifier 
                                                                                !! (unitless)
    INTEGER(i_std),INTENT (in)                              :: hist2_id         !! Second _history_ file identifier (unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)        :: index            !! Indices of the land grid points on the map 
                                                                                !! (unitless)
    INTEGER(i_std),DIMENSION (kjpindex*nvm), INTENT (in)    :: indexveg         !! Indices of the PFT tiles / soil columns on 
                                                                                !! the 3D map (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: precip_rain      !! Rainfall @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: precip_snow      !! Snowfall  @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: returnflow       !! Routed water which comes back into the soil 
                                                                                !! @tex ($kg m^{-2}$) @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: reinfiltration   !! Routed water which comes back into the soil
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: irrigation       !! Irrigation water applied to soils 
                                                                                !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)       :: vegstress_old    !! vegstress of previous step 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)       :: transpot         !! potential transpiration
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: temp_sol_new     !! New soil temperature (K)
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in)    :: frac_nobio       !! Fraction of terrestrial ice, lakes, ... 
                                                                                !! (0-1, unitless) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: totfrac_nobio    !! Total fraction of terrestrial ice+lakes+...
                                                                                !! (0-1, unitless) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: soilcap          !! Soil heat capacity @tex ($J K^{-1}$) @endtex
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)       :: vevapwet         !! Interception loss over each PFT 
                                                                                !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)       :: veget            !! Grid-cell fraction effectively covered by 
                                                                                !! vegetation for each PFT, except for soil 
                                                                                !! (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)       :: veget_max        !! PFT fractions within grid-cells: max value of 
                                                                                !! veget for vegetation PFTs and min value for bare soil (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)       :: qsintmax         !! Maximum amount of water in the canopy 
                                                                                !! interception reservoir 
                                                                                !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)       :: transpir         !! Transpiration over each PFT 
                                                                                !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: evapot           !! [DISPENSABLE] Soil Potential Evaporation  
                                                                                !! @tex ($kg m^{-2}$) @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: evapot_corr      !! [DISPENSABLE] Soil Potential Evaporation 
                                                                                !! Correction
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: flood_frac       !! Flooded fraction 
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)            :: temp_air         !! Air temperature
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)            :: u,v              !! Horizontal wind speed
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)            :: pb               !! Surface pressure
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)            :: pgflux           !! Net energy into snowpack
    REAL(r_std), DIMENSION (kjpindex),INTENT(inout)         :: soilflxresid     !! Energy flux to the snowpack
    REAL(r_std),DIMENSION (kjpindex),INTENT(in)             :: gtemp            !! First soil layer temperature
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)            :: tot_bare_soil    !! Total evaporating bare soil fraction  
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)            :: lambda_snow      !! Coefficient of the linear extrapolation of surface temperature 
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT (in)     :: cgrnd_snow       !! Integration coefficient for snow numerical scheme
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT (in)     :: dgrnd_snow       !! Integration coefficient for snow numerical scheme


    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex), INTENT (out)          :: run_off_tot      !! Diagnosed surface runoff  
                                                                                !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)        :: flood_res        !! Flood reservoir estimate 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)          :: drainage         !! Diagnosed rainage at the bottom of soil
                                                                                !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)          :: rsol             !! Resistance to bare soil evaporation 
                                                                                !! @tex ($s m^{-1}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)          :: drysoil_frac     !! Fraction of visibly dry soil, for bare soil 
                                                                                !! albedo calculation in condveg.f90 
                                                                                !! (0-1, unitless) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)          :: litterhumdiag    !! Litter humidity factor (0-1, unitless), used
                                                                                !! in stomate
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)          :: tot_melt         !! Total melt @tex ($kg m^{-2}$) @endtex
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)      :: soil_deficit   !! soil defict for flood irrigation


    !! 0.3  Modified variables

    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)        :: vevapnu          !! Bare soil evaporation @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)        :: vevapsno         !! Snow evaporation  @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)        :: vevapflo         !! Floodplains evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)        :: snow             !! Snow water equivalent @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)        :: snow_age         !! Snow age (days)
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (inout) :: snow_nobio       !! Snow water equivalent on nobio areas  
                                                                                !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (inout) :: snow_nobio_age   !! Snow age on ice, lakes, ...  (days)
    
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)    :: humrel           !! Soil moisture stress factor on transpiration and
                                                                                !! bare soil evaporation (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)    :: vegstress        !! Vegetation moisture stress (only for vegetation 
                                                                                !! growth) (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)    :: qsintveg         !! Amount of water in the canopy interception 
                                                                                !! reservoir @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)        :: floodout         !! flux out of floodplains
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (inout)   :: shumdiag         !! Mean relative soil moisture in the different
                                                                                !! levels used by thermosoil.f90 (0-1, unitless)
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)  :: snowrho          !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)  :: snowtemp         !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)  :: snowgrain        !! Snow grainsize
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)  :: snowdz           !! Snow layer thickness
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)  :: snowheat         !! Snow heat content
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)  :: snowliq          !! Liquid water content (m)
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)        :: grndflux         !! Net flux into soil W/m2
    REAL(r_std), DIMENSION (kjpindex), INTENT (inout)       :: temp_sol_add     !! additional surface temperature due to the melt of first layer
                                                                                !! at the present time-step @tex ($K$) @endtex

    !! 0.4 Local variables
    
    REAL(r_std),DIMENSION (kjpindex)                        :: soilwet          !! Temporary diagnostic of relative humidity of 
                                                                                !! total soil (0-1, unitless)
    REAL(r_std)                                             :: tempfrac
    REAL(r_std),DIMENSION (kjpindex)                        :: snowdepth        !! Snow Depth (m)
    INTEGER(i_std)                                          :: ji,jv            !! Grid-cell and PFT indices (unitless)
    REAL(r_std), DIMENSION(kjpindex)                        :: histvar          !! Ancillary variable when computations are needed
                                                                                !! before writing to history files
    CHARACTER(LEN=80)                                       :: var_name         !! To store variables names for I/O
    REAL(r_std), DIMENSION(kjpindex,nvm)                    ::irrig_demand_ratio !! pft request ratio for irrigation water

!_ ================================================================================================================================

    !! 1.a snow processes
    IF (ok_explicitsnow) THEN 

       IF (printlev>=3) WRITE (numout,*) ' ok_explicitsnow : use three-snow layer '
      
       CALL  explicitsnow_main(kjpindex,      precip_rain,    precip_snow,  temp_air,                &
                                pb,           u,              v,            temp_sol_new, soilcap,   &
                                pgflux,       frac_nobio,     totfrac_nobio,                         &
                                gtemp,                                                               &
                                lambda_snow,  cgrnd_snow,     dgrnd_snow,                            & 
                                vevapsno,     snow_age,       snow_nobio_age, snow_nobio,   snowrho, &
                                snowgrain,    snowdz,         snowtemp,     snowheat,     snow,      &
                                temp_sol_add,                                                        &
                                snowliq,         subsnownobio,   grndflux,     snowmelt,     tot_melt,  &
                                soilflxresid, subsinksoil     )
    ELSE
       CALL hydrolc_snow(kjpindex, precip_rain, precip_snow, temp_sol_new, soilcap, &
            frac_nobio, totfrac_nobio, vevapnu, vevapsno, snow, snow_age, snow_nobio, snow_nobio_age, &
            tot_melt, snowdepth)
    END IF
    
    !! 1.b canopy processes
    CALL hydrolc_vegupd(kjpindex, veget, tot_bare_soil, ruu_ch, qsintveg, gqsb, bqsb, dsg, dss,dsp, resdist)
    
    CALL hydrolc_canop(kjpindex, precip_rain, vevapwet, veget, qsintmax, tot_bare_soil, qsintveg, precisol)
    !
    ! computes surface reservoir
    !
    CALL hydrolc_flood(kjpindex, vevapnu, vevapflo, flood_frac, flood_res, floodout)
   
    !! 1.c soil hydrology processes

    !!! calculating ratio of irrigation for each pft at each point
    irrig_demand_ratio(:,:) = zero
    irrig_fin(:,:) = zero
!    irrig_totfrac(:) = zero
    DO ji = 1,kjpindex
        DO jv = 2,nvm
            IF (veget_max(ji,jv) .GT. zero) THEN
                IF (irrig_drip) THEN
                    tempfrac = veget(ji,jv)/veget_max(ji,jv)
                    IF ( (.NOT. natural(jv)) .AND. (vegstress_old(ji,jv) .LT. irrig_threshold(jv)) .AND.  &
                        & (transpot(ji,jv)*tempfrac + evapot_corr(ji)*(1-tempfrac) .GT. precip_rain(ji)) ) THEN
                        irrig_demand_ratio(ji,jv) = MIN( irrig_dosmax, irrig_fulfill(jv) * &
                                                    & ( transpot(ji,jv)*tempfrac + evapot_corr(ji)*(1-tempfrac) & 
                                                    &  - precip_rain(ji) ) ) * veget_max(ji,jv)
                    ENDIF ! since irrigated fraction is the same across pfts on the same grid, no need to consider
                ELSE ! flooding
                    IF ( (.NOT. natural(jv)) .AND. (vegstress_old(ji,jv) .LT. irrig_threshold(jv)) ) THEN 
                        irrig_demand_ratio(ji,jv) = MIN( irrig_dosmax, MAX( zero, soil_deficit(ji,jv) ) ) * veget_max(ji,jv)
                    ENDIF
                ENDIF
            ENDIF
        ENDDO
        IF ( SUM(irrig_demand_ratio(ji,:)) .GT. zero ) THEN
            irrig_demand_ratio(ji,:) = irrig_demand_ratio(ji,:) / SUM(irrig_demand_ratio(ji,:))
        ENDIF
    ENDDO 
!    WRITE(numout,*) 'irrig_demand_ratio(1,:): ',irrig_demand_ratio(1,:)
!    WRITE(numout,*) 'irrig xwang: deficit(1,:): ',transpot(1,:) - precip_rain(1)
!    WRITE(numout,*) 'irrig xwang: veget(1,:): ', veget(1,:)
!    WRITE(numout,*) 'irrig xwang: veget_max(1,:): ',veget_max(1,:)
!    WRITE(numout,*) 'irrig xwang: irrigation(1): ',irrigation(1)
    !!! end ratio_irrig, Xuhui
    CALL hydrolc_soil(kjpindex, vevapnu, precisol, returnflow, reinfiltration, irrigation, irrig_demand_ratio, veget_max, tot_melt, mx_eau_var, &  ! added irrig_demand_ratio, veget_max, for crop irrigation
         & veget, tot_bare_soil, ruu_ch, transpir,&
         & gqsb, bqsb, dsg, dss, rsol, drysoil_frac, hdry, dsp, runoff, run_off_tot, drainage, humrel, &
         & vegstress, shumdiag, litterhumdiag, irrig_fin) ! added irrig_fin by xuhui
    
    DO ji = 1,kjpindex
        DO jv = 2,nvm
            IF (.NOT. natural(jv)) THEN
                soil_deficit(ji,jv) = MAX( zero, irrig_fulfill(jv) * mx_eau_var(ji) - bqsb(ji,jv) - gqsb(ji,jv) )
            ENDIF
        ENDDO
    ENDDO
    ! computes horizontal diffusion between the water reservoirs
    IF ( ok_hdiff ) THEN
      CALL hydrolc_hdiff(kjpindex, veget, tot_bare_soil, ruu_ch, gqsb, bqsb, dsg, dss, dsp)
    ENDIF
    
    ! If we check the water balance, we end with the comparison of total water change and fluxes
    IF (check_waterbal) THEN
       CALL hydrolc_waterbal(kjpindex, index, veget, totfrac_nobio, qsintveg, snow, snow_nobio,&
            & precip_rain, precip_snow, returnflow, reinfiltration, irrigation, tot_melt, vevapwet, transpir, vevapnu, vevapsno,&
            & vevapflo, floodout, run_off_tot, drainage )
    ENDIF
    
  !! 2. Output
    
    IF (almaoutput) THEN
       CALL hydrolc_alma(kjpindex, index, qsintveg, snow, snow_nobio, soilwet)
    ENDIF

    CALL xios_orchidee_send_field("runoff",run_off_tot/dt_sechiba)
    CALL xios_orchidee_send_field("drainage",drainage/dt_sechiba)
    CALL xios_orchidee_send_field("precip_rain",precip_rain/dt_sechiba)
    CALL xios_orchidee_send_field("precip_snow",precip_snow/dt_sechiba)
    CALL xios_orchidee_send_field("irrig_fin",irrig_fin*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("qsintveg",qsintveg)
    CALL xios_orchidee_send_field("qsintveg_tot",SUM(qsintveg(:,:),dim=2))
    CALL xios_orchidee_send_field("precisol",precisol/dt_sechiba)
    IF (do_floodplains) CALL xios_orchidee_send_field("floodout",floodout/dt_sechiba)
    CALL xios_orchidee_send_field("humrel",humrel)     
    CALL xios_orchidee_send_field("qsintmax",qsintmax)
    CALL xios_orchidee_send_field("irrig_rat",irrig_demand_ratio)
    
    histvar(:)=(precip_rain(:)-SUM(precisol(:,:),dim=2))
    CALL xios_orchidee_send_field("prveg",histvar/dt_sechiba)
    
    histvar(:)=zero
    DO jv = 1, nvm
       DO ji = 1, kjpindex
          IF ( vegtot(ji) .GT. zero ) THEN
!MM veget(:,1) BUG ??!!!!!!!!!!!
             IF (jv .EQ. 1) THEN
                histvar(ji)=histvar(ji)+tot_bare_soil(ji)/vegtot(ji)*MAX((0.1-dss(ji,jv))*wmax_veg(jv), zero)
             ELSE
                histvar(ji)=histvar(ji)+veget(ji,jv)/vegtot(ji)*MAX((0.1-dss(ji,jv))*wmax_veg(jv), zero)
             ENDIF
          ENDIF
       ENDDO
    ENDDO
    CALL xios_orchidee_send_field("humtot_top",histvar) ! mrsos in output name
    CALL xios_orchidee_send_field("humtot",mean_bqsb(:)+mean_gqsb(:)) ! Total soil moisture
    CALL xios_orchidee_send_field("bqsb",mean_bqsb)
    CALL xios_orchidee_send_field("gqsb",mean_gqsb)
    CALL xios_orchidee_send_field("dss",dss)

    IF (ok_explicitsnow) THEN
       CALL xios_orchidee_send_field("snowdz",snowdz)
    ELSE
       CALL xios_orchidee_send_field("snowdz",snowdepth)
    END IF
    CALL xios_orchidee_send_field("tot_melt",tot_melt/dt_sechiba)

    IF (almaoutput) THEN
       CALL xios_orchidee_send_field("soilwet",soilwet)
       CALL xios_orchidee_send_field("delsoilmoist",delsoilmoist)
       CALL xios_orchidee_send_field("delswe",delswe)
       CALL xios_orchidee_send_field("delintercept",delintercept)
    END IF


    IF ( .NOT. almaoutput ) THEN
       CALL histwrite_p(hist_id, 'dss', kjit, dss, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'bqsb', kjit, mean_bqsb, kjpindex, index)
       CALL histwrite_p(hist_id, 'bqsb_pft', kjit, bqsb, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'gqsb', kjit, mean_gqsb, kjpindex, index)
       CALL histwrite_p(hist_id, 'runoff', kjit, run_off_tot, kjpindex, index) ! this is surface runoff = 5% of total runoff
       CALL histwrite_p(hist_id, 'runoff_pft', kjit, runoff, kjpindex*nvm,indexveg)
       CALL histwrite_p(hist_id, 'drainage', kjit, drainage, kjpindex, index) ! this 95% of total runoff
       IF ( river_routing .AND. do_floodplains ) THEN
          CALL histwrite_p(hist_id, 'floodout', kjit, floodout, kjpindex, index)
       ENDIF
       CALL histwrite_p(hist_id, 'precisol', kjit, precisol, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'rain', kjit, precip_rain, kjpindex, index)
       CALL histwrite_p(hist_id, 'snowf', kjit, precip_snow, kjpindex, index)
       CALL histwrite_p(hist_id, 'qsintmax', kjit, qsintmax, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'qsintveg', kjit, qsintveg, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'humrel',   kjit, humrel,   kjpindex*nvm, indexveg) ! this the transpiration stress factor
       CALL histwrite_p(hist_id, 'vegstress',   kjit, vegstress,   kjpindex*nvm, indexveg) ! 
       CALL histwrite_p(hist_id, 'irrig_fin',   kjit, irrig_fin,   kjpindex*nvm, indexveg) ! irrigation applications
       CALL histwrite_p(hist_id, 'soil_deficit', kjit, soil_deficit,   kjpindex*nvm, indexveg) ! 
!       CALL histwrite_p(hist_id, 'irrig_ratio', kjit, irrig_demand_ratio, kjpindex*nvm, indexveg) !irrigation demande ratio

    !! The output for "CMIP5" is handled here, assuming that fluxes in kg m^{-2} s^{-1}
    !!  But the transformation below on mrro, mrros and prveg lead to fluxes that are 48 times too small !***

       histvar(:)=zero
       DO jv = 1, nvm
          DO ji = 1, kjpindex
             IF ( vegtot(ji) .GT. zero ) THEN
!MM veget(:,1) BUG ??!!!!!!!!!!!
                IF (jv .EQ. 1) THEN
                   histvar(ji)=histvar(ji)+tot_bare_soil(ji)/vegtot(ji)*MAX((0.1-dss(ji,jv))*wmax_veg(jv), zero)
                ELSE
                   histvar(ji)=histvar(ji)+veget(ji,jv)/vegtot(ji)*MAX((0.1-dss(ji,jv))*wmax_veg(jv), zero)
                ENDIF
             ENDIF
          ENDDO
       ENDDO
       ! this is soil moisture in the top 10 cms
       CALL histwrite_p(hist_id, 'mrsos', kjit, histvar, kjpindex, index) 

       ! this is total soil moisture
       histvar(:)=mean_bqsb(:)+mean_gqsb(:)
       CALL histwrite_p(hist_id, 'mrso', kjit, histvar, kjpindex, index) 

       CALL histwrite_p(hist_id, 'mrros', kjit, run_off_tot, kjpindex, index)

       histvar(:)=run_off_tot(:)+drainage(:)
       CALL histwrite_p(hist_id, 'mrro', kjit, histvar, kjpindex, index)

       histvar(:)=(precip_rain(:)-SUM(precisol(:,:),dim=2))
       CALL histwrite_p(hist_id, 'prveg', kjit, histvar, kjpindex, index)
       CALL histwrite_p(hist_id, 'snowmelt',kjit,snowmelt,kjpindex,index)

    ELSE 
       
       ! almaoutput=true
       CALL histwrite_p(hist_id, 'Snowf', kjit, precip_snow, kjpindex, index)
       CALL histwrite_p(hist_id, 'Rainf', kjit, precip_rain, kjpindex, index)
   
       ! surface runoff = 5% of total runoff
       CALL histwrite_p(hist_id, 'Qs', kjit, run_off_tot, kjpindex, index)

       ! drainage = 95% of total runoff
       CALL histwrite_p(hist_id, 'Qsb', kjit, drainage, kjpindex, index) 
       CALL histwrite_p(hist_id, 'Qsm', kjit, snowmelt, kjpindex, index)
       CALL histwrite_p(hist_id, 'DelSoilMoist', kjit, delsoilmoist, kjpindex, index)
       CALL histwrite_p(hist_id, 'DelSWE', kjit, delswe, kjpindex, index)
       CALL histwrite_p(hist_id, 'DelIntercept', kjit, delintercept, kjpindex, index)      
       CALL histwrite_p(hist_id, 'SoilMoist', kjit, tot_watsoil_end, kjpindex, index)
       CALL histwrite_p(hist_id, 'SoilWet', kjit, soilwet, kjpindex, index)
       
       ! this the transpiration stress factor
       CALL histwrite_p(hist_id, 'humrel',   kjit, humrel,   kjpindex*nvm, indexveg) 
       CALL histwrite_p(hist2_id, 'vegstress',   kjit, vegstress, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist2_id, 'irrig_fin',   kjit, irrig_fin, kjpindex*nvm, indexveg)
       
       CALL histwrite_p(hist_id, 'RootMoist', kjit, tot_watsoil_end, kjpindex, index)
       CALL histwrite_p(hist_id, 'SubSnow', kjit, vevapsno, kjpindex, index)
       CALL histwrite_p(hist_id, 'SnowDepth', kjit, snowdepth, kjpindex, index)
       
    ENDIF
    IF ( hist2_id > 0 ) THEN
       IF ( .NOT. almaoutput ) THEN
          CALL histwrite_p(hist2_id, 'dss', kjit, dss, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'bqsb', kjit, mean_bqsb, kjpindex, index)
          CALL histwrite_p(hist2_id, 'gqsb', kjit, mean_gqsb, kjpindex, index)
          CALL histwrite_p(hist2_id, 'runoff', kjit, run_off_tot, kjpindex, index)
          CALL histwrite_p(hist2_id, 'drainage', kjit, drainage, kjpindex, index)
          IF ( river_routing .AND. do_floodplains ) THEN
             CALL histwrite_p(hist2_id, 'floodout', kjit, floodout, kjpindex, index)
          ENDIF
          CALL histwrite_p(hist2_id, 'precisol', kjit, precisol, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'rain', kjit, precip_rain, kjpindex, index)
          CALL histwrite_p(hist2_id, 'snowf', kjit, precip_snow, kjpindex, index)
          CALL histwrite_p(hist2_id, 'qsintmax', kjit, qsintmax, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'qsintveg', kjit, qsintveg, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'humrel',   kjit, humrel,   kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'snowmelt',kjit,snowmelt,kjpindex,index)

          IF (check_waterbal) THEN
             CALL histwrite_p(hist2_id, 'TotWater', kjit, tot_water_end, kjpindex, index)
             CALL histwrite_p(hist2_id, 'TotWaterFlux', kjit, tot_flux, kjpindex, index)
          ENDIF

          histvar(:)=zero
          DO jv = 1, nvm
             DO ji = 1, kjpindex
                IF ( vegtot(ji) .GT. zero ) THEN
!MM veget(:,1) BUG ??!!!!!!!!!!!
                   IF (jv .EQ. 1) THEN
                      histvar(ji)=histvar(ji)+tot_bare_soil(ji)/vegtot(ji)*MAX((0.1-dss(ji,jv))*wmax_veg(jv), zero)
                   ELSE
                      histvar(ji)=histvar(ji)+veget(ji,jv)/vegtot(ji)*MAX((0.1-dss(ji,jv))*wmax_veg(jv), zero)
                   ENDIF
                ENDIF
             ENDDO
          ENDDO
          CALL histwrite_p(hist2_id, 'mrsos', kjit, histvar, kjpindex, index)

          histvar(:)=run_off_tot(:)+drainage(:)
          CALL histwrite_p(hist2_id, 'mrro', kjit, histvar, kjpindex, index)


          ! this is total soil moisture
          histvar(:)=mean_bqsb(:)+mean_gqsb(:)
          CALL histwrite_p(hist2_id, 'mrso', kjit, histvar, kjpindex, index) 
          CALL histwrite_p(hist2_id, 'mrros', kjit, run_off_tot, kjpindex, index)

       ELSE
          CALL histwrite_p(hist2_id, 'Snowf', kjit, precip_snow, kjpindex, index)
          CALL histwrite_p(hist2_id, 'Rainf', kjit, precip_rain, kjpindex, index)
          CALL histwrite_p(hist2_id, 'Qs', kjit, run_off_tot, kjpindex, index)
          CALL histwrite_p(hist2_id, 'Qsb', kjit, drainage, kjpindex, index)
          CALL histwrite_p(hist2_id, 'Qsm', kjit, snowmelt, kjpindex, index)
          CALL histwrite_p(hist2_id, 'DelSoilMoist', kjit, delsoilmoist, kjpindex, index)
          CALL histwrite_p(hist2_id, 'DelSWE', kjit, delswe, kjpindex, index)
          CALL histwrite_p(hist2_id, 'DelIntercept', kjit, delintercept, kjpindex, index)
          CALL histwrite_p(hist2_id, 'SoilMoist', kjit, tot_watsoil_end, kjpindex, index)
          CALL histwrite_p(hist2_id, 'SoilWet', kjit, soilwet, kjpindex, index)
          
          CALL histwrite_p(hist2_id, 'RootMoist', kjit, tot_watsoil_end, kjpindex, index)
          CALL histwrite_p(hist2_id, 'SubSnow', kjit, vevapsno, kjpindex, index)
          
          CALL histwrite_p(hist2_id, 'SnowDepth', kjit, snowdepth, kjpindex, index)
          
       ENDIF
    ENDIF

    IF (printlev>=3) WRITE (numout,*) ' hydrolc_main Done '

  END SUBROUTINE hydrolc_main
 

!! ================================================================================================================================
!! SUBROUTINE 	: hydrolc_finalize
!!
!>\BRIEF         
!!
!! DESCRIPTION : This subroutine writes the module variables and variables calculated in hydrolc to restart file
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrolc_finalize( kjit,      kjpindex,   rest_id,        snow,       &
                               snow_age,  snow_nobio, snow_nobio_age, humrel,     &
                               vegstress, qsintveg,   snowrho,        snowtemp,   &
                               snowdz,     snowheat,  snowgrain,                  &
                               drysoil_frac,          rsol,           shumdiag   )


    !! 0. Variable and parameter declaration
    !! 0.1  Input variables
    INTEGER(i_std), INTENT(in)                           :: kjit             !! Current time step number (unitless)
    INTEGER(i_std), INTENT(in)                           :: kjpindex         !! Domain size (number of grid cells) (unitless)
    INTEGER(i_std),INTENT (in)                           :: rest_id          !! Restart file identifier 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: snow             !! Snow water equivalent 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: snow_age         !! Snow age (days)
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in) :: snow_nobio       !! Snow water equivalent on nobio areas
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in) :: snow_nobio_age   !! Snow age on ice, lakes, ...  (days)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)    :: humrel           !! Soil moisture stress factor on transpiration and
                                                                             !! bare soil evaporation (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)    :: vegstress        !! Vegetation moisture stress (only for vegetation 
                                                                             !! growth) (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)    :: qsintveg         !! Amount of water in the canopy interception 
                                                                             !! reservoir @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowrho          !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowtemp         !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowdz           !! Snow layer thickness
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowheat         !! Snow heat content
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowgrain        !! Snow grain size
    REAL(r_std),DIMENSION (kjpindex),INTENT(in)          :: drysoil_frac
    REAL(r_std),DIMENSION (kjpindex),INTENT(in)          :: rsol
    REAL(r_std),DIMENSION (kjpindex,nbdl),INTENT(in)     :: shumdiag
!_ ================================================================================================================================
    
    IF (printlev>=3) WRITE (numout,*) ' we have to complete restart file with HYDROLOGIC variables '
    
    CALL restput_p(rest_id, 'humrel', nbp_glo,   nvm, 1, kjit,  humrel, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'vegstress', nbp_glo,   nvm, 1, kjit,  vegstress, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'snow', nbp_glo,   1, 1, kjit,  snow, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'snow_age', nbp_glo,   1, 1, kjit,  snow_age, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'snow_nobio', nbp_glo, nnobio, 1, kjit,  snow_nobio, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'snow_nobio_age', nbp_glo, nnobio, 1, kjit, snow_nobio_age, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'bqsb', nbp_glo,   nvm, 1, kjit,  bqsb, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'gqsb', nbp_glo,   nvm, 1, kjit,  gqsb, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'dsg', nbp_glo,  nvm, 1, kjit,  dsg, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'dsp', nbp_glo,   nvm, 1, kjit,  dsp, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'dss', nbp_glo,   nvm, 1, kjit,  dss, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'hdry', nbp_glo,   1, 1, kjit,  hdry, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'qsintveg', nbp_glo, nvm, 1, kjit,  qsintveg, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'resdist', nbp_glo, nvm, 1, kjit,  resdist, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'drysoil_frac', nbp_glo, 1, 1, kjit, drysoil_frac, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'rsol', nbp_glo, 1, 1, kjit, rsol, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'shumdiag', nbp_glo, nbdl, 1, kjit,  shumdiag, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'mean_gqsb', nbp_glo, 1, 1, kjit, mean_gqsb, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'mean_bqsb', nbp_glo, 1, 1, kjit, mean_bqsb, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'mx_eau_var', nbp_glo, 1, 1, kjit, mx_eau_var, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'vegtot', nbp_glo, 1, 1, kjit, vegtot, 'scatter', nbp_glo, index_g)


    ! Write variables for explictsnow module to restart file
    IF (ok_explicitsnow) THEN
       CALL explicitsnow_finalize ( kjit,     kjpindex, rest_id,    snowrho,   &
                                    snowtemp, snowdz,   snowheat,   snowgrain)

    END IF
    
  END SUBROUTINE hydrolc_finalize

 
!! ================================================================================================================================
!! SUBROUTINE   : hydrolc_init
!!
!>\BRIEF        This routine drives the initialisation of the water budget calculations. 
!!
!! DESCRIPTION : The following sequences are performed :
!!  - Setting ok_hdiff (default is false) for horizontal diffusion between soil columns
!!  - Dynamic allocation of arrays that are local to module hydrolc
!!  - Initializing prognostic variables from input restart file
!!  - Default attribution is performed in case the model is started without a restart file
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : humrel, vegstress,snow, snow_age, snow_nobio, snow_nobio_age, qsintveg
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_ ================================================================================================================================  
  
  SUBROUTINE hydrolc_init(kjit, kjpindex,   index,      rest_id,        &
                 veget,     tot_bare_soil,  humrel,     vegstress,      &
                 shumdiag,                                              &
                 snow,      snow_age,       snow_nobio, snow_nobio_age, & 
                 qsintveg,                                              &
                 snowdz,    snowgrain,      snowrho,    snowtemp,       &
                 snowheat,  drysoil_frac,   rsol)

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables
 
    INTEGER(i_std), INTENT (in)                          :: kjit                !! Current time step number (unitless)
    INTEGER(i_std), INTENT (in)                          :: kjpindex            !! Domain size (number of grid cells) (unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)     :: index               !! Indices of the land grid points on the map 
                                                                                !! (unitless)
    INTEGER(i_std), INTENT (in)                          :: rest_id             !! _Restart_ file identifier (unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)    :: veget               !! Grid-cell fraction effectively covered by 
                                                                                !! vegetation for each PFT, except for soil 
                                                                                !! (0-1, unitless) 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)        :: tot_bare_soil       !! Total evaporating bare soil fraction    

    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)   :: humrel              !! Soil moisture stress factor on transpiration
                                                                                !! and bare soil evaporation (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)   :: vegstress           !! Vegetation moisture stress (only for 
                                                                                !! vegetation growth) (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (out)  :: shumdiag            !! Mean relative soil moisture in the different
                                                                                !! levels used by thermosoil.f90 (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)       :: snow                !! Snow water equivalent @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)       :: snow_age            !! Snow age (days)
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (out):: snow_nobio          !! Snow water equivalent on nobio areas 
                                                                                !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (out):: snow_nobio_age      !! Snow age on ice, lakes, ...  (days)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)   :: qsintveg            !! Amount of water in the canopy interception 
                                                                                !! reservoir @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(out)   :: snowdz              !! Snow depth
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(out)   :: snowgrain           !! Snow grain size
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(out)   :: snowheat            !! Snow heat content
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(out)   :: snowtemp            !! Snow temperature
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(out)   :: snowrho             !! Snow density
    REAL(r_std),DIMENSION (kjpindex),INTENT(out)         :: drysoil_frac
    REAL(r_std),DIMENSION (kjpindex),INTENT(out)         :: rsol                !! Resistance to bare soil evaporation 

    !! 0.3 Modified variables

    !! 0.4 Local variables
    
    INTEGER(i_std)                                       :: ier                 !! To receive error flag during allocation
    INTEGER(i_std)                                       :: ji,jv,ik            !! Indices for grid-cells, PFTs, and grid-cells 
                                                                                !! (unitless)
    REAL(r_std), DIMENSION (kjpindex,nvm)                :: zdsp, tmpdss        !! Ancillary variables for initializing dsp 
                                                                                !! and dss (m)
    REAL(r_std), ALLOCATABLE, DIMENSION (:,:)            :: dsp_g               !! Ancillary variable for initializing dsp (m)
                                                                                !! dss (m)   
    REAL(r_std), DIMENSION(kjpindex)                     :: a_subgrd            !! Diagnosed subgrid fraction of saturated soil in
                                                                                !! the top layer, to calculate hdry (0-1, unitless)
    CHARACTER(LEN=80)                                    :: var_name            !! To store variables names for I/O
!_ ================================================================================================================================

  !! 1. Setting ok_hdiff for horizontal diffusion between soil columns

    !Config Key   = HYDROL_OK_HDIFF
    !Config Desc  = do horizontal diffusion?
    !Config If    = OK_SECHIBA and .NOT.(HYDROL_CWRR)  
    !Config Def   = n
    !Config Help  = If TRUE, then water can diffuse horizontally between
    !Config         the PFTs' water reservoirs.
    !Config Units = [FLAG]
    ok_hdiff = .FALSE.
    CALL getin_p('HYDROL_OK_HDIFF',ok_hdiff) 

  !! 2. Make dynamic allocation with the good dimensions

    ! one dimension array allocation with possible restart value
    ALLOCATE (bqsb(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in bqsb allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    bqsb(:,:) = zero

    ALLOCATE (gqsb(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in gqsb allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    gqsb(:,:) = zero

    ALLOCATE (dsg(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in dsg allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    dsg(:,:) = zero

    ALLOCATE (dsp(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in dsp allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    dsp(:,:) = zero

    ! one dimension array allocation 
    ALLOCATE (mean_bqsb(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in mean_bqsb allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    mean_bqsb(:) = zero

    ALLOCATE (mean_gqsb(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in mean_gqsb allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    mean_gqsb(:) = zero

    ALLOCATE (dss(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in dss allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    dss(:,:) = zero

    ALLOCATE (irrig_fin(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in irrig_fin allocation. We stop. We need kjpindex*nvm words = ',kjpindex*nvm
        STOP 'hydrolc_init'
    END IF
    irrig_fin(:,:) = zero

    ALLOCATE (hdry(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in hdry allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    hdry(:) = zero

    ALLOCATE (precisol(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in precisol allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    precisol(:,:) = zero

    ALLOCATE (gdrainage(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in precisol allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    gdrainage(:,:) = zero

    ALLOCATE (subsnowveg(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in subsnowveg allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    subsnowveg(:) = zero

    ALLOCATE (subsnownobio(kjpindex,nnobio),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in subsnownobio allocation. We stop. We need kjpindex*nnobio words = ', &
          kjpindex*nnobio
        STOP 'hydrolc_init'
    END IF
    subsnownobio(:,:) = zero

    ALLOCATE (snowmelt(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in snowmelt allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    snowmelt(:) = zero

    ALLOCATE (icemelt(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in icemelt allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    icemelt(:) = zero

    ALLOCATE (subsinksoil(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
       WRITE (numout,*) ' error in subsinksoil allocation. We stop. We need kjpindex words = ',kjpindex
       STOP 'hydrolc_init'
    END IF


    ALLOCATE (mx_eau_var(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in mx_eau_var allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    mx_eau_var(:) = zero

    ALLOCATE (ruu_ch(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in ruu_ch allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    ruu_ch(:) = zero

    ALLOCATE (vegtot(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in vegtot allocation. We stop. We need kjpindex words = ',kjpindex*nvm
        STOP 'hydrolc_init'
    END IF
    vegtot(:) = zero

    ALLOCATE (resdist(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in resdist allocation. We stop. We need kjpindex words = ',kjpindex*nvm
        STOP 'hydrolc_init'
    END IF
    resdist(:,:) = zero

    ALLOCATE (runoff(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in runoff allocation. We stop. We need kjpindex words = ',kjpindex*nvm
        STOP 'hydrolc_init'
    END IF
    runoff(:,:) = zero


    !  If we check the water balance we need two more variables
    IF ( check_waterbal ) THEN

       ALLOCATE (tot_water_beg(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in tot_water_beg allocation. We stop. We need kjpindex words = ',kjpindex
          STOP 'hydrolc_init'
       END IF

       ALLOCATE (tot_water_end(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in tot_water_end allocation. We stop. We need kjpindex words = ',kjpindex
          STOP 'hydrolc_init'
       END IF

       ALLOCATE (tot_flux(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in tot_flux allocation. We stop. We need kjpindex words = ',kjpindex
          STOP 'hydrol_init'
       END IF

    ENDIF
    
    !  If we use the almaoutputs we need four more variables
    IF ( almaoutput ) THEN

       ALLOCATE (tot_watveg_beg(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in tot_watveg_beg allocation. We stop. We need kjpindex words = ',kjpindex
          STOP 'hydrolc_init'
       END IF

       ALLOCATE (tot_watveg_end(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in tot_watveg_end allocation. We stop. We need kjpindex words = ',kjpindex
          STOP 'hydrolc_init'
       END IF

       ALLOCATE (tot_watsoil_beg(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in tot_watsoil_beg allocation. We stop. We need kjpindex words = ',kjpindex
          STOP 'hydrolc_init'
       END IF

       ALLOCATE (tot_watsoil_end(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in tot_watsoil_end allocation. We stop. We need kjpindex words = ',kjpindex
          STOP 'hydrolc_init'
       END IF

       ALLOCATE (delsoilmoist(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in delsoilmoist allocation. We stop. We need kjpindex words = ',kjpindex
          STOP 'hydrolc_init'
       END IF

       ALLOCATE (delintercept(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in delintercept. We stop. We need kjpindex words = ',kjpindex
          STOP 'hydrolc_init'
       END IF

       ALLOCATE (delswe(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in delswe. We stop. We need kjpindex words = ',kjpindex
          STOP 'hydrolc_init'
       ENDIF

       ALLOCATE (snow_beg(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in snow_beg allocation. We stop. We need kjpindex words =',kjpindex
          STOP 'hydrolc_init'
       END IF

       ALLOCATE (snow_end(kjpindex),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in snow_end allocation. We stop. We need kjpindex words =',kjpindex
          STOP 'hydrolc_init'
       END IF

    ENDIF

  !!  3. Initialization of hydrologic variables:

    !! 3.a Read data from restart input file (opened by sechiba_init)
    !! for HYDROLOGIC processes
        IF (printlev>=3) WRITE (numout,*) ' we have to read a restart file for HYDROLOGIC variables'

        var_name= 'snow'        
        CALL ioconf_setatt_p('UNITS', 'kg/m^2')
        CALL ioconf_setatt_p('LONG_NAME','Snow mass')
        CALL restget_p (rest_id, var_name, nbp_glo, 1  , 1, kjit, .TRUE., snow, "gather", nbp_glo, index_g)
        
        var_name= 'snow_age'
        CALL ioconf_setatt_p('UNITS', 'd')
        CALL ioconf_setatt_p('LONG_NAME','Snow age')
        CALL restget_p (rest_id, var_name, nbp_glo, 1  , 1, kjit, .TRUE., snow_age, "gather", nbp_glo, index_g)
        
        var_name= 'snow_nobio'
        CALL ioconf_setatt_p('UNITS', 'kg/m^2')
        CALL ioconf_setatt_p('LONG_NAME','Snow on other surface types')
        CALL restget_p (rest_id, var_name, nbp_glo, nnobio  , 1, kjit, .TRUE., snow_nobio, "gather", nbp_glo, index_g)
        
        var_name= 'snow_nobio_age'
        CALL ioconf_setatt_p('UNITS', 'd')
        CALL ioconf_setatt_p('LONG_NAME','Snow age on other surface types')
        CALL restget_p (rest_id, var_name, nbp_glo, nnobio  , 1, kjit, .TRUE., snow_nobio_age, "gather", nbp_glo, index_g)
        
        var_name= 'humrel'
        CALL ioconf_setatt_p('UNITS', '-')
        CALL ioconf_setatt_p('LONG_NAME','Soil moisture stress')
        CALL restget_p (rest_id, var_name, nbp_glo, nvm, 1, kjit, .TRUE., humrel, "gather", nbp_glo, index_g)
        
        var_name= 'vegstress'
        CALL ioconf_setatt_p('UNITS', '-')
        CALL ioconf_setatt_p('LONG_NAME','Vegetation growth moisture stress')
        CALL restget_p (rest_id, var_name, nbp_glo, nvm, 1, kjit, .TRUE., vegstress, "gather", nbp_glo, index_g)
        
        var_name= 'bqsb'
        CALL ioconf_setatt_p('UNITS', 'kg/m^2')
        CALL ioconf_setatt_p('LONG_NAME','Deep soil moisture')
        CALL restget_p (rest_id, var_name, nbp_glo, nvm , 1, kjit, .TRUE., bqsb, "gather", nbp_glo, index_g)
        
        var_name= 'gqsb'
        CALL ioconf_setatt_p('UNITS', 'kg/m^2')
        CALL ioconf_setatt_p('LONG_NAME','Surface soil moisture')
        CALL restget_p (rest_id, var_name, nbp_glo, nvm , 1, kjit, .TRUE., gqsb, "gather", nbp_glo, index_g)
        
        var_name= 'dsg'
        CALL ioconf_setatt_p('UNITS', 'm')
        CALL ioconf_setatt_p('LONG_NAME','Depth of upper reservoir')
        CALL restget_p (rest_id, var_name, nbp_glo, nvm  , 1, kjit, .TRUE., dsg, "gather", nbp_glo, index_g)
        
        var_name= 'dsp'
        CALL ioconf_setatt_p('UNITS', 'm')
        CALL ioconf_setatt_p('LONG_NAME','Depth to lower reservoir')
        CALL restget_p (rest_id, var_name, nbp_glo, nvm  , 1, kjit, .TRUE., dsp, "gather", nbp_glo, index_g)
        
        var_name= 'dss'
        CALL ioconf_setatt_p('UNITS', 'm')
        CALL ioconf_setatt_p('LONG_NAME','Hauteur au dessus du reservoir de surface')
        CALL restget_p (rest_id, var_name, nbp_glo, nvm  , 1, kjit, .TRUE., dss, "gather", nbp_glo, index_g)
                
        var_name= 'hdry'
        CALL ioconf_setatt_p('UNITS', 'm')
        CALL ioconf_setatt_p('LONG_NAME','Dry soil heigth in meters')
        CALL restget_p (rest_id, var_name, nbp_glo, 1 , 1, kjit, .TRUE., hdry, "gather", nbp_glo, index_g)
                
        var_name= 'qsintveg'
        CALL ioconf_setatt_p('UNITS', 'kg/m^2')
        CALL ioconf_setatt_p('LONG_NAME','Intercepted moisture')
        CALL restget_p (rest_id, var_name, nbp_glo, nvm, 1, kjit, .TRUE., qsintveg, "gather", nbp_glo, index_g)
        
        var_name= 'resdist'
        CALL ioconf_setatt_p('UNITS', '-')
        CALL ioconf_setatt_p('LONG_NAME','Distribution of reservoirs')
        CALL restget_p (rest_id, var_name, nbp_glo, nvm, 1, kjit, .TRUE., resdist, "gather", nbp_glo, index_g)
        
        ! Read drysoil_frac. It will be initalized later in hydrolc_var_init if the varaible is not find in restart file.
        CALL restget_p (rest_id, 'drysoil_frac', nbp_glo, 1  , 1, kjit, .TRUE., drysoil_frac, "gather", nbp_glo, index_g)

        ! Read rsol. It will be initalized later in hydrolc_var_init if the varaible is not find in restart file.
        CALL restget_p (rest_id, 'rsol', nbp_glo, 1  , 1, kjit, .TRUE., rsol, "gather", nbp_glo, index_g)

        ! shumdiag : initialization if not in restart file will be done in hydrolc_var_init
        CALL restget_p (rest_id, 'shumdiag', nbp_glo, nbdl  , 1, kjit, .TRUE., shumdiag, "gather", nbp_glo, index_g)

        CALL restget_p (rest_id, 'mean_bqsb', nbp_glo, 1  , 1, kjit, .TRUE., mean_bqsb, "gather", nbp_glo, index_g)
        CALL restget_p (rest_id, 'mean_gqsb', nbp_glo, 1  , 1, kjit, .TRUE., mean_gqsb, "gather", nbp_glo, index_g)

        var_name= 'mx_eau_var'
        CALL ioconf_setatt_p('UNITS', '')
        CALL ioconf_setatt_p('LONG_NAME','')
        CALL restget_p (rest_id, var_name, nbp_glo, 1 , 1, kjit, .TRUE., mx_eau_var, "gather", nbp_glo, index_g)

        var_name= 'vegtot'
        CALL ioconf_setatt_p('UNITS', '')
        CALL ioconf_setatt_p('LONG_NAME','')
        CALL restget_p (rest_id, var_name, nbp_glo, 1 , 1, kjit, .TRUE., vegtot, "gather", nbp_glo, index_g)


        !! 3.b Assign default values if non were found in the restart file
        !Config Key   = HYDROL_SNOW
        !Config Desc  = Initial snow mass if not found in restart
        !Config If    = OK_SECHIBA
        !Config Def   = 0.0
        !Config Help  = The initial value of snow mass if its value is not found
        !Config         in the restart file. This should only be used if the model is 
        !Config         started without a restart file.
        !Config Units = [kg/m^2]
        CALL setvar_p (snow, val_exp, 'HYDROL_SNOW', zero)
        
        !Config Key   = HYDROL_SNOWAGE
        !Config Desc  = Initial snow age if not found in restart
        !Config If    = OK_SECHIBA 
        !Config Def   = 0.0
        !Config Help  = The initial value of snow age if its value is not found
        !Config         in the restart file. This should only be used if the model is 
        !Config         started without a restart file.
        !Config Units = [days]
        CALL setvar_p (snow_age, val_exp, 'HYDROL_SNOWAGE', zero)
        
        !Config Key   = HYDROL_SNOW_NOBIO
        !Config Desc  = Initial snow amount on ice, lakes, etc. if not found in restart
        !Config If    = OK_SECHIBA 
        !Config Def   = 0.0
        !Config Help  = The initial value of snow if its value is not found
        !Config         in the restart file. This should only be used if the model is 
        !Config         started without a restart file.
        !Config Units = [m]
        CALL setvar_p (snow_nobio, val_exp, 'HYDROL_SNOW_NOBIO', zero)
        
        !Config Key   = HYDROL_SNOW_NOBIO_AGE
        !Config Desc  = Initial snow age on ice, lakes, etc. if not found in restart
        !Config If    = OK_SECHIBA 
        !Config Def   = 0.0
        !Config Help  = The initial value of snow age if its value is not found
        !Config         in the restart file. This should only be used if the model is 
        !Config         started without a restart file.
        !Config Units = [days]
        CALL setvar_p (snow_nobio_age, val_exp, 'HYDROL_SNOW_NOBIO_AGE', zero)
        
        !Config Key   = HYDROL_HUMR
        !Config Desc  = Initial soil moisture stress if not found in restart
        !Config If    = OK_SECHIBA 
        !Config Def   = 1.0
        !Config Help  = The initial value of soil moisture stress if its value is not found
        !Config         in the restart file. This should only be used if the model is 
        !Config         started without a restart file.
        !Config Units = [-]
        CALL setvar_p (humrel, val_exp,'HYDROL_HUMR', un)
        CALL setvar_p (vegstress, val_exp,'HYDROL_HUMR', un)
        
        !Config Key   = HYDROL_BQSB
        !Config Desc  = Initial restart deep soil moisture if not found in restart
        !Config If    = OK_SECHIBA 
        !Config Def   = 999999. 
        !Config Help  = The initial value of deep soil moisture if its value is not found
        !Config         in the restart file. This should only be used if the model is 
        !Config         started without a restart file. Default behaviour is a saturated soil.
        !Config Units = [kg/m^2]
        CALL setvar_p (bqsb, val_exp, 'HYDROL_BQSB', wmax_veg*zmaxh)
        
        !Config Key   = HYDROL_GQSB
        !Config Desc  = Initial upper soil moisture if not found in restart
        !Config If    = OK_SECHIBA 
        !Config Def   = 0.0
        !Config Help  = The initial value of upper soil moisture if its value is not found
        !Config         in the restart file. This should only be used if the model is 
        !Config         started without a restart file.
        !Config Units = [kg/m^2]
        CALL setvar_p (gqsb, val_exp, 'HYDROL_GQSB', zero)
        
        !Config Key   = HYDROL_DSG
        !Config Desc  = Initial upper reservoir depth if not found in restart
        !Config If    = OK_SECHIBA 
        !Config Def   = 0.0
        !Config Help  = The initial value of upper reservoir depth if its value is not found
        !Config         in the restart file. This should only be used if the model is 
        !Config         started without a restart file.
        !Config Units = [m]
        CALL setvar_p (dsg, val_exp, 'HYDROL_DSG', zero)
        
        !Config Key   = HYDROL_QSV
        !Config Desc  = Initial water on canopy if not found in restart
        !Config If    = OK_SECHIBA 
        !Config Def   = 0.0
        !Config Help  = The initial value of moisture on canopy if its value 
        !Config         is not found in the restart file. This should only be used if
        !Config         the model is started without a restart file. 
        !Config Units = [kg/m^2]
        CALL setvar_p (qsintveg, val_exp, 'HYDROL_QSV', zero)
        
        !! 3.c Specific calculations to define default values for dry soil depths : dsp, dss, and hdry
        ! set inital value for dsp if needed
        !Config Key   = HYDROL_DSP
        !Config Desc  = Initial dry soil above upper reservoir if not found in restart
        !Config If    = OK_SECHIBA 
        !Config Def   = 999999.
        !Config Help  = The initial value of dry soil above upper reservoir if its value 
        !Config         is not found in the restart file. This should only be used if
        !Config         the model is started without a restart file. The default behaviour
        !Config         is to compute it from the variables above. Should be OK most of 
        !Config         the time.
        !Config Units = [m]
        !
        ! Calculate temporary variable to use for initialiaion of dsp.
        ! This variable will only be used if no value is set in run.def
        DO jv = 1,nvm
           zdsp(:,jv) = zmaxh - bqsb(:,jv) / wmax_veg(jv)
        END DO

        CALL setvar_p(dsp, val_exp, 'HYDROL_DSP', zdsp) 

        ! set inital value for dss 
        DO jv = 1,nvm
           tmpdss(:,jv) = dsg(:,jv) - gqsb(:,jv) / wmax_veg(jv)
        END DO
        
        ! Initialize dss if it is not found in restart file
        IF ( ALL( dss(:,:) == val_exp ) ) THEN
           dss(:,:)=tmpdss(:,:)
        END IF
                
        ! set inital value for hdry if not found in restart file
        ! see hydrolc_soil, step 8.4
        IF ( ALL( hdry(:) == val_exp ) ) THEN
           a_subgrd(:) = zero
           DO ji=1,kjpindex
              IF ( gqsb(ji,1)+bqsb(ji,1) .GT. zero ) THEN
                 
                 IF (.NOT. (dsg(ji,1).EQ. zero .OR. gqsb(ji,1).EQ.zero)) THEN
                    
                    ! Ajouts Nathalie - Fred - le 28 Mars 2006
                    a_subgrd(ji)=MIN(MAX(dsg(ji,1)-dss(ji,1),zero)/dsg_min,un)
                    
                 ENDIF
              ENDIF
           ENDDO
           
           ! Correction Nathalie - le 28 Mars 2006 - re-ecriture drysoil_frac/hdry - Fred Hourdin
           ! revu 22 novembre 2007
           hdry(:) = a_subgrd(:)*dss(:,1) + (un-a_subgrd(:))*dsp(:,1)
        ENDIF
        
        ! There is no need to configure the initialisation of resdist. If not available it is the vegetation map
        IF ( MINVAL(resdist) .EQ.  MAXVAL(resdist) .AND. MINVAL(resdist) .EQ. val_exp) THEN
            resdist(:,1) = tot_bare_soil(:)
            resdist(:,2:nvm) = veget(:,2:nvm)
         ENDIF
        
         !! 3.d Compute vegtot (remember that it is only frac_nobio + SUM(veget(,:)) that is equal to 1)
         IF (ALL(vegtot(:)==val_exp)) THEN
            DO ji = 1, kjpindex
               vegtot(ji) = SUM(veget(ji,2:nvm)) + tot_bare_soil(ji)
            ENDDO
         END IF
        
    ! Initialize variables for explictsnow module by reading restart file
    IF (ok_explicitsnow) THEN
       CALL explicitsnow_initialize( kjit,     kjpindex, rest_id,    snowrho,   &
                                     snowtemp, snowdz,   snowheat,   snowgrain )
    END IF

    IF (printlev>=3) WRITE (numout,*) ' hydrolc_init done '
    
  END SUBROUTINE hydrolc_init
  

!! ================================================================================================================================
!! SUBROUTINE   : hydrolc_clear
!!
!>\BRIEF        Deallocates arrays that were allocated in hydrolc_init and hydrolc_var_init. 
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGES : None
!! 
!! MAIN OUTPUT VARIABLE(S) : None
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!!\n
!_ ================================================================================================================================
  
  SUBROUTINE hydrolc_clear()
 
    IF (ALLOCATED (bqsb)) DEALLOCATE (bqsb)
    IF (ALLOCATED  (gqsb)) DEALLOCATE (gqsb)
    IF (ALLOCATED  (dsg))  DEALLOCATE (dsg)
    IF (ALLOCATED  (dsp))  DEALLOCATE (dsp)
    IF (ALLOCATED  (mean_bqsb))  DEALLOCATE (mean_bqsb)
    IF (ALLOCATED  (mean_gqsb)) DEALLOCATE (mean_gqsb)
    IF (ALLOCATED  (irrig_fin))  DEALLOCATE (irrig_fin)
    IF (ALLOCATED  (dss)) DEALLOCATE (dss)
    IF (ALLOCATED  (hdry)) DEALLOCATE (hdry)
    IF (ALLOCATED  (precisol)) DEALLOCATE (precisol)
    IF (ALLOCATED  (gdrainage)) DEALLOCATE (gdrainage)
    IF (ALLOCATED  (subsnowveg)) DEALLOCATE (subsnowveg)
    IF (ALLOCATED  (subsnownobio)) DEALLOCATE (subsnownobio)
    IF (ALLOCATED  (snowmelt)) DEALLOCATE (snowmelt)
    IF (ALLOCATED  (icemelt)) DEALLOCATE (icemelt)
    IF (ALLOCATED  (subsinksoil)) DEALLOCATE (subsinksoil)
    IF (ALLOCATED  (mx_eau_var)) DEALLOCATE (mx_eau_var)
    IF (ALLOCATED  (ruu_ch)) DEALLOCATE (ruu_ch)
    IF (ALLOCATED  (vegtot)) DEALLOCATE (vegtot)
    IF (ALLOCATED  (resdist)) DEALLOCATE (resdist)
    IF (ALLOCATED  (tot_water_beg)) DEALLOCATE (tot_water_beg)
    IF (ALLOCATED  (tot_water_end)) DEALLOCATE (tot_water_end)
    IF (ALLOCATED  (tot_flux)) DEALLOCATE (tot_flux)
    IF (ALLOCATED  (tot_watveg_beg)) DEALLOCATE (tot_watveg_beg)
    IF (ALLOCATED  (tot_watveg_end)) DEALLOCATE (tot_watveg_end)
    IF (ALLOCATED  (tot_watsoil_beg)) DEALLOCATE (tot_watsoil_beg)
    IF (ALLOCATED  (tot_watsoil_end)) DEALLOCATE (tot_watsoil_end)
    IF (ALLOCATED  (delsoilmoist)) DEALLOCATE (delsoilmoist)
    IF (ALLOCATED  (delintercept)) DEALLOCATE (delintercept)
    IF (ALLOCATED  (snow_beg)) DEALLOCATE (snow_beg)
    IF (ALLOCATED  (snow_end)) DEALLOCATE (snow_end)
    IF (ALLOCATED  (delswe)) DEALLOCATE (delswe)
    IF (ALLOCATED  (runoff)) DEALLOCATE (runoff)
    
 END SUBROUTINE hydrolc_clear
 

!! ================================================================================================================================
!! SUBROUTINE   : hydrolc_var_init
!!
!>\BRIEF        This routine initializes diagnostic hydrologic variables.
!!
!! DESCRIPTION  : The following variables are assigned :
!!  - Soil characteristics : soil water capacities mx_eau_var and ruu_ch \f$(kg m^{-2})\f$ and \f$(kg m^{-3})\f$
!!  - Initial values for diagnostic variables : mean_bqsb, mean_gqsb, mean_dsg, shumdiag, drysoil_frac, rsol, litterhumdiag
!! 
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : rsol, drysoil_frac, mx_eau_var, ruu_ch, shumdiag, litterhumdiag
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================  

  SUBROUTINE hydrolc_var_init (kjpindex, veget, veget_max, tot_bare_soil, &
                               rsol, drysoil_frac, mx_eau_var, ruu_ch, shumdiag)

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables 

    INTEGER(i_std), INTENT(in)                         :: kjpindex      !! Domain size (number of grid cells) (unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: veget         !! Grid-cell fraction effectively covered by vegetation for
                                                                        !! each PFT, except for soil (0-1, unitless) 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: veget_max     !! PFT fractions within grid-cells: max value of veget for 
                                                                        !! vegetation PFTs and min value for bare soil (0-1, 
                                                                        !! unitless) 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)      :: tot_bare_soil !! Total evaporating bare soil fraction 
    
    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: rsol          !! Resistance to bare soil evaporation 
                                                                        !! @tex ($s m^{-1}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: drysoil_frac  !! Fraction of visible dry soil  (0-1, unitless) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: mx_eau_var    !! Maximum water content of the soil 
                                                                        !! @tex ($kg m^{-2}$) @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)     :: ruu_ch        !! Volumetric soil water capacity 
                                                                        !! @tex ($kg m^{-3}$) @endtex
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (inout):: shumdiag    !! Mean relative soil moisture, diagnosed for 
                                                                        !! thermosoil.f90 (0-1, unitless) 
    
    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                                     :: ji,jv, jd     !! Indices for grid-cells, PFTs and diagnostic levels in 
                                                                        !! the soil (unitless)
    REAL(r_std), DIMENSION(kjpindex)                   :: mean_dsg      !! Mean Depth of the top layer, averaged over the soil 
                                                                        !! columns (m)
    REAL(r_std)                                        :: gtr, btr      !! Ancillary variables to compute shumdiag (0-1, unitless) 
    REAL(r_std), DIMENSION(nbdl+1)                     :: tmp_dl        !! Temporary diagnostic levels in the soil (m)
!_ ================================================================================================================================

  !! 1. Vertical diagnostic levels to compute shumdiag (relative moisture for thermosoil)
    
    tmp_dl(1) = 0
    tmp_dl(2:nbdl+1) = diaglev(1:nbdl)
    
  !! 2. Calculation of mx_eau_var and ruu_ch (soil water capacities)
     
    IF (ALL(mx_eau_var(:)==val_exp)) THEN
       mx_eau_var(:) = zero
    
       DO ji = 1,kjpindex
          DO jv = 1,nvm
             !MM veget(:,1) BUG ??!!!!!!!!!!!
             IF (jv .EQ. 1) THEN
                mx_eau_var(ji) = mx_eau_var(ji) + tot_bare_soil(ji)*wmax_veg(jv)*zmaxh
             ELSE
                mx_eau_var(ji) = mx_eau_var(ji) + veget(ji,jv)*wmax_veg(jv)*zmaxh
             ENDIF
          END DO
          IF (vegtot(ji) .GT. zero) THEN
             mx_eau_var(ji) = mx_eau_var(ji)/vegtot(ji)
          ELSE
             ! lakes, ice, cities...
             mx_eau_var(ji) = mx_eau_nobio*zmaxh
          ENDIF
       END DO
    END IF
    !! Initialize ruu_ch
    ruu_ch(:) = mx_eau_var(:) / zmaxh

    !! 3.-4. Initial values of the mean soil layer depths and water contents and shumdiag
    
    IF (ALL(mean_bqsb(:)==val_exp) .OR. ALL(mean_gqsb(:)==val_exp) .OR. ALL(shumdiag(:,:)==val_exp)) THEN
       !! 3. Initial values of the mean soil layer depths and water contents
       ! could be done with SUM instruction but this kills vectorization
       mean_bqsb(:) = zero
       mean_gqsb(:) = zero
       mean_dsg(:) = zero
       DO jv = 1, nvm
          DO ji = 1, kjpindex
             mean_bqsb(ji) = mean_bqsb(ji) + resdist(ji,jv)*bqsb(ji,jv)
             mean_gqsb(ji) = mean_gqsb(ji) + resdist(ji,jv)*gqsb(ji,jv)
             mean_dsg(ji) = mean_dsg(ji) + resdist(ji,jv)*dsg(ji,jv)
          ENDDO
       ENDDO
       mean_dsg(:) = MAX( mean_dsg(:), mean_gqsb(:)/ruu_ch(:) )

       DO ji = 1, kjpindex
          IF (vegtot(ji) .GT. zero) THEN
             mean_bqsb(ji) = mean_bqsb(ji)/vegtot(ji)
             mean_gqsb(ji) = mean_gqsb(ji)/vegtot(ji)
             mean_dsg(ji) = mean_dsg(ji)/vegtot(ji)
          ENDIF
       ENDDO

    
       !! 4. Initial value of shumdiag (see hydrolc_soil, 8.2 for details)
       DO jd = 1,nbdl
          DO ji = 1,kjpindex
             IF ( tmp_dl(jd+1) .LT. mean_dsg(ji)) THEN
                shumdiag(ji,jd) = mean_gqsb(ji)/mx_eau_var(ji)
             ELSE
                IF ( tmp_dl(jd) .LT. mean_dsg(ji)) THEN
                   gtr = (mean_dsg(ji)-tmp_dl(jd))/(tmp_dl(jd+1)-tmp_dl(jd))
                   btr = 1 - gtr
                   shumdiag(ji,jd) = gtr*mean_gqsb(ji)/mx_eau_var(ji) + &
                        & btr*mean_bqsb(ji)/mx_eau_var(ji)
                ELSE
                   shumdiag(ji,jd) = mean_bqsb(ji)/mx_eau_var(ji)
                ENDIF
             ENDIF
             shumdiag(ji,jd) = MAX(MIN(shumdiag(ji,jd), un), zero)
          ENDDO
       ENDDO
    END IF

    !! 5. Initialize drysoil_frac if it was not found in the restart file
    IF (ALL(drysoil_frac(:) == val_exp)) THEN
       drysoil_frac(:) = MIN(MAX(dss(:,1),zero)*10._r_std, un)
    END IF

  !! 6. Initial value of the resistance to bare soil evaporation (see hydrolc_soil, 8.4 for details)
    
    IF (ALL(rsol(:)==val_exp)) THEN
       rsol(:) = -un
       DO ji = 1, kjpindex
          IF (tot_bare_soil(ji) .GE. min_sechiba) THEN
          
             ! Correction Nathalie - le 28 mars 2006 - sur conseils Fred Hourdin
             ! on modifie le rsol pour que la resistance croisse subitement si on s'approche
             ! du fond. En gros, rsol=hdry*rsol_cste pour hdry < 1m70
             !rsol(ji) = dss(ji,1) * rsol_cste
             !rsol(ji) =  ( drysoil_frac(ji) + un/(10.*(zmaxh - drysoil_frac(ji))+1.e-10)**2 ) * rsol_cste
             rsol(ji) =  ( hdry(ji) + un/(10.*(zmaxh - hdry(ji))+1.e-10)**2 ) * rsol_cste
          ENDIF
       ENDDO
    END IF
    
    IF (printlev>=3) WRITE (numout,*) ' hydrolc_var_init done '

  END SUBROUTINE hydrolc_var_init


!! ================================================================================================================================
!! SUBROUTINE   : hydrolc_snow
!!
!>\BRIEF        This routine computes accumulation, sublimation, snowmelt and snow ageing 
!! over vegetated and nobio (eg land-ice) areas. Snowdepth is then updated.
!! 
!! DESCRIPTION  : In this routine the snow-related processes accumulation, sublimation, 
!! melting and ageing are computed separately on vegetated and nobio areas. This subroutine 
!! is the first one among the ones that compute the physical processes. The water budgets 
!! of the interception reservoir and the soil are performed afterwards.\n
!!
!! - Values of the main parameters used in this routine are :\n
!! tp_00=273.15K : freezing point\n
!! snowcri=1.5\f$kg m^{-2}\f$ : critical snowmass for sublimation\n
!! sneige=snowcri/1000._r_std : critical snowmass for snow melt\n
!! maxmass_snow=3000 \f$kg m^{-2}\f$ (~ 10m snow) : critical snowmass for snow stock\n
!! snow_trans=0.3 (\f$kg m^{-2}\f$) : critical constant for snow ageing\n
!! max_snow_age=50 (days) : maximum snow age\n
!! sn_dens=330 (\f$kg m^{-3}\f$) : snow density\n
!! one_day=86400 (s) : one day, expressed in seconds...
!!
!! - Accumulation\n
!! On the vegetated areas, the snow mass increases due to area-weighted snow
!! precipitations precip_snow; on nobio areas, rain additionnaly converts into snow.\n
!!
!! - Sublimation\n
!! Sublimation on vegetated and nobio areas is calculated as the area-weighed fraction
!! of vevapsno (from enerbil). In case snow on vegetated areas is limited (less than snowcri) 
!! and a significant nobio area exists, the nobio area accomodates the whole sublimation 
!! vevapsno. The nobio area also accomodates the possible excess sublimation from vegetated 
!! areas, which otherwise goes into bare soil evaporation. In this case vevapsno is updated.\n
!!
!! - Melting\n
!! Over vegetated and nobio areas, snowmelt occurs if the calculated soil temperature 
!! temp_soil_new(ji) exceeds the freezing point tp_00. The energy required to warm up the soil
!! surface above tp_00 is converted into melting, according to the following equation :
!! \latexonly
!! \input{hydrolcsnow1.tex}
!! \endlatexonly
!! \n
!! with soilcap the soil heat capacity @tex ($J K^{-1}$) @endtex and chalfu0 the latent heat of
!! fusion.\n
!! A special case occurs in areas with snowmass exceeding maxmass_snow, where
!! all the snow in excess of maxmass_snow adds to the total melting tot_melt.
!! This excess melting is considered as additionnal snowmelt for vegetated areas
!! and as ice_melt for nobio areas.\n
!!
!! - Ageing\n
!! Over vegetated areas, during the time step dt_radia (usually 1800 s), the snow age is 
!! incremented by a d_age calculated as follow:
!! \latexonly
!! \input{hydrolcsnow2.tex}
!! \endlatexonly
!! \n
!! Over nobio areas and at subfreezing temperatures, snow ageing is limited by very 
!! slow snow metamorphism, hence d_age is reduced as follow:
!! \latexonly
!! \input{hydrolcsnow3.tex}
!! \endlatexonly
!! \n
!! Scientific reference for this parametrization choice is obviously missing !
!! At the end of the routine the snowheight is diagnosed using the fixed
!! snow density sn_dens.
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLES:
!! for vegetated and nobio areas respectively:\n
!! snow(kjpindex) and snow_nobio(kjpindex)\n
!! snow_age(kjpindex) and snow_nobio_age(kjpindex)\n
!! for the whole grid-cell:\n
!! vevapnu(kjpindex)
!! vevapsno(kjpindex)
!! tot_melt(kjpindex)
!! snowdepth(kjpindex)
!!   
!! REFERENCES : None
!! 
!! FLOWCHART  : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrolc_snow (kjpindex, precip_rain, precip_snow , temp_sol_new, soilcap,&
       & frac_nobio, totfrac_nobio, vevapnu, vevapsno, snow, snow_age, snow_nobio, snow_nobio_age, &
       & tot_melt, snowdepth)

  !! 0. Variable and parameter declaration

    !! 0.1  Input variabless
 
    INTEGER(i_std), INTENT(in)                               :: kjpindex        !! Domain size  (unitless)
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: precip_rain     !! Rainfall @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: precip_snow     !! Snow precipitation @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: temp_sol_new    !! New soil temperature (K)
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: soilcap         !! Soil heat capacity @tex ($J K^{-1}$) @endtex
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(in)     :: frac_nobio      !! Fraction of continental ice, lakes, ... 
                                                                                !! (unitless)
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: totfrac_nobio   !! Total fraction of continental ice+lakes+ ... 
                                                                                !! (unitless)

    !! 0.2 Output variables

    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: tot_melt        !! Total melt @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: snowdepth       !! Snow depth (m)
    
    !! 0.3  Modified variables

    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: vevapnu         !! Bare soil evaporation @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: vevapsno        !! Snow evaporation @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: snow            !! Snow mass @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: snow_age        !! Snow age (days)
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(inout)  :: snow_nobio      !! Snomass on nobio areas 
                                                                                !! @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(inout)  :: snow_nobio_age  !! Snow age on ice, lakes, ...(days)
    
    !! 0.4 Local variables
    
    INTEGER(i_std)                                           :: ji, jv          !! indices (unitless)
    REAL(r_std), DIMENSION (kjpindex)                        :: d_age           !! Snow age change (days)
    REAL(r_std), DIMENSION (kjpindex)                        :: xx              !! temporary
    REAL(r_std)                                              :: snowmelt_tmp    !! temporary @tex ($kg m^{-2}$) @endtex
    REAL(r_std)                                              :: snow_d1k        !! The amount of snow that corresponds to a 1K cooling
    LOGICAL, DIMENSION (kjpindex)                            :: warnings 
    LOGICAL                                                  :: any_warning
!_ ================================================================================================================================
    
  !! 1. initialisation
    
    DO jv = 1, nnobio
      DO ji=1,kjpindex
        subsnownobio(ji,jv) = zero
      ENDDO
    ENDDO
    DO ji=1,kjpindex
      subsnowveg(ji) = zero
      snowmelt(ji) = zero
      icemelt(ji) = zero
      tot_melt(ji) = zero
    ENDDO
  
  !! 2. On vegetation
        
    ! 2.1. It is snowing 
    DO ji=1,kjpindex
      snow(ji) = snow(ji) + (un - totfrac_nobio(ji))*precip_snow(ji)
    ENDDO

    DO ji=1,kjpindex
      
      ! 2.2. Sublimation - separate between vegetated and no-veget fractions
      !      Care has to be taken as we might have sublimation from the
      !      the frac_nobio while there is no snow on the rest of the grid.
      IF ( snow(ji) > snowcri ) THEN
         subsnownobio(ji,iice) = frac_nobio(ji,iice)*vevapsno(ji)
         subsnowveg(ji) = vevapsno(ji) - subsnownobio(ji,iice)
      ELSE

         ! Correction Nathalie - Juillet 2006.
         ! On doit d'abord tester s'il existe un frac_nobio!
         ! Pour le moment je ne regarde que le iice
         IF ( frac_nobio(ji,iice) .GT. min_sechiba) THEN
            subsnownobio(ji,iice) = vevapsno(ji)
            subsnowveg(ji) = zero
         ELSE 
            subsnownobio(ji,iice) = zero
            subsnowveg(ji) = vevapsno(ji)
         ENDIF
      ENDIF
    ENDDO
    
    warnings(:) = .FALSE.
    any_warning = .FALSE.
    DO ji=1,kjpindex
      
      ! 2.2.1 Check that sublimation on the vegetated fraction is possible.
      IF (subsnowveg(ji) .GT. snow(ji)) THEN 

         ! What could not be sublimated goes into bare soil evaporation
         ! Nathalie - Juillet 2006 - il faut avant tout tester s'il existe du
         ! frac_nobio sur ce pixel pour eviter de puiser dans le sol!         
         IF ( frac_nobio(ji,iice) .GT. min_sechiba) THEN
            subsnownobio(ji,iice) = subsnownobio(ji,iice) + (subsnowveg(ji) - snow(ji))
         ELSE  
            vevapnu(ji) = vevapnu(ji) + (subsnowveg(ji) - snow(ji))
            warnings(ji) = .TRUE.
            any_warning = .TRUE.
         ENDIF

         ! Sublimation is thus limited to what is available
         subsnowveg(ji) = snow(ji)
         snow(ji) = zero
         vevapsno(ji) = subsnowveg(ji) + subsnownobio(ji,iice)
      ELSE 
         snow(ji) = snow(ji) - subsnowveg(ji)
      ENDIF
    ENDDO
    IF ( any_warning ) THEN
       WRITE(numout,*)' ATTENTION on prend de l eau au sol nu sur au moins un point car evapsno est trop fort!'
!!$       DO ji=1,kjpindex
!!$          IF ( warnings(ji) ) THEN
!!$             WRITE(numout,*)' ATTENTION on prend de l eau au sol nu car evapsno est trop fort!'
!!$             WRITE(numout,*)' ',ji,'   vevapnu (en mm/jour) = ',vevapnu(ji)*one_day/dt_sechiba
!!$          ENDIF
!!$       ENDDO
    ENDIF
    
    warnings(:) = .FALSE.
    any_warning = .FALSE.
    DO ji=1,kjpindex
      
      ! 2.3. snow melt only if temperature positive
      IF (temp_sol_new(ji).GT.tp_00) THEN 
         
         IF (snow(ji).GT.sneige) THEN 
            
            snowmelt(ji) = (un - frac_nobio(ji,iice))*(temp_sol_new(ji) - tp_00) * soilcap(ji) / chalfu0 
            
            ! 2.3.1 enough snow for melting or not
            IF (snowmelt(ji).LT.snow(ji)) THEN 
               snow(ji) = snow(ji) - snowmelt(ji)
            ELSE 
               snowmelt(ji) = snow(ji)
               snow(ji) = zero
            END IF
            
         ELSEIF (snow(ji).GE.zero) THEN 
            
            ! 2.3.2 not enough snow
            snowmelt(ji) = snow(ji)
            snow(ji) = zero
         ELSE 
            
            ! 2.3.3 negative snow - now snow melt
            snow(ji) = zero
            snowmelt(ji) = zero
            warnings(ji) = .TRUE.
            any_warning = .TRUE.
            
         END IF

      ENDIF
    ENDDO
    IF ( any_warning ) THEN
       DO ji=1,kjpindex
          IF ( warnings(ji) ) THEN
             WRITE(numout,*) 'hydrolc_snow: WARNING! snow was negative and was reset to zero for point ',ji,'. '
          ENDIF
       ENDDO
    ENDIF
    

      
    !! 2.4 Snow melts above a threshold
    ! Ice melt only if there is more than a given mass : maxmass_snow,
    ! But the snow cannot melt more in one time step to what corresponds to
    ! a 1K cooling. This will lead to a progressive melting of snow above
    ! maxmass_snow but it is needed as a too strong cooling can destabilise the model.
    DO ji=1,kjpindex
       IF ( snow(ji) .GT. maxmass_snow ) THEN
          snow_d1k = un * soilcap(ji) / chalfu0
          snowmelt(ji) = snowmelt(ji) + MIN((snow(ji) - maxmass_snow),snow_d1k)
          snow(ji) = snow(ji) - snowmelt(ji)
          IF ( printlev >= 3 ) WRITE (numout,*) "Snow was above maxmass_snow (", maxmass_snow,") and we melted ", snowmelt(ji)
       ENDIF
    END DO
    
   !! 3. On Land ice
    
    DO ji=1,kjpindex
      
      !! 3.1. It is snowing
      snow_nobio(ji,iice) = snow_nobio(ji,iice) + frac_nobio(ji,iice)*precip_snow(ji) + &
           & frac_nobio(ji,iice)*precip_rain(ji)
      
      !! 3.2. Sublimation - was calculated before it can give us negative snow_nobio but that is OK
      !      Once it goes below a certain values (-maxmass_snow for instance) we should kill
      !      the frac_nobio(ji,iice) !
      snow_nobio(ji,iice) = snow_nobio(ji,iice) - subsnownobio(ji,iice)
      
      !! 3.3. snow melt only for continental ice fraction
      snowmelt_tmp = zero
      IF (temp_sol_new(ji) .GT. tp_00) THEN 
         
         ! 3.3.1 If there is snow on the ice-fraction it can melt 
         snowmelt_tmp = frac_nobio(ji,iice)*(temp_sol_new(ji) - tp_00) * soilcap(ji) / chalfu0
         
         IF ( snowmelt_tmp .GT. snow_nobio(ji,iice) ) THEN
             snowmelt_tmp = MAX( zero, snow_nobio(ji,iice))
         ENDIF
         snowmelt(ji) = snowmelt(ji) + snowmelt_tmp
         snow_nobio(ji,iice) = snow_nobio(ji,iice) - snowmelt_tmp
         
      ENDIF
      
      !! 3.4 Snow melts over a threshold
      !   Ice melt only if there is more than a given mass : maxmass_snow, 
      !   But the snow cannot melt more in one time step to what corresponds to
      !   a 1K cooling. This will lead to a progressive melting of snow above
      !   maxmass_snow but it is needed as a too strong cooling can destabilise the model.
      !
      IF ( snow_nobio(ji,iice) .GT. maxmass_snow ) THEN
         snow_d1k = un * soilcap(ji) / chalfu0
         icemelt(ji) = MIN((snow_nobio(ji,iice) - maxmass_snow),snow_d1k)
         snow_nobio(ji,iice) = snow_nobio(ji,iice) - icemelt(ji)
         
         IF ( printlev >= 3 ) WRITE (numout,*) "Snow was above maxmass_snow ON ICE (", maxmass_snow,") and we melted ", icemelt(ji)
      ENDIF
      
    END DO

    
  !! 4. On other surface types - not done yet
  
    IF ( nnobio .GT. 1 ) THEN
      WRITE(numout,*) 'WE HAVE',nnobio-1,' SURFACE TYPES I DO NOT KNOW'
      CALL ipslerr_p (3,'hydrolc_snow', '', &
 &         'CANNOT TREAT SNOW ON THESE SURFACE TYPES', '')
    ENDIF

    
  !! 5. computes total melt (snow and ice)
 

    DO ji = 1, kjpindex
      tot_melt(ji) = icemelt(ji) + snowmelt(ji)
    ENDDO
 
  !! 6. computes snow age on veg and ice (for albedo)
    
    DO ji = 1, kjpindex
      
      !! 6.1 Snow age on vegetation
      IF (snow(ji) .LE. zero) THEN
        snow_age(ji) = zero
      ELSE
        snow_age(ji) =(snow_age(ji) + (un - snow_age(ji)/max_snow_age) * dt_sechiba/one_day) &
          & * EXP(-precip_snow(ji) / snow_trans)
      ENDIF
      
      !! 6.2 Snow age on ice
      ! age of snow on ice: a little bit different because in cold regions, we really
      ! cannot negect the effect of cold temperatures on snow metamorphism any more.
      IF (snow_nobio(ji,iice) .LE. zero) THEN 
        snow_nobio_age(ji,iice) = zero
      ELSE
      
        d_age(ji) = ( snow_nobio_age(ji,iice) + &
                    &  (un - snow_nobio_age(ji,iice)/max_snow_age) * dt_sechiba/one_day ) * &
                    &  EXP(-precip_snow(ji) / snow_trans) - snow_nobio_age(ji,iice)
        IF (d_age(ji) .GT. zero ) THEN
          xx(ji) = MAX( tp_00 - temp_sol_new(ji), zero )
          xx(ji) = ( xx(ji) / 7._r_std ) ** 4._r_std
          d_age(ji) = d_age(ji) / (un+xx(ji))
        ENDIF
        snow_nobio_age(ji,iice) = MAX( snow_nobio_age(ji,iice) + d_age(ji), zero )
      
      ENDIF

    ENDDO

  !! 7. Diagnose the depth of the snow layer

    DO ji = 1, kjpindex
      snowdepth(ji) = snow(ji) /sn_dens
    ENDDO

    IF (printlev>=3) WRITE (numout,*) ' hydrolc_snow done '

  END SUBROUTINE hydrolc_snow



!! ================================================================================================================================
!! SUBROUTINE      : hydrolc_canop 
!!
!>\BRIEF           This routine computes the water budget of the canopy interception reservoir. 
!!
!! DESCRIPTION     : The first sequence is to withdraw interception loss from the interception 
!! reservoir, with independent values for each PFT. The reservoir loading happens afterwards. 
!! Rain falls uniformly over the PFTs. It uniformly loads the canopy interception 
!! reservoir of each PFT, up to the interception capacity, but a fraction of rainfall always falls to the ground.
!! Throughfall is thus comprised of the fraction that always falls through, plus the remining fraction of rainfall that 
!! exceeds the interception capacity, the interception loss having been removed. 
!! \latexonly
!! \input{interception.tex}
!! \endlatexonly
!! 
!! IMPORTANT NOTE : Bare soil treatment is complex in hydrolc :
!!  - veget_max(:,1) is the fraction of bare soil from the "annual" vegetation map 
!!  - veget(:,1) is not the fraction of vegetation over bare soil but the total fraction of bare soil in the grid-cell 
!! (including the bare soil fractions over the vegetation PFTs). 
!! *** A diagram should be added in the spatial entry of the "umbrella"
!!  - For interception to be zero on bare soil, we thus need to impose qsintmax(:,1)=0
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : precisol (throughfall), qsintveg (amount of water in the canopy interception reservoir)
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================  

  SUBROUTINE hydrolc_canop (kjpindex, precip_rain, vevapwet, veget, qsintmax, tot_bare_soil, qsintveg, precisol)

  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                           :: kjpindex           !! Domain size (number of grid cells) (unitless)
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)        :: precip_rain        !! Rainfall @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)    :: vevapwet           !! Interception loss over each PFT 
                                                                               !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)    :: veget              !! Grid-cell fraction effectively covered by 
                                                                               !! vegetation for each PFT, except for soil 
                                                                               !! (0-1, unitless) 
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)    :: qsintmax           !! Maximum amount of water in the canopy 
                                                                               !! interception reservoir @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)        :: tot_bare_soil      !! Total evaporating bare soil fraction 
   
    !! 0.2 Output variables

    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(out)   :: precisol           !! Throughfall @tex ($kg m^{-2}$) @endtex

    !! 0.3  Modified variables

    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout) :: qsintveg           !! Amount of water in the canopy interception 
                                                                               !! reservoir @tex ($kg m^{-2}$) @endtex
    
    !! 0.4 Local variabless
  
    INTEGER(i_std)                                       :: ji, jv             !! Grid-cell and PFT indices (unitless)
    REAL(r_std), DIMENSION (kjpindex,nvm)                :: zqsintvegnew       !! Temporary variable for intercepted water 
                                                                               !! amount  @tex ($kg m^{-2}$) @endtex 

!_ ================================================================================================================================

    ! calcul de qsintmax a prevoir a chaque pas de temps
    ! dans ini_sechiba
    ! boucle sur les points continentaux
    ! calcul de qsintveg au pas de temps suivant
    ! par ajout du flux interception loss
    ! calcule par enerbil en fonction
    ! des calculs faits dans diffuco
    ! calcul de ce qui tombe sur le sol
    ! avec accumulation dans precisol
    ! essayer d'harmoniser le traitement du sol nu
    ! avec celui des differents types de vegetation
    ! fait si on impose qsintmax ( ,1) = 0.0
    !
    ! loop for continental subdomain
    !

    !
    ! 1. evaporation off the continents
    !
    ! 1.1 The interception loss is take off the canopy. 

    DO jv=1,nvm
      qsintveg(:,jv) = qsintveg(:,jv) - vevapwet(:,jv)
    END DO

    ! 1.2 It is raining : precip_rain is shared for each vegetation
    ! type
    !     sum (veget (1,nvm)) must be egal to 1-totfrac_nobio.
    !     iniveget computes veget each day
    !
    DO jv=1,nvm
      ! Correction Nathalie - Juin 2006 - une partie de la pluie arrivera toujours sur le sol
      ! sorte de throughfall supplementaire
      !qsintveg(:,jv) = qsintveg(:,jv) + veget(:,jv) * precip_rain(:)
!MM veget(:,1) BUG ??!!!!!!!!!!!
       IF (jv .EQ. 1) THEN
          qsintveg(:,jv) = qsintveg(:,jv) + tot_bare_soil(:) * ((1-throughfall_by_pft(jv))*precip_rain(:))
       ELSE
          qsintveg(:,jv) = qsintveg(:,jv) + veget(:,jv) * ((1-throughfall_by_pft(jv))*precip_rain(:))
       ENDIF
    END DO

    !
    ! 1.3 Limits the effect and sum what receives soil
    !
    precisol(:,:) = zero
    DO jv=1,nvm
      DO ji = 1, kjpindex
        zqsintvegnew(ji,jv) = MIN (qsintveg(ji,jv),qsintmax(ji,jv)) 
        ! correction throughfall Nathalie - Juin 2006
        !precisol(ji,jv) = qsintveg(ji,jv ) - zqsintvegnew (ji,jv)
!MM veget(:,1) BUG ??!!!!!!!!!!!
        IF (jv .EQ. 1) THEN
           precisol(ji,jv) = (tot_bare_soil(ji)*throughfall_by_pft(jv)*precip_rain(ji)) + qsintveg(ji,jv ) - zqsintvegnew (ji,jv)
        ELSE
           precisol(ji,jv) = (veget(ji,jv)*throughfall_by_pft(jv)*precip_rain(ji)) + qsintveg(ji,jv ) - zqsintvegnew (ji,jv)
        ENDIF
      ENDDO
    ENDDO
    !
    ! 1.4 swap qsintveg to the new value
    !

    DO jv=1,nvm
      qsintveg(:,jv) = zqsintvegnew (:,jv)
    END DO

    IF (printlev>=3) WRITE (numout,*) ' hydrolc_canop done '

  END SUBROUTINE hydrolc_canop



!! ================================================================================================================================
!! SUBROUTINE   : hydrolc_vegupd 
!!
!>\BRIEF        This subroutines works at adapting the distribution of water
!! in the soil and interception reservoirs between the different soil columns when the vegetation 
!! fraction of the PFTs (veget) has changed in slowproc.  
!!
!! DESCRIPTION  : Different vegetation changes are allowed in ORCHIDEE:\n 
!!  - veget_max can be updated annually in slowproc (dynamic vegetation or prescribed vegetation change)\n
!!  - veget can be updated daily in slowproc (because of changes in veget_max or depending on LAI)
!!
!! This subroutine aims at adapting the distribution of water among the different liquid water reservoirs 
!! (interception reservoir and soil moisture) after these changes of veget :
!! - the variable veget holds the "new" vegetation map
!! - the variable resdist holds the previous vegetation map 
!! *** I see no flag or time step dependance to control the call to vegupd : is it called at each time step ?
!!
!! The principle is that PFTs where "veget" has shrunk keep the same water contents in kg.m^{-2},
!! whereas the water contents are increased in PFTs where "veget" has grown.
!! *** I still have questions about redistribution to interception reservoirs which have shrunk
!! *** and about thresholding to the reservoirs' capacities (is it ensured that the capacities are not exceeded ?)
!! You may note that this occurs after evaporation and so on have been computed. It is not a 
!! problem as a new vegetation fraction will start with humrel=0 and thus will have no evaporation. 
!! If this is not the case it should have been caught above.
!! 
!! IMPORTANT NOTE : the definition of veget is not simple :
!!  - for the non-bare soil PFTs (iv > 2), veget is the fraction effectively covered by 
!! vegetation : veget \f$\le\f$ veget_max\n
!!  - for the bare soil PFT (iv=1), veget holds the fraction of bare soil, from all 
!! PFTs : veget(:,1) \f$\ge\f$ veget_max(:,1)\n
!!
!! RECENT CHANGE(S) : None
!! 
!! MAIN OUTPUT VARIABLE(S) : qsintveg, gqsb, bqsb, dsg, dss, dsp, resdist
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART   : None
!! \n
!_ ================================================================================================================================  
  
  SUBROUTINE hydrolc_vegupd(kjpindex, veget, tot_bare_soil, ruu_ch, qsintveg, gqsb, bqsb, dsg, dss, dsp, resdist)
    
  !! 0. Variable and parameter declaration

    !! 0.1  Input variables
 
    INTEGER(i_std), INTENT(in)                               :: kjpindex   !! Domain size (number of grid cells) (unitless)
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(in)       :: veget      !! Grid-cell fraction effectively covered by vegetation 
                                                                           !! for each PFT, except for soil (0-1, unitless)
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: tot_bare_soil !! Total evaporating bare soil fraction    
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: ruu_ch     !! Volumetric soil water capacity 
                                                                           !! @tex ($kg m^{-3}$) @endtex

    !! 0.2 Output variables

    !! 0.3  Modified variables

    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)  :: qsintveg      !! Water on vegetation due to interception
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout)     :: gqsb       !! Water content in the top layer 
                                                                           !! @tex ($kg m^{-2}$) @endtex    
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout)     :: bqsb       !! Water content in the bottom layer 
                                                                           !! @tex ($kg m^{-2}$) @endtex     
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout)     :: dsg        !! Depth of the top layer (m)   
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout)     :: dss        !! Depth of dry soil at the top, whether in the top or
                                                                           !! bottom layer (m)   
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout)     :: dsp        !! Depth of dry soil in the bottom layer plus depth of
                                                                           !! top layer (m) 
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(inout)    :: resdist    !! Previous values of "veget" (0-1, unitless)
    
    !! 0.4 Local variables
    
    INTEGER(i_std)                                :: ji,jv                 !!  Grid-cell and PFT indices (unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm)           :: qsintveg2            !! Ancillary qsintveg @tex ($kg m^{-2}$) @endtex  
    REAL(r_std), DIMENSION (kjpindex,nvm)          :: bdq, gdq             !! Changes in surface and bottom soil layer water 
                                                                           !! content @tex ($kg m^{-2}; positive$) @endtex
    REAL(r_std), DIMENSION (kjpindex,nvm)          :: qsdq                 !! Changes in interception reservoir water content  
                                                                           !! @tex ($kg m^{-2}; positive$) @endtex
    REAL(r_std), DIMENSION (kjpindex,nvm)          :: vmr                  !! Variation of "veget" since previous values 
                                                                           !! (-1,1; unitless)
    REAL(r_std), DIMENSION(kjpindex)               :: gtr                  !! Total water mass to redistribute between the top 
                                                                           !! layers of a grid-cell @tex ($kg m^{-2}; positive) 
                                                                           !! @ endtex
    REAL(r_std), DIMENSION(kjpindex)               :: btr                  !! Total water mass to redistribute between the bottom
                                                                           !! layers of a grid-cell @tex ($kg m^{-2}; positive) 
                                                                           !! @ endtex
    REAL(r_std), DIMENSION(kjpindex)               :: vtr                  !! Total fraction  over which "veget" has decreased 
                                                                           !! (dimensionless; positive)
    REAL(r_std), DIMENSION(kjpindex)               :: qstr                 !! Total water mass to redistribute between the 
                                                                           !! interception reservoirs of a grid-cell 
                                                                           !! @tex ($kg m^{-2}; positive$) @endtex
    REAL(r_std), DIMENSION(kjpindex)               :: fra                  !! Weight for redistribution (dimensionless; negative)
    REAL(r_std), DIMENSION(kjpindex) :: vegchtot                           !! Total change of "veget" in the grid-point 
                                                                           !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), PARAMETER                         :: EPS1 = EPSILON(un)   !! Very small (scalar) *** why not use min_sechiba ?
!_ ================================================================================================================================ 
    
  !! 1. vmr is the change in veget in the different PFTs

    ! vmr is the change in veget in the different PFTs
    ! (dimensionless; negative if the new "veget" is smaller, i.e. if the PFT has shrunk)
    ! By construction, the algebraic sum of vmr in a grid-cell is zero
    DO jv = 1, nvm
      DO ji = 1, kjpindex
!MM veget(:,1) BUG ??!!!!!!!!!!!
         IF (jv .EQ. 1) THEN 
            IF ( ABS(tot_bare_soil(ji)-resdist(ji,jv)) .GT. EPS1 ) THEN
               vmr(ji,jv) = tot_bare_soil(ji)-resdist(ji,jv)
            ELSE
               vmr(ji,jv) = zero
            ENDIF
         ELSE
            IF ( ABS(veget(ji,jv)-resdist(ji,jv)) .GT. EPS1 ) THEN
               vmr(ji,jv) = veget(ji,jv)-resdist(ji,jv)
            ELSE
               vmr(ji,jv) = zero
            ENDIF
         ENDIF
  !! 2. qsintveg2 is the intercepted water in mm 

        ! qsintveg2 is the intercepted water in mmif the total volume 
        ! was redistributed over the previous "veget" fractions 
        ! This the distribution of intercepted water that needs to be 
        ! changed if "veget" changes 

         IF (resdist(ji,jv) .GT. zero) THEN
            qsintveg2(ji,jv) = qsintveg(ji,jv)/resdist(ji,jv)
         ELSE
            qsintveg2(ji,jv) = zero
         ENDIF
      ENDDO
    ENDDO


 !! 3. vegchtot is the total change of "veget" in the grid-points

    ! vegchtot is the total change of "veget" in the grid-points, 
    ! integrated over the PFTs (vegchtot in kg m^{-2})
    ! It is the sum of the absolute values of changes, it may thus 
    ! be larger than 1 : it serves as a flag of change in each grid-point
    vegchtot(:) = zero
    DO jv = 1, nvm
      DO ji = 1, kjpindex
        vegchtot(ji) = vegchtot(ji) + ABS( vmr(ji,jv) )
      ENDDO
    ENDDO
    

  !! 4. In the grid-points with "veget" change, we define changes in water content
    
    DO jv = 1, nvm
      DO ji = 1, kjpindex
        IF ( vegchtot(ji) .GT. zero ) THEN

           ! change in surface soil layer water content @tex ($kg m^{-2}$) @endtex
          gdq(ji,jv) = ABS(vmr(ji,jv)) * gqsb(ji,jv)

          ! change in bottom soil layer water content @tex ($kg m^{-2}$) @endtex
          bdq(ji,jv) = ABS(vmr(ji,jv)) * bqsb(ji,jv)

          ! change in interception reservoir water content  @tex ($kg m^{-2}$) @endtex
          qsdq(ji,jv) = ABS(vmr(ji,jv)) * qsintveg2(ji,jv)
        ENDIF
      ENDDO
    ENDDO

  !! 5. Total water mass to redistribute in a grid-point = sum of water changes from PFTs where "veget" decreases

    !  Calculate the total water mass that we need to redistribute by grid-point, for each water reservoir
    !  We sum up the water changes from PFTs where "veget" decreases : this is the water that needs to be redistributed
    !  vtr is the total fraction  over which "veget" has decreased (> 0)
    !  By construction, it is balanced by the total fraction where "veget" has increased

    gtr(:) = zero
    btr(:) = zero
    qstr(:) = zero
    vtr(:) = zero
    !
    !
    DO jv = 1, nvm
      DO ji = 1, kjpindex
        IF ( ( vegchtot(ji) .GT. zero ) .AND. ( vmr(ji,jv) .LT. zero ) ) THEN
          gtr(ji) = gtr(ji) + gdq(ji,jv)
          btr(ji) = btr(ji) + bdq(ji,jv)
          qstr(ji) = qstr(ji) + qsdq(ji,jv) 

          ! vtr is necessarily le 0 since we enter here only if vmr <0
          vtr(ji) = vtr(ji) - vmr(ji,jv)
        ENDIF
      ENDDO
    ENDDO
   

!! 6. Put the water to redistribute from the PFTs that "shrank" into the PFTs that "growed"

    !     In doing so, water contents are kept constant in PFTs that shrank, and they increase in PFTs that growed
    !     fra is the weight for that redistribution, scaled to vtr which is the total amount to redistribute 
    !  *** Feasability of the redistribution : it doesn't seem to be checked ???
    !  *** In the soil, if the water capacities are constant between PFTs, it's OK ;this is the default for wmax_veg in 
    !  *** constantes_veg.f90
    !  *** But qsintmax is different between PFTsand also evolves in time : how is this managed ???
    DO jv = 1, nvm
      DO ji = 1, kjpindex

        ! "veget" changed
        IF ( vegchtot(ji) .GT. zero .AND. ABS(vtr(ji)) .GT. EPS1) THEN

            ! negative when vmr positive, thus in the condition below 
            fra(ji) = vmr(ji,jv) / vtr(ji)

            ! the PFT growed, thus its water contents must be updated
             IF ( vmr(ji,jv) .GT. zero)  THEN
!MM veget(:,1) BUG ??!!!!!!!!!!!
              IF (jv .EQ. 1) THEN
                 IF (tot_bare_soil(ji) .GT. zero) THEN
                    gqsb(ji,jv) = (resdist(ji,jv)*gqsb(ji,jv) + fra(ji)*gtr(ji))/tot_bare_soil(ji)
                    bqsb(ji,jv) = (resdist(ji,jv)*bqsb(ji,jv) + fra(ji)*btr(ji))/tot_bare_soil(ji)
                 ENDIF
              ELSE
                 IF (veget(ji,jv) .GT. zero) THEN
                    gqsb(ji,jv) = (resdist(ji,jv)*gqsb(ji,jv) + fra(ji)*gtr(ji))/veget(ji,jv)
                    bqsb(ji,jv) = (resdist(ji,jv)*bqsb(ji,jv) + fra(ji)*btr(ji))/veget(ji,jv)
                 ENDIF
              ENDIF
              qsintveg(ji,jv) = qsintveg(ji,jv) + fra(ji)* qstr(ji)
             ELSE

              ! vmr negative *** I don't understand why we remove water from the interception reservoir in PFTs which shrank
              qsintveg(ji,jv) = qsintveg(ji,jv) - qsdq(ji,jv)
             ENDIF

             ! Then we update the soil layers depths.
             ! But we do not  change dss, so that the redistribution does directly affect transpiration
             ! constantes : min_sechiba = 1.E-8_r_std
             IF (gqsb(ji,jv) .LT. min_sechiba) THEN
                dsg(ji,jv) = zero
             ELSE
                dsg(ji,jv) = (dss(ji,jv) * ruu_ch(ji) + gqsb(ji,jv)) &
                             / ruu_ch(ji)
             ENDIF
             dsp(ji,jv) = zmaxh - bqsb(ji,jv) / ruu_ch(ji)
        ENDIF
      ENDDO
    ENDDO


    !! 7. We update resdist for the next "veget" change
    
!MM veget(:,1) BUG ??!!!!!!!!!!!
    resdist(:,1) = tot_bare_soil(:)
    DO jv = 2, nvm
       resdist(:,jv) = veget(:,jv)
    ENDDO


  !! 8. Where vegetation fraction is zero, set water to that of bare soil.

    !   This does not create any additional water.
    DO jv = 2, nvm
      DO ji = 1, kjpindex
        IF ( veget(ji,jv) .LT. EPS1 ) THEN
          gqsb(ji,jv) = gqsb(ji,1)
          bqsb(ji,jv) = bqsb(ji,1)
          dsg(ji,jv) = dsg(ji,1)
          dss(ji,jv) = dss(ji,1)
          dsp(ji,jv) = dsp(ji,1)
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE hydrolc_vegupd

!! ================================================================================================================================
!! SUBROUTINE            : hydrolc_flood
!!
!>\BRIEF                   this routine computes the evolution of the surface reservoir (floodplain)
!!
!! DESCRIPTION           : 
!!
!! RECENT CHANGE(S) : None
!! 
!! MAIN OUTPUT VARIABLE(S) : floodout
!!
!! REFERENCE(S) : Same as for module hydrolc
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrolc_flood (kjpindex, vevapnu, vevapflo, flood_frac, flood_res, floodout)
    ! input scalar 
    INTEGER(i_std), INTENT(in)                               :: kjpindex 
    ! input fields
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: flood_frac       !! Fraction of floodplains in grid box
    ! modified fields
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: floodout         !! Flux to take out from floodplains
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: flood_res        !! Floodplains reservoir estimate
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: vevapnu          !! Bare soil evaporation
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: vevapflo         !! Floodplains evaporation
    ! local declaration
    INTEGER(i_std)                                          :: ji, jst, jv       !! indices
    REAL(r_std)                                              :: k_m              !! conductivity in the soil
    REAL(r_std)                                              :: temp             !! 

!_ ================================================================================================================================

    !- 
    !- 1. Take out vevapflo from the reservoir and transfer the remaining to vevapnu 
    !-
    DO ji = 1,kjpindex
       temp = MIN(flood_res(ji), vevapflo(ji))
       flood_res(ji) = flood_res(ji) - temp
       vevapnu(ji) = vevapnu(ji) + vevapflo(ji) - temp
       vevapflo(ji) = temp
    ENDDO

    !- 
    !- 2. Compute the total flux from floodplain floodout (transfered to routing) 
    !-
    DO ji = 1,kjpindex
       floodout(ji) = vevapflo(ji) - flood_frac(ji) * SUM(precisol(ji,:))
    ENDDO

    !-
    !- 3. Discriminate between precip over land and over floodplain
    !-
    DO jv=1, nvm
       DO ji = 1,kjpindex
          precisol(ji,jv) = precisol(ji,jv) * (1 - flood_frac(ji))
       ENDDO
    ENDDO 

    IF (printlev>=3) WRITE (numout,*) ' hydrolc_flood done'

  END SUBROUTINE hydrolc_flood

!! ================================================================================================================================
!! SUBROUTINE      : hydrolc_soil
!!
!>\BRIEF           This routines computes the soil water budget and the related soil water 
!! fluxes and soil moisture diagnostics using the two-layer Choisnel scheme.
!!
!! DESCRIPTION     : 
!!
!! 1. Main processes: The Choisnel scheme relies on two soil layers. 
!! As show in the figure below, the upper one has a variable depth 
!! and can disappear after dry spells. It is created by infiltration (of throughfall or snow melt), 
!! in which case the layer is saturated. If this top layer is already present, infiltration can either 
!! fill it or make it deeper (Ducoudré et al., 1993). 
!! \latexonly
!! \includegraphics[scale = 0.5]{choisnelvariables.pdf}
!! \endlatexonly
!!
!! In this framework, most water fluxes updating soil moisture act from top:\n
!!  - throughfall, melted snow and ice, and irrigation increase soil moisture (of the top layer if present);
!!  - transpiration and bare soil evaporation reduce soil moisture (of the top layer if present);
!!  - return flow from rivers increases bottom soil moisture. \n
!!
!! Soil moisture stress on bare soil evaporation and transpiration (for diffuco), vegetation growth and 
!! litter processes (for stomate), proceed respectively via a soil resistance (rsol), and three soil 
!! moisture stress factor (humrel, vegstress, and litterhumdiag), which are all controlled by the 
!! height of dry soil at the top of soil: \n
!! - Soil moisture stress factor on transpiration and bare soil evaporation: humrel = \f$U_s\f$ \n
!! \latexonly
!! \input{humrel.tex}
!! \endlatexonly \n
!! - Resistance to bare soil evaporation: rsol = \f$r_\mathrm{soil}\f$ \n
!! \latexonly
!! \input{rsol.tex}
!! \endlatexonly
!!
!! Runoff is only produced when the entire soil column is saturated, as in the bucket 
!! scheme of Manabe (1969). Drainage at the bottom of soil and surface runoff are only diagnosed,  
!! mostly for use in the routing module, using a constant 95% - 5% redistribution (Ngo-Duc et al., 2005).
!! Internal drainage is allowed from the top to the bottom layer, following Ducharne et al. (1998).
!! \latexonly
!! \input{gdrainage.tex}
!! \endlatexonly
!!
!! Irrigation (de Rosnay et al., 2003) and return flow are optional inflows, calculated by the 
!! routing scheme (Ngo-Duc et al., 2005; Guimberteau, 2010).
!! 
!! 2. Subgrid variability:
!! The subgrid variability of soil is described by introducing as many soil columns as PFTs 
!! (de Rosnay & Polcher, 1998). The water budget is performed independently in each PFT/soil 
!! column, but the bottom soil moisture is merged between all PFTs at the end of the calculations.
!! Therefore, vegetation withdraws from a shared bottom moisture (grid-cell weighted average).
!! There can also be horizontal diffusion between the top layers, if the flag ok_hdiff is true (the default value 
!! is false). This is performed in the subroutine hydrolc_hdiff, call after the present subroutine.
!!
!! The areas of each soil column are defined by veget (the vegetation fraction of the PFTs),
!! and not by veget_max (the maximum area of the PFT defined annually in slowproc). 
!! As veget can change at a daily time step, a redistribution of water between the soil 
!! columns is required : this is done in hydrolc_vegupd.
!! 
!! 3. Water budget issues :
!! Different issues can arise when solving the above processes :
!!  - negative total soil moisture (tracked using flag warning, but no action taken, cf. 4)
!!  - dsg > dsp after merging the bottom layers, solved iteratively (6.2)
!!  - we may also have a pathological behavior of rsol if hdry > dpu_cste (8.4)
!!
!! 4. Diagnostics for other subroutines: humrel for diffuco (7.1), vegstress for stomate (8.1),
!! shumdiag for stomate (8.2), drysoil_frac for condveg (8.3), rsol for diffuco (8.4), 
!! litterhumdiag for stomate (8.5).
!!
!! MAIN OUTPUT VARIABLE(S) : rsol, drysoil_frac, hdry,  
!! run_off_tot (surface runoff), drainage, humrel, vegstress, shumdiag, litterhumdiag
!! gqsb, bqsb, dsg, dss, dsp 
!!
!! REFERENCE(S) :
!!  - Ducoudré, N, Laval, K & Perrier, A, 1993. SECHIBA, a new set of parameterisations
!! of the hydrologic exchanges at the land-atmosphere interface within the LMD Atmospheric General
!! Circulation Model. Journal of Climate, 6, pp. 248-273.
!!  - Ducharne, A. Laval, K. and Polcher, J. (1998) Sensitivity of the hydrological cycle to the parameterization 
!! of soil hydrology in a GCM. Climate Dynamics, 14:307-327.
!!  - de Rosnay, P. and Polcher J. (1998) Modeling root water uptake in a complex land surface scheme coupled 
!! to a GCM. Hydrology and Earth System Sciences, 2(2-3):239-256.
!!  - de Rosnay, P., Polcher, J., Laval, K. et Sabre, M. (2003). Integrated parameterization of irrigation in
!! the land surface model ORCHIDEE. Validation over Indian Peninsula. Geophys. Res. Lett, 30(19):HLS2-1 -
!! HLS2-4.
!!  - Ngo-Duc, T., J. Polcher, and K. Laval (2005), A 53-year forcing data set for land surface models, 
!! J. Geophys. Res., 110, D06116.
!!  - ALMA : http://www.lmd.jussieu.fr/~polcher/ALMA/
!!  - Ducoudré, N. (1990). Sensibilite du climat simule a la parametrisation des echanges de vapeur d'eau entre 
!! la biosphere et l'atmosphere. These de Doctorat, Université Paris 6.
!!  - Ducharne A (1997). Le cycle de l'eau : modelisation de l'hydrologie continentale, etude de ses interactions 
!! avec le climat, These de Doctorat, Université Paris 6.
!!  - de Rosnay, P. (1999). Representation des interactions sol-plante-atmosphere dans le modele de circulation generale
!! du LMD. These de doctorat, Université Paris 6.
!!  - Guimberteau, M, 2010. Modelisation de l'hydrologie continentale et influences de l'irrigation sur le cycle de l'eau, 
!! These de doctorat, Université Paris 6.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
    

  SUBROUTINE hydrolc_soil(kjpindex, vevapnu, precisol, returnflow, reinfiltration, irrigation, irrig_demand_ratio, veget_max, tot_melt, mx_eau_var, &  !added irrig_demand_ratio, veget_max, for crop irrigation, xuhui
       & veget, tot_bare_soil, ruu_ch, transpir,&
       & gqsb, bqsb, dsg, dss, rsol, drysoil_frac, hdry, dsp, runoff, run_off_tot, drainage, &
       & humrel, vegstress, shumdiag, litterhumdiag,  irrig_fin)
     
  !! 0. Variable and parameter declaration

    !! 0.1  Input variables
 
    INTEGER(i_std), INTENT(in)                          :: kjpindex       !! Domain size (number of grid cells) (unitless)
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: vevapnu        !! Bare soil evaporation  @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)   :: precisol       !! Throughfall  @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: returnflow     !! Routed water which comes back into the soil 
                                                                          !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: reinfiltration !! Water returning to the top reservoir
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: irrigation     !! Irrigation water applied to soils 
                                                                          !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(in)  :: irrig_demand_ratio !! ratio of irrigation water applied for each pft
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(in)  :: veget_max    !!
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: tot_melt       !! Total melt @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: mx_eau_var     !! Maximum water content of the soil 
                                                                          !! @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(in)  :: veget          !! Grid-cell fraction effectively covered by vegetation 
                                                                          !! for each PFT, except for soil (0-1, unitless) 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: tot_bare_soil  !! Total evaporating bare soil fraction    
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: ruu_ch         !! Volumetric soil water capacity 
                                                                          !! @tex ($kg m^{-3}$) @endtex
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)   :: transpir       !! Transpiration over each PFT @tex ($kg m^{-2}$) @endtex 

    !! 0.2 Output variables

    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(out)   :: runoff         !! runoff for each pft
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)      :: run_off_tot    !! Diagnosed surface runoff  @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)      :: drainage       !! Diagnosed drainage at the bottom of soil
                                                                          !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(out) :: humrel         !! Soil moisture stress factor on transpiration and bare
                                                                          !! soil evaporation (0-1, unitless) ! Relative humidity
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(out) :: vegstress      !! Vegetation moisture stress (only for vegetation
                                                                          !!  growth) (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (out) :: shumdiag       !! Mean relative soil moisture in the different levels 
                                                                          !! used by thermosoil.f90 (0-1, unitless) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: litterhumdiag  !! Litter humidity factor (0-1, unitless), used in 
                                                                          !! stomate
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)      :: rsol           !! Resistance to bare soil evaporation (s m^{-1}) 
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)      :: drysoil_frac   !! Fraction of visible dry soil  (0-1, unitless)
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)      :: hdry           !! Mean top dry soil height (m) version beton
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(out) :: irrig_fin      !! application of irrigation water for each pft(mm)

    !! 0.3  Modified variables

    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout):: gqsb           !! Water content in the top layer 
                                                                          !! @tex ($kg m^{-2}$) @endtex              
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout):: bqsb           !! Water content in the bottom layer 
                                                                          !! @tex ($kg m^{-2}$) @endtex     
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout):: dsg            !! Depth of the top layer (m) 
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout):: dss            !! Depth of dry soil at the top, whether in the top or 
                                                                          !! bottom layer (m)
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout):: dsp            !! Depth of dry soil in the bottom layer plus depth of
                                                                          !! top layer (m) 
    
    !! 0.4 Local variables
   
    INTEGER(i_std)                                      :: ji,jv, jd      !! Indices for grid-cells, PFTs and diagnostic levels in
                                                                          !! the soil (unitless)
!    REAL(r_std), DIMENSION(kjpindex,nvm)                :: runoff         !! Total runoff @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION(kjpindex)                    :: zhumrel_lo     !! Soil moisture stress factors on transpiration for the
                                                                          !! bottom and top layers (0-1, unitless)
    REAL(r_std), DIMENSION(kjpindex)                    :: zhumrel_up     !! Soil moisture stress factors on transpiration for the
                                                                          !! bottom and top layers (0-1, unitless)    
    REAL(r_std), DIMENSION(kjpindex,nvm)                :: zeflux         !! Evaporation to be withdrawn from soil 
                                                                          !! @tex ($kg m^{-2}) @endtex ! *** why divided by veget ?
    REAL(r_std), DIMENSION(kjpindex,nvm)                :: zpreci         !! Throughfall @tex ($kg m^{-2}$) @endtex  
                                                                          !! ! *** why divided by veget ?
    LOGICAL, DIMENSION(kjpindex,nvm)                    :: warning        !! To track negative total soil moisture cases in soil 
                                                                          !! columns (true/false)
    REAL(r_std)                                         :: gtr, btr       !! Fractions of top and botttom soil layers to the 
                                                                          !! different diagnostic soil layers (0-1, unitless)
    REAL(r_std), DIMENSION(kjpindex)                    :: mean_dsg       !! Mean depth of water in top soil layer (m)
    LOGICAL                                             :: OnceMore       !! To repeat the correction to solve dsg > dsp after 
                                                                          !! diffusion between the bottom layers (true/false)
    INTEGER(i_std), PARAMETER                           :: nitermax = 100 !! Maximum number of iterations to dsg > dsp after 
                                                                          !! diffusion between the bottom layers (unitless) 
    INTEGER(i_std)                                      :: niter          !! Counter of iterations to solve dsg > dsp after 
                                                                          !! diffusion between the bottom layers (unitless) 
    INTEGER(i_std)                                      :: nbad           !! Number of negative total soil moisture cases 
                                                                          !! (unitless); this is before diffusion between the 
                                                                          !! bottom layers, cf. "warning" ***
    LOGICAL, DIMENSION(kjpindex,nvm)                    :: lbad           !! Tags "bad" PFTs where dsg > dsp after diffusion 
                                                                          !! between the bottom layers (true/false)
    LOGICAL, DIMENSION(kjpindex)                        :: lbad_ij        !! Tags "bad" grid-points where at least one PFT exhibits
                                                                          !!  dsg > dsp after diffusion between the bottom layers 
                                                                          !! (true/false) 
    REAL(r_std)                                         :: gqseuil        !! Ancillary variables to compute drainage between the 
                                                                          !! two soil layers @tex ($kg m^{-2}$) @endtex
    REAL(r_std)                                         :: eausup         !! Ancillary variables to compute drainage between the 
                                                                          !! two soil layers @tex ($kg m^{-2}$) @endtex
    REAL(r_std)                                         :: wd1            !! Ancillary variables to compute drainage between the 
                                                                          !! two soil layers @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION(nbdl+1)                      :: tmp_dl         !! Temporary diagnostic levels in the soil (m)
    REAL(r_std), DIMENSION(kjpindex,nvm)                :: a_subgrd       !! Diagnosed subgrid fraction of saturated soil in the 
                                                                          !! top layer, to calculate hdry (0-1, unitless)
!_ ================================================================================================================================
    
  !! 1. Preliminary calculations
    
    !!  We define input and output fluxes at the soil surface, for each PFT
    !!  The input flux is throughfall.
    !!  The ouput flux is the total evaporation withdrawn from the soil (transpiration and bare soil evaporation, BSE)
    ! *** I don't understand why the fluxes are divided by veget if they are in kg.m^{-2} within each PFT *** 
    DO jv=1,nvm
      DO ji = 1, kjpindex
!MM veget(:,1) BUG ??!!!!!!!!!!!
         IF (jv .EQ. 1) THEN
            IF ( tot_bare_soil(ji) .GT. zero ) THEN
               zeflux(ji,jv) = transpir(ji,jv)/tot_bare_soil(ji)
               zpreci(ji,jv) = precisol(ji,jv)/tot_bare_soil(ji)
            ELSE
               zeflux(ji,jv) = zero
               zpreci(ji,jv) = zero
            ENDIF
         ELSE
            IF ( veget(ji,jv) .GT. zero ) THEN
               zeflux(ji,jv) = transpir(ji,jv)/veget(ji,jv)
               zpreci(ji,jv) = precisol(ji,jv)/veget(ji,jv)
            ELSE
               zeflux(ji,jv) = zero
               zpreci(ji,jv) = zero
            ENDIF
         ENDIF
      ENDDO
    ENDDO
    
    !!  For bare soil evaporation, we need to distinguish two cases. 
    !!  This should only apply if there is vegetation but we do not test this case.
    !!  Case 1 - if bare soil is present, BSE is extracted from the bare soil fraction
    DO ji = 1, kjpindex
      IF ( (vegtot(ji) .GT. zero) .AND. (tot_bare_soil(ji) .GT. min_sechiba) ) THEN
        zeflux(ji,1) = vevapnu(ji)/tot_bare_soil(ji)
      ENDIF
    ENDDO

    !!   Case 2 - if bare soil is not present, BSE is uniformly redistributed among the vegetation fractions
    !!   This case is possible because of transfers (snow for instance). 
!MM veget(:,1) BUG ??!!!!!!!!!!!
!!    DO jv = 2, nvm
!!      DO ji = 1, kjpindex
!!        IF ( (vegtot(ji) .GT. zero) .AND. (tot_bare_soil(ji) .LE. min_sechiba)&
!!             & .AND. (veget(ji,jv) .GT. min_sechiba)) THEN
!!          zeflux(ji,jv) =  zeflux(ji,jv) + vevapnu(ji)/vegtot(ji)
!!!        ENDIF
!!      ENDDO
!!    ENDDO
    
    ! Temporary diagnostic levels in the soil to diagnose shumdiag
    tmp_dl(1) = 0
    tmp_dl(2:nbdl+1) = diaglev(1:nbdl)

  !! 2. Updating soil moisture

    !!  We update soil moisture:
    !!  The top soil moisture reacts to evaporation, throughfall, snow and ice melt, and inflow from irrigation
    !!  The bottom soil moisture can receive returnflow from routing.f90
    DO jv=1,nvm
      DO ji=1,kjpindex
      
        ! Evaporated water is taken out of the ground
        gqsb(ji,jv) = gqsb(ji,jv) - zeflux(ji,jv)
        !
        ! 1.2 Add snow and ice melt, troughfall from vegetation, reinfiltration and irrigation.
        !
        IF(vegtot(ji) .NE. zero) THEN

           !  snow and ice melt, reinfiltration and troughfall from vegetation
           gqsb(ji,jv) = gqsb(ji,jv) + zpreci(ji,jv) + (tot_melt(ji)+reinfiltration(ji))/vegtot(ji)
           
           ! We take care to add the irrigation only to the vegetated part if possible
           !
           IF (ABS(vegtot(ji)-tot_bare_soil(ji)) .LE. min_sechiba) THEN

              ! vegtot == bare_soil 
!              gqsb(ji,jv) = gqsb(ji,jv) + irrigation(ji)/vegtot(ji)
              ! we only irrigated the crop pfts
              IF (irrig_demand_ratio(ji,jv) .GT. zero) THEN
                  ! WRITE(numout,*) "irrig_demand_ratio (", ji, ", ", jv, "): ", irrig_demand_ratio(ji,jv)
                  irrig_fin(ji,jv) = irrigation(ji)*irrig_demand_ratio(ji,jv)/veget_max(ji,jv)
                  gqsb(ji,jv) = gqsb(ji,jv) + irrig_fin(ji,jv)
!                  bqsb(ji,jv) = bqsb(ji,jv) + irrig_fin(ji,jv)
              ENDIF
           ELSE

              ! no vegetation, : we only add irrigation in the PFTs where vegetation can grow  
              IF ( jv > 1 ) THEN
                 ! Only add the irrigation to the upper soil if there is a reservoir.
                 ! Without this the water evaporates right away.
                 IF ( gqsb(ji,jv) > zero ) THEN
!                    gqsb(ji,jv) = gqsb(ji,jv) + irrigation(ji)/(vegtot(ji)-tot_bare_soil(ji))
                     IF (irrig_demand_ratio(ji,jv) .GT. zero) THEN
                         ! WRITE(numout,*) "irrig_demand_ratio (", ji, ", ", jv, "): ", irrig_demand_ratio(ji,jv)
                         irrig_fin(ji,jv) = irrigation(ji)*irrig_demand_ratio(ji,jv)/veget_max(ji,jv)
                         gqsb(ji,jv) = gqsb(ji,jv) + irrig_fin(ji,jv)
!                         bqsb(ji,jv) = bqsb(ji,jv) + irrig_fin(ji,jv) 
                         ! we always inject irrigation water into lower layer
                     ENDIF
                 ELSE
                     IF (irrig_demand_ratio(ji,jv) .GT. zero) THEN
                         ! WRITE(numout,*) "irrig_demand_ratio (", ji, ", ", jv, "): ", irrig_demand_ratio(ji,jv)
                         irrig_fin(ji,jv) = irrigation(ji)*irrig_demand_ratio(ji,jv)/veget_max(ji,jv)
                         bqsb(ji,jv) = bqsb(ji,jv) + irrig_fin(ji,jv)
                     ENDIF
!                    bqsb(ji,jv) = bqsb(ji,jv) + irrigation(ji)/(vegtot(ji)-tot_bare_soil(ji))
                 ENDIF
              ENDIF
           ENDIF
           
           ! We add the water returning from rivers to the lower reservoir.
           bqsb(ji,jv) = bqsb(ji,jv) + returnflow(ji)/vegtot(ji)
           
        ENDIF
        
      END DO
    ENDDO
    
  !! 3. We compute runoff and adjust the soil layers' depth and water content

    !!     The depth of top dry soil, dss, is of particular importance as it controls the soil moisture stresses 
    !!     to transpiration and BSE     
    runoff(:,:) = zero
    
    warning(:,:) = .FALSE.
    DO jv=1,nvm
      DO ji = 1, kjpindex
        
        !! 3.1 Soil moisture in excess of total water holding capacity runs off
        runoff(ji,jv) = MAX(gqsb(ji,jv) + bqsb(ji,jv) - mx_eau_var(ji), zero)
        
        !! 3.2 If the soil is saturated (runoff is generated): no top layer; dss = dsp 
        IF (mx_eau_var(ji) .LE. (gqsb(ji,jv) + bqsb(ji,jv))) THEN
            !
            gqsb(ji,jv) = zero
            dsg(ji,jv) = zero
            bqsb(ji,jv) = mx_eau_var(ji)
            dsp(ji,jv) = zero
            dss(ji,jv) = dsp (ji,jv)

        ELSEIF ((gqsb(ji,jv) + bqsb(ji,jv)).GE.zero) THEN
            
            !! 3.3 Else, if the top layer holds more water than allowed by its depth, the top layer deepens
            !! The top layer is saturated, so that dss=0.
            !! In this case, dsp is useless, and is not updated, 
            !! even if bqsb has changed because of irrigation and return flow.
            IF (gqsb(ji,jv) .GT. dsg(ji,jv) * ruu_ch(ji)) THEN
                !
                dsg(ji,jv) = gqsb(ji,jv) / ruu_ch(ji)
                dss(ji,jv) = zero
            ELSEIF (gqsb(ji,jv) .GT. zero ) THEN
 
            !! 3.4 If the top layer is not saturated, its total depth does not change and we only update its dry soil depth
            !! In this case, dsp is useless, and is not updated, 
            !! even if bqsb has changed because of irrigation and return flow.
                !
                dss(ji,jv) = ((dsg(ji,jv) * ruu_ch(ji)) - gqsb(ji,jv)) / ruu_ch(ji) 
            ELSE

            !! 3.5 If the top layer's moisture is negative, it vanishes and the required moisture is withdrawn from the bottom layer
            !! dsp is updated accordingly, and defines the depth of top dray soil dss.
                !
                bqsb(ji,jv) = bqsb(ji,jv) + gqsb(ji,jv)
                dsp(ji,jv) = zmaxh - bqsb(ji,jv) / ruu_ch(ji) ! dsp>dpu if bqsb<0 *** we can be here with bsqb<0 and bqsb+gqsb>0
                gqsb(ji,jv) = zero
                dsg(ji,jv) = zero
                dss(ji,jv) = dsp(ji,jv)
            END IF

        ELSE 

        !! 3.6 If the total soil moisture is negative: we keep track of the problem for further warning.
        !! In this extreme case of dry soil, the top layer is absent.
        !! The top dry soil depth, dsp, is equal to the soil depth.
        !! But we keep the negative moisture for water budget closure, and assign it to the bottom layer.
        ! Ceci ne devrait jamais arriver plus d'une fois par point. C-a-d une fois la valeur negative
        ! atteinte les flux doivent etre nuls. On ne signale que ce cas donc. 
        ! *** But this is obviously not the case in CMIP5 runs ***
        ! *** Also, I don't see the use of conditionning upon zeflux(ji,jv) .GT. zero : negative moisture is always a problem !
            
            IF ( ( zeflux(ji,jv) .GT. zero ) .AND. &
                 ( gqsb(ji,jv) + bqsb(ji,jv) .LT. -1.e-10 ) ) THEN
               warning(ji,jv) = .TRUE.

               ! WRITE (numout,*) 'WARNING! Soil Moisture will be negative' 
               ! WRITE (numout,*) 'ji, jv = ', ji,jv
               ! WRITE (numout,*) 'mx_eau_var = ', mx_eau_var(ji)
               ! WRITE (numout,*) 'veget, resdist =', veget(ji,jv), resdist(ji,jv)
               ! WRITE (numout,*) 'bqsb = ', bqsb(ji,jv)
               ! WRITE (numout,*) 'gqsb = ', gqsb(ji,jv)
               ! WRITE (numout,*) 'dss = ', dss(ji,jv)
               ! WRITE (numout,*) 'dsg = ', dsg(ji,jv)
               ! WRITE (numout,*) 'dsp = ', dsp(ji,jv)
               ! WRITE (numout,*) 'humrel = ', humrel(ji, jv)
               ! WRITE (numout,*) 'Soil evaporation = ', zeflux(ji,jv)
               ! WRITE (numout,*) 'input = ',precisol(ji, jv), tot_melt(ji)
               ! WRITE (numout,*) '============================'
            ENDIF

            
            bqsb(ji,jv) = gqsb(ji,jv) + bqsb(ji,jv) ! this will be negative 
            dsp(ji,jv) = zmaxh
            gqsb(ji,jv) = zero
            dsg(ji,jv) = zero
            dss(ji,jv) = dsp(ji,jv)
            
        ENDIF
        
      ENDDO
    ENDDO
    
  !! 4. If there are some PFTs with negative moisture, it is written in the run log
    
    nbad = COUNT( warning(:,:) .EQV. .TRUE. )
    IF ( nbad .GT. 0 ) THEN
      WRITE(numout,*) 'hydrolc_soil: WARNING! Soil moisture was negative at', &
                       nbad, ' points:'
      !DO jv = 1, nvm
      !  DO ji = 1, kjpindex
      !    IF ( warning(ji,jv) ) THEN
      !      WRITE(numout,*) '  ji,jv = ', ji,jv
      !      WRITE (numout,*) 'mx_eau_var = ', mx_eau_var(ji)
      !      WRITE (numout,*) 'veget, resdist =', veget(ji,jv), resdist(ji,jv)
      !      WRITE (numout,*) 'bqsb = ', bqsb(ji,jv)
      !      WRITE (numout,*) 'gqsb = ', gqsb(ji,jv)
      !      WRITE (numout,*) 'runoff = ',runoff(ji,jv)
      !      WRITE (numout,*) 'dss = ', dss(ji,jv)
      !      WRITE (numout,*) 'dsg = ', dsg(ji,jv)
      !      WRITE (numout,*) 'dsp = ', dsp(ji,jv)
      !      WRITE (numout,*) 'humrel = ', humrel(ji, jv)
      !      WRITE (numout,*) 'Soil evaporation = ', zeflux(ji,jv)
      !      WRITE (numout,*) 'Soil precipitation = ',zpreci(ji,jv)
      !      WRITE (numout,*) 'input = ',precisol(ji, jv), tot_melt(ji)
      !      WRITE (numout,*) 'returnflow = ',returnflow(ji)
      !      WRITE (numout,*) '============================'
      !    ENDIF
      !  ENDDO
      !ENDDO
    ENDIF
    
  !! 5. Top layers that are very large or very dry can be deadlock situations for the Choisnel scheme
 
    !!  Top layers that are very large or that hold a tiny amount of water .
    !!  They are handled here.
    IF (printlev>=3) WRITE(numout,*)  'hydro_soil 2.0 : Resolve deadlocks'
    DO jv=1,nvm
      DO ji=1,kjpindex
        
        !! 5.1 If the top layer is almost dry, we merge the two layers 
        IF ( ABS(dsp(ji,jv)-dsg(ji,jv)) .LT. min_sechiba ) THEN ! min_sechiba = 1.e-8 (constantes.f90)
            bqsb(ji,jv) = bqsb(ji,jv) + gqsb(ji,jv)
            dsp(ji,jv) = zmaxh - bqsb(ji,jv) / ruu_ch(ji) ! *** can this make dsp > dpu_cste
            gqsb(ji,jv) = zero
            dsg(ji,jv) = zero
            dss(ji,jv) = dsp(ji,jv)
        ENDIF
        
        !!  5.2 Internal drainage from the top to the bottom layer
        !!   Much faster when the relative moisture of the top layer exceeds 75\% of the water holding capacity.
        !!   The parameters are exp_drain = 1.5, min_drain = 0.002 mm/h and max_drain = 0.2 mm/h
        gqseuil = min_sechiba * deux * ruu_ch(ji) ! min_sechiba = 1.e-8 (constantes.f90)
        eausup = dsg(ji,jv) * ruu_ch(ji)
        wd1 = .75*eausup
        
        IF (eausup .GT. gqseuil) THEN ! dsg > 2.e-8 m
            gdrainage(ji,jv) = min_drain *  (gqsb(ji,jv)/eausup)
            
            IF ( gqsb(ji,jv) .GE. wd1 .AND. dsg(ji,jv) .GT. 0.10 ) THEN
                
                gdrainage(ji,jv) = gdrainage(ji,jv) + &
                   (max_drain-min_drain)*((gqsb(ji,jv)-wd1) / (eausup-wd1))**exp_drain
                
            ENDIF
            
            gdrainage(ji,jv)=MIN(gdrainage(ji,jv), MAX(gqsb(ji,jv), zero))
            
        ELSE
            gdrainage(ji,jv)=zero ! *** why not remove the top layer in that case ?? 
        ENDIF
        !
        gqsb(ji,jv) = gqsb(ji,jv) -  gdrainage(ji,jv)
        bqsb(ji,jv) = bqsb(ji,jv) + gdrainage(ji,jv)
        dsg(ji,jv) = dsg(ji,jv) -  gdrainage(ji,jv) / ruu_ch(ji)
        dsp(ji,jv) = zmaxh - bqsb(ji,jv)/ruu_ch(ji) ! *** this line shouldn't make dsp>dpu_cste
      ENDDO
    ENDDO
    
  !! 6. We want the vegetation to share a common bottom moisture:
    !! Thus, we must account for moisture diffusion between the soil columns.
    IF (printlev>=3) WRITE(numout,*)  'hydrolc_soil 3.0 : Vertical diffusion'

    !!  6.1 Average of bottom and top moisture over the "veget" fractions
    mean_bqsb(:) = zero
    mean_gqsb(:) = zero
    DO jv = 1, nvm
      DO ji = 1, kjpindex
        IF ( vegtot(ji) .GT. zero ) THEN
!MM veget(:,1) BUG ??!!!!!!!!!!!
           IF (jv .EQ. 1) THEN
              mean_bqsb(ji) = mean_bqsb(ji) + tot_bare_soil(ji)/vegtot(ji)*bqsb(ji,jv)
              mean_gqsb(ji) = mean_gqsb(ji) + tot_bare_soil(ji)/vegtot(ji)*gqsb(ji,jv)
           ELSE
              mean_bqsb(ji) = mean_bqsb(ji) + veget(ji,jv)/vegtot(ji)*bqsb(ji,jv)
              mean_gqsb(ji) = mean_gqsb(ji) + veget(ji,jv)/vegtot(ji)*gqsb(ji,jv)
           ENDIF
        ENDIF
      ENDDO
    ENDDO
    
    OnceMore = .TRUE.
    niter = 0
    lbad_ij(:)=.TRUE. 
    DO WHILE ( OnceMore .AND. ( niter .LT. nitermax ) )  ! nitermax prevents infinite loops (should actually never occur)
      
      niter = niter + 1
     
!!!! we test disabling the average of soil columns 
!!      !! 6.2 We assign the mean value in all the soil columns in a grid-cell
!!      DO jv = 1, nvm
!!        DO ji = 1, kjpindex
!!           IF (lbad_ij(ji)) THEN
!!!MM veget(:,1) BUG ??!!!!!!!!!!!
!!              IF (jv .EQ. 1) THEN
!!                 IF (tot_bare_soil(ji).GT.zero) THEN
!!                    !
!!                    bqsb(ji,jv) = mean_bqsb(ji)
!!                    dsp(ji,jv) = zmaxh - bqsb(ji,jv)/ruu_ch(ji)
!!                 ENDIF
!!              ELSE IF ( veget(ji,jv) .GT. zero ) THEN
!!                 !
!!                 bqsb(ji,jv) = mean_bqsb(ji)
!!                 dsp(ji,jv) = zmaxh - bqsb(ji,jv)/ruu_ch(ji) ! *** can this line make dsp>dpu_cste ?
!!              ENDIF
!!           ENDIF
!!           
!!        ENDDO
!!      ENDDO    
      
      !! 6.3 Iterative adjustment if top layer larger than new average dsp
      !! After averaging the bottom moistures, dsp becomes the mean depth of soil that is not filled by bottom moisture.
      !! In "bad" points where dsg > dsp, we merge the two soil layers.
      !! This adjustment needs to be done iteratively (WHILE loop)     
      !! We diagnose the bad points where dsg>dsp
!MM veget(:,1) BUG ??!!!!!!!!!!!
!!$      lbad(:,:) = ( ( dsp(:,:) .LT. dsg(:,:) ) .AND. &
!!$                    ( dsg(:,:) .GT. zero ) .AND. &
!!$                    ( veget(:,:) .GT. zero ) )
      lbad(:,:) = ( ( dsp(:,:) .LT. dsg(:,:) ) .AND. &
                    ( dsg(:,:) .GT. zero ) )
      DO jv = 1, nvm
         DO ji = 1, kjpindex
            IF (jv .EQ. 1) THEN
               lbad(ji,jv) = lbad(ji,jv) .AND. ( tot_bare_soil(ji) .GT. zero )
            ELSE
               lbad(ji,jv) = lbad(ji,jv) .AND. ( veget(ji,jv) .GT. zero )
            ENDIF
         ENDDO
      ENDDO

      
      !! If there are no such points, we'll do no further iteration 
      IF ( COUNT( lbad(:,:) ) .EQ. 0 ) OnceMore = .FALSE.
      DO ji = 1, kjpindex
        IF (COUNT(lbad(ji,:)) == 0 ) lbad_ij(ji)=.FALSE.
      ENDDO
      
      !! In the bad PFTs, we merge the two soil layers.
      DO jv = 1, nvm
!YM
!        !
!        DO ji = 1, kjpindex
!          IF ( veget(ji,jv) .GT. zero ) THEN
!            !
!            bqsb(ji,jv) = mean_bqsb(ji)
!            dsp(ji,jv) = zmaxh - bqsb(ji,jv)/ruu_ch(ji)
!          ENDIF
!          !
!        ENDDO
        !
        DO ji = 1, kjpindex
          IF ( lbad(ji,jv) ) THEN
            !
            runoff(ji,jv) = runoff(ji,jv) + &
                            MAX( bqsb(ji,jv) + gqsb(ji,jv) - mx_eau_var(ji), zero)
            !
            bqsb(ji,jv) = MIN( bqsb(ji,jv) + gqsb(ji,jv), mx_eau_var(ji))
            !
            gqsb(ji,jv) = zero
            dsp(ji,jv) = zmaxh - bqsb(ji,jv)/ruu_ch(ji)  ! *** could this line make dsp>dpu_cste ? 
                                                           ! *** set a max to dpu_cste to be sure !
            dss(ji,jv) = dsp(ji,jv)
            dsg(ji,jv) = zero
            
          ENDIF
        ENDDO
      ENDDO  
      
      !! New average of bottom and top moisture over the "veget" fractions, for the next iteration
      mean_bqsb(:) = zero
      mean_gqsb(:) = zero
      DO jv = 1, nvm
        DO ji = 1, kjpindex
          IF ( vegtot(ji) .GT. zero ) THEN
!MM veget(:,1) BUG ??!!!!!!!!!!!
             IF (jv .EQ. 1) THEN
                mean_bqsb(ji) = mean_bqsb(ji) + tot_bare_soil(ji)/vegtot(ji)*bqsb(ji,jv)
                mean_gqsb(ji) = mean_gqsb(ji) + tot_bare_soil(ji)/vegtot(ji)*gqsb(ji,jv)
             ELSE
                mean_bqsb(ji) = mean_bqsb(ji) + veget(ji,jv)/vegtot(ji)*bqsb(ji,jv)
                mean_gqsb(ji) = mean_gqsb(ji) + veget(ji,jv)/vegtot(ji)*gqsb(ji,jv)
             ENDIF
          ENDIF
        ENDDO
      ENDDO
      
    ENDDO ! end while, but no warning if nitermax has been reached ***
    
  !! 7. Compute total runoff from all soil columns and partition it into drainage and surface runoff

    ! *** why isn't run_off_tot divided by vegtot ?
    IF (printlev>=3) WRITE(numout,*)  'hydrolc_soil 4.0: Computes total runoff'

    !! 7.1 Average total runoff
    run_off_tot(:) = zero
    DO ji = 1, kjpindex
       IF ( vegtot(ji) .GT. zero ) THEN
          run_off_tot(ji) = runoff(ji,1)*tot_bare_soil(ji) + SUM(runoff(ji,2:nvm)*veget(ji,2:nvm))
       ELSE
          run_off_tot(ji) = tot_melt(ji) + irrigation(ji) + reinfiltration(ji)
       ENDIF
    ENDDO
    
    !! 7.2 Diagnose drainage and surface runoff (95% and 5%)
    drainage(:) = 0.95 * run_off_tot(:)
    run_off_tot(:) = run_off_tot(:) - drainage(:)       
    ! *** from now on, we lost track of total runoff
    ! *** the name "run_off_tot" is VERY misleading
    
  !! 8. Diagnostics which are needed to carry information to other modules:

    IF (printlev>=3) WRITE(numout,*)  'hydro_soil 5.0: Diagnostics'
    
    ! Reset dsg if necessary
    WHERE (gqsb(:,:) .LE. zero) dsg(:,:) = zero
    
    ! Average moisture profile for shumdiag
    DO ji=1,kjpindex
      mean_dsg(ji) = mean_gqsb(ji)/ruu_ch(ji) ! mean depth of water in top layer in meters
    ENDDO
    
    !! 8.1 Moisture stress factors on vegetation (humrel and vegstress)
    !! Moisture stress factors on vegetation:
    !!  - "humrel" on transpiration (for condveg) exponentially decreases with top dry soil depth;
    !!  the decay factor is the one of root density with depth (from 1 to 8 depending on PFTs),
    !!  following de Rosnay and Polcher (1998) 
    !!  - "vegstress" on vegetation growth (for stomate) 
    IF (printlev>=3) WRITE(numout,*)  'hydro_soil 6.0 : Moisture stress'

    a_subgrd(:,:) = zero

    DO jv = 1, nvm
      DO ji=1,kjpindex
         !
         ! computes relative surface humidity
         !
         ! Only use the standard formulas if total soil moisture is larger than zero.
         ! Else stress functions are set to zero. 
         ! This will avoid that large negative soil moisture accumulate over time by the
         ! the creation of small skin reservoirs which evaporate quickly.
         !
         IF ( gqsb(ji,jv)+bqsb(ji,jv) .GT. zero ) THEN
            !
            IF (dsg(ji,jv).EQ. zero .OR. gqsb(ji,jv).EQ.zero) THEN
               humrel(ji,jv) = EXP( - humcste(jv) * zmaxh * (dsp(ji,jv)/zmaxh) )
               dsg(ji,jv) = zero
                   
               ! humrel = 0 if dsp is larger than its value at the wilting point, or if the bottom layer's soil is negative
               ! *** the condition based on qwilt (= 5.0) doesn't make much sense: (i) the Choisnel scheme works in available  
               ! *** moisture, i.e. above wilting point (Ducharne & Laval, 2000), so that the moisture at withing point is ZERO; 
               ! *** (ii) with the chosen values in constantes_soil.f90, qwilt/ruu_ch is around 0.033 and dpu_cste-qwilt/ruu_ch
               ! *** is around dpu
               IF (dsp(ji,jv).GT.(zmaxh - min_sechiba) .OR. bqsb(ji,jv).LT.zero) THEN
                  humrel(ji,jv) = zero
               ENDIF

               vegstress(ji,jv) = humrel(ji,jv)
               
            ELSE

               !! 8.1.2 If there are two soil layers, we need the "transpiring" fraction "a_subgrd"
               !! We compute humrel as the average of the values defined by dss and dsp.
	       !! The weights depend on "a_subgrd", which is defined by redistributing the top layer's 
	       !! moisture in a virtual layer of depth dsg_min (set to 1 mm in hydrolc), 
               !! what defines a totally dry and a totally saturated fraction (a_subgrd):
               !!  - if the top layer's moisture > 1mm, then a_subgrd=1, and the humrel is normally deduced from dss only
               !!  - if the top layer's moisture is very small, a_subgrd decreases from 1 to 0 as the top layer's moisture 
               !!    vanishes, and it serves to describe a smooth transition to the case where the top soil layer has
               !!    vanished and humrel depends on dsp.
               !! \latexonly
 	       !! \includegraphics[scale = 1]{asubgrid.pdf}
               !! \endlatexonly
               !
               zhumrel_lo(ji) = EXP( - humcste(jv) * dsp(ji,jv)) 
               zhumrel_up(ji) = EXP( - humcste(jv) * dss(ji,jv))
               a_subgrd(ji,jv)=MIN(MAX(dsg(ji,jv)-dss(ji,jv),zero)/dsg_min,un)
               humrel(ji,jv)=a_subgrd(ji,jv)*zhumrel_up(ji)+(un-a_subgrd(ji,jv))*zhumrel_lo(ji)
               
               !! As we need a slower variable for vegetation growth, vegstress is computed differently from humrel.
               ! *** la formule ci-dessous est d'un empirisme absolu : on ajoute deux stresses, on en retranche un autre...????
               ! que veut dire slower variable ??
               vegstress(ji,jv) = zhumrel_lo(ji) + zhumrel_up(ji) - EXP( - humcste(jv) * dsg(ji,jv) ) 
               
            ENDIF
            
         ELSE 
 
            !! 8.1.3 If total moisture is negative, the two moisture stress factors are set to zero. 
            !! This should avoid that large negative soil moisture accumulate over time because of small skin layers
            !! which transpire quickly.
            ! *** Yet, CMIP5 exhibits such behaviors => because of rsol ?? or other reasons ?? The routing shouldn't 
            ! *** create such situations, but couldn't some patches here or there ???
            humrel(ji,jv) = zero
            vegstress(ji,jv) = zero
            
         ENDIF
        !
      ENDDO
    ENDDO

    !! Calculates the water limitation factor.
    humrel(:,:) = MAX( min_sechiba, MIN( humrel(:,:)/0.5, un ))
    
    !! 8.2 Mean relative soil moisture in the diagnostic soil levels: shumdiag (for thermosoil)
    !! It is deduced from the mean moisture and depth of the two soil layers in the grid cell,
    !! depending on how the soil diagnostic levels intersect the two mean soil depths
    DO jd = 1,nbdl
      DO ji = 1, kjpindex
         IF ( tmp_dl(jd+1) .LT. mean_dsg(ji)) THEN ! mean_dsg = mean depth of water in top layer in meters

            ! If the diagnostic soil level entirely fits into the mean top soil layer depth, they have the same 
            ! relative moisture
            shumdiag(ji,jd) = mean_gqsb(ji)/mx_eau_var(ji)
         ELSE 
            IF ( tmp_dl(jd) .LT. mean_dsg(ji)) THEN

               ! If the diagnostic soil level intersects both soil layers, its relative moisture is the weighted
               ! mean of the ones of two soil layers
               gtr = (mean_dsg(ji)-tmp_dl(jd))/(tmp_dl(jd+1)-tmp_dl(jd))
               btr = 1 - gtr
               shumdiag(ji,jd) = gtr*mean_gqsb(ji)/mx_eau_var(ji) + &
                    & btr*mean_bqsb(ji)/mx_eau_var(ji)
            ELSE

               ! If the diagnostic soil level entirely fits into the mean bottom soil layer depth,
               ! they have the same relative moisture 
               shumdiag(ji,jd) = mean_bqsb(ji)/mx_eau_var(ji)
            ENDIF
         ENDIF
         shumdiag(ji,jd) = MAX(MIN(shumdiag(ji,jd), un), zero)
      ENDDO
    ENDDO

    !! 8.3 Fraction of visibly dry soil in the bare soil fraction for soil albedo
    !!  if we want to account for its dependance on soil moisture in condveg.
    !!  We redistribute the top layer's moisture in a virtual layer of depth 0.1 m,
    !!  what defines a totally saturated and a totally dry fraction (drysoil_frac). 
    !!  The latter is thus 1 if dss > 0.1 m. 
    drysoil_frac(:) = MIN(MAX(dss(:,1),zero)*10._r_std, un)

    !! 8.4 Resistance to bare soil evaporation
    !!  This resistance increases with the height of top dry soil, described by hdry.
    !!  It depends on both dss and dsp (dry soil heights) in the bare soil fraction, 
    !!  and, as for the soil moisture stress factor on transpiration (humrel), 
    !!  we use "a_subgrd" (see 8.1.2) to describe a smooth transition between cases 
    !!  when the top layer's moisture > 1mm and hdry=dss and cases when the top layer's 
    !!  has vanished and hdy=dsp. 
    !!  The relationship between rsol and dry soil height has been changed since 
    !!  de Rosnay and Polcher (1998), to make rsol becomes VERY large when hdry 
    !!  gets close to dpu_cste 
    !!  Owing to this non-linear dependance, BSE tends to zero when the depth of dry reaches dpu_cste.
    ! *** if hdry > dpu (several cases might lead to this pathological situation), 
    ! *** we shift to the other limb of the hyperbolic function, and rsol decreases towards hdry*rsol_cste 
    ! *** can this explain the accumulation of very negative soil moisture in arid zones in CMIP5 ???
    ! *** COULD'NT WE SIMPLY SET THAT hdry CANNOT EXCEED dpu_cste ???
    hdry(:) = a_subgrd(:,1)*dss(:,1) + (un-a_subgrd(:,1))*dsp(:,1) 
    
    rsol(:) = -un
    DO ji = 1, kjpindex
       IF (tot_bare_soil(ji) .GE. min_sechiba) THEN
          
          ! Correction Nathalie - le 28 mars 2006 - sur conseils Fred Hourdin
          ! on modifie le rsol pour que la resistance croisse subitement si on s'approche
          ! du fond. En gros, rsol=hdry*rsol_cste pour hdry < 1m70
          !rsol(ji) = dss(ji,1) * rsol_cste
          rsol(ji) =  ( hdry(ji) + un/(10.*(zmaxh - hdry(ji))+1.e-10)**2 ) * rsol_cste
       ENDIF
    ENDDO
    
    !! 8.5 Litter humidity factor, used in stomate
    !!  It varies from 1 when mean top dry soil height is 0, and decreases as mean top dry soil height increases
    litterhumdiag(:) = EXP( - hdry(:) / hcrit_litter ) ! hcrit_litter=0.08_r_std

    !!  Special case of it has just been raining a few drops: the top layer exists, but its height 
    !!  is ridiculously small and the mean top dry soil height is almost zero: we assume a dry litter 
    WHERE ( ( hdry(:) .LT. min_sechiba ) .AND. &  ! constantes : min_sechiba = 1.E-8_r_std
            ( mean_dsg(:) .GT. min_sechiba ) .AND. ( mean_dsg(:) .LT. 5.E-4 ) )
      litterhumdiag(:) = zero
    ENDWHERE
    
    IF (printlev>=3) WRITE (numout,*) ' hydrolc_soil done '

  END SUBROUTINE hydrolc_soil
  

  SUBROUTINE hydrolc_waterbal_init (kjpindex, qsintveg, snow, snow_nobio)
    
    !! 0. Variable and parameter declaration
    !! 0.1 Input variables
    INTEGER(i_std), INTENT (in)                            :: kjpindex       !! Domain size (number of grid cells) (unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)      :: qsintveg       !! Amount of water in the canopy interception 
                                                                             !! reservoir @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: snow           !! Snow water equivalent @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(in)   :: snow_nobio     !! Snow water equivalent on nobio areas  
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    !! 0.4 Local variables
    INTEGER(i_std)                                         :: ji, jv, jn
    REAL(r_std),DIMENSION (kjpindex)                       :: watveg         !! Total amount of intercepted water in a grid-cell
    REAL(r_std),DIMENSION (kjpindex)                       :: sum_snow_nobio !! Total amount of snow from the "nobio" fraction 
                                                                             !! in a grid-cell @tex ($kg m^{-2}$) @endtex 
!_ ================================================================================================================================

  !! 1. We initialize the total amount of water in terrestrial grid-cells at the beginning of the first time step
    
    tot_water_beg(:) = zero
    watveg(:) = zero
    sum_snow_nobio(:) = zero
    
    DO jv = 1, nvm
       watveg(:) = watveg(:) + qsintveg(:,jv)
    ENDDO
    
    DO jn = 1, nnobio ! nnobio=1
       sum_snow_nobio(:) = sum_snow_nobio(:) + snow_nobio(:,jn)
    ENDDO
    
    DO ji = 1, kjpindex
       tot_water_beg(ji) = (mean_bqsb(ji) + mean_gqsb(ji))*vegtot(ji) + &
            &  watveg(ji) + snow(ji) + sum_snow_nobio(ji)
    ENDDO
    tot_water_end(:) = tot_water_beg(:)
    
  END SUBROUTINE hydrolc_waterbal_init

!! ================================================================================================================================
!! SUBROUTINE   : hydrolc_waterbal 
!!
!>\BRIEF        This subroutine checks the water balance closure in each terrestrial grid-cell
!! over a time step.
!!
!! DESCRIPTION  : The change in total water amount over the time step must equal the algebraic sum of the  
!! water fluxes causing the change, integrated over the timestep. The equality is checked to some precision, 
!! set for double precision calculations (REAL*8). Note that this precision depends on the time step.
!! This verification does not make much sense in REAL*4 as the precision is the same as some of the fluxes.
!! The computation is only done over the soil area, as over glaciers (and lakes?) we do not have 
!! water conservation. 
!! *** The above sentence is not consistent with what I understand from the code, since snow_nobio is accounted for.
!! *** what is the value of epsilon(1) ?
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : None
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================   
  
  SUBROUTINE hydrolc_waterbal (kjpindex, index, veget, totfrac_nobio, qsintveg, snow, snow_nobio,&
       & precip_rain, precip_snow, returnflow, reinfiltration, irrigation, tot_melt, vevapwet, transpir, vevapnu,&
       & vevapsno, vevapflo, floodout, run_off_tot, drainage)
    
  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT (in)                            :: kjpindex       !! Domain size (number of grid cells) (unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)       :: index          !! Indices of the points on the map
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)      :: veget          !! Grid-cell fraction effectively covered by 
                                                                             !! vegetation for each PFT, except for soil 
                                                                             !! (0-1, unitless) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: totfrac_nobio  !! Total fraction of terrestrial ice+lakes+...
                                                                             !! (0-1, unitless) 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)      :: qsintveg       !! Amount of water in the canopy interception 
                                                                             !! reservoir @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: snow           !! Snow water equivalent @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(inout):: snow_nobio     !! Snow water equivalent on nobio areas  
                                                                             !! @tex ($kg m^{-2}$) @endtex 
									     ! Ice water balance
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: precip_rain    !! Rainfall @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: precip_snow    !! Snowfall  @tex ($kg m^{-2}$) @endtex  
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: returnflow     !! Routed water which comes back into the soil 
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: reinfiltration !! Water returning from routing to the top reservoir
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: irrigation     !! Irrigation water applied to soils 
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: tot_melt       !! Total melt @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)      :: vevapwet       !! Interception loss over each PFT 
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)      :: transpir       !! Transpiration over each PFT 
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: vevapnu        !! Bare soil evaporation @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: vevapflo       !! Floodplains evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: vevapsno       !! Snow evaporation @tex ($kg m^{-2}$) @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: floodout       !! Outflow from floodplains
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: run_off_tot    !! Diagnosed surface runoff @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: drainage       !! Diagnosed rainage at the bottom of soil  
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    
    !! 0.2 Output variables
    
    !! 0.3 Modified variables

    !! 0.4 Local variables
    
    INTEGER(i_std)                                         :: ji, jv, jn     !!  Grid-cell, PFT and "nobio" fraction indices 
                                                                             !! (unitless)
    REAL(r_std)                                            :: allowed_err    !! Allowed error in the water budget closure 
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex)                       :: watveg         !! Total amount of intercepted water in a grid-cell  
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex)                       :: delta_water    !! Change in total water amount 
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex)                       :: tot_flux       !! Algebraic sum of the water fluxes integrated over 
                                                                             !! the timestep @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex)                       :: sum_snow_nobio !! Total amount of snow from the "nobio" fraction 
                                                                             !! in a grid-cell @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex)                       :: sum_vevapwet   !! Sum of interception loss in the grid-cell 
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex)                       :: sum_transpir   !! Sum of  transpiration in the grid-cell 
                                                                             !! @tex ($kg m^{-2}$) @endtex 
!_ ================================================================================================================================

  !! 1. We check the water balance : this is done at the end of a time step
    tot_water_end(:) = zero
    
    !! 1.1 If the "nobio" fraction does not complement the "bio" fraction, we issue a warning
    !!   If the "nobio" fraction (ice, lakes, etc.) does not complement the "bio" fraction (vegetation and bare soil),
    !!   we do not need to go any further, and we output a warning ! *** how are oceans treated ?
    DO ji = 1, kjpindex

       ! Modif Nathalie
       ! IF ( (un - (totfrac_nobio(ji) + vegtot(ji))) .GT. EPSILON(un) ) THEN
       IF ( (un - (totfrac_nobio(ji) + vegtot(ji))) .GT. (100*EPSILON(un)) ) THEN
          WRITE(numout,*) 'HYDROL problem in vegetation or frac_nobio on point ', ji
          WRITE(numout,*) 'totfrac_nobio : ', totfrac_nobio(ji)
          WRITE(numout,*) 'vegetation fraction : ', vegtot(ji)
          !STOP 'in hydrolc_waterbal'
       ENDIF
    ENDDO
    
    !! 1.2 We calculate the total amount of water in grid-cells at the end the time step
    watveg(:) = zero
    sum_vevapwet(:) = zero
    sum_transpir(:) = zero
    sum_snow_nobio(:) = zero

    !cdir NODEP
    DO jv = 1,nvm
       watveg(:) = watveg(:) + qsintveg(:,jv)
       sum_vevapwet(:) = sum_vevapwet(:) + vevapwet(:,jv)
       sum_transpir(:) = sum_transpir(:) + transpir(:,jv)
    ENDDO

    !cdir NODEP
    DO jn = 1,nnobio
       sum_snow_nobio(:) = sum_snow_nobio(:) + snow_nobio(:,jn)
    ENDDO
    
    !cdir NODEP
    DO ji = 1, kjpindex
       tot_water_end(ji) = (mean_bqsb(ji) + mean_gqsb(ji))*vegtot(ji) + &
            & watveg(ji) + snow(ji) + sum_snow_nobio(ji)
    ENDDO
     
    !! 2.3 Calculate the change in total water amount during the time step
    !!  Calculate the change in total water amount during the time, stepand the algebraic sum
    !!  of the water fluxes supposed to cause the total water amount change.
    !!  If the model conserves water, they should be equal, since the fluxes are used in their integrated form, 
    !!  over the time step dt_sechiba, with a unit of kg m^{-2}.
    DO ji = 1, kjpindex
       
       delta_water(ji) = tot_water_end(ji) - tot_water_beg(ji)
       
       tot_flux(ji) =  precip_rain(ji) + precip_snow(ji) + returnflow(ji) + reinfiltration(ji) + irrigation(ji) - &
             & sum_vevapwet(ji) - sum_transpir(ji) - vevapnu(ji) - vevapsno(ji) - vevapflo(ji) + &
             & floodout(ji)- run_off_tot(ji) - drainage(ji) 
       
    ENDDO
    
       !! 1.4 We do the check, given some minimum required precision
       !!       This is a wild guess and corresponds to what works on an IEEE machine under double precision (REAL*8).
       !!       If the water balance is not closed at this precision, we issue a warning with some diagnostics.
       allowed_err = 50000*EPSILON(un) ! *** how much is epsilon(1) ? where is it defined ?
       
    DO ji = 1, kjpindex
       IF ( ABS(delta_water(ji)-tot_flux(ji)) .GT. allowed_err ) THEN
          WRITE(numout,*) 'HYDROL does not conserve water. The erroneous point is : ', ji
          WRITE(numout,*) 'The error in mm/d is :', (delta_water(ji)-tot_flux(ji))/dt_sechiba*one_day, &
               & ' and in mm/dt : ', delta_water(ji)-tot_flux(ji)
          WRITE(numout,*) 'delta_water : ', delta_water(ji), ' tot_flux : ', tot_flux(ji)
          WRITE(numout,*) 'Actual and allowed error : ', ABS(delta_water(ji)-tot_flux(ji)), allowed_err
          WRITE(numout,*) 'vegtot : ', vegtot(ji)
          WRITE(numout,*) 'precip_rain : ', precip_rain(ji)
          WRITE(numout,*) 'precip_snow : ',  precip_snow(ji)
          WRITE(numout,*) 'Water from irrigation, floodplains:', reinfiltration(ji), returnflow(ji), irrigation(ji)
          WRITE(numout,*) 'Total water in soil :', mean_bqsb(ji) + mean_gqsb(ji)
          WRITE(numout,*) 'Water on vegetation :', watveg(ji)
          WRITE(numout,*) 'Snow mass :', snow(ji)
          WRITE(numout,*) 'Snow mass on ice :', sum_snow_nobio(ji)
          WRITE(numout,*) 'Melt water :', tot_melt(ji)
          WRITE(numout,*) 'evapwet : ', vevapwet(ji,:)
          WRITE(numout,*) 'transpir : ', transpir(ji,:)
          WRITE(numout,*) 'evapnu, evapsno, evapflo : ', vevapnu(ji), vevapsno(ji), vevapflo(ji)
          WRITE(numout,*) 'floodout : ', floodout(ji)
          WRITE(numout,*) 'drainage : ', drainage(ji)
          !STOP 'in hydrolc_waterbal'
       ENDIF
       
    ENDDO
    
  !! 2. Transfer the total water amount at the end of the current timestep to the begining of the next one
    
    tot_water_beg = tot_water_end
    
  END SUBROUTINE hydrolc_waterbal
 

!! ================================================================================================================================
!! SUBROUTINE  : hydrolc_alma_init 
!!
!>\BRIEF       Initialize variables needed in hydrolc_alma
!!
!! DESCRIPTION : None
!!
!! RECENT CHANGE(S) : None
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n   
!_ ================================================================================================================================   

  SUBROUTINE hydrolc_alma_init (kjpindex, index, qsintveg, snow, snow_nobio)
    
  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT (in)                         :: kjpindex    !! Domain size (number of grid cells) (unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)    :: index       !! Indices of the points on the map (unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)   :: qsintveg    !! Amount of water in the canopy interception reservoir 
                                                                       !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: snow        !! Snow water equivalent  @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in):: snow_nobio  !! Snow water equivalent on nobio areas 
                                                                       !! @tex ($kg m^{-2}$) @endtex 
    
    !! 0.4 Local variabless
    
    INTEGER(i_std)                                      :: ji          !! Grid-cell indices (unitless)
    REAL(r_std)                                         :: watveg      !! Total amount of intercepted water in a grid-cell 
                                                                       !! @tex ($kg m^{-2}$) @endtex 
!_ ================================================================================================================================

    !! 1. Initialize water in terrestrial grid-cells at the beginning of the first time step

    !! Initialize the amounts of water in terrestrial grid-cells at the beginning of the first time step, 
    !! for the three water reservoirs (interception, soil and snow). 
    tot_watveg_beg(:) = zero
    tot_watsoil_beg(:) = zero
    snow_beg(:)        = zero 
    
    DO ji = 1, kjpindex
       watveg = SUM(qsintveg(ji,:))
       tot_watveg_beg(ji) = watveg
       tot_watsoil_beg(ji) = mean_bqsb(ji) + mean_gqsb(ji)
       snow_beg(ji)        = snow(ji)+ SUM(snow_nobio(ji,:))  
    ENDDO
    
    tot_watveg_end(:) = tot_watveg_beg(:)
    tot_watsoil_end(:) = tot_watsoil_beg(:)
    snow_end(:)        = snow_beg(:)
    
  END SUBROUTINE hydrolc_alma_init

!! ================================================================================================================================
!! SUBROUTINE  : hydrolc_alma 
!!
!>\BRIEF       This routine computes diagnostic variables required under the ALMA standards:
!! changes in water amounts over the time steps (in the soil, snow and interception reservoirs); total water amount 
!! at the end of the time steps; soil relative humidity.
!!
!! DESCRIPTION : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!  - soilwet: mean relative humidity of soil in the grid-cells (0-1, unitless)
!!  - delsoilmoist, delswe, delintercept: changes in water amount during the time step @tex ($kg m^{-2}$) @endtex, 
!!    for the three water reservoirs (soil,snow,interception)
!!  - tot_watsoil_end: total water amount at the end of the time steps aincluding the three water resrevoirs  
!! @tex ($kg m^{-2}$) @endtex. 
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n   
!_ ================================================================================================================================   
  
  SUBROUTINE hydrolc_alma (kjpindex, index, qsintveg, snow, snow_nobio, soilwet)
    
  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT (in)                         :: kjpindex    !! Domain size (number of grid cells) (unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)    :: index       !! Indices of the points on the map (unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)   :: qsintveg    !! Amount of water in the canopy interception reservoir 
                                                                       !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: snow        !! Snow water equivalent  @tex ($kg m^{-2}$) @endtex 
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in):: snow_nobio  !! Snow water equivalent on nobio areas 
                                                                       !! @tex ($kg m^{-2}$) @endtex 
    
    !! 0.2 Output variables
    
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: soilwet     !! Mean relative humidity of soil in the grid-cells 
                                                                       !! (0-1, unitless)
    
    !! 0.3 Modified variables

    !! 0.4 Local variabless
    
    INTEGER(i_std)                                      :: ji          !! Grid-cell indices (unitless)
    REAL(r_std)                                         :: watveg      !! Total amount of intercepted water in a grid-cell 
                                                                       !! @tex ($kg m^{-2}$) @endtex 
!_ ================================================================================================================================

  !! 1. We calculate the required variables at the end of each time step 
    
    tot_watveg_end(:) = zero
    tot_watsoil_end(:) = zero
    snow_end(:) = zero
    delintercept(:) = zero
    delsoilmoist(:) = zero
    delswe(:) = zero
    
    DO ji = 1, kjpindex
       watveg = SUM(qsintveg(ji,:))
       tot_watveg_end(ji) = watveg
       tot_watsoil_end(ji) = mean_bqsb(ji) + mean_gqsb(ji)
       snow_end(ji) = snow(ji)+ SUM(snow_nobio(ji,:))
       !  
       delintercept(ji) = tot_watveg_end(ji) - tot_watveg_beg(ji)
       delsoilmoist(ji) = tot_watsoil_end(ji) - tot_watsoil_beg(ji)
       delswe(ji)       = snow_end(ji) - snow_beg(ji)
       !
    ENDDO
    
  !! 2. Transfer the total water amount at the end of the current timestep to the begining of the next one.
    
    tot_watveg_beg = tot_watveg_end
    tot_watsoil_beg = tot_watsoil_end
    snow_beg(:) = snow_end(:)
    
  !! 3. We calculate the mean relative humidity of soil in the grid-cells
    DO ji = 1,kjpindex
       IF ( mx_eau_var(ji) > 0 ) THEN
          soilwet(ji) = tot_watsoil_end(ji) / mx_eau_var(ji)
       ELSE
          soilwet(ji) = zero
       ENDIF
    ENDDO
    
  END SUBROUTINE hydrolc_alma
 
  
!! ================================================================================================================================
!! SUBROUTINE     : hydrolc_hdiff 
!!
!>\BRIEF          This subroutine performs horizontal diffusion of water between each PFT/soil column, 
!! if the flag ok_hdiff is true. 
!!
!! DESCRIPTION    : The diffusion is realized on the water contents of both the top and bottom 
!! soil layers, but the bottom soil layers share the same soil moisture since the end of hydrolc_soil (step 6).
!! This subroutine thus only modifies the moisture of the top soil layer.
!!  \latexonly
!!  \input{hdiff.tex}
!!  \endlatexonly
!!
!! RECENT CHANGE(S) : None
!!      
!! MAIN OUTPUT VARIABLE(S) : gqsb, bqsb, dsg, dss, dsp
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrolc_hdiff(kjpindex, veget, tot_bare_soil, ruu_ch, gqsb, bqsb, dsg, dss, dsp)

  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT (in)                          :: kjpindex         !! Domain size  (number of grid cells) (unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)    :: veget            !! Grid-cell fraction effectively covered by 
                                                                             !! vegetation for each PFT, except for soil 
                                                                             !! (0-1, unitless) 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)        :: tot_bare_soil    !! Total evaporating bare soil fraction 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: ruu_ch           !! Volumetric soil water capacity 
                                                                             !! @tex ($kg m^{-3}$) @endtex
    
    !! 0.2 Output variables

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout) :: gqsb             !! Water content in the top layer 
                                                                             !! @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout) :: bqsb             !! Water content in the bottom layer 
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout) :: dsg              !! Depth of the top layer (m) 
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout) :: dss              !! Depth of dry soil at the top, whether in the top
                                                                             !! or bottom layer (m)
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout) :: dsp              !! Depth of dry soil in the bottom layer plus depth
                                                                             !! of top layer (m)
    
    !! 0.4 Local variables

    REAL(r_std), DIMENSION (kjpindex)                    :: bqsb_mean        !! Mean value of bqsb in each grid-cell 
                                                                             !! @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex)                    :: gqsb_mean        !! Mean value of gqsb in each grid-cell 
                                                                             !! @tex ($kg m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex)                    :: dss_mean         !! Mean value of dss in each grid-cell (m)
    REAL(r_std), DIMENSION (kjpindex)                    :: vegtot           !! Total fraction of grid-cell covered by PFTs (bare
                                                                             !! soil + vegetation) (0-1, unitless) 
                                                                             ! *** this variable is declared at the module level
    REAL(r_std)                                          :: x                !! Coefficient of diffusion by time step 
                                                                             !! (0-1, unitless)
    INTEGER(i_std)                                       :: ji,jv            !! Indices for grid-cells and PFTs (unitless)
    REAL(r_std), SAVE                                    :: tau_hdiff        !! Time scale for horizontal water diffusion (s) 
!$OMP THREADPRIVATE(tau_hdiff)
    LOGICAL, SAVE                                        :: lstep_init=.TRUE. !! Flag to set tau_hdiff at the first time step 
!$OMP THREADPRIVATE(lstep_init)
!_ ================================================================================================================================

  !! 1. First time step: we assign a value to the time scale for horizontal water diffusion (in seconds) 
    
    IF ( lstep_init ) THEN

      !Config Key   = HYDROL_TAU_HDIFF
      !Config Desc  = time scale (s) for horizontal diffusion of water
      !Config Def   = one_day
      !Config If    = HYDROL_OK_HDIFF
      !Config Help  = Defines how fast diffusion occurs horizontally between
      !Config         the individual PFTs' water reservoirs. If infinite, no
      !Config         diffusion.
      !Config Units = [seconds]
      tau_hdiff = one_day   ! 86400 s
      CALL getin_p('HYDROL_TAU_HDIFF',tau_hdiff)

      WRITE (numout,*) 'Hydrol: Horizontal diffusion, tau (s)=',tau_hdiff

      lstep_init = .FALSE.

    ENDIF

  !! 2. We calculate mean values of bqsb, gqsb and dss over the grid-cell ("bio" fraction).

    ! Calculate mean values of bqsb, gqsb and dss over the grid-cell ("bio" fraction)
    ! This could be done with SUM instruction but this kills vectorization
    ! *** We compute here vegtot=SUM(veget), but it is already defined at a higher level (declared in the module) ***
    bqsb_mean(:) = zero
    gqsb_mean(:) = zero
    dss_mean(:) = zero
    vegtot(:) = zero
    !
    DO jv = 1, nvm
      DO ji = 1, kjpindex
!MM veget(:,1) BUG ??!!!!!!!!!!!
           IF (jv .EQ. 1) THEN
              bqsb_mean(ji) = bqsb_mean(ji) + tot_bare_soil(ji)*bqsb(ji,jv)
              gqsb_mean(ji) = gqsb_mean(ji) + tot_bare_soil(ji)*gqsb(ji,jv)
              dss_mean(ji) = dss_mean(ji) + tot_bare_soil(ji)*dss(ji,jv)
              vegtot(ji) = vegtot(ji) + tot_bare_soil(ji)
           ELSE
              bqsb_mean(ji) = bqsb_mean(ji) + veget(ji,jv)*bqsb(ji,jv)
              gqsb_mean(ji) = gqsb_mean(ji) + veget(ji,jv)*gqsb(ji,jv)
              dss_mean(ji) = dss_mean(ji) + veget(ji,jv)*dss(ji,jv)
              vegtot(ji) = vegtot(ji) + veget(ji,jv)
           ENDIF
      ENDDO
    ENDDO
 
    DO ji = 1, kjpindex
      IF (vegtot(ji) .GT. zero) THEN
        bqsb_mean(ji) = bqsb_mean(ji)/vegtot(ji)
        gqsb_mean(ji) = gqsb_mean(ji)/vegtot(ji)
        dss_mean(ji) = dss_mean(ji)/vegtot(ji)
      ENDIF
    ENDDO

  !! 3.  Relax PFT values towards grid-cell mean
   
    !! Relax values towards mean : the diffusion is proportional to the deviation to the mean,
    !!  and inversely proportional to the timescale tau_hdiff
    !!  We change the moisture of two soil layers, but also the depth of dry in the top soil layer. 
    !!  Therefore, we need to accordingly adjust the top soil layer depth 
    !!  and the dry soil depth above the bottom moisture (dsp).
    ! *** This sequence does not seem to be fully consistent with the variable definitions, 
    ! *** especially for the dry soil depths dss and dsp:
    ! *** (i) is the "diffusion" of dss correct is some PFTs only have one soil layer (dss=dsp), 
    ! *** given that hydrolc_soil acted to merge the bottom soil moistures bqsb
    ! *** (ii) if gqsb is very small, we let the top layer vanish, but dss is not updated to dsp 
    !
    ! *** Also, it would be make much more sense to perform the diffusion in hydrolc_soil, when the 
    ! *** bottom soil moistures are homogenized. It would benefit from the iterative consistency 
    ! *** check between the dsg and dsp, and it would prevent from doing some calculations twice. 
    ! *** Finally, it would be better to keep the water balance calculation for the real end of the 
    ! *** hydrological processes.
    !    
    ! *** FORTUNATELY, the default value of ok_hdiff is false (hydrolc_init.f90)
    x = MAX( zero, MIN( dt_sechiba/tau_hdiff, un ) )
    
    DO jv = 1, nvm
      DO ji = 1, kjpindex

        ! *** bqsb does not change as bqsb(ji,jv)=bqsb_mean(ji) since hydrolc_soil, step 6
        bqsb(ji,jv) = (un-x) * bqsb(ji,jv) + x * bqsb_mean(ji) 
        gqsb(ji,jv) = (un-x) * gqsb(ji,jv) + x * gqsb_mean(ji)

        ! *** is it meaningful to apply the diffusion equation to dss ?
        dss(ji,jv) = (un-x) * dss(ji,jv) + x * dss_mean(ji) 
        
        IF (gqsb(ji,jv) .LT. min_sechiba) THEN
          dsg(ji,jv) = zero ! *** in this case, we should also set dss to dsp (defined below)
        ELSE
          dsg(ji,jv) = (dss(ji,jv) * ruu_ch(ji) + gqsb(ji,jv)) / ruu_ch(ji)
        ENDIF
        dsp(ji,jv) = zmaxh - bqsb(ji,jv) / ruu_ch(ji) ! no change here since bqsb does not change

      ENDDO
    ENDDO

  END SUBROUTINE hydrolc_hdiff
  
END MODULE hydrolc