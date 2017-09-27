! =================================================================================================================================
! MODULE       : grassland_grazing
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see
! ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF       This module excute grazing practice of
!! grassland management, (1) initialize variables used in grazing,
!! (2) calculate energy requirement of animal, (3) calculate
!! animal intake, (4) calculate biomass change and animal 
!! trampling, (5) calculate milk/meat production, 
!! (6) calculate animal respiration and enteric fermentation 
!! methane emission, (7) calculate animal excreta (manure/urine),
!! (8) write animal related output 
!!
!!\n DESCRIPTION : None
!!
!! RECENT CHANGE(S) : None
!!
!! REFERENCE(S) : None
!!
!! \n
!_
!================================================================================================================================
MODULE grassland_grazing

  USE xios_orchidee
  USE grassland_fonctions
  USE grassland_constantes
  USE stomate_data
  USE constantes
  USE ioipsl
  USE ioipsl_para
!  USE parallel

  IMPLICIT NONE 

  PUBLIC animal_clear

  LOGICAL, SAVE :: l_first_Animaux        = .TRUE.  
  REAL(r_std), PARAMETER :: fnurine        = 0.6
  ! repartition de n dans l'urine et les fèces (-)
  REAL(r_std), PARAMETER :: kintake        = 1.0
  ! parameter zu intake (m**2/m**2)
  REAL(r_std), PARAMETER :: fmethane       = 0.03
  ! c-pertes en méthane (-)
  REAL(r_std), PARAMETER :: AnimalqintakeM = 3.0
  REAL(r_std), PARAMETER :: franimal       = 0.5
  ! c-pertes en respiration (-)

  ! parameter subroutine :: grazing_fonction
  REAL(r_std), PARAMETER :: rf1 = 0.17
  REAL(r_std), PARAMETER :: rf3 = 0.22
  REAL(r_std), PARAMETER :: rf7 = 0.36
  REAL(r_std ), PARAMETER :: t_seuil_OMD = 288.15
  ! threshold temperature for calculation of temperature effect on OMD (K)
!gmjc 05Feb2016 avoid wet grazing
  REAL(r_std), PARAMETER :: ct_threshold = 10.0
  REAL(r_std), PARAMETER :: ct_max = 12
  REAL(r_std), PARAMETER :: moi_threshold = 0.99
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!! Variables locales au module
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: milk
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: milkn
  ! n dans le lait (kg n /(m**2*d))
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: milkc
  ! c dans le lait (kg c /(m**2*d))     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: ranimal
  ! c perte en respiration (kg c /(m**2*d))
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: Methane
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: faecesnsumprev
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: milkndaily  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: faecesndaily
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: urinendaily  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: milksum
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: nelgrazingsum  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: milkcsum                       
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: ranimalsum                     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: Methanesum            
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: urinecsum 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: faecescsum   
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: faecesnsum
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: urinensum 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: milknsum                       
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: milknsumprev  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: urinensumprev 
  INTEGER(i_std)   , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: stockingstart
  INTEGER(i_std)   , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: stockingend
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: wshtotstart   
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: grazingc
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: grazingn
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: forage_complementc
  ! C flux associated to complemtation with forage and concentrate (kg C m-2 d-1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: forage_complementn
  ! N flux associated to complemtation with forage and concentrate (kg C m-2 d-1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: forage_complementcsum
  ! C flux associated to complemtation with forage and concentrate (kg C m-2 d-1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: forage_complementnsum
  ! N flux associated to complemtation with forage and concentrate (kg C m-2 d-1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: grazingsum
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: grazingcsum
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: grazingnsum       
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: grazingnsumprev       
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: grazingndaily     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: methane_ani
  ! Enteric methane emission per animal(kg C animal-1 d-1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: methane_aniSum
  ! Annual enteric methane emission per animal(kg C animal-1 )
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: milkanimalSum
  ! Annual milk production per animal(kg C animal-1 )
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: milkanimal
  INTEGER(i_std)     , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: ugb
  ! equals 0 (no animals) or 1 (animals)
  INTEGER(i_std)     , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: ok_ugb
  ! 1 if autogestion is optimal; 0 else
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: extra_feed
  ! Forage necessary to feed animals at barn when stocking rate autogestion (kg DM m-2)

  !local module Variables for cow (npts,2) for young and adult cow
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  Wanimalcow
  ! Animal liveweight (kg/animal) (young:1, adult:2)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  BCScow
  ! Body score condition cow (young in first, and adult in second) (/5)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  BCScow_prev
  ! previous Body score condition cow (young in first, and adult in second) (/5)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  AGEcow
  ! Age of cow (necessary for dairy cow and not necessary for suckler cow) (month)

  !Local modul variable for complementation
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  Forage_quantity_period
  ! forage quantity for the current grazing period (Kg/Animal/d)

  !local module variable for milk productivity cow
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  MPcowsum
  ! Annual milk production of cows (young in first, and adult in second)(kg/y) 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  MPcow2sum
  ! Annual milk production of a cow (young in first, and adult in second)(kg/animal/d)     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  MPcow2_prec
  ! Daily actual milk production per animal for primiparous or multiparous cows at previous time step (kg/animal/d)


  !local modul variable for Bilan N C cow 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  MPcowN
  ! N in daily milk production per m2 for primiparous or multiparous cows (kgN/m2/d)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  MPcowC
  ! C in daily milk production per m2 for primiparous or multiparous cows (kgC/m2/d)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  MPcowCsum
  ! Cumulated C in milk production per m2 for primiparous or multiparous cows (kgC/m2)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  MPcowNsum
  ! Cumulated N in milk production per m2 for primiparous or multiparous cows (kgN/m2)

  !Intake cow
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  DMIcowsum
  ! Cumulated intake per m2 for primiparous or multiparous cows(kg/m2)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  DMIcowNsum
  ! N in Cumulated intake per m2 for primiparous or multiparous cows(kgN/m2)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  DMIcowCsum
  ! C in Cumulated intake per m2 for primiparous or multiparous cows(kgC/m2)    
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   ::  DMIcowanimalsum
  ! Cumulated animal intake for primiparous or multiparous cows(kg/animal)
  !local module variable for calves
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  Wanimalcalf
  ! Calf liveweigth (kg/animal)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  DMIcalfsum
  ! Cumulated calf intake per m2(kg/m2)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  DMIcalfnsum
  ! N in cumulated calf intake per m2(kgN/m2)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  DMIcalfanimalsum
  ! Cumulated calf intake per animal kg/animal)  

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  Tcalving
  ! Calving date (d)  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  Tsevrage
  ! Suckling period of calves (d)  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  Age_sortie_calf
  ! Calf age at sale (d)  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  Pyoung
  ! Fraction of young or primiparous in the cattle (-)  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  Wcalfborn
  ! Calf liveweigth at birth (kg/animal)
  INTEGER,      ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  calfinit
  ! Boolean to calf weight computation
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  Wanimalcalfinit
  ! Initial calf liveweigth (kg/animal) (birth liveweight or liveweight at the beginning of the grazing period)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   ::  nanimaltot_prec
  ! nanimaltot at previous time step (animal/m2)

  INTEGER(i_std)     , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: Gestation
  ! equals 0 (outside of the gestation period) or 1 (during gestation)
  INTEGER(i_std)     , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: Calf
  ! equals 0 (when calves are sale or at barn) or 1 (when calves are at pasture)

  INTEGER(i_std)     , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: compte_pature
  ! Number of the pasture periode when stocking rate automanagement
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: autogestion_weightcow
  ! Initial cow liveweight when stocking rate automanagement (kg/animal)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: autogestion_BCScow
  ! Initial BCS when stocking rate automanagement (-)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: autogestion_AGEcow
  ! Initial age when stocking rate automanagement (months)
  INTEGER(i_std)     , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: autogestion_init
  ! to intialize cow liveweight and BCS the first time step when f_autogestion=2
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: QIc
  ! to intialize concentrate amount per kg of milk per day or per kg of Liveweight per day (Kg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: EVf
  ! to intialize forage energy content  (UF/kg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: FVf
  ! to intialize forage fill value  (UE/kg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: EVc
  ! to intialize concentrate energy content(UF/kg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: fN_forage
  ! Nitrogen fraction in the forage (kgN/kg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: fN_concentrate
  ! Nitrogen fraction in the concentrate (kgN/kg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: NEBcow_prec
  ! Net energy Balance at previous time step (young:1, mature:2)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: MPwmax
  ! Maximum of theoretical milk production (kg/animal/d)
  INTEGER(i_std)     , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: Fday_pasture
  ! the first julian day of the actual pasture periode
  INTEGER(i_std)     , ALLOCATABLE, SAVE, DIMENSION(:,:)   :: delai_ugb
  ! time before start grazing is possible
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: Local_autogestion_out
  ! Fraction F (npts,1), ratio F (npts,2), and lenght of the grazing period when autgestion
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: PEmax
  ! Perte d'etat maximale des vaches laitières sur la periode de paturage 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: PEpos
  ! Perte d'etat possible des vaches laitières au jour j
  REAL(r_std),              SAVE                 :: BM_threshold
  ! Biomass threshold above which animals are moved out the paddock (kg/m2)
  REAL(r_std),              SAVE                 :: BM_threshold_turnout
  ! [autogestion] Biomass threshold above which animals are moved in the paddock (kg/m2)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: DMIc
  ! concentrate ingested with auto-complementation (dairy cow only)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: DMIf
  ! forage ingested with auto-complementation (suckler cow only)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: NER
  ! Net energy requirement (MJ)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: Substrate_grazingwc
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: Substrate_grazingwn
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: grazingcstruct
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: grazingnstruct

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: DNDFlam
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: DNDF
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: NDF
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: DNDFI
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: DNDFstem
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: DNDFear
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: NDFmean
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: plam
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: pstem
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: pear
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: MassePondTot
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: grazingstruct
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: grazinglam
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: grazingstem
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: grazingear

!  REAL(r_std),ALLOCATABLE,    SAVE , DIMENSION(:,:) :: nb_grazingdays
  REAL(r_std),ALLOCATABLE,    SAVE , DIMENSION(:,:) :: amount_yield
  REAL(r_std),ALLOCATABLE,    SAVE , DIMENSION(:,:) :: consump
  REAL(r_std),ALLOCATABLE,    SAVE , DIMENSION(:,:) :: outside_food
  REAL(r_std),ALLOCATABLE,    SAVE , DIMENSION(:,:) :: add_nb_ani
!gmjc
  REAL(r_std),ALLOCATABLE,    SAVE , DIMENSION(:,:) :: ct_dry
! counter determine the days of wet/dry soil
  REAL(r_std), SAVE    :: buffer_snow = 3
  REAL(r_std), SAVE    :: buffer_wet = 0.05
  ! flag that disable grazing by snowmass default FALSE = no impact
  LOGICAL, SAVE :: avoid_snowgrazing
  ! flag that disable grazing by wet soil default FALSE = no impact
  LOGICAL, SAVE :: avoid_wetgrazing
  ! flag that disable grazing by low air temperature < 273.15K default FALSE =
  ! no impact
  LOGICAL, SAVE :: avoid_coldgrazing
  REAL(r_std),ALLOCATABLE,    SAVE , DIMENSION(:) :: t2m_below_zero
!end gmjc
  REAL(r_std), SAVE    ::   DNDFlam1             = 0.92
  REAL(r_std), SAVE    ::   DNDFlam2             = 0.82
  REAL(r_std), SAVE    ::   DNDFlam3             = 0.76
  REAL(r_std), SAVE    ::   DNDFlam4             = 0.74

  REAL(r_std), ALLOCATABLE,    SAVE , DIMENSION(:,:)    ::   NDFlam    !0.6
  REAL(r_std), ALLOCATABLE,    SAVE , DIMENSION(:,:)    ::   NDFstem   !0.7
  REAL(r_std), ALLOCATABLE,    SAVE , DIMENSION(:,:)    ::   NDFear    !0.8

  REAL(r_std), SAVE    ::   DNDFstem1             = 0.84
  REAL(r_std), SAVE    ::   DNDFstem2             = 0.65
  REAL(r_std), SAVE    ::   DNDFstem3             = 0.53
  REAL(r_std), SAVE    ::   DNDFstem4             = 0.50

  REAL(r_std), SAVE    ::   DNDFear1             = 0.76
  REAL(r_std), SAVE    ::   DNDFear2             = 0.48
  REAL(r_std), SAVE    ::   DNDFear3             = 0.30
  REAL(r_std), SAVE    ::   DNDFear4             = 0.26

  REAL(r_std), SAVE    ::   LimDiscremine        = 0.10
  
  INTEGER(i_std)                  , SAVE                 :: mgraze_C3
  INTEGER(i_std)                  , SAVE                 :: mgraze_C4
  INTEGER(i_std)                  , SAVE                 :: mnatural_C3
  INTEGER(i_std)                  , SAVE                 :: mnatural_C4

  REAL(r_std), ALLOCATABLE,    SAVE , DIMENSION(:,:)      :: able_grazing

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!! FONCTION PRINCIPALE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  SUBROUTINE Animaux_main(&
     npts                      , &
     dt                        , &
     devstage                  , &
     wsh                       , &
     intakemax                 , &
     snowfall_daily            , &
     wshtot                    , &
     Animalwgrazingmin         , &
     AnimalkintakeM            , &
     nel                       , &
     wanimal                   , &
     nanimaltot                , &
     ntot                      , &
     intake                    , &
     urinen                    , &
     faecesn                   , &
     urinec                    , &
     faecesc                   , &
     tgrowth                   , &
     new_year                  , &
     new_day                   , &
     nanimal                   , &
     tanimal                   , &
     danimal                   , &
     tcutmodel                 , &
     tjulian                   , &
     import_yield              , &
     intakesum                 , &
     intakensum                , &
     fn                        , &
     c                         , &
     n                         , &
     leaf_frac                 , &
     intake_animal             , &
     intake_animalsum          , &
     biomass,trampling,sr_ugb,sr_wild,   &
     compt_ugb,nb_ani,grazed_frac, &
     AnimalDiscremineQualite,    &
     YIELD_RETURN,sr_ugb_init,   &
     year_count1,year_count2,    &
     grazing_litter, litter_avail_totDM, &
     intake_animal_litter, intake_litter, &
     nb_grazingdays, &
!gmjc top 5 layer grassland soil moisture for grazing
     moiavail_daily, tmc_topgrass_daily,fc_grazing, &
     after_snow, after_wet, wet1day, wet2day, &
     snowmass_daily,t2m_daily, &
!end gmjc
     ranimal_gm, ch4_pft_gm, Fert_PRP)
    !!!!!!!!!!!!!!!!
    ! Déclaration des variables
    !!!!!!!!!!!!!!!!

    INTEGER(i_std)                            , INTENT(in)    :: npts
    REAL(r_std)                               , INTENT(in)    :: dt
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(in)    :: devstage
    ! stade de développement
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(in)    :: wsh
    ! totalité de masse sèche structurelle des pousses  (kg/m**2)  ----> total structural dry mass of shoots
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(in)    :: intakemax
    ! Potential eating rate of lactating cows (kg/(GVE*m**2)       ----> potential intake
    REAL(r_std), DIMENSION(npts)              , INTENT(in)    :: snowfall_daily
    ! neige                                                        ----> snow  
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(in)    :: wshtot
    ! totalité de masse sèche  de la pousse (kg/m**2)              ----> total dry mass of the shoots
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(in)    :: Animalwgrazingmin
    !  ????----> LiLH
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(in)    :: AnimalkintakeM
    !  ????----> LiLH
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: nel
    ! énergie nette de lactation (mj/kg)
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(in)    :: wanimal
    ! weight of lactating cows (kg)
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(inout) :: nanimaltot
    ! densité de paturage (gve/m**2)
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(in)    :: ntot
    ! concentration totale en n 
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: intake
    ! intake
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: urinen
    ! n dans l'urine (kg n /(m**2 d))     
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: faecesn
    ! n dans les fèces (kg n /(m**2*d)) 
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: urinec
    ! c dans les urines
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: faecesc
    ! c dans les fèces (kg c /(m**2*d))
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(in)    :: tgrowth
    ! instant de la repousse
    LOGICAL                                   , INTENT(in)    :: new_year
    LOGICAL                                   , INTENT(in)    :: new_day                            
    INTEGER(i_std)                            , INTENT(in)    :: tcutmodel
    ! flag for management 
    INTEGER(i_std)                            , INTENT(in)    :: tjulian
    ! day julian
    REAL(r_std), DIMENSION(npts,nvm,nstocking), INTENT(in)    :: nanimal
    ! densité du paturage  h (1,..,nstocking) (gve/m**2)
    REAL(r_std), DIMENSION(npts,nvm,nstocking), INTENT(in)    :: tanimal
    ! début du paturage    h (1,..,nstocking) (d)       
    REAL(r_std), DIMENSION(npts,nvm,nstocking), INTENT(in)    :: danimal
    ! durée du paturage    h (1,..,nstocking) (d)       
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)    :: import_yield
    ! rendement de la prairie fauchee (g m-2 yr-1) (autogestion NV runs saturant nonlimitant)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  :: intakesum
    ! Yearly intake (kg animal-1 y-1)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  :: intakensum
    ! N in daily intake per m2(kgN/m2)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)     :: fn
    ! nitrogen in structural dry matter
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)     :: n
    ! nitrogen substrate concentration in plant,(kg n/kg)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)     :: c
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout):: leaf_frac
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)    :: intake_animal
    ! Daily intake per animal(kg animal-1 d-1)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  :: intake_animalsum
    ! Yearly intake per animal(kg animal-1 d-1)
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout):: biomass
    ! totalité de masse sèche du shoot(kg/m**2)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out):: trampling
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  sr_ugb
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  sr_wild
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  compt_ugb
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  nb_ani
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  grazed_frac
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  ::  AnimalDiscremineQualite
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  YIELD_RETURN
    REAL(r_std), DIMENSION(npts), INTENT(in)  ::  sr_ugb_init
    INTEGER(i_std)                              , INTENT(in)    :: year_count1
    INTEGER(i_std)                              , INTENT(in)    :: year_count2
    !gmjc for autogestion 5 grazing AGB and litter
    ! flag determine grazing litter (1) or AGB (0)
    INTEGER(i_std), DIMENSION(npts,nvm), INTENT(inout)  ::  grazing_litter
    ! available litter for grazing (exclude litter from manure) kg/DM/m^2
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  ::  litter_avail_totDM 
    ! daily animal intake per LSU 10 kgDM/LSU/day
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)  ::  intake_animal_litter 
    ! animal intake kgDM/m^2/day
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)  ::  intake_litter
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  :: nb_grazingdays
    !end gmjc
!gmjc top 5 layer grassland soil moisture for grazing
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  ::  moiavail_daily
    REAL(r_std),DIMENSION (npts), INTENT(in)       :: tmc_topgrass_daily
    REAL(r_std),DIMENSION (npts), INTENT(in)       :: fc_grazing
    REAL(r_std),DIMENSION (npts), INTENT(inout)    :: after_snow
    REAL(r_std),DIMENSION (npts), INTENT(inout)    :: after_wet
    REAL(r_std),DIMENSION (npts), INTENT(inout)    :: wet1day
    REAL(r_std),DIMENSION (npts), INTENT(inout)    :: wet2day
    REAL(r_std),DIMENSION (npts), INTENT(in)       :: snowmass_daily
    REAL(r_std),DIMENSION (npts), INTENT(in)       :: t2m_daily
!end gmjc
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  :: ranimal_gm
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  :: ch4_pft_gm
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  :: Fert_PRP

    INTEGER(i_std)                          :: h,i,j,k
    REAL(r_std), DIMENSION(npts)      :: xtmp_npts
    REAL(r_std), DIMENSION(npts,nvm)      :: wshtotgrazing
    REAL(r_std), DIMENSION(npts,nvm)      :: deltaanimal

    INTEGER(i_std)                          :: type_animal    
    ! local Variables:

    REAL(r_std)     , DIMENSION(npts,nvm)   :: nb_ani_old
    ! Actual stocking rate per ha of total pasture "D" at previous iteration (animal (ha of total grassland)-1)
    REAL(r_std)     , DIMENSION(npts,2) :: tampon
    REAL(r_std), DIMENSION(npts,nvm)            :: wshtotinit

    tampon=0.0


    ! 1 initialisation 
    init_animal : IF (l_first_animaux) THEN

      IF (blabla_pasim) PRINT *, 'PASIM Animals : initialisation'

      avoid_wetgrazing = .FALSE.
      CALL getin_p('GRM_AVOID_WETGRAZING',avoid_wetgrazing)
      WRITE (numout,*) 'avoid_wetgrazing',avoid_wetgrazing

      avoid_snowgrazing = .TRUE.
      CALL getin_p('GRM_AVOID_SNOWGRAZING',avoid_snowgrazing)
      WRITE (numout,*) 'avoid_snowgrazing',avoid_snowgrazing
      avoid_coldgrazing = .TRUE.
      CALL getin_p('GRM_AVOID_COLDGRAZING',avoid_coldgrazing)
      WRITE (numout,*) 'avoid_coldgrazing',avoid_coldgrazing

      CALL Animal_Init(npts, nanimal , type_animal , intake_tolerance)

      CALL variablesPlantes(&
           npts,biomass,&
           c,n,intake_animal,intakemax,&
           AnimalDiscremineQualite)
    END IF init_animal

    ! 2 at the end of year EndOfYear 
    ! updating grazing variables for restart and/or next year
    n_year : IF (new_year .EQ. .TRUE. ) THEN

      IF (blabla_pasim) PRINT *, 'PASIM Animals : initialisation pour une nouvelle année'

      ! 2.1 initialize variables 
      ! not necessary for trunk restart every year
      nanimaltot   = 0.0
      faecesnsum     = 0.0
      faecesnsumprev = 0.0
      milksum        = 0.0
      nelgrazingsum  = 0.0
      milkcsum       = 0.0
      ranimalsum     = 0.0
      MethaneSum     = 0.0
      faecescsum     = 0.0
      urinecsum      = 0.0
      faecesnsum     = 0.0
      urinensum      = 0.0
      urinensumprev  = 0.0
      milknsum       = 0.0
      milknsumprev   = 0.0
      stockingstart  = 0
      stockingend    = 0
      grazingnsum    = 0.0
      grazingcsum    = 0.0
      grazingnsumprev= 0.0
      grazingsum     = 0.0
      intake_animalsum = 0.0
      intakesum      = 0.0
      intakensum      = 0.0
      milkanimalsum = 0.0
      milkanimal    = 0.0
      methane_aniSum= 0.0

      ugb                   = 0
!JCcomment for not start immidiently 
!      delai_ugb             = -1
!        print *,  'min_grazing', min_grazing
      YIELD_RETURN=0.0
      !************************************************
      ! modifications added by Nicolas Vuichard

      !modif ugb0azot

      !070703 AIG à confirmer
      !********* Stocking rate calculation if grazing autogestion ********** 
      ! the model will pass the loop if flag "non limitant" 
      ! The module calculates the optimal yield "Y" of a cut grassland plot,
      ! when optimizing cut events and N fertilisation.
      ! Then the model simulates the same grasslang plot with animals. Stocking rate "S"
      ! is incremented at each optimization step. For each stocking rate, the program
      ! determines the number of days for which animal in the barn (365 - compt_ugb(:))and 
      ! thus, the forage necessary to feed them at the barn "X".
      ! The fraction F of grazed pastures is calculated as: Y (1-F) - X = 0
      !                                                     F = Y /(Y+X)
      !                                                     F = 1 / (1 + X/Y)
      ! Then the program calculates the actual stocking rate per ha of grazed pasture "D",
      ! D = SF
      ! code equivalences
      ! Y = import_yield
      ! X = extra_feed
      ! S = sr_ugb
      ! F = 1 / (1 + extra_feed(:) / (import_yield * 0.85))
      ! D = nb_ani
      ! 0.85 = 1 - 0.15: pertes à la récolte
     !MODIF INN
     ! Pouvoir rentrer dans la boucle quand (f_autogestion .EQ. 2) AND (f_fertilization .EQ. 1)
      IF ((tcutmodel .EQ. 0) .AND.  (f_autogestion .EQ. 0) .AND. (f_postauto .EQ. 0)) THEN
        nb_grazingdays(:,:)=compt_ugb(:,:)
        compt_ugb(:,:) = 0
      ENDIF

      IF(f_nonlimitant .EQ. 0) THEN
          !modif nico ugb
        ! mauto_C3 mauto_C4 auto grazing
        IF (f_autogestion .EQ. 2) THEN
          DO j=2,nvm
            IF (is_grassland_manag(j) .AND. & !(.NOT.  is_c4(j)) .AND.  &
               (.NOT. is_grassland_cut(j)).AND.(.NOT.is_grassland_grazed(j)))THEN
            !equal to mauto_C3 and mauto_C4
              WHERE ((ok_ugb(:,j) .EQ. 0))
                ! import_yield has been calculated when initialize in main
                ! grassland_management
                !15.5 : amount of dry matter (Kg) per animal in stabulation
                WHERE ( import_yield(:,j) .GT. 0.0 )
                  extra_feed(:,j)  = (365 - compt_ugb(:,j)) * 18 * sr_ugb(:,j) 
                  nb_ani_old(:,j)  = nb_ani(:,j)
                  nb_ani(:,j)      = 1 / (1 + extra_feed(:,j) / (import_yield(:,j) * 0.85)) * sr_ugb(:,j)
                  grazed_frac(:,j) =  1 / (1 + extra_feed(:,j) / (import_yield(:,j) * 0.85))
                ELSEWHERE
                  nb_ani(:,j) = 0.0
                  grazed_frac(:,j) = 0.0
                  sr_ugb(:,j) =0.0
                  ok_ugb(:,j) = 1
                ENDWHERE                    
              !JCCOMMENT increment < 0.5% considering 
              ! stop adding stocking rate
                WHERE (((nb_ani(:,j)-nb_ani_old(:,j))/nb_ani(:,j)) .LT. 0.005 &
                       .AND. (grazed_frac(:,j) .LT. 0.7) .AND. &
                       (sr_ugb(:,j) .GT.0.0))
                  ok_ugb(:,j) = 1
                  sr_ugb(:,j) = sr_ugb(:,j) - 0.00001
                ! avoid all cut grassland
                ELSEWHERE (grazed_frac(:,j) .LE. 0.25)                  
                  ok_ugb(:,j) = 1
                  sr_ugb(:,j) = sr_ugb(:,j) - 0.00001
                ELSEWHERE
                  sr_ugb(:,j) = sr_ugb(:,j) + 0.00002
                END WHERE
!JCCOMMENT move the check above to make sure it will not stop too early
! e.g., still grazed_frac > 0.7 but it stoped with ok_ugb = 1  
!                WHERE ((grazed_frac(:,j) .GT. 0.7).AND.(sr_ugb(:,j) .GT.0.0))
!                  sr_ugb(:,j) = sr_ugb(:,j) + 0.00001
!                END WHERE
              END WHERE ! ok_ugb
              ! save nb_grazingdays for restart and history write
              nb_grazingdays(:,j) = compt_ugb(:,j)
              compt_ugb(:,j) = 0
            END IF ! manag + c3 or c4
          END DO ! nvm
        ENDIF ! autogestion=2
        ! f_autogestion = 3 4 5
          !modif nico ugb
      ! 3: auto cut and graze for PFT m_cut and m_grazed with increasing sr_ugb
      ! search for curve of extra_feed requirement
      ! that compared to yield from fixing fraction of harvested grassland or
      ! crop feed 
        IF (f_autogestion .EQ. 3) THEN
          WHERE ((ok_ugb(:,mgraze_C3) .EQ. 0))
            extra_feed(:,mgraze_C3)  = (365 - compt_ugb(:,mgraze_C3)) * 18 *sr_ugb(:,mgraze_C3)
            sr_ugb(:,mgraze_C3) = sr_ugb(:,mgraze_C3) + 0.00001
          END WHERE
          nb_grazingdays(:,mgraze_C3) = compt_ugb(:,mgraze_C3)
          compt_ugb(:,mgraze_C3) = 0
          WHERE ((ok_ugb(:,mgraze_C4) .EQ. 0))
            extra_feed(:,mgraze_C4)  = (365 - compt_ugb(:,mgraze_C4)) * 18 *sr_ugb(:,mgraze_C4)
            sr_ugb(:,mgraze_C4) = sr_ugb(:,mgraze_C4) + 0.00001
          END WHERE
          nb_grazingdays(:,mgraze_C4) = compt_ugb(:,mgraze_C4)
          compt_ugb(:,mgraze_C4) = 0
        ENDIF
      ! 4: auto cut and graze for PFT m_cut and m_grazed with constant sr_ugb
      ! search for extra_feed requirement with certain stocking rate
      ! under climate change or CO2 change
        IF (f_autogestion .EQ. 4) THEN
          WHERE ((ok_ugb(:,mgraze_C3) .EQ. 0))
            amount_yield(:,mgraze_C3)=import_yield(:,mgraze_C3)
            extra_feed(:,mgraze_C3)  = (365 - compt_ugb(:,mgraze_C3)) * 18*sr_ugb(:,mgraze_C3)
          END WHERE
          nb_grazingdays(:,mgraze_C3) = compt_ugb(:,mgraze_C3)
          compt_ugb(:,mgraze_C3) = 0
          WHERE ((ok_ugb(:,mgraze_C4) .EQ. 0))
            amount_yield(:,mgraze_C4)=import_yield(:,mgraze_C4)
            extra_feed(:,mgraze_C4)  = (365 - compt_ugb(:,mgraze_C4)) * 18*sr_ugb(:,mgraze_C4)
          END WHERE
          nb_grazingdays(:,mgraze_C4) = compt_ugb(:,mgraze_C4)
          compt_ugb(:,mgraze_C4) = 0
        ENDIF
      ! 5: auto graze for PFT m_grazed with grazing litter during winter for LGM
        !gmjc for grazing biomass in summer and litter in winter
        IF (f_autogestion .EQ. 5) THEN
          WHERE ((ok_ugb(:,mgraze_C3) .EQ. 0) .AND. &
           &   (compt_ugb(:,mgraze_C3) .GE. 310))
            sr_ugb(:,mgraze_C3) = sr_ugb(:,mgraze_C3) + 0.000001
          ELSEWHERE ((ok_ugb(:,mgraze_C3) .EQ. 0) .AND. &
           &   (compt_ugb(:,mgraze_C3) .LT. 300))
            sr_ugb(:,mgraze_C3) = sr_ugb(:,mgraze_C3) - 0.000001
          END WHERE
          nb_grazingdays(:,mgraze_C3) = compt_ugb(:,mgraze_C3)
          compt_ugb(:,mgraze_C3) = 0
          WHERE ((ok_ugb(:,mgraze_C4) .EQ. 0) .AND. &
           &   (compt_ugb(:,mgraze_C4) .GE. 310))
            sr_ugb(:,mgraze_C4) = sr_ugb(:,mgraze_C4) + 0.000001
          ELSEWHERE ((ok_ugb(:,mgraze_C4) .EQ. 0) .AND. &
           &   (compt_ugb(:,mgraze_C4) .LT. 300))
            sr_ugb(:,mgraze_C4) = sr_ugb(:,mgraze_C4) - 0.000001
          END WHERE
          nb_grazingdays(:,mgraze_C4) = compt_ugb(:,mgraze_C4)
          compt_ugb(:,mgraze_C4) = 0
        ENDIF          
        !end gmjc
        
        ! start selection of f_postauto
        !modif nico ugb
        ! NOTE: import_yield has been calculated in main_grassland_management
        ! just before EndOfYear here
        IF ((f_postauto .EQ. 1) .OR. (f_postauto .EQ. 2)) THEN

          WHERE ((ok_ugb(:,mgraze_C3) .EQ. 0) .AND. (sr_ugb(:,mgraze_C3) .GT. 0.0))
            extra_feed(:,mgraze_C3)  = (365 - compt_ugb(:,mgraze_C3)) * 18.0*sr_ugb(:,mgraze_C3)
            ! total yield of las year (kg DM/m^2 total grassland)
            amount_yield(:,mgraze_C3) = import_yield(:,mgraze_C3) * (1-grazed_frac(:,mgraze_C3)) * 0.85
            ! total animal indoor consumption of last year (kg DM/m^2 total grassland)       
            consump(:,mgraze_C3) = (365 - compt_ugb(:,mgraze_C3)) * 18.0 * nb_ani(:,mgraze_C3)
            ! food surplus (outside_food > 0) or deficit (outside_food < 0)
            outside_food(:,mgraze_C3) = amount_yield(:,mgraze_C3)-consump(:,mgraze_C3)
            ! farmers' decision of buy (add_nb_ani > 0) or sell (add_nb_ani < 0) animals 
            ! 0.2 means that farmers' decision will based the on the mean status
            ! of the past 5 years
            add_nb_ani(:,mgraze_C3) = outside_food(:,mgraze_C3)/ (18.0 * 365) * 0.2
            !! New animal density for total grassland
            nb_ani(:,mgraze_C3)=nb_ani(:,mgraze_C3)+add_nb_ani(:,mgraze_C3)
            !! New fraction of grazed grassland in total grassland (keep the same stocking rate)
            WHERE (sr_ugb(:,mgraze_C3) .GT. 0.0)
            grazed_frac(:,mgraze_C3)=nb_ani(:,mgraze_C3)/sr_ugb(:,mgraze_C3)
            ENDWHERE
            WHERE (sr_ugb(:,mgraze_C3) .LE. 0.0)
            grazed_frac(:,mgraze_C3)=0.0
            sr_ugb(:,mgraze_C3)=0.0
            nb_ani(:,mgraze_C3)=0.0
            ENDWHERE
            !! Threshold of fraction as least 30 % was cut 
            WHERE ((grazed_frac(:,mgraze_C3) .GT. 0.7) .AND. (sr_ugb(:,mgraze_C3) .GT. 0.0)) 
              sr_ugb(:,mgraze_C3)=sr_ugb(:,mgraze_C3)+0.00001
              grazed_frac(:,mgraze_C3)=nb_ani(:,mgraze_C3)/sr_ugb(:,mgraze_C3)
            END WHERE
            WHERE (grazed_frac(:,mgraze_C3) .GT. 1.0)
              grazed_frac(:,mgraze_C3)=1.0
            ENDWHERE            
          ELSEWHERE
            ! prevent the sr_ugb to be 0
            ! to give it possibility to re-increase
            ! especially for the first year when import_yield might be 0
            sr_ugb(:,mgraze_C3) = 1e-6
            nb_ani(:,mgraze_C3) = 5e-7
            grazed_frac(:,mgraze_C3) = 0.5
            amount_yield(:,mgraze_C3) = 0.0
            outside_food(:,mgraze_C3) = 0.0
            consump(:,mgraze_C3) = 0.0
            add_nb_ani(:,mgraze_C3) = 0.0
          END WHERE
          WHERE ((ok_ugb(:,mgraze_C4) .EQ. 0) .AND. (sr_ugb(:,mgraze_C4).GT. 0.0))
            extra_feed(:,mgraze_C4)  = (365 - compt_ugb(:,mgraze_C4)) *18.0*sr_ugb(:,mgraze_C4)
            ! total yield of las year (kg DM/m^2 total grassland)
            amount_yield(:,mgraze_C4) = import_yield(:,mgraze_C4) *(1-grazed_frac(:,mgraze_C4)) * 0.85
            ! total animal indoor consumption of last year (kg DM/m^2 total grassland)       
            consump(:,mgraze_C4) = (365 - compt_ugb(:,mgraze_C4)) * 18.0 *nb_ani(:,mgraze_C4)
            ! food surplus (outside_food > 0) or deficit (outside_food < 0)
            outside_food(:,mgraze_C4) = amount_yield(:,mgraze_C4)-consump(:,mgraze_C4)
            ! farmers' decision of buy (add_nb_ani > 0) or sell (add_nb_ani < 0) animals 
            add_nb_ani(:,mgraze_C4) = outside_food(:,mgraze_C4)/ (18.0 *365) * 0.2
            !! New animal density for total grassland
            nb_ani(:,mgraze_C4)=nb_ani(:,mgraze_C4)+add_nb_ani(:,mgraze_C4)
            !! New fraction of grazed grassland in total grassland (keep
            !the same stocking rate)
            WHERE (sr_ugb(:,mgraze_C4) .GT. 0.0)
              grazed_frac(:,mgraze_C4)=nb_ani(:,mgraze_C4)/sr_ugb(:,mgraze_C4)
            ENDWHERE
            WHERE (sr_ugb(:,mgraze_C4) .LE. 0.0)
              grazed_frac(:,mgraze_C4)=0.0
              sr_ugb(:,mgraze_C4)=0.0
              nb_ani(:,mgraze_C4)=0.0
            ENDWHERE
            !! Threshold of fraction as least 30 % was cut 
            WHERE ((grazed_frac(:,mgraze_C4) .GT. 0.7) .AND.(sr_ugb(:,mgraze_C4) .GT. 0.0))
              sr_ugb(:,mgraze_C4)=sr_ugb(:,mgraze_C4)+0.00002
              grazed_frac(:,mgraze_C4)=nb_ani(:,mgraze_C4)/sr_ugb(:,mgraze_C4)
            END WHERE
            WHERE (grazed_frac(:,mgraze_C4) .GT. 1.0)
              grazed_frac(:,mgraze_C4)=1.0
            ENDWHERE
          ELSEWHERE
            sr_ugb(:,mgraze_C4) = 1e-6
            nb_ani(:,mgraze_C4) = 5e-7
            grazed_frac(:,mgraze_C4) = 0.5
            amount_yield(:,mgraze_C4) = 0.0
            outside_food(:,mgraze_C4) = 0.0
            consump(:,mgraze_C4) = 0.0
            add_nb_ani(:,mgraze_C4) = 0.0
          END WHERE

          nb_grazingdays(:,mgraze_C3) = compt_ugb(:,mgraze_C3)
          compt_ugb(:,mgraze_C3) = 0
          nb_grazingdays(:,mgraze_C4) = compt_ugb(:,mgraze_C4)
          compt_ugb(:,mgraze_C4) = 0
        ENDIF ! f_postauto=1 or 2

        ! F_POSTAUTO=5 for global simulation with
        ! prescibed livestock density read from extra file 
        ! grazed_frac is not used
        ! but extra_feed might be used in the future
        IF (f_postauto .EQ. 5) THEN
          WHERE ((ok_ugb(:,mgraze_C3) .EQ. 0) .AND. &
                (sr_ugb(:,mgraze_C3) .GT. 0.0))
            extra_feed(:,mgraze_C3)  = (365 - compt_ugb(:,mgraze_C3)) * 18.0*sr_ugb(:,mgraze_C3)
            amount_yield(:,mgraze_C3) = import_yield(:,mgraze_C3) * (1-grazed_frac(:,mgraze_C3)) * 0.85
            consump(:,mgraze_C3) = 0.0 !(365 - compt_ugb(:,mgraze_C3)) * 18.0 * nb_ani(:,mgraze_C3)
            outside_food(:,mgraze_C3) = 0.0 !amount_yield(:,mgraze_C3)-consump(:,mgraze_C3)
            add_nb_ani(:,mgraze_C3) = 0.0 !outside_food(:,mgraze_C3)/ (18.0 * 365) * 0.2
            nb_ani(:,mgraze_C3)=nb_ani(:,mgraze_C3)+add_nb_ani(:,mgraze_C3)
            WHERE (sr_ugb(:,mgraze_C3) .GT. 0.0)
              grazed_frac(:,mgraze_C3)=0.5 !nb_ani(:,mgraze_C3)/sr_ugb(:,mgraze_C3)
            ENDWHERE
            WHERE (sr_ugb(:,mgraze_C3) .LE. 0.0)
              grazed_frac(:,mgraze_C3)=0.0
              sr_ugb(:,mgraze_C3)=0.0
              nb_ani(:,mgraze_C3)=0.0
            ENDWHERE
          ELSEWHERE
            sr_ugb(:,mgraze_C3) = 0.0
            nb_ani(:,mgraze_C3) = 0.0
            grazed_frac(:,mgraze_C3)=0.0
            amount_yield(:,mgraze_C3) =0.0
            outside_food(:,mgraze_C3) = 0.0
            consump(:,mgraze_C3) =0.0
            add_nb_ani(:,mgraze_C3) = 0.0
          END WHERE

          WHERE ((ok_ugb(:,mgraze_C4) .EQ. 0) .AND. (sr_ugb(:,mgraze_C4).GT. 0.0))
            extra_feed(:,mgraze_C4)  = (365 - compt_ugb(:,mgraze_C4)) *18.0*sr_ugb(:,mgraze_C4)
            amount_yield(:,mgraze_C4) = import_yield(:,mgraze_C4) *(1-grazed_frac(:,mgraze_C4)) * 0.85
            consump(:,mgraze_C4) = 0.0 !(365 - compt_ugb(:,mgraze_C4)) * 18.0 *nb_ani(:,mgraze_C4)
            outside_food(:,mgraze_C4) = 0.0 !amount_yield(:,mgraze_C4)-consump(:,mgraze_C4)
            add_nb_ani(:,mgraze_C4) = 0.0 !outside_food(:,mgraze_C4)/ (18.0 *365) * 0.2
            nb_ani(:,mgraze_C4)=nb_ani(:,mgraze_C4)+add_nb_ani(:,mgraze_C4)
            WHERE (sr_ugb(:,mgraze_C4) .GT. 0.0)
              grazed_frac(:,mgraze_C4)=0.5 !nb_ani(:,mgraze_C4)/sr_ugb(:,mgraze_C4)
            ENDWHERE
            WHERE (sr_ugb(:,mgraze_C4) .LE. 0.0)
              grazed_frac(:,mgraze_C4)=0.0
              sr_ugb(:,mgraze_C4)=0.0
              nb_ani(:,mgraze_C4)=0.0
            ENDWHERE
          ELSEWHERE
            sr_ugb(:,mgraze_C4) = 0.0
            nb_ani(:,mgraze_C4) = 0.0
            grazed_frac(:,mgraze_C4)=0.0
            amount_yield(:,mgraze_C4) =0.0
            outside_food(:,mgraze_C4) = 0.0
            consump(:,mgraze_C4) =0.0
            add_nb_ani(:,mgraze_C4) = 0.0
          END WHERE
          nb_grazingdays(:,mgraze_C3) = compt_ugb(:,mgraze_C3)
          compt_ugb(:,mgraze_C3) = 0
          nb_grazingdays(:,mgraze_C4) = compt_ugb(:,mgraze_C4)
          compt_ugb(:,mgraze_C4) = 0
          ! due to possible grazing by wild animal
          ! we save nb_grazingdays for possible use
          nb_grazingdays(:,mnatural_C3) = compt_ugb(:,mnatural_C3)
          compt_ugb(:,mnatural_C3) = 0
          nb_grazingdays(:,mnatural_C4) = compt_ugb(:,mnatural_C4)
          compt_ugb(:,mnatural_C4) = 0
        ENDIF ! f_postauto=5

        !! F_POSTAUTO=3 for control simulation with 
        !! constant livestock density and grazed fraction
        !! add yield_return to return extra forage to soil
        IF (f_postauto .EQ. 3)THEN
          WHERE ((ok_ugb(:,mgraze_C3) .EQ. 0) .AND. (sr_ugb(:,mgraze_C3) .GT. 0.0))
            ! total yield of las year (kg DM/m^2 total grassland)
            amount_yield(:,mgraze_C3) = import_yield(:,mgraze_C3) * (1-grazed_frac(:,mgraze_C3)) * 0.85
            ! total animal indoor consumption of last year (kg DM/m^2
            ! total grassland)                 
            consump(:,mgraze_C3) = (365 - compt_ugb(:,mgraze_C3)) * 18.0 * nb_ani(:,mgraze_C3)
            ! food surplus (outside_food > 0) or deficit (outside_food <
            ! 0)
            outside_food(:,mgraze_C3) = amount_yield(:,mgraze_C3)-consump(:,mgraze_C3)
            WHERE ((outside_food(:,mgraze_C3) .GT. 0.0 ) .AND. (grazed_frac(:,mgraze_C3) .LT. 1.0))
              YIELD_RETURN(:,mgraze_C3) = outside_food(:,mgraze_C3) / (1-grazed_frac(:,mgraze_C3))
            ELSEWHERE
              YIELD_RETURN(:,mgraze_C3)=0.0
            ENDWHERE
          ELSEWHERE
            sr_ugb(:,mgraze_C3) = 0.0
            nb_ani(:,mgraze_C3) = 0.0
            grazed_frac(:,mgraze_C3)=0.0
            amount_yield(:,mgraze_C3) =0.0
            outside_food(:,mgraze_C3) = 0.0
            consump(:,mgraze_C3) =0.0
            YIELD_RETURN(:,mgraze_C3) = 0.0
          END WHERE
          nb_grazingdays(:,mgraze_C3) = compt_ugb(:,mgraze_C3)
          compt_ugb(:,mgraze_C3) = 0

          WHERE ((ok_ugb(:,mgraze_C4) .EQ. 0) .AND. (sr_ugb(:,mgraze_C4).GT. 0.0))
            ! total yield of las year (kg DM/m^2 total grassland)
            amount_yield(:,mgraze_C4) = import_yield(:,mgraze_C4) *(1-grazed_frac(:,mgraze_C4)) * 0.85
            ! total animal indoor consumption of last year (kg DM/m^2
            ! total grassland)                 
            consump(:,mgraze_C4) = (365 - compt_ugb(:,mgraze_C4)) * 18.0 *nb_ani(:,mgraze_C4)
            ! food surplus (outside_food > 0) or deficit (outside_food <
            ! 0)
            outside_food(:,mgraze_C4) =amount_yield(:,mgraze_C4)-consump(:,mgraze_C4)
            WHERE ((outside_food(:,mgraze_C4) .GT. 0.0 ) .AND.(grazed_frac(:,mgraze_C4) .LT. 1.0))
              YIELD_RETURN(:,mgraze_C4) = outside_food(:,mgraze_C4) /(1-grazed_frac(:,mgraze_C4))
            ELSEWHERE
              YIELD_RETURN(:,mgraze_C4)=0.0
            ENDWHERE
          ELSEWHERE
            sr_ugb(:,mgraze_C4) = 0.0
            nb_ani(:,mgraze_C4) = 0.0
            grazed_frac(:,mgraze_C4)=0.0
            amount_yield(:,mgraze_C4) =0.0
            outside_food(:,mgraze_C4) = 0.0
            consump(:,mgraze_C4) =0.0
            YIELD_RETURN(:,mgraze_C4) = 0.0
          END WHERE
          nb_grazingdays(:,mgraze_C4) = compt_ugb(:,mgraze_C4)
          compt_ugb(:,mgraze_C4) = 0

        ENDIF ! f_postauto=3

        !! F_POSTAUTO=4 for historical simulation with 
        !! prescribed increased then decreased livestock density 
        !! and constant grazed fraction
        !! add yield_return to return extra forage to soil
!!!! gmjc 09Aug2016 Europe future run 1
!! with constant nb_ani, but varied grazed_frac according to varied sr_ugb
        IF (f_postauto .EQ. 4)THEN
          WHERE ((ok_ugb(:,mgraze_C3) .EQ. 0) .AND. (sr_ugb(:,mgraze_C3) .GT. 0.0))
            ! total yield of las year (kg DM/m^2 total grassland)
            amount_yield(:,mgraze_C3) = import_yield(:,mgraze_C3) * (1-grazed_frac(:,mgraze_C3)) * 0.85
            ! total animal indoor consumption of last year (kg DM/m^2
            ! total grassland)                 
            consump(:,mgraze_C3) = (365 - compt_ugb(:,mgraze_C3)) * 18.0 * nb_ani(:,mgraze_C3)
            ! food surplus (outside_food > 0) or deficit (outside_food <
            ! 0)
            outside_food(:,mgraze_C3) = amount_yield(:,mgraze_C3)-consump(:,mgraze_C3)
            ! farmers' decision of buy (add_nb_ani > 0) or sell (add_nb_ani < 0)
            ! animals
            ! 0.2 means that farmers' decision will based the on the mean status
            ! of the past 5 years
            add_nb_ani(:,mgraze_C3) = outside_food(:,mgraze_C3)/ (18.0 * 365) * 0.2
            !add_nb_ani(:,mgraze_C3) = zero
            !! New animal density for total grassland
            nb_ani(:,mgraze_C3)=nb_ani(:,mgraze_C3)!+add_nb_ani(:,mgraze_C3)
            !! New fraction of grazed grassland in total grassland (keep the
            !same stocking rate)
            WHERE (sr_ugb(:,mgraze_C3) .GT. 0.0)
            grazed_frac(:,mgraze_C3)=(nb_ani(:,mgraze_C3)+add_nb_ani(:,mgraze_C3))/sr_ugb(:,mgraze_C3)
            ENDWHERE
            WHERE (sr_ugb(:,mgraze_C3) .LE. 0.0)
            grazed_frac(:,mgraze_C3)=0.0
            sr_ugb(:,mgraze_C3)=0.0
            nb_ani(:,mgraze_C3)=0.0
            ENDWHERE
            !! Threshold of fraction as least 30 % was cut
            WHERE ((grazed_frac(:,mgraze_C3) .GT. 0.7) .AND. (sr_ugb(:,mgraze_C3) .GT. 0.0))
              sr_ugb(:,mgraze_C3)=sr_ugb(:,mgraze_C3)+0.00001
              grazed_frac(:,mgraze_C3)=nb_ani(:,mgraze_C3)/sr_ugb(:,mgraze_C3)
            END WHERE
            WHERE (grazed_frac(:,mgraze_C3) .GT. 1.0)
              grazed_frac(:,mgraze_C3)=1.0
            ENDWHERE

            YIELD_RETURN(:,mgraze_C3) = zero
!            WHERE ((outside_food(:,mgraze_C3) .GT. 0.0 ) .AND. (grazed_frac(:,mgraze_C3) .LT. 1.0))
!              YIELD_RETURN(:,mgraze_C3) = outside_food(:,mgraze_C3) / (1-grazed_frac(:,mgraze_C3))
!            ELSEWHERE
!              YIELD_RETURN(:,mgraze_C3)=0.0
!            ENDWHERE
!            sr_ugb(:,mgraze_C3) = sr_ugb_init(:) * &
!               (1+year_count1*0.0033-year_count2*0.0263)
!            nb_ani(:,mgraze_C3) = sr_ugb(:,mgraze_C3) * grazed_frac(:,mgraze_C3)
          ELSEWHERE
            sr_ugb(:,mgraze_C3) = 1e-6
            nb_ani(:,mgraze_C3) = 5e-7
            grazed_frac(:,mgraze_C3)= 0.5
            amount_yield(:,mgraze_C3) = 0.0
            outside_food(:,mgraze_C3) = 0.0
            consump(:,mgraze_C3) = 0.0
            add_nb_ani(:,mgraze_C3) = 0.0
            YIELD_RETURN(:,mgraze_C3) = 0.0
          END WHERE
          nb_grazingdays(:,mgraze_C3) = compt_ugb(:,mgraze_C3)
          compt_ugb(:,mgraze_C3) = 0

          WHERE ((ok_ugb(:,mgraze_C4) .EQ. 0) .AND. (sr_ugb(:,mgraze_C4).GT. 0.0))
            ! total yield of las year (kg DM/m^2 total grassland)
            amount_yield(:,mgraze_C4) = import_yield(:,mgraze_C4) *(1-grazed_frac(:,mgraze_C4)) * 0.85
            ! total animal indoor consumption of last year (kg DM/m^2
            ! total grassland)                 
            consump(:,mgraze_C4) = (365 - compt_ugb(:,mgraze_C4)) * 18.0 *nb_ani(:,mgraze_C4)
            ! food surplus (outside_food > 0) or deficit (outside_food <
            ! 0)
            outside_food(:,mgraze_C4) =amount_yield(:,mgraze_C4)-consump(:,mgraze_C4)
            ! farmers' decision of buy (add_nb_ani > 0) or sell (add_nb_ani < 0)
            ! animals
            add_nb_ani(:,mgraze_C4) = outside_food(:,mgraze_C4)/ (18.0 *365) *0.2
            !add_nb_ani(:,mgraze_C4) = zero
            !! New animal density for total grassland
            nb_ani(:,mgraze_C4)=nb_ani(:,mgraze_C4)!+add_nb_ani(:,mgraze_C4)
            !! New fraction of grazed grassland in total grassland (keep
            !the same stocking rate)
            WHERE (sr_ugb(:,mgraze_C4) .GT. 0.0)
              grazed_frac(:,mgraze_C4)=(nb_ani(:,mgraze_C4)+add_nb_ani(:,mgraze_C4))/sr_ugb(:,mgraze_C4)
            ENDWHERE
            WHERE (sr_ugb(:,mgraze_C4) .LE. 0.0)
              grazed_frac(:,mgraze_C4)=0.0
              sr_ugb(:,mgraze_C4)=0.0
              nb_ani(:,mgraze_C4)=0.0
            ENDWHERE
            !! Threshold of fraction as least 30 % was cut
            WHERE ((grazed_frac(:,mgraze_C4) .GT. 0.7) .AND.(sr_ugb(:,mgraze_C4) .GT. 0.0))
              sr_ugb(:,mgraze_C4)=sr_ugb(:,mgraze_C4)+0.00002
              grazed_frac(:,mgraze_C4)=nb_ani(:,mgraze_C4)/sr_ugb(:,mgraze_C4)
            END WHERE
            WHERE (grazed_frac(:,mgraze_C4) .GT. 1.0)
              grazed_frac(:,mgraze_C4)=1.0
            ENDWHERE

            YIELD_RETURN(:,mgraze_C4) = zero
!            WHERE ((outside_food(:,mgraze_C4) .GT. 0.0 ) .AND.(grazed_frac(:,mgraze_C4) .LT. 1.0))
!              YIELD_RETURN(:,mgraze_C4) = outside_food(:,mgraze_C4) /(1-grazed_frac(:,mgraze_C4))
!            ELSEWHERE
!              YIELD_RETURN(:,mgraze_C4)=0.0
!            ENDWHERE
!            sr_ugb(:,mgraze_C4) = sr_ugb_init(:) * &
!               (1+year_count1*0.0033-year_count2*0.0263)
!            nb_ani(:,mgraze_C4) = sr_ugb(:,mgraze_C4) *grazed_frac(:,mgraze_C4)
          ELSEWHERE
            sr_ugb(:,mgraze_C4) = 1e-6
            nb_ani(:,mgraze_C4) = 5e-7
            grazed_frac(:,mgraze_C4)= 0.5
            amount_yield(:,mgraze_C4) = 0.0
            outside_food(:,mgraze_C4) = 0.0
            consump(:,mgraze_C4) = 0.0
            add_nb_ani(:,mgraze_C4) = 0.0
            YIELD_RETURN(:,mgraze_C4) = 0.0
          END WHERE
          nb_grazingdays(:,mgraze_C4) = compt_ugb(:,mgraze_C4)
          compt_ugb(:,mgraze_C4) = 0
        ENDIF ! f_postauto=4

      ENDIF ! f_nonlimitant=0

    END IF n_year

    ! one per day
    n_day : IF (new_day .EQ. .TRUE. ) THEN

      IF (blabla_pasim) PRINT *, 'PASIM Animals : initialisation for new_day'

      wshtotgrazing  = wshtotstart
      faecesnsumprev = faecesnsum
      milknsumprev   = milknsum
      urinensumprev  = urinensum
      grazingnsumprev= grazingnsum
      
      able_grazing = 500.
      nanimaltot =0.0

      calc_nanimaltot  : IF ((tcutmodel .EQ. 0) .AND.  (f_autogestion .EQ. 0) &
                           .AND. (f_postauto .EQ. 0) ) THEN

        nanimaltot (:,:)  = 0.0
        h  = 1
        !DO WHILE(h .LT. nstocking)
          WHERE((tjulian .GE. tanimal(:,:,h)) .AND. &
                (tjulian .LT. (tanimal(:,:,h) + danimal(:,:,h))))

            nanimaltot (:,:) = nanimaltot (:,:) + nanimal(:,:,h)

          END WHERE
          h  = h  + 1
        !END DO

        WHERE (wshtot(:,:) .GE. (min_grazing + 0.05))
          delai_ugb(:,:) = delai_ugb(:,:) +1
          WHERE ((delai_ugb(:,:) .GE. 0) .AND. &
               (nanimaltot(:,:) .GT. 0.0))
            ugb(:,:) = 1
          ELSEWHERE
            ugb(:,:) = 0
          ENDWHERE
        ELSEWHERE ((wshtot(:,:) .LT. (min_grazing + 0.05)) .AND. &
            (wshtot(:,:) .GE. min_grazing))
          WHERE ((delai_ugb(:,:) .GE. 0) .AND. (nanimaltot(:,:) .GT. 0.0))
            ugb(:,:) = 1
          ELSEWHERE
            ugb(:,:) = 0
          ENDWHERE
        ELSEWHERE (wshtot(:,:) .LT. min_grazing)

          nanimaltot (:,:) = 0.0
          ugb(:,:)           = 0
          delai_ugb(:,:) = -15

        END WHERE
        WHERE (ugb(:,:) .EQ. 1)

            compt_ugb(:,:)  = compt_ugb(:,:) + 1
            

        END WHERE


      ELSEIF (tcutmodel .EQ. 1) THEN

        WHERE ((nanimal(:,:,1) .GT. 0.0) .AND. (devstage(:,:) .GT. devstocking) .AND. &
            (stockingstart(:,:) .EQ. 0))

            nanimaltot (:,:) = nanimal(:,:,1)
          stockingstart(:,:) = 1

        END WHERE
        DO j=2,nvm
          IF (tjulian .GT. tseasonendmin) THEN
            WHERE ((stockingstart(:,j) .EQ. 1) .AND. (stockingend(:,j) .EQ. 0) .AND. &
                (snowfall_daily(:) .GT. 0.0))

              stockingend(:,j)  = 1

            END WHERE
          END IF
        END DO
        WHERE (stockingend(:,:) .EQ. 1)

            nanimaltot (:,:)  = 0.0

        ELSEWHERE ( (nanimal(:,:,1) .GT. 0.0) .AND. &
              (stockingstart(:,:) .EQ. 1))

            deltaanimal(:,:) = MIN (0.0001,(wshtot(:,:) - nanimaltot(:,:)*intake(:,:))/intakemax(:,:))
            nanimaltot (:,:)  = MIN (MAX (0.0, nanimaltot (:,:)  +deltaanimal(:,:)), nanimaltotmax)

        END WHERE

      ENDIF calc_nanimaltot

!gmjc 05Feb2016 calculate count days of wet/dry soil
    IF ( .NOT. hydrol_cwrr ) THEN
      WHERE (moiavail_daily .GT. moi_threshold)
        ct_dry(:,:) = ct_dry(:,:) - 1
      ELSEWHERE
        ct_dry(:,:) = ct_dry(:,:) + 1
      ENDWHERE
      WHERE (ct_dry .GE. ct_max)
        ct_dry(:,:) = ct_max
      ELSEWHERE (ct_dry .LE. 0)
        ct_dry(:,:) = 0
      ENDWHERE
    ELSE
      DO j=1,nvm
        WHERE (tmc_topgrass_daily .GT. 1.5 )!tmc_topgrass_sat_daily) !fc_grazing)
!JCMODIF fc_grazing is soiltype dependent now 0.15 0.25 0.35!tmcf_threshold)
          ct_dry(:,j) = ct_dry(:,j) - 1
        ELSEWHERE
          ct_dry(:,j) = ct_dry(:,j) + 1
        ENDWHERE
      ENDDO
        WHERE (ct_dry .GE. ct_max)
          ct_dry(:,:) = ct_max
        ELSEWHERE (ct_dry .LE. 0)
          ct_dry(:,:) = 0
        ENDWHERE
    ENDIF
!end gmjc

!gmjc 25July2016 
! incorporating impact of tmc_topgrass_daily, snowmass_daily and t2m_daily
! on grazing
IF (avoid_wetgrazing) THEN
  DO i=1,npts
    IF (tmc_topgrass_daily(i) .GT. (fc_grazing(i) - buffer_wet)) THEN
      IF (wet1day(i) .LE. 4 .AND. wet2day(i) .LE. 4) THEN
        after_wet(i) = 10
      ELSE
        after_wet(i) = after_wet(i) -1      
      ENDIF
      wet2day(i) = wet1day(i) + 1
      wet1day(i) = 1      
    ELSE
      after_wet(i) = after_wet(i) -1 
      wet1day(i) = wet1day(i) + 1
      wet2day(i) = wet2day(i) + 1
    ENDIF
  ENDDO 
  WHERE (wet1day .GT. 6) 
    wet1day(:) = 6
  ELSEWHERE
    wet1day(:) = wet1day(:)
  ENDWHERE
  WHERE (wet2day .GT. 6)
    wet2day(:) = 6
  ELSEWHERE
    wet2day(:) = wet2day(:)
  ENDWHERE  
  WHERE (after_wet .LT. 0)
    after_wet(:) = 0
  ELSEWHERE
    after_wet(:) = after_wet(:)
  ENDWHERE
ELSE
  after_wet(:) = 0
ENDIF ! avoid_wetgrazing
IF (avoid_coldgrazing) THEN
  WHERE (t2m_daily .LE. 273.15)
    t2m_below_zero(:) = 1
  ELSEWHERE
    t2m_below_zero(:) = 0
  ENDWHERE
  WHERE (t2m_below_zero .LT. 0)
    t2m_below_zero(:) = 0
  ELSEWHERE
    t2m_below_zero(:) = t2m_below_zero(:)
  ENDWHERE
  ELSE
    t2m_below_zero(:) = 0
ENDIF

IF (avoid_snowgrazing) THEN
  WHERE (snowmass_daily .GT. 0.01)
    after_snow(:) = buffer_snow
  ELSEWHERE 
    after_snow(:) = after_snow(:) - 1
  ENDWHERE
  WHERE (after_snow .LT. 0)
    after_snow(:) = 0
  ELSEWHERE
    after_snow(:) = after_snow(:)
  ENDWHERE
ELSE  
  after_snow(:) = 0
ENDIF ! avoid_snowgrazing

!end gmjc
      IF (f_autogestion .EQ. 2) THEN
        DO j=2,nvm
          IF (is_grassland_manag(j) .AND. (.NOT. is_grassland_cut(j)) .AND. &
                (.NOT.is_grassland_grazed(j)))THEN
!JCCOMMENT delete the start of grazing after 15 days
!            WHERE (wshtot(:,j) .GE. (min_grazing + 0.05))
! BM_threshold_turnout = 0.08333  
            WHERE (wshtot(:,j) .GE. 0.13 .AND. ct_dry(:,j) .GE. ct_threshold)

              delai_ugb(:,j) = delai_ugb(:,j) +1
!              WHERE (delai_ugb(:,j) .GE. 0)
                ugb(:,j) = 1
!              ENDWHERE

!            ELSEWHERE (wshtot(:,j) .LT. min_grazing)
! BM_threshold =0.058
            ELSEWHERE (wshtot(:,j) .LT. 0.058)

              nanimaltot (:,j) = 0.0
              ugb(:,j)           = 0
              delai_ugb(:,j) = -15

            ELSEWHERE (ct_dry(:,j) .LT. ct_threshold)
              nanimaltot (:,j) = 0.0
              ugb(:,j)           = 0

            END WHERE
            IF (tjulian .GT. tseasonendmin) THEN
              WHERE (snowfall_daily(:) .GT. 1e-3 .OR. t2m_below_zero(:) .GT. 0.5 &
                     .OR. after_wet(:) .GT. 0.5 .OR. after_snow(:) .GT. 0.5)
                nanimaltot (:,j) = 0.0
                ugb(:,j)           = 0
              END WHERE
            ENDIF

            WHERE (ugb(:,j) .EQ. 1)

              compt_ugb(:,j)  = compt_ugb(:,j) + 1
              nanimaltot (:,j) = sr_ugb(:,j)

            END WHERE

          END IF!manag not cut not graze
        END DO ! nvm
      END IF ! f_autogestion =2

      ! JCMODIF for LGM autogestion = 3 move it as postauto =5
!       IF ((f_autogestion .EQ. 3) .OR. (f_autogestion .EQ. 4))  THEN
      IF  (f_autogestion .EQ. 4)  THEN
        WHERE (wshtot(:,mgraze_C3) .GE. (min_grazing + 0.05))

          delai_ugb(:,mgraze_C3) = delai_ugb(:,mgraze_C3) +1
          WHERE (delai_ugb(:,mgraze_C3) .GE. 0 .AND. &
                ct_dry(:,mgraze_C3) .GE. ct_threshold)
            ugb(:,mgraze_C3) = 1
          ENDWHERE

        ELSEWHERE (wshtot(:,mgraze_C3) .LT. min_grazing)

            nanimaltot (:,mgraze_C3) = 0.0
            ugb(:,mgraze_C3)           = 0
            delai_ugb(:,mgraze_C3) = -15
        END WHERE
        WHERE (ct_dry(:,mgraze_C3) .LT. ct_threshold)
          nanimaltot (:,mgraze_C3) = 0.0
          ugb(:,mgraze_C3) = 0
        ENDWHERE
        IF (tjulian .GT. tseasonendmin) THEN
          WHERE (snowfall_daily(:) .GT. 1e-3 .OR. t2m_below_zero(:) .GT. 0.5 &
                .OR. after_wet(:) .GT. 0.5 .OR. after_snow(:) .GT. 0.5)
            nanimaltot (:,mgraze_C3) = 0.0
            ugb(:,mgraze_C3)           = 0
          ENDWHERE
        ENDIF
        WHERE (ugb(:,mgraze_C3) .EQ. 1)
            compt_ugb(:,mgraze_C3)  = compt_ugb(:,mgraze_C3) + 1
            nanimaltot (:,mgraze_C3) = sr_ugb(:,mgraze_C3)
        END WHERE

        WHERE (wshtot(:,mgraze_C4) .GE. (min_grazing + 0.05))

          delai_ugb(:,mgraze_C4) = delai_ugb(:,mgraze_C4) +1
          WHERE (delai_ugb(:,mgraze_C4) .GE. 0 .AND. &
                ct_dry(:,mgraze_C4) .GE. ct_threshold)
            ugb(:,mgraze_C4) = 1
          ENDWHERE

        ELSEWHERE (wshtot(:,mgraze_C4) .LT. min_grazing)

            nanimaltot (:,mgraze_C4) = 0.0
            ugb(:,mgraze_C4)           = 0
            delai_ugb(:,mgraze_C4) = -15
        END WHERE
        WHERE (ct_dry(:,mgraze_C4) .LT. ct_threshold)
          nanimaltot (:,mgraze_C4) = 0.0
          ugb(:,mgraze_C4) = 0
        ENDWHERE
        IF (tjulian .GT. tseasonendmin) THEN
          WHERE (snowfall_daily(:) .GT. 1e-3 .OR. t2m_below_zero(:) .GT. 0.5 &
                .OR. after_wet(:) .GT. 0.5 .OR. after_snow(:) .GT. 0.5)
            nanimaltot (:,mgraze_C4) = 0.0
            ugb(:,mgraze_C4)           = 0
          ENDWHERE
        ENDIF
        WHERE (ugb(:,mgraze_C4) .EQ. 1)
            compt_ugb(:,mgraze_C4)  = compt_ugb(:,mgraze_C4) + 1
            nanimaltot (:,mgraze_C4) = sr_ugb(:,mgraze_C4)
        END WHERE

      ENDIF ! f_autogestion=4

      IF ((f_postauto .EQ. 1) .OR. (f_postauto .EQ. 2) .OR. &
           (f_postauto .EQ. 3) .OR. (f_postauto .EQ. 4)) THEN

!        WHERE (wshtot(:,mgraze_C3) .GE. (min_grazing + 0.05))
        WHERE (wshtot(:,mgraze_C3) .GE. 0.13)
          delai_ugb(:,mgraze_C3) = delai_ugb(:,mgraze_C3) +1
!JCMODIF Feb2015 for start grazing too late
          WHERE (delai_ugb(:,mgraze_C3) .GE. 0 .AND. &
                ct_dry(:,mgraze_C3) .GE. ct_threshold)
            ugb(:,mgraze_C3) = 1
          ENDWHERE

!        ELSEWHERE (wshtot(:,mgraze_C3) .LT. min_grazing)
        ELSEWHERE (wshtot(:,mgraze_C3) .LT. 0.058)
            nanimaltot (:,mgraze_C3) = 0.0
            ugb(:,mgraze_C3)           = 0
            delai_ugb(:,mgraze_C3) = -15
        END WHERE
        WHERE (ct_dry(:,mgraze_C3) .LT. ct_threshold)
          nanimaltot (:,mgraze_C3) = 0.0
          ugb(:,mgraze_C3) = 0
        ENDWHERE
        IF (tjulian .GT. tseasonendmin) THEN
          WHERE (snowfall_daily(:) .GT. 1e-3 .OR. t2m_below_zero(:) .GT. 0.5 &
                .OR. after_wet(:) .GT. 0.5 .OR. after_snow(:) .GT. 0.5)
            nanimaltot (:,mgraze_C3) = 0.0
            ugb(:,mgraze_C3)           = 0
          ENDWHERE
        ENDIF
        WHERE (ugb(:,mgraze_C3) .EQ. 1)
            compt_ugb(:,mgraze_C3)  = compt_ugb(:,mgraze_C3) + 1
            nanimaltot (:,mgraze_C3) = sr_ugb(:,mgraze_C3)
        END WHERE

!        WHERE (wshtot(:,mgraze_C4) .GE. (min_grazing + 0.05))
        WHERE (wshtot(:,mgraze_C4) .GE. 0.13)
          delai_ugb(:,mgraze_C4) = delai_ugb(:,mgraze_C4) +1
          WHERE (delai_ugb(:,mgraze_C4) .GE. 0 .AND. &
                ct_dry(:,mgraze_C4) .GE. ct_threshold)
            ugb(:,mgraze_C4) = 1
          ENDWHERE

!        ELSEWHERE (wshtot(:,mgraze_C4) .LT. min_grazing)
        ELSEWHERE (wshtot(:,mgraze_C4) .LT. 0.058)
            nanimaltot (:,mgraze_C4) = 0.0
            ugb(:,mgraze_C4)           = 0
            delai_ugb(:,mgraze_C4) = -15
        END WHERE
        WHERE (ct_dry(:,mgraze_C4) .LT. ct_threshold)
          nanimaltot (:,mgraze_C4) = 0.0
          ugb(:,mgraze_C4) = 0
        ENDWHERE
        IF (tjulian .GT. tseasonendmin) THEN
          WHERE (snowfall_daily(:) .GT. 1e-3 .OR. t2m_below_zero(:) .GT. 0.5 &
                .OR. after_wet(:) .GT. 0.5 .OR. after_snow(:) .GT. 0.5)
            nanimaltot (:,mgraze_C4) = 0.0
            ugb(:,mgraze_C4)           = 0
          ENDWHERE
        ENDIF
        WHERE (ugb(:,mgraze_C4) .EQ. 1)
            compt_ugb(:,mgraze_C4)  = compt_ugb(:,mgraze_C4) + 1
            nanimaltot (:,mgraze_C4) = sr_ugb(:,mgraze_C4)
        END WHERE
      ENDIF ! f_postauto=1 2 3 4 

      ! JCMODIF for differen sr_ugb given varied threshold
      ! with 1 LSU of 250 gDM and stop grazing with 0.8 * 250 g DM
      ! with < 1 LSU of 2*2^(1-sr_ugb*10000)*sr_ugb*10000*125
      ! e.g., 0.5 LSU 180 gDM  0.1 LSU 46 gDM
      ! 0.01 LSU 5 gDM  
!!! gmjc for global simulation with wild animal grazing natural grassland
      IF ((f_postauto .EQ. 5) .OR. (f_autogestion .EQ. 3)) THEN
!      IF (f_autogestion .EQ. 3) THEN
        able_grazing(:,mgraze_C3) = sr_ugb(:,mgraze_C3) * 10000.0 * 130.0 * & 
               2.0**(1.0-(sr_ugb(:,mgraze_C3)*10000.0))/1000.0
        able_grazing(:,mgraze_C4) = sr_ugb(:,mgraze_C4) * 10000.0 * 130.0 * &
               2.0**(1.0-(sr_ugb(:,mgraze_C4)*10000.0))/1000.0
        ! > 1 LSU/ha using 0.25 kgDM
        WHERE (sr_ugb(:,mgraze_C3) .GE. 0.0001)
          WHERE (wshtot(:,mgraze_C3) .GE. 0.13)
            
            delai_ugb(:,mgraze_C3) = delai_ugb(:,mgraze_C3) +1
            WHERE (delai_ugb(:,mgraze_C3) .GE. 0 .AND. &
                  ct_dry(:,mgraze_C3) .GE. ct_threshold)
              ugb(:,mgraze_C3) = 1
              grazing_litter(:,mgraze_C3) = 0
            ENDWHERE

          ELSEWHERE (wshtot(:,mgraze_C3) .LT. 0.058)

              nanimaltot (:,mgraze_C3) = 0.0
              ugb(:,mgraze_C3)           = 0
              delai_ugb(:,mgraze_C3) = -15
              grazing_litter(:,mgraze_C3) = 2
          END WHERE
          WHERE (ct_dry(:,mgraze_C3) .LT. ct_threshold)
            nanimaltot (:,mgraze_C3) = 0.0
            ugb(:,mgraze_C3) = 0
            grazing_litter(:,mgraze_C3) = 2
          ENDWHERE
        ELSEWHERE (sr_ugb(:,mgraze_C3) .GE. 0.00002 .AND. sr_ugb(:,mgraze_C3) .LT. 0.0001)
          WHERE (wshtot(:,mgraze_C3) .GE. able_grazing(:,mgraze_C3))

            delai_ugb(:,mgraze_C3) = delai_ugb(:,mgraze_C3) +1
            WHERE (delai_ugb(:,mgraze_C3) .GE. 0 .AND. &
                  ct_dry(:,mgraze_C3) .GE. ct_threshold)
              ugb(:,mgraze_C3) = 1
              grazing_litter(:,mgraze_C3) = 0
            ENDWHERE

          ELSEWHERE (wshtot(:,mgraze_C3) .LT. able_grazing(:,mgraze_C3)*0.45)

              nanimaltot (:,mgraze_C3) = 0.0
              ugb(:,mgraze_C3)           = 0
              delai_ugb(:,mgraze_C3) = -15
              grazing_litter(:,mgraze_C3) = 2
          END WHERE
          WHERE (ct_dry(:,mgraze_C3) .LT. ct_threshold)
            nanimaltot (:,mgraze_C3) = 0.0
            ugb(:,mgraze_C3) = 0
            grazing_litter(:,mgraze_C3) = 2
          ENDWHERE
        ELSEWHERE (sr_ugb(:,mgraze_C3) .LT. 0.00002)
          WHERE (wshtot(:,mgraze_C3) .GE. able_grazing(:,mgraze_C3))

            delai_ugb(:,mgraze_C3) = delai_ugb(:,mgraze_C3) +1
            WHERE (delai_ugb(:,mgraze_C3) .GE. 0 .AND. &
                  ct_dry(:,mgraze_C3) .GE. ct_threshold)
              ugb(:,mgraze_C3) = 1
              grazing_litter(:,mgraze_C3) = 0
            ENDWHERE

          ELSEWHERE (wshtot(:,mgraze_C3) .LT. able_grazing(:,mgraze_C3)*0.45)

              nanimaltot (:,mgraze_C3) = 0.0
              ugb(:,mgraze_C3)           = 0
              delai_ugb(:,mgraze_C3) = -15
              grazing_litter(:,mgraze_C3) = 2
          END WHERE
          WHERE (ct_dry(:,mgraze_C3) .LT. ct_threshold)
            nanimaltot (:,mgraze_C3) = 0.0
            ugb(:,mgraze_C3) = 0
            grazing_litter(:,mgraze_C3) = 2
          ENDWHERE
        ENDWHERE
          IF (tjulian .GT. tseasonendmin) THEN
            WHERE (snowfall_daily(:) .GT. 1e-3 .OR. t2m_below_zero(:) .GT. 0.5 &
                   .OR. after_snow(:) .GT. 0.5)
! wet grazing is only avoid at Europe scale
!                  .OR. after_wet(:) .GT. 0.5 .OR. after_snow(:) .GT. 0.5)
              nanimaltot (:,mgraze_C3) = 0.0
              ugb(:,mgraze_C3)           = 0
              grazing_litter(:,mgraze_C3) = 2
            ENDWHERE
          ENDIF
          WHERE (ugb(:,mgraze_C3) .EQ. 1)
              compt_ugb(:,mgraze_C3)  = compt_ugb(:,mgraze_C3) + 1
            WHERE (sr_ugb(:,mgraze_C3) .GT. 0.00002)
              nanimaltot (:,mgraze_C3) = sr_ugb(:,mgraze_C3)
            ELSEWHERE
              nanimaltot (:,mgraze_C3) = 0.00002
            ENDWHERE
          END WHERE
        ! > 1 LSU/ha using 0.25 kgDM
        WHERE (sr_ugb(:,mgraze_C4) .GE. 0.0001)
          WHERE (wshtot(:,mgraze_C4) .GE. 0.13)

            delai_ugb(:,mgraze_C4) = delai_ugb(:,mgraze_C4) +1
            WHERE (delai_ugb(:,mgraze_C4) .GE. 0 .AND. &
                  ct_dry(:,mgraze_C4) .GE. ct_threshold)
              ugb(:,mgraze_C4) = 1
              grazing_litter(:,mgraze_C4) = 0
            ENDWHERE

          ELSEWHERE (wshtot(:,mgraze_C4) .LT. 0.058)

              nanimaltot (:,mgraze_C4) = 0.0
              ugb(:,mgraze_C4)           = 0
              delai_ugb(:,mgraze_C4) = -15
              grazing_litter(:,mgraze_C4) = 2
          END WHERE
          WHERE (ct_dry(:,mgraze_C4) .LT. ct_threshold)
            nanimaltot (:,mgraze_C4) = 0.0
            ugb(:,mgraze_C4) = 0
            grazing_litter(:,mgraze_C4) = 2
          ENDWHERE
        ELSEWHERE (sr_ugb(:,mgraze_C4) .GE. 0.00002 .AND. sr_ugb(:,mgraze_C4) .LT. 0.0001)
          WHERE (wshtot(:,mgraze_C4) .GE. able_grazing(:,mgraze_C4))

            delai_ugb(:,mgraze_C4) = delai_ugb(:,mgraze_C4) +1
            WHERE (delai_ugb(:,mgraze_C4) .GE. 0 .AND. &
                  ct_dry(:,mgraze_C4) .GE. ct_threshold)
              ugb(:,mgraze_C4) = 1
              grazing_litter(:,mgraze_C4) = 0
            ENDWHERE

          ELSEWHERE (wshtot(:,mgraze_C4) .LT. able_grazing(:,mgraze_C4)*0.45)

              nanimaltot (:,mgraze_C4) = 0.0
              ugb(:,mgraze_C4)           = 0
              delai_ugb(:,mgraze_C4) = -15
              grazing_litter(:,mgraze_C4) = 2
          END WHERE
          WHERE (ct_dry(:,mgraze_C4) .LT. ct_threshold)
            nanimaltot (:,mgraze_C4) = 0.0
            ugb(:,mgraze_C4) = 0
            grazing_litter(:,mgraze_C4) = 2
          ENDWHERE
        ELSEWHERE (sr_ugb(:,mgraze_C4) .LT. 0.00002)
          WHERE (wshtot(:,mgraze_C4) .GE. able_grazing(:,mgraze_C4))

            delai_ugb(:,mgraze_C4) = delai_ugb(:,mgraze_C4) +1
            WHERE (delai_ugb(:,mgraze_C4) .GE. 0 .AND. &
                  ct_dry(:,mgraze_C4) .GE. ct_threshold)
              ugb(:,mgraze_C4) = 1
              grazing_litter(:,mgraze_C4) = 0
            ENDWHERE

          ELSEWHERE (wshtot(:,mgraze_C4) .LT. able_grazing(:,mgraze_C4)*0.45)

              nanimaltot (:,mgraze_C4) = 0.0
              ugb(:,mgraze_C4)           = 0
              delai_ugb(:,mgraze_C4) = -15
              grazing_litter(:,mgraze_C4) = 2
          END WHERE
          WHERE (ct_dry(:,mgraze_C4) .LT. ct_threshold)
            nanimaltot (:,mgraze_C4) = 0.0
            ugb(:,mgraze_C4) = 0
            grazing_litter(:,mgraze_C4) = 2
          ENDWHERE
        ENDWHERE
          IF (tjulian .GT. tseasonendmin) THEN
            WHERE (snowfall_daily(:) .GT. 1e-3 .OR. t2m_below_zero(:) .GT. 0.5 &
                   .OR. after_snow(:) .GT. 0.5)
! wet grazing is only avoid at Europe
!                  .OR. after_wet(:) .GT. 0.5 .OR. after_snow(:) .GT. 0.5)
              nanimaltot (:,mgraze_C4) = 0.0
              ugb(:,mgraze_C4)           = 0
              grazing_litter(:,mgraze_C4) = 2
            ENDWHERE
          ENDIF
          WHERE (ugb(:,mgraze_C4) .EQ. 1)
              compt_ugb(:,mgraze_C4)  = compt_ugb(:,mgraze_C4) + 1
            WHERE (sr_ugb(:,mgraze_C4) .GT. 0.00002)
              nanimaltot (:,mgraze_C4) = sr_ugb(:,mgraze_C4)
            ELSEWHERE
              nanimaltot (:,mgraze_C4) = 0.00002
            ENDWHERE
          END WHERE
!!!!!! gmjc for global simulation with wild animal grazing natural grassland
        able_grazing(:,mnatural_C3) = sr_wild(:,mnatural_C3) * 10000.0 * 130.0 * &
               2.0**(1.0-(sr_wild(:,mnatural_C3)*10000.0))/1000.0
        able_grazing(:,mnatural_C4) = sr_wild(:,mnatural_C4) * 10000.0 * 130.0 * &
               2.0**(1.0-(sr_wild(:,mnatural_C4)*10000.0))/1000.0

        WHERE (able_grazing(:,mnatural_C3) .GE. 0.13)
          able_grazing(:,mnatural_C3) = 0.13
        ELSEWHERE (able_grazing(:,mnatural_C3) .LT. 0.006)
          able_grazing(:,mnatural_C3) = 0.006
        ENDWHERE
        WHERE (able_grazing(:,mnatural_C4) .GE. 0.13)
          able_grazing(:,mnatural_C4) = 0.13
        ELSEWHERE (able_grazing(:,mnatural_C4) .LT. 0.006)
          able_grazing(:,mnatural_C4) = 0.006
        ENDWHERE
        !
        ! > 1 LSU/ha using 0.25 kgDM
        ! grazing biomass or litter
        WHERE (wshtot(:,mnatural_C3) .GE. able_grazing(:,mnatural_C3) .AND. &
              sr_wild(:,mnatural_C3) .GT. 0.0)
          delai_ugb(:,mnatural_C3) = delai_ugb(:,mnatural_C3) +1
          WHERE (delai_ugb(:,mnatural_C3) .GE. 0)
            ! can grazing
            ugb(:,mnatural_C3) = 1
            ! grazing biomass
            grazing_litter(:,mnatural_C3) = 0
          ELSEWHERE (delai_ugb(:,mnatural_C3) .LT. 0)
            WHERE (litter_avail_totDM(:,mnatural_C3) .GE. able_grazing(:,mnatural_C3))
              ! can grazing
              ugb(:,mnatural_C3) = 1
              ! grazing litter
              grazing_litter(:,mnatural_C3) = 1
            ELSEWHERE (litter_avail_totDM(:,mnatural_C3) .LT. able_grazing(:,mnatural_C3))
              ! cannot grazing
              ugb(:,mnatural_C3) = 0
              ! no grazing
              grazing_litter(:,mnatural_C3) = 2
            ENDWHERE
          ENDWHERE
        ELSEWHERE (wshtot(:,mnatural_C3) .LT. able_grazing(:,mnatural_C3) .AND. &
              sr_wild(:,mnatural_C3) .GT. 0.0)
            delai_ugb(:,mnatural_C3) = -15
          WHERE (litter_avail_totDM(:,mnatural_C3) .GE. able_grazing(:,mnatural_C3))
            ! can grazing
            ugb(:,mnatural_C3) = 1
            ! grazing litter
            grazing_litter(:,mnatural_C3) = 1
          ELSEWHERE (litter_avail_totDM(:,mnatural_C3) .LT. able_grazing(:,mnatural_C3))
            ! cannot grazing
            ugb(:,mnatural_C3) = 0
            ! no grazing
            grazing_litter(:,mnatural_C3) = 2
          ENDWHERE
        ENDWHERE
        WHERE (ugb(:,mnatural_C3) .EQ. 1)
            compt_ugb(:,mnatural_C3)  = compt_ugb(:,mnatural_C3) + 1
            nanimaltot (:,mnatural_C3) = sr_wild(:,mnatural_C3)
        END WHERE
        ! C4 grass
        ! > 1 LSU/ha using 0.25 kgDM
        ! grazing biomass or litter
        WHERE (wshtot(:,mnatural_C4) .GE. able_grazing(:,mnatural_C4) .AND. &
              sr_wild(:,mnatural_C4) .GT. 0.0)
          delai_ugb(:,mnatural_C4) = delai_ugb(:,mnatural_C4) +1
          WHERE (delai_ugb(:,mnatural_C4) .GE. 0)
            ! can grazing
            ugb(:,mnatural_C4) = 1
            ! grazing biomass
            grazing_litter(:,mnatural_C4) = 0
          ELSEWHERE (delai_ugb(:,mnatural_C4) .LT. 0)
            WHERE (litter_avail_totDM(:,mnatural_C4) .GE. able_grazing(:,mnatural_C4))
              ! can grazing
              ugb(:,mnatural_C4) = 1
              ! grazing litter
              grazing_litter(:,mnatural_C4) = 1
            ELSEWHERE (litter_avail_totDM(:,mnatural_C4) .LT. able_grazing(:,mnatural_C4))
              ! cannot grazing
              ugb(:,mnatural_C4) = 0
              ! no grazing
              grazing_litter(:,mnatural_C4) = 2
            ENDWHERE
          ENDWHERE
        ELSEWHERE (wshtot(:,mnatural_C4) .LT. able_grazing(:,mnatural_C4) .AND. & 
              sr_wild(:,mnatural_C4) .GT. 0.0)
            delai_ugb(:,mnatural_C4) = -15
          WHERE (litter_avail_totDM(:,mnatural_C4) .GE. able_grazing(:,mnatural_C4))
            ! can grazing
            ugb(:,mnatural_C4) = 1
            ! grazing litter
            grazing_litter(:,mnatural_C4) = 1
          ELSEWHERE (litter_avail_totDM(:,mnatural_C4) .LT. able_grazing(:,mnatural_C4))
            ! cannot grazing
            ugb(:,mnatural_C4) = 0
            ! no grazing
            grazing_litter(:,mnatural_C4) = 2
          ENDWHERE
        ENDWHERE
        WHERE (ugb(:,mnatural_C4) .EQ. 1)
            compt_ugb(:,mnatural_C4)  = compt_ugb(:,mnatural_C4) + 1
            nanimaltot (:,mnatural_C4) = sr_wild(:,mnatural_C4)
        END WHERE


      ENDIF ! f_postauto=5 or f_autogestion=3

      ! gmjc for MICT LGM grazing biomass and litter
      ! differen sr_ugb given varied threshold
      ! with 1 LSU of 250 gDM and stop grazing with 0.5 * 250 g DM
      ! with < 1 LSU of 2*2^(1-sr_ugb*10000)*sr_ugb*10000*125
      ! e.g., 0.5 LSU 180 gDM  0.1 LSU 46 gDM
      ! 0.01 LSU 5 gDM  
      IF (f_autogestion .EQ. 5) THEN

        able_grazing(:,mgraze_C3) = sr_ugb(:,mgraze_C3) * 10000.0 * 250.0 * & 
               2.0**(1.0-(sr_ugb(:,mgraze_C3)*10000.0))/1000.0
        able_grazing(:,mgraze_C4) = sr_ugb(:,mgraze_C4) * 10000.0 * 250.0 * &
               2.0**(1.0-(sr_ugb(:,mgraze_C4)*10000.0))/1000.0
        WHERE (able_grazing(:,mgraze_C3) .GE. 0.25)
          able_grazing(:,mgraze_C3) = 0.25
        ELSEWHERE (able_grazing(:,mgraze_C3) .LT. 0.006)
          able_grazing(:,mgraze_C3) = 0.006
        ENDWHERE
        WHERE (able_grazing(:,mgraze_C4) .GE. 0.25)
          able_grazing(:,mgraze_C4) = 0.25
        ELSEWHERE (able_grazing(:,mgraze_C3) .LT. 0.006)
          able_grazing(:,mgraze_C4) = 0.006
        ENDWHERE
        !
        ! > 1 LSU/ha using 0.25 kgDM
        ! grazing biomass or litter
        WHERE (wshtot(:,mgraze_C3) .GE. 0.5*able_grazing(:,mgraze_C3))
          delai_ugb(:,mgraze_C3) = delai_ugb(:,mgraze_C3) +1
          WHERE (delai_ugb(:,mgraze_C3) .GE. 0)
            ! can grazing
            ugb(:,mgraze_C3) = 1
            ! grazing biomass
            grazing_litter(:,mgraze_C3) = 0
          ELSEWHERE (delai_ugb(:,mgraze_C3) .LT. 0)
            WHERE (litter_avail_totDM(:,mgraze_C3) .GE. 0.5*able_grazing(:,mgraze_C3))
              ! can grazing
              ugb(:,mgraze_C3) = 1
              ! grazing litter
              grazing_litter(:,mgraze_C3) = 1
            ELSEWHERE (litter_avail_totDM(:,mgraze_C3) .LT. 0.5*able_grazing(:,mgraze_C3))
              ! cannot grazing
              ugb(:,mgraze_C3) = 0
              ! no grazing
              grazing_litter(:,mgraze_C3) = 2
            ENDWHERE
          ENDWHERE
        ELSEWHERE (wshtot(:,mgraze_C3) .LT. 0.5*able_grazing(:,mgraze_C3))
            delai_ugb(:,mgraze_C3) = -15
          WHERE (litter_avail_totDM(:,mgraze_C3) .GE. 0.5*able_grazing(:,mgraze_C3))
            ! can grazing
            ugb(:,mgraze_C3) = 1
            ! grazing litter
            grazing_litter(:,mgraze_C3) = 1
          ELSEWHERE (litter_avail_totDM(:,mgraze_C3) .LT. 0.5*able_grazing(:,mgraze_C3))
            ! cannot grazing
            ugb(:,mgraze_C3) = 0
            ! no grazing
            grazing_litter(:,mgraze_C3) = 2
          ENDWHERE
        ENDWHERE
        WHERE (ugb(:,mgraze_C3) .EQ. 1)
            compt_ugb(:,mgraze_C3)  = compt_ugb(:,mgraze_C3) + 1
            nanimaltot (:,mgraze_C3) = sr_ugb(:,mgraze_C3)
        END WHERE
!        WRITE(numout,*) 'zd ','sr_ugb', mgraze_C3,sr_ugb(:,mgraze_C3)
!        WRITE(numout,*) 'zd ','litter_ava',mgraze_C3,litter_avail_totDM(:,mgraze_C3)
!        WRITE(numout,*) 'zd ','able_gr',mgraze_C4,able_grazing(:,mgraze_C3)
!        WRITE(numout,*) 'zd ','animal',mgraze_C4,intake_animal_litter(:,mgraze_C3)
!        WRITE(numout,*) 'zd ','mgraze',mgraze_C3,grazing_litter(:,mgraze_C3)
        ! C4 grass
        ! > 1 LSU/ha using 0.25 kgDM
        ! grazing biomass or litter
        WHERE (wshtot(:,mgraze_C4) .GE. 0.5*able_grazing(:,mgraze_C4))
          delai_ugb(:,mgraze_C4) = delai_ugb(:,mgraze_C4) +1
          WHERE (delai_ugb(:,mgraze_C4) .GE. 0)
            ! can grazing
            ugb(:,mgraze_C4) = 1
            ! grazing biomass
            grazing_litter(:,mgraze_C4) = 0
          ELSEWHERE (delai_ugb(:,mgraze_C4) .LT. 0)
            WHERE (litter_avail_totDM(:,mgraze_C4) .GE. 0.5*able_grazing(:,mgraze_C4))
              ! can grazing
              ugb(:,mgraze_C4) = 1
              ! grazing litter
              grazing_litter(:,mgraze_C4) = 1
            ELSEWHERE (litter_avail_totDM(:,mgraze_C4) .LT. 0.5*able_grazing(:,mgraze_C4))
              ! cannot grazing
              ugb(:,mgraze_C4) = 0
              ! no grazing
              grazing_litter(:,mgraze_C4) = 2
            ENDWHERE
          ENDWHERE
        ELSEWHERE (wshtot(:,mgraze_C4) .LT. 0.5*able_grazing(:,mgraze_C4))
            delai_ugb(:,mgraze_C4) = -15
          WHERE (litter_avail_totDM(:,mgraze_C4) .GE. 0.5*able_grazing(:,mgraze_C4))
            ! can grazing
            ugb(:,mgraze_C4) = 1
            ! grazing litter
            grazing_litter(:,mgraze_C4) = 1
          ELSEWHERE (litter_avail_totDM(:,mgraze_C4) .LT. 0.5*able_grazing(:,mgraze_C4))
            ! cannot grazing
            ugb(:,mgraze_C4) = 0
            ! no grazing
            grazing_litter(:,mgraze_C4) = 2
          ENDWHERE
        ENDWHERE
        WHERE (ugb(:,mgraze_C4) .EQ. 1)
            compt_ugb(:,mgraze_C4)  = compt_ugb(:,mgraze_C4) + 1
            nanimaltot (:,mgraze_C4) = sr_ugb(:,mgraze_C4)
        END WHERE
      ENDIF ! f_autogestion=5


    END IF n_day


    CALL nel_grazing_calcul(&
       npts, dt             , &
       nanimaltot         , &
       devstage, tgrowth, nel, &
       ntot)

    CALL Grazing_intake(&
       npts, dt, wsh     , &
       intakemax         , &
       Animalwgrazingmin , &
       AnimalkintakeM    , &
       intake            , &
       intakesum         , &
       tanimal           , &
       danimal           , &
       tjulian           , &
       intakensum        , &
       fn                , &
       n                 , &
       intake_animal     , &
       intake_animalsum  , &
       nanimaltot        , &
       intake_litter     , &
       intake_animal_litter, &
       grazing_litter)

    CALL variablesPlantes(&
       npts,biomass,&
       c,n,intake_animal,intakemax,&
       AnimalDiscremineQualite)

    CALL chg_plante(&
       npts, dt, biomass  , &
       c, n,leaf_frac     , &
       wsh, wshtot        , &
       nanimaltot, intake_animal, &
       trampling,intake, &
       NDF,DNDF,DNDFI, &
       grazing_litter)
    
!    CALL variablesPlantes(&
!       npts,biomass,NDF,DNDF,DNDFI,&
!       c,n,intake_animal,intakemax,&
!       AnimalDiscremineQualite)


    CALL Milk_Animal(&
       npts, dt, nel, intake_animal, &
       wanimal, nanimaltot )

    !gmjc 110525
    !!!!!! In order to get the variables that needed by Respiration_Methane and Urine_Faeces
    !!!!!! we need to calculate new grazingn and grazingc using intake from above
    !!!!!! So we call modified cal_grazing which from MODULE applic_plant to get variables needed
    CALL cal_grazing(&
       npts                  , &
       nanimaltot            , &
       intake_animal         , &
       wsh                   , &
       wshtot                , &
       c                     , &
       n                     , &
       fn                    , &
       Substrate_grazingwc  , &
       Substrate_grazingwn  , &
       grazingcstruct        , &
       grazingnstruct        , &
       intake)

  IF (f_autogestion .NE. 5 .AND. f_postauto .NE. 5) THEN
    WHERE (nanimaltot.NE.0)
      grazingn  = grazingnstruct + Substrate_grazingwn
!JCMODIF to balance the carbon with 45% of intake DM
!      grazingc  = grazingcstruct + Substrate_grazingwc
      grazingc = intake * CtoDM
!ENDJCMODIF
    ELSEWHERE
      grazingn=0
      grazingc=0
    END WHERE

  ELSEIF (f_autogestion .EQ. 5 .OR. f_postauto .EQ. 5) THEN
    ! grazing AGB
    WHERE (nanimaltot.NE.0 .AND. grazing_litter(:,:) .EQ. 0)
      grazingn  = grazingnstruct + Substrate_grazingwn
!JCMODIF to balance the carbon with 45% of intake DM
!      grazingc  = grazingcstruct + Substrate_grazingwc
      grazingc = intake * CtoDM
!ENDJCMODIF
    ! grazing litter
    ELSEWHERE (nanimaltot.NE.0 .AND. grazing_litter(:,:) .EQ. 1)
      
      grazingc = intake_litter * CtoDM
      grazingn = grazingc * fn / fcsh
    ELSEWHERE
      grazingn=0
      grazingc=0
    END WHERE  

  ENDIF ! f_autogestion = 5

    DO j=2,nvm
      CALL Euler_funct (npts,dt,grazingn(:,j), grazingnsum(:,j))       
      CALL Euler_funct (npts, dt, grazingc(:,j), grazingcsum(:,j))  
    END DO

    CALL Respiration_Methane(&
       npts, dt, grazingc, &
       nanimaltot, DNDFI, wanimal )

    CALL Urine_Faeces(&
       npts, dt          , &
       grazingn, grazingc, &
       urinen, faecesn   , &
       urinec, faecesc )

    Fert_PRP = urinen + faecesn

    ! kgC m-2 day-1 -> gC m-1 day-1
    ranimal_gm = ranimal*1e3
    ch4_pft_gm = Methane*1e3

    CALL xios_orchidee_send_field("GRAZINGC",grazingc)
    CALL xios_orchidee_send_field("NANIMALTOT",nanimaltot)
    CALL xios_orchidee_send_field("INTAKE_ANIMAL",intake_animal)
    CALL xios_orchidee_send_field("INTAKE",intake)
    CALL xios_orchidee_send_field("TRAMPLING",trampling)
    CALL xios_orchidee_send_field("CT_DRY",ct_dry)
    CALL xios_orchidee_send_field("INTAKE_ANIMAL_LITTER",intake_animal_litter)
    CALL xios_orchidee_send_field("INTAKE_LITTER",intake_litter)
    CALL xios_orchidee_send_field("SR_WILD",sr_wild)
    CALL xios_orchidee_send_field("MILK",milk)
    CALL xios_orchidee_send_field("MILKC",milkc)
    CALL xios_orchidee_send_field("METHANE",Methane)
    CALL xios_orchidee_send_field("RANIMAL",ranimal)
    CALL xios_orchidee_send_field("URINEC",urinec)
    CALL xios_orchidee_send_field("FAECESC",faecesc)
    CALL xios_orchidee_send_field("GRAZED_FRAC",grazed_frac)
    CALL xios_orchidee_send_field("NB_ANI",nb_ani)
    CALL xios_orchidee_send_field("IMPORT_YIELD",import_yield)
    CALL xios_orchidee_send_field("NB_GRAZINGDAYS",nb_grazingdays)
    CALL xios_orchidee_send_field("OUTSIDE_FOOD",outside_food)
    CALL xios_orchidee_send_field("AFTER_SNOW",after_snow)
    CALL xios_orchidee_send_field("AFTER_WET",after_wet)
    CALL xios_orchidee_send_field("WET1DAY",wet1day)
    CALL xios_orchidee_send_field("WET2DAY",wet2day)

    !grazed
    CALL histwrite_p(hist_id_stomate ,'GRAZINGC',itime ,grazingc ,npts*nvm, horipft_index) 
    CALL histwrite_p(hist_id_stomate ,'GRAZINGCSUM',itime ,grazingcsum ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'NANIMALTOT',itime ,nanimaltot  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'INTAKE_ANIMAL' ,itime ,intake_animal  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'INTAKE'    ,itime ,intake     ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'INTAKESUM' ,itime ,intakesum  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'TRAMPLING' ,itime ,trampling  ,npts*nvm, horipft_index)
!gmjc for avoid grazing domestic over wet soil
    CALL histwrite_p(hist_id_stomate ,'CT_DRY' ,itime ,ct_dry  ,npts*nvm, horipft_index)
!gmjc for grazing litter
    CALL histwrite_p(hist_id_stomate ,'INTAKE_ANIMAL_LITTER' ,itime ,intake_animal_litter ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'INTAKE_LITTER'    ,itime ,intake_litter     ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'GRAZING_LITTER' ,itime ,float(grazing_litter)  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'SR_WILD' ,itime ,sr_wild  ,npts*nvm, horipft_index)
!end gmjc
    !milk
    CALL histwrite_p(hist_id_stomate ,'MILK'      ,itime ,milk       ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'MILKSUM'   ,itime ,milksum    ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'MILKCSUM'  ,itime ,milkcsum   ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'MILKC'     ,itime ,milkc      ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'MILKN'     ,itime ,milkn      ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate, 'MILKANIMAL'    ,itime , milkanimal,npts*nvm, horipft_index )

    !methane & respiration
    CALL histwrite_p(hist_id_stomate ,'METHANE',itime ,Methane ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'METHANE_ANI',itime ,Methane_ani ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'RANIMALSUM',itime ,ranimalsum ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'METHANESUM',itime ,MethaneSum ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'RANIMAL'   ,itime ,ranimal    ,npts*nvm, horipft_index)

    !farces and urine
    CALL histwrite_p(hist_id_stomate ,'FAECESNSUM',itime ,faecesnsum ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'FAECESCSUM',itime ,faecescsum ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'URINECSUM' ,itime ,urinecsum  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'URINENSUM' ,itime ,urinensum  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'NEL'       ,itime ,nel        ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'URINEN'    ,itime ,urinen     ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'URINEC'    ,itime ,urinec     ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'FAECESC'   ,itime ,faecesc    ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'FAECESN'   ,itime ,faecesn    ,npts*nvm, horipft_index)

    CALL histwrite_p(hist_id_stomate ,'GRAZED_FRAC' ,itime ,grazed_frac  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'NB_ANI' ,itime ,nb_ani  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'IMPORT_YIELD' ,itime ,import_yield  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'EXTRA_FEED' ,itime ,extra_feed  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'COMPT_UGB',itime ,compt_ugb ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'NB_GRAZINGDAYS',itime ,nb_grazingdays,npts*nvm, horipft_index)

    CALL histwrite_p(hist_id_stomate ,'AMOUNT_YIELD',itime ,amount_yield ,npts*nvm,horipft_index)
    CALL histwrite_p(hist_id_stomate ,'CONSUMP',itime ,consump ,npts*nvm,horipft_index)
    CALL histwrite_p(hist_id_stomate ,'OUTSIDE_FOOD',itime ,outside_food,npts*nvm,horipft_index)

    CALL histwrite_p(hist_id_stomate ,'ADD_NB_ANI',itime ,add_nb_ani ,npts*nvm,horipft_index)


  END SUBROUTINE Animaux_main


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!  Animal_Init : ALL CHANGED ACCORDING TO PASIM 2011 Animal_Init and 
!!!!  used by both Animaux_main and Animaux_main_dynamic
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE Animal_Init(&
     npts              , &
     nanimal           , &
     type_animal       , &
     intake_tolerance)   

    INTEGER (i_std)                   , INTENT(in) :: npts
    REAL(r_std), DIMENSION(npts,nvm,nstocking), INTENT(in) :: nanimal             ! Stocking density  h (1,..,nstocking) (animal m-2)
    INTEGER (i_std)                   ,  INTENT(in) :: type_animal         ! 1: Dairy cows, 2: Suckler cows, 3: Old module, 4: Dairy heifers, 5 : Suckler heifers
    REAL(r_std),                            INTENT(in) :: intake_tolerance    ! Intake tolerance threshold (-)


    LOGICAL :: l_error = .FALSE. 
    INTEGER(i_std) :: ier,j

    !
    ! initialisation
    !

    IF (blabla_pasim) PRINT *, 'PASIM Animals : allocation memory in Animals_Orchidee'
    

    l_first_animaux =.FALSE.
    l_error = .FALSE.
    ALLOCATE (milk              (npts,nvm), stat=ier)
    ALLOCATE (milkn             (npts,nvm), stat=ier)
    ALLOCATE (milkc             (npts,nvm), stat=ier)
    ALLOCATE (ranimal           (npts,nvm), stat=ier)
    ALLOCATE (Methane           (npts,nvm), stat=ier)
    ALLOCATE (faecesnsumprev    (npts,nvm), stat=ier)
    ALLOCATE (milkndaily        (npts,nvm), stat=ier)
    ALLOCATE (faecesndaily      (npts,nvm), stat=ier)
    ALLOCATE (urinendaily       (npts,nvm), stat=ier)
    ALLOCATE (milksum           (npts,nvm), stat=ier)
    ALLOCATE (nelgrazingsum     (npts,nvm), stat=ier)
    ALLOCATE (milkcsum          (npts,nvm), stat=ier)
    ALLOCATE (ranimalsum        (npts,nvm), stat=ier)
    ALLOCATE (Methanesum        (npts,nvm), stat=ier)
    ALLOCATE (urinecsum         (npts,nvm), stat=ier)
    ALLOCATE (faecescsum        (npts,nvm), stat=ier)
    ALLOCATE (urinensum         (npts,nvm), stat=ier)
    ALLOCATE (faecesnsum        (npts,nvm), stat=ier)
    ALLOCATE (milknsum          (npts,nvm), stat=ier)
    ALLOCATE (milknsumprev      (npts,nvm), stat=ier)
    ALLOCATE (urinensumprev     (npts,nvm), stat=ier)
    ALLOCATE (stockingstart     (npts,nvm), stat=ier)
    ALLOCATE (stockingend       (npts,nvm), stat=ier)
    ALLOCATE (wshtotstart       (npts,nvm), stat=ier)
    ALLOCATE (grazingsum        (npts,nvm), stat=ier)
    ALLOCATE (grazingcsum       (npts,nvm), stat=ier)
    ALLOCATE (grazingnsum       (npts,nvm), stat=ier)
    ALLOCATE (grazingc          (npts,nvm), stat=ier)
    ALLOCATE (grazingn          (npts,nvm), stat=ier)
    ALLOCATE (grazingnsumprev   (npts,nvm), stat=ier)
    ALLOCATE (grazingndaily     (npts,nvm), stat=ier)
    ALLOCATE (forage_complementc(npts,nvm), stat=ier)
    ALLOCATE (forage_complementn(npts,nvm), stat=ier)
    ALLOCATE (forage_complementcsum(npts,nvm), stat=ier)
    ALLOCATE (forage_complementnsum(npts,nvm), stat=ier)
    ALLOCATE (methane_ani       (npts,nvm), stat=ier)
    ALLOCATE (methane_aniSum    (npts,nvm), stat=ier)
    ALLOCATE (milkanimalsum     (npts,nvm), stat=ier)
    ALLOCATE (milkanimal     (npts,nvm), stat=ier)
    ALLOCATE (ugb               (npts,nvm), stat=ier)
    ALLOCATE (ok_ugb            (npts,nvm), stat=ier)
    ALLOCATE (extra_feed        (npts,nvm), stat=ier)
    ALLOCATE (Wanimalcow     (npts,nvm,2),stat=ier)
    ALLOCATE (BCScow         (npts,nvm,2),stat=ier)
    ALLOCATE (BCScow_prev    (npts,nvm,2),stat=ier)
    ALLOCATE (AGEcow         (npts,nvm,2),stat=ier)
    ALLOCATE (Forage_quantity_period (npts,nvm),stat=ier)
    ALLOCATE (MPcowCsum      (npts,nvm,2),stat=ier)
    ALLOCATE (MPcowNsum      (npts,nvm,2),stat=ier)
    ALLOCATE (MPcowN         (npts,nvm,2),stat=ier)
    ALLOCATE (MPcowC         (npts,nvm,2),stat=ier)
    ALLOCATE (MPcowsum       (npts,nvm,2),stat=ier)
    ALLOCATE (MPcow2sum      (npts,nvm,2),stat=ier)
    ALLOCATE (MPcow2_prec     (npts,nvm,2),stat=ier)
    ALLOCATE (DMIcowsum      (npts,nvm,2),stat=ier)
    ALLOCATE (DMIcowNsum     (npts,nvm,2),stat=ier)
    ALLOCATE (DMIcowCsum     (npts,nvm,2),stat=ier)
    ALLOCATE (DMIcowanimalsum (npts,nvm,2),stat=ier)
    ALLOCATE (Wanimalcalf        (npts,nvm),stat=ier)
    ALLOCATE (DMIcalfsum         (npts,nvm),stat=ier)
    ALLOCATE (DMIcalfnsum        (npts,nvm),stat=ier)
    ALLOCATE (DMIcalfanimalsum   (npts,nvm),stat=ier)  
    ALLOCATE (Tcalving           (npts,nvm), stat=ier)
    ALLOCATE (Tsevrage           (npts,nvm), stat=ier)
    ALLOCATE (Age_sortie_calf    (npts,nvm), stat=ier)
    ALLOCATE (Pyoung             (npts,nvm), stat=ier)
    ALLOCATE (Wcalfborn          (npts,nvm), stat=ier)
    ALLOCATE (calfinit           (npts,nvm),stat=ier)
    ALLOCATE (Wanimalcalfinit    (npts,nvm), stat=ier)
    ALLOCATE (calf               (npts,nvm),stat=ier)
    ALLOCATE (nanimaltot_prec    (npts,nvm), stat=ier)
    ALLOCATE (Gestation          (npts,nvm),stat=ier)
    ALLOCATE (compte_pature      (npts,nvm), stat=ier)
    ALLOCATE (autogestion_weightcow (npts,nvm,4), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (autogestion_BCScow    (npts,nvm,4), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (autogestion_AGEcow    (npts,nvm,4), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (autogestion_init   (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (QIc   (npts,nvm,2)            , stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (EVf   (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (EVc   (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (FVf   (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (fN_forage   (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (fN_concentrate   (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (NEBcow_prec    (npts,nvm,2)  , stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (MPwmax             (npts,nvm,2)    , stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (Fday_pasture       (npts,nvm)            , stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (delai_ugb             (npts,nvm)    , stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (Local_autogestion_out (npts,nvm,n_out)    , stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (PEmax (npts,nvm,2),stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (PEpos (npts,nvm,2),stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (DMIc (npts,nvm,2),stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (DMIf (npts,nvm,2),stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (NER (npts,nvm,2),stat=ier); l_error=l_error .OR. (ier .NE. 0)  
    ALLOCATE (Substrate_grazingwc       (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (Substrate_grazingwn       (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (grazingcstruct            (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (grazingnstruct            (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (DNDFlam                   (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (DNDF                      (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (NDF                       (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (DNDFI                     (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (DNDFstem                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (DNDFear                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (NDFmean                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (NDFlam                   (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (NDFstem                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (NDFear                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)

    ALLOCATE (plam                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (pstem                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (pear                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (MassePondTot                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (grazingstruct                (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (grazinglam                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (grazingstem                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (grazingear                  (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)


!    ALLOCATE (nb_grazingdays            (npts,nvm), stat=ier); l_error=l_error .OR. (ier.NE. 0)
    ALLOCATE (amount_yield              (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (consump                   (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (outside_food              (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (add_nb_ani                (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)

    ALLOCATE (able_grazing                (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
!gmjc
    ALLOCATE (ct_dry                (npts,nvm), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    ALLOCATE (t2m_below_zero        (npts), stat=ier); l_error=l_error .OR. (ier .NE. 0)
    IF ( l_error ) THEN
        STOP 'Animaux_init: error in memory allocation'
    ENDIF

    IF (blabla_pasim) PRINT *, 'PASIM Animals : end of allocation memory in Animals_Orchidee'
    milk              = 0.0
    milknsumprev      = 0.0
    urinensumprev     = 0.0
    milknsum          = 0.0
    ranimalsum        = 0.0
    milkcsum          = 0.0
    urinecsum         = 0.0
    faecescsum        = 0.0
    urinensum         = 0.0
    faecesnsum        = 0.0
    Methanesum        = 0.0
    milksum           = 0.0
    nelgrazingsum     = 0.0
    milkndaily        = 0.0
    faecesndaily      = 0.0
    urinendaily       = 0.0
    milkn             = 0.0
    milkc             = 0.0
    ranimal           = 0.0
    methane           = 0.0
    faecesnsumprev    = 0.0
    stockingstart     = 0
    stockingend       = 0
    wshtotstart(:,:)    = 0.0
    grazingsum        = 0.0
    grazingcsum       = 0.0
    grazingnsum       = 0.0
    grazingc          = 0.0
    grazingn          = 0.0
    grazingnsumprev   = 0.0
    grazingndaily     = 0.0
    forage_complementc= 0.0
    forage_complementn= 0.0
    forage_complementcsum= 0.0
    forage_complementnsum= 0.0
    methane_ani       = 0.0
    methane_aniSum    = 0.0
    milkanimalsum     = 0.0
    milkanimal        = 0.0
    MPcowsum=0.0
    MPcow2sum=0.0
    MPcowN=0.0
    MPcowC=0.0
    MPcowCsum=0.0
    MPcowNsum=0.0
    DMIcowsum=0.0
    DMIcowNsum=0.0
    DMIcowCsum=0.0
    DMIcowanimalsum=0.0
    DMIcalfanimalsum=0.0
    Wanimalcow    = 0.0
    BCScow        = 0.0
    AGEcow       = 0.0
    Forage_quantity_period = 0.0
    Wanimalcalf       = 0.0
    Wanimalcalfinit   = 0.0
    nanimaltot_prec   = 0.0
    compte_pature     = 0.0
    autogestion_weightcow = 0.0
    autogestion_BCScow = 0.0
    autogestion_AGEcow = 0.0
    QIc= 0.0
    EVf = 0.0
    EVc = 0.0
    FVf = 0.0
    autogestion_init = 0.0
    NEBcow_prec= 0.0
    MPwmax=0.0
    NER = 0.0
    DNDF = 0.0
    NDF = 0.0
    DNDFI = 0.0
    NDFmean                  = 0.0
    NDFear                    = 0.80     !!! @equation principal::NDFear
    NDFlam                    = 0.60     !!! @equation principal::NDFlam
    NDFstem                   = 0.70     !!! @equation principal::NDFstem

    DNDFstem                 = 0.0
    DNDFlam                  = 0.0
    DNDFear                  = 0.0
    pstem                    = 0.0
    plam                     = 0.0
    pear                     = 0.0
    MassePondTot             = 0.0
    grazingstruct            = 0.0
    grazinglam               = 0.0
    grazingstem              = 0.0
    grazingear               = 0.0


    BM_threshold=0.0
    BM_threshold_turnout = 0.0
    IF(type_animal.EQ.1) THEN
          BM_threshold=LOG10((1.-intake_tolerance)/16.95)/(-0.00275*10000)
          BM_threshold_turnout = LOG10((1- (intake_tolerance +0.1))/16.95)/(-0.00275*10000)
    ELSE
          BM_threshold=LOG10(1.-intake_tolerance)/(-0.0012*10000)
          BM_threshold_turnout=LOG10(1-(intake_tolerance +0.1))/(-0.0012*10000)
    ENDIF
!print *,'BM_threshold',BM_threshold,BM_threshold_turnout
    DO j=2,nvm
      IF (is_grassland_grazed(j).AND.(.NOT.is_grassland_cut(j)) .AND. &
          (.NOT. is_c4(j)) .AND. (.NOT.is_tree(j)))THEN
        mgraze_C3=j
      END IF
      IF (is_grassland_grazed(j).AND.(.NOT.is_grassland_cut(j)) .AND. &
          (is_c4(j)) .AND. (.NOT.is_tree(j)))THEN
        mgraze_C4=j
      END IF
        IF ( (.NOT.is_grassland_manag(j)) .AND.(.NOT.is_grassland_grazed(j)).AND. &
          (.NOT.is_grassland_cut(j)) .AND. (.NOT. is_c4(j)) .AND. (.NOT.is_tree(j)) &
          .AND. natural(j))THEN
          mnatural_C3=j
        END IF
        IF ( (.NOT.is_grassland_manag(j)) .AND.(.NOT.is_grassland_grazed(j)).AND. &
          (.NOT.is_grassland_cut(j)) .AND. (is_c4(j)) .AND. (.NOT.is_tree(j)) &
          .AND. natural(j))THEN
          mnatural_C4=j
        END IF
    END DO
!    nb_grazingdays(:,:) = 0.0
    amount_yield(:,:) = 0.0
    consump(:,:) = 0.0
    outside_food(:,:) = 0.0
    add_nb_ani(:,:) = 0.0
!gmjc
    ct_dry(:,:) = 11.0
    t2m_below_zero(:) = 0.0
    IF (f_postauto .NE. 1) THEN

          Local_autogestion_out = 0.0

          ugb            = 0

          ok_ugb         = 1

          delai_ugb=-15
    ELSE

          Local_autogestion_out = 0.0

          ugb            = 0

          ok_ugb         = 1

          delai_ugb=-15

    ENDIF


    IF ((f_autogestion .GE. 2) .OR. (f_postauto .NE. 0)) THEN

        ok_ugb = 0

    ENDIF
   

  END SUBROUTINE Animal_Init



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!  GRAZING INTAKE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE Grazing_intake(&
     npts              , &
     dt                , &
     wsh               , &
     intakemax         , &
     Animalwgrazingmin , &
     AnimalkintakeM    , &
     intake            , &
     intakesum         , &
     tanimal           , &
     danimal           , &
     tjulian           , &
     intakensum        , &
     fn                , &
     n                 , &
     intake_animal     , &
     intake_animalsum  , &
     nanimaltot        , &
     intake_litter     , &
     intake_animal_litter, &
     grazing_litter)

    !! Declarations des variables
    INTEGER(i_std)                    , INTENT(in)  :: npts
    REAL(r_std)                 , INTENT(in)  :: dt
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: wsh
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: intakemax  

    ! variables dependant du type des animaux sur les prairies
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: Animalwgrazingmin ! 0.03
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: AnimalkintakeM
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: intake
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout) :: intakesum
    ! Yearly intake per m2 (kg m-2 y-1)  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)   :: intake_animal
    ! Daily intake per animal(kg animal-1 d-1)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout) :: intake_animalsum
    ! Yearly intake per animal(kg animal-1 y-1)

    REAL(r_std), DIMENSION(npts,nvm,nstocking), INTENT(in) :: tanimal
    ! début du paturage    h (1,..,nstocking) (d)
    REAL(r_std), DIMENSION(npts,nvm,nstocking), INTENT(in) :: danimal
    ! durée du paturage    h (1,..,nstocking) (d)
    INTEGER(i_std), INTENT(in)                     :: tjulian
    ! Julian day (-)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout) :: intakensum
    ! N in daily intake per m2(kgN/m2)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)    :: fn
    ! nitrogen in structural dry matter
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)    :: n
    ! nitrogen substrate concentration in plant,(kg n/kg)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)    :: nanimaltot
    ! Stocking rate (animal m-2)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: intake_litter
    ! Daily intake per animal(kg animal-1 d-1)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)   :: intake_animal_litter    
    INTEGER(i_std), DIMENSION(npts,nvm), INTENT(in)    :: grazing_litter

    INTEGER           :: i,h,j
    REAL(r_std), DIMENSION(npts,nvm)  ::temp

    intake = 0.0
    intake_animal = 0.0
    intake_litter = 0.0
    intake_animal_litter = 0.0

    IF (f_autogestion .NE. 5 .AND. f_postauto .NE. 5) THEN
    !grazing intake per animal
    ! JC MODIF for global simulation
    ! start to have intake after 5gDM/m^2
    WHERE ((wsh - (Animalwgrazingmin-0.025)) .LE. 0.0)

        intake_animal = 0.0

        intake = 0.0

    ELSEWHERE (wsh .GE. 0.150)

        intake_animal = intakemax * &
           ((wsh - Animalwgrazingmin)** AnimalqintakeM/ &
           ((AnimalkintakeM - Animalwgrazingmin)**AnimalqintakeM + &
           (wsh - Animalwgrazingmin)**AnimalqintakeM))

        intake = intake_animal * nanimaltot
 
    ELSEWHERE (wsh .LT. 0.150 .and. ((wsh - (Animalwgrazingmin-0.025)) .GT. 0.0))

        intake_animal = intakemax * 0.8

        intake = intake_animal * nanimaltot

    END WHERE


    WHERE (nanimaltot .EQ.0)
     intake_animal=0.0
    ENDWHERE
    ! cumulated value

    DO j=2,nvm
      DO i=1,npts
        h  = 1
        !DO WHILE(h .LT. nstocking)
        ! During the grazing period, wich begins at tanimal and finishes at tanimal+danimal
           IF((tjulian .GE. tanimal(i,j,h)) .AND. &
                (tjulian .LT. (tanimal(i,j,h) + danimal(i,j,h)))) THEN
            CALL Euler_funct(1, dt, intake(i,j), intakesum(i,j))
            CALL Euler_funct(1, dt, intake_animal(i,j), intake_animalsum(i,j))
            temp(i,j)=intake(i,j)*(n(i,j)+fn(i,j))
            CALL Euler_funct(1, dt, temp(i,j), intakensum(i,j))
          ENDIF
          h= h+1
        !ENDDO
      ENDDO
    ENDDO

    ELSEIF (f_autogestion .EQ. 5 .OR. f_postauto .EQ. 5) THEN
      
      WHERE (ugb(:,:) .EQ. 1 .AND. grazing_litter(:,:) .EQ. 0 &
            & .AND. nanimaltot .GT. 0.0 )
        intake_animal = 18.0 ! 20kgDM/LSU/day for grazing biomass 
        intake = intake_animal * nanimaltot
        intake_animal_litter = 0.0
        intake_litter =0.0
      ELSEWHERE (ugb(:,:) .EQ. 1 .AND. grazing_litter(:,:) .EQ. 1 &
            & .AND. nanimaltot .GT. 0.0 )
        intake_animal = 0.0 ! 10kgDM/LSU/day for grazing litter
        intake = 0.0
        intake_animal_litter = 10.0
        intake_litter = intake_animal_litter * nanimaltot
      ELSEWHERE
        intake_animal = 0.0 
        intake = 0.0
        intake_animal_litter = 0.0
        intake_litter =0.0
      ENDWHERE

    ENDIF
  END SUBROUTINE Grazing_intake

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!! MILK ANIMAL
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE Milk_Animal(&
     npts      , &
     dt        , &
     nel       , &
     intake_animal , &
     wanimal   , &
     nanimaltot )

    !! Déclaration des variables
    INTEGER(i_std)                    , INTENT(in)  :: npts
    REAL(r_std)                 , INTENT(in)  :: dt
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: nel
    !nettoenergie laktation (mj/kg)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: intake_animal
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: wanimal
    !lebendgewicht laktierender kuehe (kg)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: nanimaltot
    !beweidungsdichte (gve/m**2)
    INTEGER           :: j

    !JCMODIF for global simulation assuming no milk production
    IF (f_autogestion .EQ. 0 .AND. f_postauto .EQ. 0 ) THEN

    !(forschungsanstalt posieux, 1994)
    WHERE (nanimaltot  .GT. 0)
    milkanimal = MAX(0.0,(nel*intake_animal - (wanimal/20.0 + 5.0))/3.14)

    milk       = nanimaltot *milkanimal 
    milkc      = 0.0588*milk 
    milkn      = 0.00517*milk 
    ELSEWHERE
      milkanimal = 0.0
      milk = 0.0
      milkc = 0.0
      milkn = 0.0
    END WHERE

    DO j=2,nvm
      CALL Euler_funct(npts, dt, milk(:,j) , milksum(:,j))
      CALL Euler_funct(npts, dt, milkc(:,j), milkcsum(:,j))
      CALL Euler_funct(npts, dt, milkn(:,j), milknsum(:,j))

      milkndaily(:,j)  = milknsum(:,j)  - milknsumprev(:,j) 
      CALL Euler_funct(npts, dt, nel(:,j)*intake_animal(:,j)*nanimaltot(:,j) , nelgrazingsum(:,j))

      CALL Euler_funct(npts, dt, milkanimal(:,j), milkanimalsum(:,j))
      !!! @equation animaux::milkanimalsum
    END DO
 
    ELSE ! all other auto management
      milkanimal = 0.0
      milk = 0.0
      milkc = 0.0
      milkn = 0.0
    ENDIF

  END SUBROUTINE Milk_Animal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!! RESPIRATION METHANE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE Respiration_Methane(&
     npts       , &
     dt         , &
     grazingc   , &
     nanimaltot, DNDFI, wanimal)

    INTEGER(i_std)                    , INTENT(in)  :: npts
    REAL(r_std)                 , INTENT(in)  :: dt
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: grazingc
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: nanimaltot 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: DNDFI
    ! Amount of digestible neutral detergent fiber in the intake (kg d-1)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: Wanimal
    ! Animal life weight (kg)

    ! variables locales
    REAL(r_std), DIMENSION(npts,nvm) :: methane_ani !c im methan (kg c /(m**2*d))
    INTEGER           :: j

    !respiration and methane loss
    !(minonzio et al., 1998)

    ranimal = franimal * grazingc 
    
    methane = fmethane * grazingc 

    WHERE (nanimaltot  .GT. 0.0)

        WHERE((aCH4 + bCH4 * DNDFI) .GE. 0.0)

        !(2) p88 equation (1)
        ! Inversion de ach4 & bch4

            methane_ani = (ach4 + bch4 * DNDFI)*wanimal*ch4toc
            methane  = methane_ani*nanimaltot

        ELSEWHERE

            methane = 0.0
            methane_ani = 0.0

        END WHERE


    ELSEWHERE
        methane = 0.0
        methane_ani = 0.0
    END WHERE
    DO j=2,nvm  
      CALL Euler_funct(npts, dt, ranimal(:,j), ranimalsum(:,j))
    
      CALL Euler_funct(npts, dt, methane(:,j), Methanesum(:,j))

      CALL Euler_funct(npts, dt, methane_ani(:,j), Methane_aniSum(:,j)) 

    ENDDO
  END SUBROUTINE Respiration_Methane

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!! URINE FAECES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE Urine_Faeces(&
     npts      , &
     dt        , &
     grazingn  , &
     grazingc  , &
     urinen    , &
     faecesn   , &
     urinec    , &
     faecesc  )

    INTEGER(i_std)                    , INTENT(in)  :: npts
    REAL(r_std)                 , INTENT(in)  :: dt
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: grazingn
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: grazingc       
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: urinen   
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: faecesn  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: urinec
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: faecesc

    ! variables locales
    REAL(r_std), DIMENSION(npts,nvm) :: excretan 
    INTEGER           :: j
    !urine and faeces
    !(thornley 1998)

    !n in excreta
    excretan = grazingn - milkn 

    ! équation (4.4d) de "Grassland dynamics" Thornley

    urinen   = fnurine*excretan 
    faecesn  = (1.0 - fnurine)*excretan 


    DO j=2,nvm
      CALL Euler_funct(npts, dt, urinen, urinensum(:,j))
      urinendaily(:,j)  = urinensum(:,j)  - urinensumprev(:,j) 

      CALL Euler_funct(npts, dt, faecesn(:,j), faecesnsum(:,j))
      faecesndaily(:,j)  = faecesnsum(:,j)  - faecesnsumprev(:,j) 
    END DO
    !c respired and in excreta
    ! équation (4.4e) de "grassland dynamics" thornley
    urinec  = fnurine*excretan*12.0/28.0
    ! = urinen 12.0/28.0 
    ! 12 => un atome de C
    ! 28 => deux atomes de N

    faecesc = &
       grazingc   - &  ! gross C intake  
       milkc      - &  ! lait
       ranimal    - &  ! maintenance respiration
       methane    - &  ! methane production 
       urinec          ! urine            



    DO j=2,nvm
      CALL Euler_funct(npts, dt, urinec(:,j), urinecsum(:,j))
      CALL Euler_funct(npts, dt, faecesc(:,j), faecescsum(:,j))
    ENDDO

  END SUBROUTINE Urine_Faeces



! ******************************************************************************
!!!!!!!!!!!!   JCmodif 110525 del calculation of grazingc and grazingn
!!!!!!!!!!!!   they have been moved before Respiration
   
  SUBROUTINE nel_grazing_calcul(&
     npts                 , &
     dt                   , &
     nanimaltot         , &
     devstage             , &
     tgrowth              , &
     nel                  , &
     ntot)


    INTEGER(i_std)                    , INTENT(in)  :: npts
    ! r_std du domaine
    REAL(r_std)                 , INTENT(in)  :: dt
    ! pas de temps
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: nanimaltot
    ! nombre d'animaux 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: devstage
    ! stade de développement de la pousse        
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: tgrowth
    ! instant de repousse de la coupe actuelle(d)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: nel
    ! energie nette de lactation (mj/kg)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: ntot
    ! concentration en n totale (kg n/kg) 


    ! variables locales :
    REAL(r_std), DIMENSION(npts,nvm)     :: os
    ! organische substanz (kg/kg)
    REAL(r_std), DIMENSION(npts,nvm)     :: rp
    ! rohproteingehalt (kg/kg)
    REAL(r_std), DIMENSION(npts,nvm)     :: be
    ! bruttoenergie (mj/kg)
    REAL(r_std), DIMENSION(npts,nvm)     :: vos
    ! verdauliche organische substanz (kg/kg) 
    REAL(r_std), DIMENSION(npts,nvm)     :: fvos
    REAL(r_std), DIMENSION(npts,nvm)     :: vp
    REAL(r_std), DIMENSION(npts,nvm)     :: ue
    ! energie métabolisable (mj/kg)
    REAL(r_std), DIMENSION(npts,nvm)     :: knel
    REAL(r_std), DIMENSION(npts,nvm)     :: rf
    ! rohfasergehalt (concentration en cellulose) (kg/kg) 
    REAL(r_std), DIMENSION(npts,nvm)     :: temp_ratio
  
    os     (:,:) = 0.0
    rp     (:,:) = 0.0
    be     (:,:) = 0.0
    vos    (:,:) = 0.0
    fvos   (:,:) = 0.0
    vp     (:,:) = 0.0
    ue     (:,:) = 0.0
    knel   (:,:) = 0.0
    rf     (:,:) = 0.0
   
    !calcul de nel
    os(:,:)  = 0.9
    rp(:,:)  = 6.25*ntot(:,:) 
    be(:,:)  = 18.8*os(:,:) + 7.8 *rp (:,:)
    
    WHERE (devstage .LT. 2.0)
        
        rf  = MIN (rf7 , rf1 + (rf3 - rf1)*devstage/devear)
        
    ELSEWHERE (nanimaltot  .LE. 0.0)
        
        rf = MIN (rf7, rf1 + (rf3 - rf1)*tgrowth/49.0)
        
    ELSEWHERE
        rf  = rf1 
        
    END WHERE
       
   
    fvos(:,:)  = 0.835 + &
       0.114*rp(:,:) /os(:,:)  - &
       1.45*(rf(:,:) /os(:,:) )**2
    
    vos(:,:)  = fvos(:,:) *os(:,:) 
    
    vp(:,:)  = rp(:,:) * (0.33 + 3.3*rp(:,:)/os(:,:) - 6.1*(rp(:,:)/os(:,:))**2)

    WHERE (vp .GT. 0.0) 
      temp_ratio=vos/vp
    ELSEWHERE
      temp_ratio=8.
    ENDWHERE
      WHERE (temp_ratio .LT. 7.0)
   
        ue =  14.2*vos + 5.9 *vp 
   
      ELSEWHERE
   
        ue = 15.1*vos 
   
      END WHERE
   
    knel(:,:)  = 0.463 + 0.24*ue(:,:) /be(:,:) 
    
    nel(:,:)  = knel(:,:) * ue(:,:) * 0.9752
   
   
    
  END SUBROUTINE nel_grazing_calcul





!  SUBROUTINE deallocation_animaux
  SUBROUTINE animal_clear
    INTEGER(i_std) :: ier
    IF (ALLOCATED(milk )) DEALLOCATE (milk             )
    IF (ALLOCATED(milkn )) DEALLOCATE (milkn             )
    IF (ALLOCATED(milkc )) DEALLOCATE (milkc             )
    IF (ALLOCATED(ranimal )) DEALLOCATE (ranimal           )
    IF (ALLOCATED(methane )) DEALLOCATE (methane           )
    IF (ALLOCATED(faecesnsumprev )) DEALLOCATE (faecesnsumprev    )
    IF (ALLOCATED(milkndaily )) DEALLOCATE (milkndaily        )
    IF (ALLOCATED(faecesndaily )) DEALLOCATE (faecesndaily      )
    IF (ALLOCATED(urinendaily )) DEALLOCATE (urinendaily       )
    IF (ALLOCATED(milksum )) DEALLOCATE (milksum           )
    IF (ALLOCATED(nelgrazingsum )) DEALLOCATE (nelgrazingsum     )
    IF (ALLOCATED(ranimalsum )) DEALLOCATE (ranimalsum        )
    IF (ALLOCATED(milkcsum )) DEALLOCATE (milkcsum          )
    IF (ALLOCATED(Methanesum )) DEALLOCATE (Methanesum        )
    IF (ALLOCATED(urinecsum )) DEALLOCATE (urinecsum         )
    IF (ALLOCATED(faecescsum )) DEALLOCATE (faecescsum        )
    IF (ALLOCATED(urinensum )) DEALLOCATE (urinensum         )
    IF (ALLOCATED(faecesnsum )) DEALLOCATE (faecesnsum        )
    IF (ALLOCATED(milknsum )) DEALLOCATE (milknsum          )
    IF (ALLOCATED(milknsumprev )) DEALLOCATE (milknsumprev      )
    IF (ALLOCATED(urinensumprev )) DEALLOCATE (urinensumprev     )
    IF (ALLOCATED(stockingstart )) DEALLOCATE (stockingstart     )
    IF (ALLOCATED(stockingend )) DEALLOCATE (stockingend       )
    IF (ALLOCATED(wshtotstart )) DEALLOCATE (wshtotstart       )
    IF (ALLOCATED(grazingsum )) DEALLOCATE (grazingsum        )
    IF (ALLOCATED(grazingcsum )) DEALLOCATE (grazingcsum       )
    IF (ALLOCATED(grazingnsum )) DEALLOCATE (grazingnsum       )
    IF (ALLOCATED(grazingc )) DEALLOCATE (grazingc          )
    IF (ALLOCATED(grazingn )) DEALLOCATE (grazingn          )
    IF (ALLOCATED(grazingnsumprev )) DEALLOCATE (grazingnsumprev   )
    IF (ALLOCATED(grazingndaily )) DEALLOCATE (grazingndaily     )
    IF (ALLOCATED(forage_complementc)) DEALLOCATE(forage_complementc)
    IF (ALLOCATED(forage_complementn)) DEALLOCATE(forage_complementn)
    IF (ALLOCATED(forage_complementcsum)) DEALLOCATE(forage_complementcsum)
    IF (ALLOCATED(forage_complementnsum)) DEALLOCATE(forage_complementnsum)
    IF (ALLOCATED(methane_ani)) DEALLOCATE(methane_ani)
    IF (ALLOCATED(methane_aniSum)) DEALLOCATE(methane_aniSum)
    IF (ALLOCATED(milkanimalsum)) DEALLOCATE(milkanimalsum)
    IF (ALLOCATED(milkanimal)) DEALLOCATE(milkanimal)
    IF (ALLOCATED(ugb)) DEALLOCATE(ugb)
    IF (ALLOCATED(ok_ugb)) DEALLOCATE(ok_ugb)
    IF (ALLOCATED(extra_feed)) DEALLOCATE(extra_feed)
    IF (ALLOCATED(Wanimalcow)) DEALLOCATE(Wanimalcow)
    IF (ALLOCATED(BCScow)) DEALLOCATE(BCScow)
    IF (ALLOCATED(BCScow_prev)) DEALLOCATE(BCScow_prev)
    IF (ALLOCATED(AGEcow)) DEALLOCATE(AGEcow)
    IF (ALLOCATED(Forage_quantity_period)) DEALLOCATE(Forage_quantity_period)
    IF (ALLOCATED(MPcowCsum)) DEALLOCATE(MPcowCsum)
    IF (ALLOCATED(MPcowNsum)) DEALLOCATE(MPcowNsum)
    IF (ALLOCATED(MPcowN)) DEALLOCATE(MPcowN)
    IF (ALLOCATED(MPcowC)) DEALLOCATE(MPcowC)
    IF (ALLOCATED(MPcowsum)) DEALLOCATE(MPcowsum)
    IF (ALLOCATED(MPcow2sum)) DEALLOCATE(MPcow2sum)
    IF (ALLOCATED(MPcow2_prec)) DEALLOCATE(MPcow2_prec)
    IF (ALLOCATED(DMIcowsum)) DEALLOCATE(DMIcowsum)
    IF (ALLOCATED(DMIcowNsum)) DEALLOCATE(DMIcowNsum)
    IF (ALLOCATED(DMIcowCsum)) DEALLOCATE(DMIcowCsum)
    IF (ALLOCATED(DMIcowanimalsum)) DEALLOCATE(DMIcowanimalsum)
    IF (ALLOCATED(Wanimalcalf)) DEALLOCATE(Wanimalcalf)
    IF (ALLOCATED(DMIcalfsum)) DEALLOCATE(DMIcalfsum)
    IF (ALLOCATED(DMIcalfnsum)) DEALLOCATE(DMIcalfnsum)
    IF (ALLOCATED(DMIcalfanimalsum)) DEALLOCATE(DMIcalfanimalsum)
    IF (ALLOCATED(Tcalving)) DEALLOCATE(Tcalving)
    IF (ALLOCATED(Tsevrage)) DEALLOCATE(Tsevrage)
    IF (ALLOCATED(Age_sortie_calf)) DEALLOCATE(Age_sortie_calf)
    IF (ALLOCATED(Pyoung)) DEALLOCATE(Pyoung)
    IF (ALLOCATED(Wcalfborn)) DEALLOCATE(Wcalfborn)
    IF (ALLOCATED(calfinit)) DEALLOCATE(calfinit)
    IF (ALLOCATED(Wanimalcalfinit)) DEALLOCATE(Wanimalcalfinit)
    IF (ALLOCATED(calf)) DEALLOCATE(calf)
    IF (ALLOCATED(nanimaltot_prec)) DEALLOCATE(nanimaltot_prec)
    IF (ALLOCATED(Gestation)) DEALLOCATE(Gestation)
    IF (ALLOCATED(compte_pature)) DEALLOCATE(compte_pature)
    IF (ALLOCATED(autogestion_weightcow)) DEALLOCATE(autogestion_weightcow)
    IF (ALLOCATED(autogestion_BCScow)) DEALLOCATE(autogestion_BCScow)
    IF (ALLOCATED(autogestion_AGEcow)) DEALLOCATE(autogestion_AGEcow)
    IF (ALLOCATED(autogestion_init)) DEALLOCATE(autogestion_init)
    IF (ALLOCATED(QIc)) DEALLOCATE(QIc)
    IF (ALLOCATED(EVf)) DEALLOCATE(EVf)
    IF (ALLOCATED(EVc)) DEALLOCATE(EVc)
    IF (ALLOCATED(FVf)) DEALLOCATE(FVf)
    IF (ALLOCATED(fN_forage)) DEALLOCATE(fN_forage)
    IF (ALLOCATED(fN_concentrate)) DEALLOCATE(fN_concentrate)
    IF (ALLOCATED(NEBcow_prec)) DEALLOCATE(NEBcow_prec)
    IF (ALLOCATED(MPwmax)) DEALLOCATE(MPwmax)
    IF (ALLOCATED(Fday_pasture)) DEALLOCATE(Fday_pasture)
    IF (ALLOCATED(delai_ugb)) DEALLOCATE(delai_ugb)
    IF (ALLOCATED(Local_autogestion_out)) DEALLOCATE(Local_autogestion_out)
    IF (ALLOCATED(PEmax)) DEALLOCATE(PEmax)
    IF (ALLOCATED(PEpos)) DEALLOCATE(PEpos)
    IF (ALLOCATED(DMIc)) DEALLOCATE(DMIc)
    IF (ALLOCATED(DMIf)) DEALLOCATE(DMIf)
    IF (ALLOCATED(NER)) DEALLOCATE(NER)
    IF (ALLOCATED(Substrate_grazingwc)) DEALLOCATE(Substrate_grazingwc)
    IF (ALLOCATED(Substrate_grazingwn)) DEALLOCATE(Substrate_grazingwn)
    IF (ALLOCATED(grazingcstruct)) DEALLOCATE(grazingcstruct)
    IF (ALLOCATED(grazingnstruct)) DEALLOCATE(grazingnstruct)
    IF (ALLOCATED(DNDFlam)) DEALLOCATE(DNDFlam)
    IF (ALLOCATED(DNDF)) DEALLOCATE(DNDF)
    IF (ALLOCATED(NDF)) DEALLOCATE(NDF)
    IF (ALLOCATED(DNDFI)) DEALLOCATE(DNDFI)
    IF (ALLOCATED(DNDFstem)) DEALLOCATE(DNDFstem)
    IF (ALLOCATED(DNDFear)) DEALLOCATE(DNDFear)
    IF (ALLOCATED(NDFmean)) DEALLOCATE(NDFmean)
    IF (ALLOCATED(NDFlam)) DEALLOCATE(NDFlam)
    IF (ALLOCATED(NDFstem)) DEALLOCATE(NDFstem)
    IF (ALLOCATED(NDFear)) DEALLOCATE(NDFear)
    IF (ALLOCATED(plam)) DEALLOCATE(plam)
    IF (ALLOCATED(pstem)) DEALLOCATE(pstem)
    IF (ALLOCATED(pear)) DEALLOCATE(pear)
    IF (ALLOCATED(MassePondTot)) DEALLOCATE(MassePondTot)
    IF (ALLOCATED(grazingstruct)) DEALLOCATE(grazingstruct)
    IF (ALLOCATED(grazinglam)) DEALLOCATE(grazinglam)
    IF (ALLOCATED(grazingstem)) DEALLOCATE(grazingstem)
    IF (ALLOCATED(grazingear)) DEALLOCATE(grazingear)
!    IF (ALLOCATED(nb_grazingdays)) DEALLOCATE(nb_grazingdays)
    IF (ALLOCATED(amount_yield)) DEALLOCATE(amount_yield)
    IF (ALLOCATED(consump)) DEALLOCATE(consump)
    IF (ALLOCATED(outside_food)) DEALLOCATE(outside_food)
    IF (ALLOCATED(add_nb_ani)) DEALLOCATE(add_nb_ani)
    IF (ALLOCATED(able_grazing)) DEALLOCATE(able_grazing)
!gmjc
    IF (ALLOCATED(ct_dry)) DEALLOCATE(ct_dry)


  END SUBROUTINE animal_clear
!  END SUBROUTINE deallocation_animaux

  SUBROUTINE cal_grazing(&
     npts                  , &
     nanimaltot            , &
     intake_animal         , &
     wsh                   , &
     wshtot                , &
     c                     , &
     n                     , &
     fn                    , &
     Substrate_grazingwc  , &
     Substrate_grazingwn  , &
     grazingcstruct        , &
     grazingnstruct        , &
     intake)

    ! liste des variables d'entrée
    INTEGER (i_std)                   , INTENT(in)  :: npts
    ! nombre de points de simulations                    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: nanimaltot
    ! densité de paturage (gve/m**2)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: intake_animal
    ! ingéré
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: wsh
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: wshtot
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: c
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: n
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: fn
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: Substrate_grazingwc
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: Substrate_grazingwn
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: grazingcstruct
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: grazingnstruct
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in) :: intake

    WHERE (wshtot .GT. 0.0)

        Substrate_grazingwc  = intake*c * wsh/wshtot
        Substrate_grazingwn  = intake*n * wsh/wshtot
        grazingstruct   = intake * wsh/wshtot

        grazingcstruct  = fcsh * grazingstruct ! kg C/(m2d)
        grazingnstruct  = fn   * grazingstruct ! kg N/(m2d)

    ELSEWHERE (wshtot .EQ. 0.0)

        Substrate_grazingwc  = 0.0
        Substrate_grazingwn  = 0.0

        grazingstruct   = 0.0
        grazingcstruct  = fcsh * grazingstruct ! kg C/(m2d)
        grazingnstruct  = fn   * grazingstruct ! kg N/(m2d)

    END WHERE


  END SUBROUTINE cal_grazing

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!   chg_plante was introduced from Grassland_Management, put after intake calculation
!!!!!!!!   to get the biomass change, and calculate DNDF NDF & DNDFI for dynamic
!!!!!!!!   DNDF NDF & DNDFI were cited from SUBROUTINE variablesPlantes of PASIM2011
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE chg_plante(&
     npts, dt, biomass  , &
     c, n,leaf_frac     , & 
     wsh, wshtot        , &
     nanimaltot, intake_animal, &
     trampling,intake, &
     NDF,DNDF,DNDFI, &
     grazing_litter)

    ! idée : enlever un pourcentage de la masse sèche de la limbe, et de la tige (et de l'épis ??)
    ! idea: remove a percentage of the dry mass of leaf and stem (and ears?)

    ! 1. variables d'entrées de la subroutine
    ! input variables of the subroutine

    INTEGER(i_std)                                , INTENT(in)   :: npts
    REAL(r_std)                             , INTENT(in)   :: dt
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout):: biomass
    ! totalité de masse sèche du shoot (kg/m2)  --> total dry mass of shoot
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: c
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: n    
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout)       :: leaf_frac
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: wsh
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: wshtot
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: nanimaltot
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: intake_animal
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)       :: trampling
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: intake
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)       :: DNDF
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)       :: NDF
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)       :: DNDFI
    INTEGER(i_std), DIMENSION(npts,nvm), INTENT(in)    :: grazing_litter

    REAL(r_std), DIMENSION(npts,nvm) :: wlam
    ! masse sèche (structurelle) de la limbe (kg/m2) ----> dry mass (structural) of the lamina  
    REAL(r_std), DIMENSION(npts,nvm) :: wst
    ! masse sèche (structurelle) de la tige  (kg/m2) ----> dry mass (structural) of the stem
    REAL(r_std), DIMENSION(npts,nvm) :: wear
    ! masse sèche (structurelle) de la tige  (kg/m2) ----> dry mass (structural) of the ear
    REAL(r_std), DIMENSION(npts,nvm) :: lm_old_ani

    REAL(r_std), DIMENSION(npts,nvm) :: tmp_fracsum
    REAL(r_std), DIMENSION(npts,nvm,nleafages) :: tmp_frac
    INTEGER(i_std) :: m

    REAL(r_std), DIMENSION(npts,nvm)     :: fGrazinglam
    REAL(r_std), DIMENSION(npts,nvm)     :: PlantLaminazlamgrazing
    REAL(r_std), DIMENSION(npts,nvm)     :: fGrazingstem
    REAL(r_std), DIMENSION(npts,nvm)     :: PlantEarzeargrazing
    REAL(r_std), DIMENSION(npts,nvm)     :: PlantStemzstemgrazing

    DNDF           (:,:) = 0.0
    NDF            (:,:) = 0.0
    DNDFI          (:,:) = 0.0
! Initialisations    
    fGrazinglam             (:,:) = 0.0
    PlantLaminazlamgrazing  (:,:) = 0.0
    fGrazingstem            (:,:) = 0.0
    PlantEarzeargrazing     (:,:) = 0.0
    PlantStemzstemgrazing   (:,:) = 0.0
    lm_old_ani(:,:) = 0.0

    IF (blabla_pasim) PRINT *, 'PASIM main grassland : call chg_plante'


    wlam(:,:) = (biomass(:,:,ileaf,icarbon)/(1000*CtoDM)) / &
         (1.0 + (mc /12.0)*c(:,:) + (mn /14.0)*n(:,:) )      ! leaf dry mass 
    wst(:,:)  = (biomass(:,:,isapabove,icarbon)/(1000*CtoDM)) / &
         (1.0 + (mc /12.0)*c(:,:) + (mn /14.0)*n(:,:) )      ! stem dry mass
    wear(:,:) = (biomass(:,:,ifruit,icarbon)/(1000*CtoDM)) / &
         (1.0 + (mc /12.0)*c(:,:) + (mn /14.0)*n(:,:) )      ! ear dry mass

    WHERE (wshtot .GT. 0.0)
        grazingstruct = intake * wsh/wshtot
    ELSEWHERE 

        grazingstruct = 0.0

    END WHERE
    !!!!!!!!
    !gmjc 130418 component selection in animal intake
    !!!!!!!!
    WHERE ((wlam .GT. 0.0) .AND. (MassePondTot .GT. 0.0) &
              .AND. (grazingstruct .GT. 0.0))
        ! # factor of lam structural dry mass preference
        fgrazinglam = plam*wlam/MassePondTot

        ! # structural dry matter flux from LAMS into the animal per unit ground aera
        grazinglam = fgrazinglam*grazingstruct

        ! # fraction of the intake in the available lam strutural dry mass
        PlantLaminazlamgrazing = grazinglam/(wlam)

        DNDFlam = &
           DNDFlam1*leaf_frac(:,:,1) + &
           DNDFlam2*leaf_frac(:,:,2) + &
           DNDFlam3*leaf_frac(:,:,3) + &
           DNDFlam4*leaf_frac(:,:,4)

    ELSEWHERE

        fgrazinglam  = 0.

        grazinglam = 0.     

        plam = 0.0

        PlantLaminazlamgrazing = 0.0

        DNDFlam = 0.0

    END WHERE

    ! updating leaf dry mass
    wlam = wlam * (1. - PlantLaminazlamgrazing)
    WHERE (wlam .LT. 0.0)
        wlam = 0.0 
    ENDWHERE

    IF (ANY(PlantLaminazlamgrazing .GT. 1.0)) THEN
      print *, 'warning: Component LAM not enough for grazing'
      print *, grazingstruct(:,5)
      print *, wlam(:,5)
    ENDIF
    IF (ANY(PlantLaminazlamgrazing .LT. 0.0))  print *, 'warning: Component LAM over grazing'
!print *, 'PlantLam'
    WHERE ((wst .GT. 0.0) .AND. (MassePondTot .GT. 0.0) .AND. &
         (grazingstruct .GT. 0.0))
        ! # factor of stem structural dry mass preference
        fgrazingstem = pstem*wst/MassePondTot

        ! # structural dry matter flux from STEMS into the animal per unit ground aera
        grazingstem = fgrazingstem*grazingstruct

        ! # fraction of the intake in the available stem strutural dry mass
        PlantStemzstemgrazing = grazingstem/wst

        DNDFstem = &
           DNDFstem1*leaf_frac(:,:,1) + &
           DNDFstem2*leaf_frac(:,:,2) + &
           DNDFstem3*leaf_frac(:,:,3) + &
           DNDFstem4*leaf_frac(:,:,4)

   ELSEWHERE

        fgrazingstem  = 0.

        grazingstem = 0.

        PlantStemzstemgrazing = 0.0

        pstem = 0.0

        DNDFstem = 0.0

    END WHERE
!gmjc 20141121 for avoid over grazing stem and leaf simutaneously
    WHERE ((fgrazingstem + fgrazinglam) .GT. 1.0 .AND. (grazingstruct .GT. 0.0) &
           .AND.( wst .GT. 0.0))
      fgrazingstem = 1.0 - fgrazinglam
      grazingstem = fgrazingstem*grazingstruct
      PlantStemzstemgrazing = grazingstem/wst
        DNDFstem = &
           DNDFstem1*leaf_frac(:,:,1) + &
           DNDFstem2*leaf_frac(:,:,2) + &
           DNDFstem3*leaf_frac(:,:,3) + &
           DNDFstem4*leaf_frac(:,:,4)
    ENDWHERE
!end gmjc
    ! updating stem dry mass
    wst = wst * (1. - PlantStemzstemgrazing)
    WHERE (wst .LT. 0.0)
        wst = 0.0
    ENDWHERE

    IF (ANY(PlantStemzstemgrazing .GT. 1.0))  print *, 'warning: Component STEM not enough for grazing'

    IF (ANY(PlantStemzstemgrazing .LT. 0.0))  print *, 'warning: Component STEM over grazing'
!print *, 'PlantStem',PlantStemzstemgrazing(:,6)
! # structural dry matter flux from EARS into the animal per unit ground aera
    grazingear = (1. - fgrazingstem - fgrazinglam)*grazingstruct

    WHERE (wear .GT. 0.0)

        PlantEarzeargrazing =  grazingear/wear

        DNDFear = &
           DNDFear1*leaf_frac(:,:,1) + &
           DNDFear2*leaf_frac(:,:,2) + &
           DNDFear3*leaf_frac(:,:,3) + &
           DNDFear4*leaf_frac(:,:,4)

    ELSEWHERE

        PlantEarzeargrazing = 0.0

        grazingear = 0.0
          
        pear = 0.0

        DNDFear = 0.0

    END WHERE

    ! updating ear dry mass
    wear = wear * (1. - PlantEarzeargrazing)
    WHERE (wear .LT. 0.0)
        wear = 0.0
    ENDWHERE

    IF (ANY(PlantEarzeargrazing .GT. 1.0))  print *, 'warning: Component EAR not enough for grazing' 
    IF (ANY(PlantEarzeargrazing .LT. 0.0))  print *, 'warning: Component STEM LAM over grazing'
!print *, 'PlantEar',PlantEarzeargrazing(:,6)
    !!!!!!!!
    !gmjc 120409 new update leaf_frac for each class
    !!!! we assumed a grazing preference with 70% age class 1, 30% age clas 2 3 4
    WHERE (grazinglam .GT. 0.0 .AND. wlam .GT. 0)
      lm_old_ani=wlam+grazinglam

    WHERE (leaf_frac(:,:,1)*lm_old_ani .GT.  0.90 * grazinglam)
      !!if there is enough biomass of leaf age 1 for eating (0.7 of total intake), animal prefer to eat more
      !young leaf
      leaf_frac(:,:,1) = (leaf_frac(:,:,1)*lm_old_ani - 0.9 * grazinglam)/wlam

    ELSEWHERE
      !!if not enough biomass of leaf age 1 can be eat, only 10% of it left
      leaf_frac(:,:,1) = (leaf_frac(:,:,1)*lm_old_ani * 0.10)/wlam
    END WHERE
    ENDWHERE
    tmp_fracsum(:,:)=0.0
    tmp_frac(:,:,:)= 0.0
    DO m = 2, nleafages
      tmp_frac(:,:,m)= leaf_frac(:,:,m)
      tmp_fracsum(:,:)= tmp_fracsum(:,:)+ tmp_frac(:,:,m)
    ENDDO
    DO m = 2, nleafages
      WHERE (tmp_fracsum(:,:) .GT. 0.0)
      leaf_frac(:,:,m)=tmp_frac(:,:,m)/tmp_fracsum(:,:)*(1.0-leaf_frac(:,:,1))
      ENDWHERE
    ENDDO
!print *,'after frac'
    !!! 05212013 gmjc NDF and DNDF DNDFI in grazed grassland put after grazing
    WHERE (grazingstruct .GT. 0.)

        ! # FRACTION OF DIGESTIBLE FIBRES IN THE TOTAL FIBRES
        ! Vuichard Thesis p.86 equation (4)
        !---------------------    

        DNDF = (&
           DNDFlam  * grazinglam  + &
           DNDFstem * grazingstem + &
           DNDFear  * grazingear) / grazingstruct
    
        ! # FRACTION OF FIBRES IN THE INTAKE
        ! Vuichard Thesis p.86 equation (3)
        !---------------------

        NDF = (&
           NDFlam  * grazinglam  + &
           NDFstem * grazingstem + &
           NDFear  * grazingear) / grazingstruct

    ELSEWHERE
        DNDF = 0.0
        NDF  = 0.0
    END WHERE
    WHERE ((ABS(wlam+wst) .GT. 10e-15) .AND. (intake_animal .GT. 0.0))

        DNDFI = NDF * DNDF * intake_animal * dm2om
    ELSEWHERE
        DNDFI = 0.0
    ENDWHERE


    !!!!!!!!!!!!!!!!!!!!!!!!!!! Trampingling and excretal returns effects
    !! according to Vuichard,2007 an additional 0.8% of the aboveground herbage
    !biomass is returned each day
    !! to litter for an instantaneous stocking rate of 1 LSU/ha
   ! when grazing AGB trampling exist
   ! when grazing litter, now assumed to be without trampling
    WHERE (nanimaltot(:,:) .GT. 0.0 .AND. grazing_litter(:,:) .NE. 1 )
       trampling(:,:) = nanimaltot(:,:) * 10000 * 0.008 * &
               (wlam(:,:)+wst(:,:)+wear(:,:))* 1000*CtoDM * &
               (1.0 + (mc /12.0)*c(:,:) + (mn /14.0)*n(:,:) )       
       wlam(:,:) = wlam(:,:) * (1 - nanimaltot(:,:) * 10000 * 0.008 )
       wst(:,:) = wst(:,:) * (1 - nanimaltot(:,:) * 10000 * 0.008 )
       wear(:,:) =  wear(:,:) * (1 - nanimaltot(:,:) * 10000 * 0.008 )
!!JCMODIF for gaps in NBP calculation
!       trampling(:,:) = nanimaltot * 10000 * 0.008 *(biomass(:,:,ileaf)+biomass(:,:,isapabove)+biomass(:,:,ifruit))

    ELSEWHERE
       trampling(:,:) = 0.0
    ENDWHERE

    biomass(:,:,ileaf,icarbon)     = (wlam(:,:) * 1000*CtoDM) * &
         (1.0 + (mc /12.0)*c(:,:) + (mn /14.0)*n(:,:) )
    biomass(:,:,isapabove,icarbon) = (wst(:,:)  * 1000*CtoDM) * &
         (1.0 + (mc /12.0)*c(:,:) + (mn /14.0)*n(:,:) )
    biomass(:,:,ifruit,icarbon)    = (wear(:,:)  * 1000*CtoDM) * &
         (1.0 + (mc /12.0)*c(:,:) + (mn /14.0)*n(:,:) )



  END SUBROUTINE chg_plante


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!!!!!!!!   variablesPlantes was introduced from Plantes.f90 of PaSim
!!!!!!!!   to get state variables need be intake selection before chg_plante
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE variablesPlantes(&
       npts,biomass,&
       c,n,intake_animal,intakemax,&
       AnimalDiscremineQualite)

    ! 1. variables d'entrées de la subroutine
    ! input variables of the subroutine

    INTEGER(i_std)                                , INTENT(in)   :: npts
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(in):: biomass
    ! totalité de masse sèche du shoot (kg/m2)  --> total dry mass of shoot
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)     :: c
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)     :: n  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: intake_animal
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: intakemax
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  :: AnimalDiscremineQualite

    REAL(r_std), DIMENSION(npts,nvm) :: wlam
    ! masse sèche(structurelle) de la limbe (kg/m2) ----> dry mass (structural) of the lamina
    REAL(r_std), DIMENSION(npts,nvm) :: wst
    ! masse sèche(structurelle) de la tige  (kg/m2) ----> dry mass (structural) of the stem
    REAL(r_std), DIMENSION(npts,nvm) :: wear
    ! masse sèche(structurelle) de la tige  (kg/m2) ----> dry mass (structural) of the ear

    REAL(r_std), DIMENSION(npts,nvm) :: test_lam
    REAL(r_std), DIMENSION(npts,nvm) :: test_stem
    REAL(r_std), DIMENSION(npts,nvm) :: test_ear
    REAL(r_std), DIMENSION(npts,nvm) :: ncomp
    REAL(r_std), DIMENSION(npts,nvm) :: betaGrazing

    REAL(r_std), DIMENSION(npts,nvm) :: DNDF_total
    REAL(r_std), DIMENSION(npts,nvm) :: NDF_total

    REAL(r_std), DIMENSION(npts,nvm) :: exposant_lam
    REAL(r_std), DIMENSION(npts,nvm) :: exposant_stem

    test_lam       (:,:) = 0.0
    test_stem      (:,:) = 0.0
    test_ear       (:,:) = 0.0
    exposant_lam   (:,:) = 0.0
    exposant_stem  (:,:) = 0.0

    IF (blabla_pasim) PRINT *, 'PASIM main grassland : call variablesPlantes'


    wlam(:,:) = (biomass(:,:,ileaf,icarbon)/(1000*CtoDM)) / &
         (1.0 + (mc /12.0) * c(:,:)+ (mn /14.0)*n(:,:) )      ! leaf dry mass 
    wst(:,:)  = (biomass(:,:,isapabove,icarbon)/(1000*CtoDM)) / &
         (1.0 + (mc /12.0) * c(:,:)+ (mn /14.0)*n(:,:) )      ! stem dry mass
    wear(:,:) = biomass(:,:,ifruit,icarbon)/(1000*CtoDM) / &
         (1.0 + (mc /12.0)* c(:,:) + (mn/14.0)*n(:,:) )      ! ear dry mass

    !!!! update state variables from PaSim variablesPlantes
    ! # TEST 
    WHERE (wlam .GT. 0.)
      test_lam = 1.
    ELSEWHERE
      test_lam = 0.
    ENDWHERE
    WHERE (wst .GT. 0.) 
      test_stem = 1.
    ELSEWHERE
      test_stem = 0.
    ENDWHERE
    WHERE (wear .GT. 0.)
      test_ear = 1.
    ELSEWHERE
      test_ear = 0.
    ENDWHERE

    ! # NUMBER OF SHOOT EXISTING COMPARTMENTS
    ncomp = test_lam + test_stem + test_ear
    ! I check that ncomp > 0 to avoid divisions when ncomp is nul
    WHERE (ncomp .GT. 0.0)
        NDFmean = (&
           NDFlam  * test_lam  + &
           NDFstem * test_stem + &
           NDFear  * test_ear) / ncomp
    ELSEWHERE
       NDFmean=0.0
    ENDWHERE

        !  # PARAMETER beta FOR THE CALCULATION OF ANIMAL'S PREFERENCE FOR ONE
        !  COMPARTMENT
        ! Vuichard Thesis p.66 equation (64) 
    WHERE (ncomp .GT. 1.)
    ! 070531 AIG end    

        betaGrazing = (2.* AnimalDiscremineQualite * ncomp)/&
           (100. * (ncomp - 1.) * (1. - 2.*LimDiscremine))
    ELSEWHERE
        betaGrazing = 0.0
    END WHERE

    WHERE (ABS(wlam+wst) .GT. 10e-15)

        DNDF_total = (&
            DNDFlam  * wlam  + &
            DNDFstem * wst + & 
            DNDFear  * wear) / (wlam+wst+wear)

        NDF_total = (&
            NDFlam  * wlam  + &
            NDFstem * wst + & 
            NDFear  * wear) / (wlam+wst+wear)

    ENDWHERE


    !---------------------
    ! WEIGHTING FACTORS CORREPONDING TO THE ANIMAL'S INTAKE PREFERENCE
    !---------------------
    WHERE ((ABS(wlam+wst) .GT. 10e-15) .AND. (intake_animal .GT. 0.0))
        ! # for the sheath&stem compartment
       exposant_stem = -2. * betagrazing * &
            MAX(0.,1.-(intakemax - intake_animal))*(NDFmean - NDFstem )*100.

        pstem = 1./(ncomp)*((1. - 2.*LimDiscremine)*(1. - exp(exposant_stem))/ &
           (1. + EXP(exposant_stem))+1.)

        ! # for the lam compartment
        exposant_lam = -2.*betagrazing * &
             MAX(0.,1.-(intakemax - intake_animal))*(NDFmean - NDFlam)*100.

        plam = 1./(ncomp)*((1. - 2.*LimDiscremine)*(1. - EXP(exposant_lam)) / &
           (1. + EXP(exposant_lam))+1.)

!gmjc 08Sep2015 to avoid pstem and plam over 1
        WHERE (pstem .GT. 1.0)
          pstem = 1.0
        ELSEWHERE (pstem .LT. 0.0)
          pstem = 0.0
        ENDWHERE
        WHERE (plam .GT. 1.0)
          plam = 1.0
        ELSEWHERE (plam .LT. 0.0)
          plam = 0.0
        ENDWHERE
        WHERE ((plam + pstem) .GT. 1.0)
          plam = 1.0
          pstem = 0.0
        ENDWHERE
!end gmjc
        ! # for the ear compartment
        pear = 1. - (plam + pstem)

        MassePondTot = plam * wlam + pstem * wst + pear * wear
    ELSEWHERE
        pstem = 0.0
        plam = 0.0
        pear = 0.0
        MassePondTot = 0.0

    ENDWHERE

  END SUBROUTINE variablesPlantes


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!FROM PASIM2011 Animaux.f90 JC 110524
!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !***************************************************************************************************
  !***************************************************************************************************
  !                                    MODULE ANIMALE ALLAITANT/LAITIER                           
  !***************************************************************************************************
  !***************************************************************************************************

  SUBROUTINE Animaux_main_dynamic(&
     npts, dt, devstage                  , &
     intakemax, snowfall_daily, wshtot, wsh        , &
     nel, nanimaltot                     , &
     intake                              , &
     import_yield                        , &
     new_year, new_day                   , &
     nanimal, tanimal, danimal           , &
     PIYcow, PIMcow, BCSYcow             , &
     BCSMcow, PICcow, AGE_cow_P, AGE_cow_M , &
     tcutmodel, tjulian                  , &
     intakesum                           , &
     intakensum, fn,ntot, c, n, leaf_frac, &
     intake_animal, intake_animalsum     , &
     tadmin, type_animal                 , &
     tadmoy, IC_tot, Autogestion_out     , &
     Forage_quantity,tmoy_14             , &
     intake_tolerance                    , &
     q_max_complement                    , &
     biomass, urinen, faecesn, urinec, faecesc, &
     file_param_init,trampling,sr_ugb,sr_wild   , &
     compt_ugb,nb_ani,grazed_frac,AnimalDiscremineQualite, &
     grazing_litter, nb_grazingdays)

    ! Declarations:

    INTEGER(i_std), INTENT(in)                                    :: npts
    ! Number of spatial points (-)
    REAL(r_std ), INTENT(in)                               :: dt
    ! Time step (d)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)              :: devstage
    ! Developmental stage (-)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout)           :: intakemax
    ! intake capacity of the cattle (kg/(animal*m**2)
    REAL(r_std ), DIMENSION(npts), INTENT(in)              :: snowfall_daily
    ! Snow cover (mm)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)              :: wshtot
    ! Total (structure + substrate) shoot dry matter(kg m-2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)              :: wsh
    ! (structure + substrate) shoot dry matter(kg m-2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)             :: nel
    ! Net energy content of the forage (MJ kg-1)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout)           :: nanimaltot
    ! Stocking rate (animal m-2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)             :: intake
    ! intake (kg DM m2-)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout)              :: import_yield
    ! ajout de Nicolas pour les runs saturant nonlimitant
    LOGICAL, INTENT(in)                                    :: new_year
    LOGICAL, INTENT(in)                                    :: new_day
    INTEGER(i_std), INTENT(in)                                    :: tcutmodel
    INTEGER(i_std ), INTENT(in)                               :: tjulian
    ! Julian day (-)
    REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(inout) :: nanimal
    ! Stocking density  h (1,..,nstocking) (animal m-2)
    REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(inout) :: tanimal
    ! Beginning of the grazing period    h (1,..,nstocking) (d)
    REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(inout) :: danimal
    ! Lenght of the grazing period    h (1,..,nstocking) (d)
    REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(inout) :: PIYcow
    ! Initial weight of Young cow (Kg)
    REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(inout) :: PIMcow
    ! Initial weight of Mature cow (Kg)
    REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(inout) :: BCSYcow
    ! Initial body score condition of Young cow(Kg)
    REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(inout) :: BCSMcow
    ! Initial body score condition of mature cow(Kg)
    REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(inout) :: PICcow
    ! Initial weight of cow's calves (Kg)
    REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(inout) :: AGE_cow_P
    ! Average age of dairy primiparous cows for autogestion
    REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(inout) :: AGE_cow_M
    ! Average age of dairy multiparous cows for autogestion
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout)           :: intakesum
    ! Yearly intake (kg animal-1 y-1)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout)           :: intakensum
    ! N in daily intake per m2(kgN/m2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)              :: fn
    ! nitrogen in structural dry matter
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)              :: ntot
    ! nitrogen substrate concentration in plant,(kg n/kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)              :: c
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)              :: n
    ! nitrogen substrate concentration in plant,(kg n/kg)
    REAL(r_std ), DIMENSION(npts,nvm,nleafages), INTENT(inout)              :: leaf_frac
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)             :: intake_animal
    ! Daily intake per animal(kg animal-1 d-1)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout)           :: intake_animalsum
    ! Yearly intake per animal(kg animal-1 d-1)
    REAL(r_std ), DIMENSION(npts), INTENT(in)              :: tadmin
    ! Daily minimum temperature
    REAL(r_std ), DIMENSION(npts), INTENT(in)              :: tadmoy
    ! Daily average temperature (K)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)             :: IC_tot
    ! Daily average ingested capacity of cows (kg)
    REAL(r_std ), DIMENSION(npts,nvm,n_out),INTENT(out)        :: Autogestion_out
    ! Fraction F (npts,1), ratio F (npts,2), and lenght of the grazing period when autgestion

    ! To write in import_yiels File(npts,3)
    INTEGER(i_std),                       INTENT(in)              :: type_animal
    ! 1: Dairy cows, 2: Suckler cows, 3: Old module, 4: Dairy heifers, 5 : Suckler heifers
    REAL(r_std ), DIMENSION(npts,nvm,nstocking),INTENT(inout)  :: Forage_quantity
    ! Net energy ingested for cow (young in first, and adult in second) (MJ)
    REAL(r_std ), DIMENSION(npts),  INTENT(in)             :: tmoy_14
    ! 14 day running average of daily air temperature (K)
    REAL(r_std ),                   INTENT(in)             :: intake_tolerance
    ! intake tolerance threshold (-)
    REAL(r_std ),                   INTENT(in)             :: q_max_complement
    ! Maximum quantity of forage or concentrate to supplement animals when auto-supplementation (kg)
    REAL(r_std), DIMENSION(npts,nvm,nparts), INTENT(inout):: biomass
    ! totalité de masse sèche du shoot(kg/m**
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: urinen
    ! n dans l'urine (kg n /(m**2 d))     
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: faecesn
    ! n dans les fèces (kg n /(m**2*d)) 
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: urinec
    ! c dans les urines
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: faecesc
    ! c dans les fèces (kg c /(m**2*d))
    CHARACTER(len=500)      , INTENT(in)  :: file_param_init
    REAL(r_std), DIMENSION(npts,nvm)          , INTENT(out)   :: trampling
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  sr_ugb
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  sr_wild
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  compt_ugb
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  nb_ani
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  grazed_frac
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  ::  AnimalDiscremineQualite
    INTEGER(i_std), DIMENSION(npts,nvm), INTENT(inout)  :: grazing_litter
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  :: nb_grazingdays

    ! - nanimaltotmax : maximum stocking rate during optimisation (animal/ha)

    !Variable Local : Variable n'ayant pas besoin d'etre sauvées entre les appels du module Main_animal_cow
    REAL(r_std )     , DIMENSION(npts,nvm)  :: wshtotgrazing
    ! Grazing shoot biomass (kg DM m-2)
    REAL(r_std )     , DIMENSION(npts,nvm)  :: deltaanimal
    REAL(r_std )     , DIMENSION(npts,nvm)  :: extra_feed
    ! Forage necessary to feed animals at barn when stocking rate autogestion (kg DM m-2)
    REAL(r_std )     , DIMENSION(npts,nvm)  :: nb_ani_old
    ! Actual stocking rate per ha of total pasture "D" at previous iteration (animal (ha of total grassland)-1)
    INTEGER(i_std)          , DIMENSION(npts,nvm)  :: ugb_last
    ! Equals 0 (no animals) or 1 (animals) for console display

    REAL(r_std ), DIMENSION(npts,nvm)              :: OMD
    ! Digestible organic matter in the intake(kg/kg)
    REAL(r_std ), DIMENSION(npts,nvm,2)            :: NEIcow
    ! Total net energy intake (1:young, 2:adult) (MJ)
    ! to check
    REAL(r_std ), DIMENSION(npts,nvm,2)            :: NEIh
    ! Net energy intake from the ingested herbage(1:young, 2:adult) (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2)            :: NEIf
    ! Net energy intake from the ingested forage(1:young, 2:adult) (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2)            :: NEIc
    ! Net energy intake from the ingested concentrate(1:young, 2:adult) (MJ)

    !milk
    REAL(r_std ), DIMENSION(npts,nvm,2)     :: MPwcow2
    ! Daily potential milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2)     :: MPcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2)     :: MPcow
    ! Daily milk production per m2 for primiparous or multiparous cows (kg/m-2/d)
    REAL(r_std ), DIMENSION(npts,nvm)       :: milkKG
    ! Daily actual milk production per animal for the whole cattle (kg/animal/d)

    !intake capacity and DMI
    REAL(r_std ), DIMENSION(npts,nvm,2)     :: ICcow
    ! Cow intake capacity of primiparous or multiparous cows(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2)     :: DMIcowanimal
    ! Daily animal intake for primiparous or multiparous cows(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2)     :: DMIcow
    ! Daily intake per m2 for primiparous or multiparous cows(kg/m2/d)
    REAL(r_std ), DIMENSION(npts,nvm)       :: ICcalf
    ! Calf intake capacity  (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm)       :: DMIcalfanimal
    ! Daily calf intake per animal(kg/animal/d)         
    REAL(r_std ), DIMENSION(npts,nvm)       :: DMIcalf
    ! Daily calf intake per m2 (Kg/d)         

    !Energie Balance
    REAL(r_std ), DIMENSION(npts,nvm)       ::  NELherbage
    ! Energetic content of the herbage (MJ/kg) 
    REAL(r_std ), DIMENSION(npts,nvm,2)     ::  NEPcow
    ! Net energy for production (young :1 , adult:2) (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2)     ::  NEPlactcow
    ! Net energy for milk production (young :1 , adult:2) (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2)     ::  NEPgestcow
    ! Net energy for gestation (suckler cows)(young :1 , adult:2) (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2)     ::  NEMcow
    ! Net energy for maintenance (young :1 , adult:2) (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2)     ::  NEBcow
    ! Net energy Balance (young :1 , adult:2) (MJ) 
    REAL(r_std ), DIMENSION(npts,nvm,2)     ::  NEGcow
    ! Net energy for gestation (dairy cows)(young :1 , adult:2) (MJ) 
    REAL(r_std ), DIMENSION(npts,nvm)       ::  NEIcalf
    ! Net energy intake for calves (from milk and ingested herbage) (MJ)
    REAL(r_std ), DIMENSION(npts,nvm)       ::  NEIherbagecalf
    ! Net energy intake for calves (from only ingested herbage) (MJ)
    REAL(r_std ), DIMENSION(npts,nvm)       ::  NEImilkcalf
    ! Net energy intake for calves (from only ingested milk) (MJ)
    REAL(r_std ), DIMENSION(npts,nvm)       ::  NEGcalf
    ! Net energy for calf growth (MJ)
    REAL(r_std ), DIMENSION(npts,nvm)       ::  NEMcalf
    ! Net energy for calf maintenance (MJ)
    !BILAN N C    
    REAL(r_std ), DIMENSION(npts,nvm)       ::  faecesNcow
    ! Nitrogen in faeces (young in first, and adult in second)(Kg N m-2)   
    REAL(r_std ), DIMENSIOn(npts,nvm)       ::  faecesCcow
    ! Carbon in faeces (young in first, and adult in second)(Kg C m-2)
    REAL(r_std ), DIMENSIOn(npts,nvm)       ::  urineNcow
    ! Nitrogen in urine (young in first, and adult in second)(Kg N m-2)
    REAL(r_std ), DIMENSIOn(npts,nvm)       ::  urineCcow
    ! Carbon in Urine (young in first, and adult in second)(Kg C m-2)
    REAL(r_std ), DIMENSION(npts,nvm)       :: nWeekLact
    ! Lactation week (in weeks from calving)
    REAL(r_std ), DIMENSION(npts,nvm)       :: nweekGest
    ! Gestation week (in weeks from mating)
    REAL(r_std ), DIMENSION(npts,nvm,2)     :: AGE_animal
    ! Animal age in case of simulation of dairy cows (months)
    REAL(r_std ), DIMENSION(npts,nvm,2)     :: CH4h
    ! Daily enteric methane production from ingested herbage  (kg C animal-1 d-1)
    REAL(r_std ), DIMENSION(npts,nvm,2)     :: deltaBCS
    ! Body condition score variation between two consecutive time steps (-) 
    INTEGER(i_std), DIMENSION(npts,nvm)            :: in_grazing
    INTEGER(i_std)                             :: i,j
    ! For loop
    REAL(selected_real_kind(3,2))       :: tempTjulian
    ! TO round Tjulian

    REAL(r_std ),DIMENSION(npts,nvm)        :: FVh
    ! Herbage Fill Value (UE)
    REAL(r_std ), DIMENSION(npts,nvm,2)     :: MPpos
    ! Possible milk production of dairy cows according to the diet (kg/animal/d)   

    REAL(r_std), DIMENSION(npts,nvm)       ::  WanimalMOYcow
    ! The average weigth of live of the cattle (Kg / animal)

    REAL(r_std), DIMENSION(npts,nvm,2)     ::  CH4animal
    ! Daily enteric methane production from ingested herbage  (kg C animal-1 d-1)

    REAL(r_std), DIMENSION(npts)  :: xtmp_npts
    INTEGER(i_std)                            :: h,k     !!! for Verif_management

    REAL(r_std)  :: tcalving_t
    REAL(r_std)  :: tsevrage_t
    REAL(r_std)  :: Age_sortie_calf_t
    REAL(r_std)  :: Pyoung_t
    REAL(r_std)  :: Wcalfborn_t
    REAL(r_std)  :: EVc_t
    REAL(r_std)  :: EVf_t
    REAL(r_std)  :: FVf_t
    REAL(r_std)  :: fN_forage_t
    REAL(r_std)  :: fN_concentrate_t

    REAL(r_std), DIMENSION(2)        :: QIc_t
    REAL(r_std), DIMENSION(4)        :: autogestion_weightcow_t
    REAL(r_std), DIMENSION(4)        :: autogestion_BCScow_t
    REAL(r_std), DIMENSION(4)        :: autogestion_AGEcow_t
    REAL(r_std), DIMENSION(2)        :: MPwmax_t
    INTEGER(i_std) :: ier
    REAL(r_std),DIMENSION(npts)  :: toto 

    !TEMPORAIRE
    MPpos=0.0
    MPwcow2=0.0
    MPcow2=0.0
    MPcow=0.0
    milkKG=0.0
    ICcow=0.0
    ICcalf=0.0
    DMIcowanimal=0.0
    DMIcalfanimal=0.0
    DMIcow=0.0
    DMIcalf=0.0
    NELherbage=0.0
    NEIcow=0.0
    ! to check
    NEIh=0.0
    NEIf=0.0
    NEIc=0.0
    NEPcow=0.0
    NEPlactcow=0.0
    NEPgestcow=0.0
    NEMcow=0.0
    NEBcow=0.0
    NEIcalf=0.0
    NEIherbagecalf=0.0
    NEImilkcalf=0.0
    NEGcalf=0.0
    NEMcalf=0.0
    faecesNcow=0.0
    faecesCcow=0.0
    urineNcow=0.0
    urineCcow=0.0
    OMD=0.0
    AGE_animal=0
    FVh=0.0

    !  initialisation

    init_animal : IF (l_first_animaux) THEN 

        IF (blabla_pasim) PRINT *, 'PASIM Animals : initialisation'

        CALL Animal_Init(npts, nanimal , type_animal , intake_tolerance)

        CALL variablesPlantes(&
           npts,biomass,&
           c,n,intake_animal,intakemax,&
           AnimalDiscremineQualite)

        !----------------------------------
        ! 0 - Input data Reading
        !----------------------------------
        !!!!JC comm we do not need to read these variables now, but needed for new animals
        !        CALL read_init_animals(&
        !           npts, nbfichier_par, nsoil, &
        !           parfile_input, error_point, &
        !           lim_inf, lim_sup, Type_animal)
        !!!!!!!!!!!read variables for new animal module
        !file_param_init='/home/orchidee_ns/lhli/Modele_ORCHIDEE/Management/param_init.txt'

        !CALL getin('FILE_PARAM_INIT',file_param_init)

        ! lecture données dans le fichier  ==> read data from the file
        ! pour l'instant uniquement lecture d'un seul point d'espace de management, mais possibilité plusieurs années

        OPEN(unit=61, file = file_param_init)

        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:) 
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)

        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)

        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)

        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)

        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)

        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)

        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) toto(:)
        READ(61, *, iostat = ier) tcalving_t
        READ(61, *, iostat = ier) tsevrage_t
        READ(61, *, iostat = ier) Age_sortie_calf_t

        READ(61, *, iostat = ier) Pyoung_t
        READ(61, *, iostat = ier) Wcalfborn_t
      IF ((type_animal.EQ.1).OR.(type_animal.EQ.2)) THEN
        READ(61, *, iostat = ier) (MPwmax_t(h),h=1,2)
      ELSE
        READ(61, *, iostat = ier) MPwmax_t(1)
      ENDIF
        READ(61, *, iostat = ier) QIc_t(1)
        READ(61, *, iostat = ier) EVc_t

        READ(61, *, iostat = ier) EVf_t
        READ(61, *, iostat = ier) FVf_t
        READ(61, *, iostat = ier) fN_forage_t
        READ(61, *, iostat = ier) fN_concentrate_t
      !Comme le concetrate est spécifié par l'utilisateur, primipare et multipare ou le même apport

      QIc_t(2)=QIc_t(1)
      ! 21/01/09 AIG

      ! On recalcule la concentration en N du fourrage et du concentré à partir de la MAT

      ! = matière azotée totale renseignée en entrée par l'utilisateur.

      fN_forage_t= fN_forage_t/(6.25*1000)

      fN_concentrate_t= fN_concentrate_t/(6.25*1000)

      IF(f_complementation.EQ.0) THEN

        QIc_t(1)=0.0

        QIc_t(2)=0.0

      ENDIF

      IF (f_autogestion.EQ.2) THEN
      ! Initial cow liveweight when stocking rate automanagement (kg /animal)
        READ(61, *, iostat = ier) (autogestion_weightcow_t(h),h=1,2)
      ! Initial BCS when stocking rate automanagement (-)
        READ(61, *, iostat = ier) (autogestion_BCScow_t(h),h=1,2)
      ! Initial age when stocking rate automanagement (months)
        READ(61, *, iostat = ier) (autogestion_AGEcow_t(h),h=1,2)
        autogestion_weightcow_t(3)=autogestion_weightcow_t(1)

        autogestion_weightcow_t(4)=autogestion_weightcow_t(2)

        autogestion_BCScow_t(3)=autogestion_BCScow_t(1)

        autogestion_BCScow_t(4)=autogestion_BCScow_t(2)

        autogestion_AGEcow_t(3)=autogestion_AGEcow_t(1)

        autogestion_AGEcow_t(4)=autogestion_AGEcow_t(2)                

      ENDIF  

      DO i=1,npts
        tcalving(i,:)=tcalving_t
        tsevrage(i,:)=tsevrage_t
        Age_sortie_calf(i,:)=Age_sortie_calf_t
        Pyoung(i,:)=Pyoung_t
        Wcalfborn(i,:)=Wcalfborn_t
        EVc(i,:)=EVc_t
        EVf(i,:)=EVf_t
        FVf(i,:)=FVf_t
        fN_forage(i,:)=fN_forage_t
        fN_concentrate(i,:)=fN_concentrate_t
        DO h=1,2
          MPwmax(i,:,h)=MPwmax_t(h)
          QIc(i,:,h)=QIc_t(h)
        END DO
        DO h=1,4
          autogestion_weightcow(i,:,h)=autogestion_weightcow_t(h)
          autogestion_BCScow(i,:,h)=autogestion_BCScow_t(h)
          autogestion_AGEcow(i,:,h)=autogestion_AGEcow_t(h)
        END DO
      END DO
        CLOSE (61)

      !!!!!!JC comm test management file, if the grazing period was overlap, can be used 
        h=0
        IF ((tcutmodel .EQ. 0) .AND. (f_autogestion .NE. 2)) THEN
            h=Verif_management(npts,nstocking, tanimal,danimal)
        ENDIF

        IF(h.EQ.1) THEN
           STOP "ERROR : Overlap of grazing periode in management file"
        ENDIF

    END IF init_animal


    !______________________________________________
    !----------------------------------
    !       - CALL OF FUNCTIONS - 
    !----------------------------------
    !______________________________________________
    ! once per year
    n_year : IF (new_year .EQV. .TRUE. ) THEN

        nanimaltot     = 0.0
        nanimaltot_prec= 0.0
        faecesnsum     = 0.0
        milksum        = 0.0
        nelgrazingsum  = 0.0
        milkcsum       = 0.0
        ranimalsum     = 0.0
        MethaneSum     = 0.0
        faecescsum     = 0.0
        urinecsum      = 0.0
        urinensum      = 0.0
        milknsum       = 0.0
        stockingstart  = 0
        stockingend    = 0
        grazingnsum    = 0.0
        grazingcsum    = 0.0
        intakesum      = 0.0
        intake_animalsum = 0.0
        intakensum      = 0.0
        milkanimalsum = 0.0
        methane_aniSum= 0.0
        MPcow2_prec=0
        DMIc=0.0
        DMIf=0.0
 
        !réinitialisation des variable global cow
        MPcowsum=0.0
        MPcow2sum=0.0
        MPcowN=0.0
        MPcowC=0.0
        MPcowCsum = 0.0
        MPcowNsum = 0.0
        DMIcowsum = 0.0

        DMIcowNsum = 0.0
        DMIcowCsum = 0.0
        DMIcowanimalsum = 0.0
        DMIcalfanimalsum = 0.0
        DMIcalfsum=0.0
        calfinit=0

        autogestion_init=0.0
        Fday_pasture=0
        compte_pature=0
        !pour remettre aux valeurs de cond_init
        autogestion_BCScow(:,:,1)=autogestion_BCScow(:,:,3)
        autogestion_BCScow(:,:,2)=autogestion_BCScow(:,:,4)
        autogestion_weightcow(:,:,1)=autogestion_weightcow(:,:,3)
        autogestion_weightcow(:,:,2)=autogestion_weightcow(:,:,4)
        autogestion_AGEcow(:,:,1)=autogestion_AGEcow(:,:,3)
        autogestion_AGEcow(:,:,2)=autogestion_AGEcow(:,:,4)
        !Autogestion_out(:,3)=0.0        

        Autogestion_out(:,:,1)=0.0
        Autogestion_out(:,:,2)=0.0


        !tout les ans on réinitialise les variables permettant d'ecrire le fichier management        
        IF (f_autogestion.EQ.2) THEN
           tanimal=0.0
           danimal=0.0
           nanimal=0.0
           BCSYcow=0.0
           BCSMcow=0.0
           PICcow=0.0
           PIYcow=0.0
           PIMcow=0.0
           AGE_cow_P=0.0
           AGE_cow_M=0.0
           Forage_quantity=0.0
        ENDIF
        ugb                   = 0

        delai_ugb             = -1

        !************************************************
        ! modifications added by Nicolas Vuichard

        !modif ugb0azot

        !070703 AIG à confirmer
        !********* Stocking rate calculation if grazing autogestion ********** 
        ! the model will pass the loop if flag "non limitant" 
        ! The module calculates the optimal yield "Y" of a cut grassland plot,
        ! when optimizing cut events and N fertilisation.
        ! Then the model simulates the same grasslang plot with animals. Stocking rate "S"
        ! is incremented at each optimization step. For each stocking rate, the program
        ! determines the number of days for which animals are in the barn (365 - compt_ugb(:))
        ! and thus, the forage necessary to feed them at the barn "X".
        ! The fraction F of grazed pastures is calculated as: Y (1-F) - X = 0
        !                                                     F = Y /(Y+X)
        !                                                     F = 1 / (1 + X/Y)
        ! Then the program calculates the actual stocking rate per ha of total pasture "D",
        ! D = SF
        ! code equivalences
        ! Y = import_yield
        ! X = extra_feed
        ! S = sr_ugb
        ! F = 1 / (1 + extra_feed(:) / (import_yield * 0.85))
        ! D = nb_ani
        ! 0.85 = 1 - 0.15: pertes à la récolte

        !Local_autogestion_out(:,1): ratio X/Y: fourrages non consommés/fourrages disponibles
        !Local_autogestion_out(:,2): fraction of grazed pastures

        IF(f_nonlimitant .EQ. 0) THEN
            !modif nico ugb
            IF (f_autogestion .EQ. 2) THEN
              DO j=2,nvm
                 IF (is_grassland_manag(j) .AND. (.NOT.is_grassland_cut(j)) .AND. &
                      (.NOT.is_grassland_grazed(j)))THEN

               print*, "Number of grazed days (d):", compt_ugb(:,j)
               print*, "Stocking rate S for the grazed pasture(animal.m-2):", sr_ugb(:,j)
               !print*, "fraction F of grazed pastures (-): ", Local_autogestion_out(:,1)
               print*, "Forage requirements/Forage available (-): ", Local_autogestion_out(:,j,1)
               !print*, "Global stocking rate D (animal.m-2:)", sr_ugb(:,j)* Local_autogestion_out(:,1)
               print*, "Global stocking rate D (animal.m-2:)", sr_ugb(:,j) * Local_autogestion_out(:,j,2)
               !print*, "Ratio of grazed vs cut grasslands: ", Local_autogestion_out(:,2)
               print*, "Fraction F of grazed pastures (-): ", Local_autogestion_out(:,j,2)
               print*,"--------------"

               WHERE ((ok_ugb(:,j) .EQ. 0))

                    extra_feed(:,j)  = (365 - compt_ugb(:,j)) * 18 * sr_ugb(:,j) 
                    nb_ani_old(:,j)  = nb_ani(:,j)
                    nb_ani(:,j)      = 1 / (1 + extra_feed(:,j) / (import_yield(:,j) * 0.85)) * sr_ugb(:,j)

                    !Local_autogestion_out(:,1)=1 / (1 + extra_feed(:) / (import_yield * 0.85))
                    !Local_autogestion_out(:,2)=1/(1+Local_autogestion_out(:,1))
                    Local_autogestion_out(:,j,1)= extra_feed(:,j) / (import_yield(:,j) * 0.85)
                    Local_autogestion_out(:,j,2)=1 / (1 + Local_autogestion_out(:,j,1))
                    Autogestion_out(:,j,  3)= compt_ugb(:,j)
                    
                    grazed_frac(:,j) =  1 / (1 + extra_feed(:,j) / (import_yield(:,j) * 0.85))


                    WHERE ((ABS(nb_ani(:,j)-nb_ani_old(:,j))/nb_ani(:,j)) .LT. 0.01)

                        ok_ugb(:,j) = 1
                        sr_ugb(:,j) = sr_ugb(:,j) -0.00001
                    ELSEWHERE
                       !recherche du 0 par la méthode de newton                       
                       Local_autogestion_out(:,j,1)= extra_feed(:,j) / (import_yield(:,j) * 0.85)
                       Local_autogestion_out(:,j,2)=1 / (1 + Local_autogestion_out(:,j,1))
                       Autogestion_out(:,j,  3)= compt_ugb(:,j)

                        WHERE ((ABS(nb_ani(:,j)-nb_ani_old(:,j))/nb_ani(:,j)) .LT. 0.01)

                        ok_ugb(:,j) = 1
                        sr_ugb(:,j) = sr_ugb(:,j) - 0.00001

                        ELSEWHERE
                        sr_ugb(:,j) = sr_ugb(:,j) + 0.00001

                        END WHERE

                    END WHERE
                ENDWHERE
                print*,"---critere nb_ani :", (ABS(nb_ani(:,j)-nb_ani_old(:,j))/nb_ani(:,j))

                nb_grazingdays(:,j) = compt_ugb(:,j)
                compt_ugb(:,j) = 0
                print*, "sr_ugb_apres:", sr_ugb(:,j)
                print*, "ok_ugb :", ok_ugb(:,j)
                print*,"--------------"
              END IF
            END DO

            ENDIF
        ENDIF
        !fin modif ugb0azot
   
        IF(f_nonlimitant .EQ. 0) THEN
            !modif nico ugb
            IF (f_postauto .EQ. 1) THEN

                WHERE ((ok_ugb(:,mgraze_C3) .EQ. 0))
                  ! total yield of last year (kg DM/m^2 total grassland)
                   amount_yield(:,mgraze_C3) = import_yield(:,mgraze_C3) * &
                        (1-grazed_frac(:,mgraze_C3)) * 0.85
                  ! total animal indoor consumption of last year (kg DM/m^2 total grassland)                 
                   consump(:,mgraze_C3) = (365 - compt_ugb(:,mgraze_C3)) * &
                        18.0 * nb_ani(:,mgraze_C3)
                  ! food surplus (outside_food > 0) or deficit (outside_food < 0)
                  outside_food(:,mgraze_C3) = amount_yield(:,mgraze_C3)-consump(:,mgraze_C3)
                  ! farmers' decision of buy (add_nb_ani > 0) or sell (add_nb_ani < 0) animals 
                  add_nb_ani(:,mgraze_C3) = outside_food(:,mgraze_C3)/ (18.0 * 365)*0.2
                  ! New animal density for total grassland
                  nb_ani(:,mgraze_C3)=nb_ani(:,mgraze_C3)+add_nb_ani(:,mgraze_C3)
                  ! New fraction of grazed grassland in total grassland (keep the same stocking rate)
                  grazed_frac(:,mgraze_C3)=nb_ani(:,mgraze_C3)/sr_ugb(:,mgraze_C3)
                  ! Threshold of fraction as least 30 % was cut 
                  WHERE (grazed_frac(:,mgraze_C3) .GT. 0.7)
                    sr_ugb(:,mgraze_C3)=sr_ugb(:,mgraze_C3)+0.00002
                    grazed_frac(:,mgraze_C3)=nb_ani(:,mgraze_C3)/sr_ugb(:,mgraze_C3)
                  END WHERE
                  Local_autogestion_out(:,mgraze_C3,1)= extra_feed(:,mgraze_C3)/ &
                       (import_yield(:,mgraze_C3) * 0.85)
                    Local_autogestion_out(:,mgraze_C3,2)=1 / (1+Local_autogestion_out(:,mgraze_C3,1))
                    Autogestion_out(:,mgraze_C3,  3)= compt_ugb(:,mgraze_C3)
                END WHERE
    
                nb_grazingdays(:,mgraze_C3) = compt_ugb(:,mgraze_C3)
                compt_ugb(:,mgraze_C3) = 0 

                WHERE ((ok_ugb(:,mgraze_C4) .EQ. 0))
                  ! total yield of last year (kg DM/m^2 total grassland)
                   amount_yield(:,mgraze_C4) = import_yield(:,mgraze_C4) * &
                        (1-grazed_frac(:,mgraze_C4)) * 0.85
                  ! total animal indoor consumption of last year (kg DM/m^2
                  ! total grassland)                 
                  consump(:,mgraze_C4) = (365 - compt_ugb(:,mgraze_C4)) * 18.0 *nb_ani(:,mgraze_C4)
                  ! food surplus (outside_food > 0) or deficit (outside_food <
                  ! 0)
                  outside_food(:,mgraze_C4) =amount_yield(:,mgraze_C4)-consump(:,mgraze_C4)
                  ! farmers' decision of buy (add_nb_ani > 0) or sell
                  ! (add_nb_ani < 0) animals 
                  add_nb_ani(:,mgraze_C4) = outside_food(:,mgraze_C4)/ (18.0 *365)*0.2
                  ! New animal density for total grassland
                  nb_ani(:,mgraze_C4)=nb_ani(:,mgraze_C4)+add_nb_ani(:,mgraze_C4)
                  ! New fraction of grazed grassland in total grassland (keep
                  ! the same stocking rate)
                  grazed_frac(:,mgraze_C4)=nb_ani(:,mgraze_C4)/sr_ugb(:,mgraze_C4)
                  ! Threshold of fraction as least 30 % was cut 
                  WHERE (grazed_frac(:,mgraze_C4) .GT. 0.7)
                    sr_ugb(:,mgraze_C4)=sr_ugb(:,mgraze_C4)+0.00002
                    grazed_frac(:,mgraze_C4)=nb_ani(:,mgraze_C4)/sr_ugb(:,mgraze_C4)
                  END WHERE
                  Local_autogestion_out(:,mgraze_C4,1)=extra_feed(:,mgraze_C4)/&
                       (import_yield(:,mgraze_C4) * 0.85)
                    Local_autogestion_out(:,mgraze_C4,2)=1 /(1+Local_autogestion_out(:,mgraze_C4,1))
                    Autogestion_out(:,mgraze_C4,  3)= compt_ugb(:,mgraze_C4)
                END WHERE

                nb_grazingdays(:,mgraze_C4) = compt_ugb(:,mgraze_C4)
                compt_ugb(:,mgraze_C4) = 0

    
            ENDIF

!gmjc postauto=5
            !! F_POSTAUTO=5 for global simulation with
            !! prescibed livestock density read from
            !! extra file
            IF (f_postauto .EQ. 5) THEN
                WHERE ((ok_ugb(:,mgraze_C3) .EQ. 0) .AND. &
                      (sr_ugb(:,mgraze_C3) .GT. 0.0))
                   extra_feed(:,mgraze_C3)  = (365 - compt_ugb(:,mgraze_C3)) * &
                        18.0*sr_ugb(:,mgraze_C3)
                  ! total yield of las year (kg DM/m^2 total grassland)
                   amount_yield(:,mgraze_C3) = import_yield(:,mgraze_C3) * &
                        (1-grazed_frac(:,mgraze_C3)) * 0.85
                  ! total animal indoor consumption of last year (kg DM/m^2 total grassland)
                   consump(:,mgraze_C3) = 0.0
                   !(365 - compt_ugb(:,mgraze_C3)) * 18.0 * nb_ani(:,mgraze_C3)
                  ! food surplus (outside_food > 0) or deficit (outside_food < 0)
                   outside_food(:,mgraze_C3) = 0.0
                   !amount_yield(:,mgraze_C3)-consump(:,mgraze_C3)
                  ! farmers' decision of buy (add_nb_ani > 0) or sell (add_nb_ani < 0) animals
                   add_nb_ani(:,mgraze_C3) = 0.0
                   !outside_food(:,mgraze_C3)/ (18.0 * 365) * 0.2
                  !! New animal density for total grassland
                  nb_ani(:,mgraze_C3)=nb_ani(:,mgraze_C3)+add_nb_ani(:,mgraze_C3)
                  !! New fraction of grazed grassland in total grassland (keep the same stocking rate)
                  WHERE (sr_ugb(:,mgraze_C3) .GT. 0.0)
                     grazed_frac(:,mgraze_C3)=0.5
                     !nb_ani(:,mgraze_C3)/sr_ugb(:,mgraze_C3)
                  ENDWHERE
                  WHERE (sr_ugb(:,mgraze_C3) .LE. 0.0)
                  grazed_frac(:,mgraze_C3)=0.0
                  sr_ugb(:,mgraze_C3)=0.0
                  nb_ani(:,mgraze_C3)=0.0
                  ENDWHERE
!                  !! Threshold of fraction as least 30 % was cut
!                  WHERE ((grazed_frac(:,mgraze_C3) .GT. 0.7) .AND. (sr_ugb(:,mgraze_C3) .GT. 0.0))
!                    sr_ugb(:,mgraze_C3)=sr_ugb(:,mgraze_C3)+0.00001
!                    grazed_frac(:,mgraze_C3)=nb_ani(:,mgraze_C3)/sr_ugb(:,mgraze_C3)
!                  END WHERE
!                  WHERE (grazed_frac(:,mgraze_C3) .GT. 1.0)
!                    grazed_frac(:,mgraze_C3)=1.0
!                  ENDWHERE
                    Local_autogestion_out(:,mgraze_C3,1)= extra_feed(:,mgraze_C3)/(import_yield(:,mgraze_C3) * 0.85)
                    Local_autogestion_out(:,mgraze_C3,2)=1 / (1+Local_autogestion_out(:,mgraze_C3,1))
                    Autogestion_out(:,mgraze_C3,  3)= compt_ugb(:,mgraze_C3)
                ELSEWHERE
                  sr_ugb(:,mgraze_C3) = 0.0
                  nb_ani(:,mgraze_C3) = 0.0
                  grazed_frac(:,mgraze_C3)=0.0
                  amount_yield(:,mgraze_C3) =0.0
                  outside_food(:,mgraze_C3) = 0.0
                  consump(:,mgraze_C3) =0.0
                  add_nb_ani(:,mgraze_C3) = 0.0
                  extra_feed(:,mgraze_C3) = 0.0
                  Local_autogestion_out(:,mgraze_C3,1)= extra_feed(:,mgraze_C3)/&
                       (import_yield(:,mgraze_C3) * 0.85)
                    Local_autogestion_out(:,mgraze_C3,2)=1 / (1+Local_autogestion_out(:,mgraze_C3,1))
                    Autogestion_out(:,mgraze_C3,  3)= compt_ugb(:,mgraze_C3)
                END WHERE

                WHERE ((ok_ugb(:,mgraze_C4) .EQ. 0) .AND. (sr_ugb(:,mgraze_C4).GT. 0.0))

                   extra_feed(:,mgraze_C4)  = (365 - compt_ugb(:,mgraze_C4)) * &
                        18.0*sr_ugb(:,mgraze_C4)
                  ! total yield of las year (kg DM/m^2 total grassland)
                   amount_yield(:,mgraze_C4) = import_yield(:,mgraze_C4) * &
                        (1-grazed_frac(:,mgraze_C4)) * 0.85
                  ! total animal indoor consumption of last year (kg DM/m^2 total grassland)
                  consump(:,mgraze_C4) = 0.0 !(365 - compt_ugb(:,mgraze_C4)) * 18.0 *nb_ani(:,mgraze_C4)
                  ! food surplus (outside_food > 0) or deficit (outside_food < 0)
                  outside_food(:,mgraze_C4) = 0.0 !amount_yield(:,mgraze_C4)-consump(:,mgraze_C4)
                  ! farmers' decision of buy (add_nb_ani > 0) or sell (add_nb_ani < 0) animals
                  add_nb_ani(:,mgraze_C4) = 0.0 !outside_food(:,mgraze_C4)/ (18.0 *365) * 0.2
                  !! New animal density for total grassland
                  nb_ani(:,mgraze_C4)=nb_ani(:,mgraze_C4)+add_nb_ani(:,mgraze_C4)
                  !! New fraction of grazed grassland in total grassland (keep
                  !the same stocking rate)
                  WHERE (sr_ugb(:,mgraze_C4) .GT. 0.0)
                  grazed_frac(:,mgraze_C4)=0.5 !nb_ani(:,mgraze_C4)/sr_ugb(:,mgraze_C4)
                  ENDWHERE
                  WHERE (sr_ugb(:,mgraze_C4) .LE. 0.0)
                  grazed_frac(:,mgraze_C4)=0.0
                  sr_ugb(:,mgraze_C4)=0.0
                  nb_ani(:,mgraze_C4)=0.0
                  ENDWHERE

!                  !! Threshold of fraction as least 30 % was cut
!                  WHERE ((grazed_frac(:,mgraze_C4) .GT. 0.9) .AND.(sr_ugb(:,mgraze_C4) .GT. 0.0))
!                    sr_ugb(:,mgraze_C4)=sr_ugb(:,mgraze_C4)+0.00002
!                    grazed_frac(:,mgraze_C4)=nb_ani(:,mgraze_C4)/sr_ugb(:,mgraze_C4)
!                  END WHERE
!                  WHERE (grazed_frac(:,mgraze_C4) .GT. 1.0)
!                    grazed_frac(:,mgraze_C4)=1.0
!                  ENDWHERE
                  Local_autogestion_out(:,mgraze_C4,1)=extra_feed(:,mgraze_C4)/&
                       (import_yield(:,mgraze_C4) * 0.85)
                    Local_autogestion_out(:,mgraze_C4,2)=1 /(1+Local_autogestion_out(:,mgraze_C4,1))
                    Autogestion_out(:,mgraze_C4,  3)= compt_ugb(:,mgraze_C4)
                ELSEWHERE
                  sr_ugb(:,mgraze_C4) = 0.0
                  nb_ani(:,mgraze_C4) = 0.0
                  grazed_frac(:,mgraze_C4)=0.0
                  amount_yield(:,mgraze_C4) =0.0
                  outside_food(:,mgraze_C4) = 0.0
                  consump(:,mgraze_C4) =0.0
                  add_nb_ani(:,mgraze_C4) = 0.0
                  extra_feed(:,mgraze_C4) = 0.0
                  Local_autogestion_out(:,mgraze_C4,1)=extra_feed(:,mgraze_C4)/&
                       (import_yield(:,mgraze_C4) * 0.85)
                    Local_autogestion_out(:,mgraze_C4,2)=1 /(1+Local_autogestion_out(:,mgraze_C4,1))
                    Autogestion_out(:,mgraze_C4,  3)= compt_ugb(:,mgraze_C4)
                END WHERE


                nb_grazingdays(:,mgraze_C3) = compt_ugb(:,mgraze_C3)
                compt_ugb(:,mgraze_C3) = 0

                nb_grazingdays(:,mgraze_C4) = compt_ugb(:,mgraze_C4)
                compt_ugb(:,mgraze_C4) = 0

            ENDIF
!end gmjc

        ENDIF

    END IF n_year

    ugb_last(:,:)=ugb(:,:)
    ! once per day    
    n_day : IF (new_day .EQV. .TRUE. ) THEN

        wshtotgrazing  = wshtotstart


        !MAJ age animal
        !!JCCOMM 120412 in this case if there is not enough biomass for animal, they
        !will be removed until next tanimal
        in_grazing=0
        CALL in_management(npts,nstocking,tanimal,danimal,tjulian,in_grazing)
        nanimaltot=nanimaltot*in_grazing
        DO j=2,nvm
           IF (is_grassland_manag(j) .AND. (.NOT.is_grassland_cut(j)).AND.&
                (.NOT.is_grassland_grazed(j)))THEN

           DO k=1,nstocking
             DO i=1,npts
                IF (tanimal(i,j,k).EQ. tjulian .AND.f_autogestion.NE.2 .AND. &
                     f_postauto .NE. 1) THEN
                 Wanimalcow(i,j,1)=PIYcow(i,j,k) ! Lecture du poids des jeunes vaches
                 ! si module vache ou bien des poids de génisses si module génisses
                 Wanimalcow(i,j,2)=PIMcow(i,j,k)
                 BCScow(i,j,1)    =BCSYcow(i,j,k)
                 BCScow(i,j,2)    =BCSMcow(i,j,k)
                 AGEcow(i,j,1)    =AGE_cow_P(i,j,k)
                 AGEcow(i,j,2)    =AGE_cow_M(i,j,k)
                 nanimaltot(i,j)  =nanimal(i,j,k)
                 Fday_pasture(i,j) =tanimal(i,j,k)
                 !calcul de la perte d'etat max a l'entré de pature et initialisation a 0 de la note d'etat BCScow_prev
                 BCScow_prev=0

                 IF(type_animal.EQ.1) THEN
                  CALL calcul_perte_etat(npts,tjulian,BCScow,MPwmax,tcalving,PEmax)
                 ENDIF

                 !On affecte PEpos a PEmax pour le premier pas de temps 
                 PEpos=PEmax

                 IF(f_complementation.EQ.0) THEN
                    Forage_quantity_period(i,j)=0.0
                 ELSE
                    Forage_quantity_period(i,j)=Forage_quantity(i,j,k)
                 ENDIF
                 IF(PICcow(i,j,k).NE.0) THEN
                        wanimalcalfinit(i,j)     =PICcow(i,j,k)
                 ELSE
                        Wanimalcalfinit(i,j)     =Wcalfborn(i,j)
                 ENDIF
                 calfinit(i,j)=0
              ENDIF

              IF (( wshtot(i,j).GT.BM_threshold+0.05) .AND.f_autogestion.NE.2 .AND. &
                   f_postauto .NE. 1 &
                   .AND. (tjulian .GE. tanimal(i,j,k)) .AND. &
                   (tjulian .LT. (tanimal(i,j,k) + danimal(i,j,k))) ) THEN
                 nanimaltot(i,j)  =nanimal(i,j,k)
             ENDIF
          ENDDO ! npts


          DO i=1,npts
            IF(tjulian .EQ.tcalving(i,j)) THEN
               Wanimalcalf(i,j)=Wcalfborn(i,j)
            END IF
         END DO
       END DO !k
     END IF
   END DO!nvm

! #  CALCULS
! Cas ou le paturage est calcule par le modele
! Stocking rate calculation if grazing autogestion
!-------------------------------------------------

! CALCUL 1 : 
!-------------------------------------------------

!   tcutmodel = 1 dans le fichier de conditions initiales
! flag qui existait dans la version initiale de PaSim permettant de faire 
! des fauches 'automatiquement'
! le module d'autogestion developpe par N Vuichard utilise ce flagpour le 
! mode 'fauche' mais de manière 'transparente (pas besoin de l'activer)
! pour info:
! dans cette configuration, 
! - il fallait que le chargement de la premiere periode de paturage soit renseigne pour
! initialiser le calcul du modele
! - les animaux etaient sortis au dela de tseasonendmin = 250 (07/09)
! - le chargement calcule etait seuille entre 0 et nanimaltotmax = 10 UGB/ha
! - pasim ajoutait journalièrement 'deltanimal' animaux soit au minimum 1 UGB/ha, sinon
! un nombre d'animaux calcule comme le ratio biomasse disponible:capacité d'ingestion maximale
! d'un animal
! AVEC wshtot - wshtotgrazing: biomasse disponible au jour j c'est a dire non paturee
!                   intakemax: valeur de la capacité d'ingestion maximale d'un animal
! (à defaut 15kg MS/UGB/m2)

        calc_nanimaltot : IF (tcutmodel .EQ. 1) THEN
          DO j=2,nvm
             IF (is_grassland_manag(j) .AND. (.NOT.is_grassland_cut(j)).AND.&
                  (.NOT.is_grassland_grazed(j)))THEN


                WHERE ((nanimal(:,j,1) .GT. 0.0) .AND. (devstage(:,j) .GT. devstocking) .AND. &
                     (stockingstart(:,j) .EQ. 0))

                nanimaltot(:,j) = nanimal(:,j,1)
                stockingstart(:,j) = 1

            END WHERE
          
            IF (tjulian .GT. tseasonendmin) THEN
               WHERE ((stockingstart(:,j) .EQ. 1) .AND. (stockingend(:,j) .EQ. 0) .AND. &
                    (snowfall_daily(:) .GT. 1e-3))

                    stockingend(:,j)  = 1

                END WHERE
            END IF
          
            WHERE (stockingend(:,j) .EQ. 1)

                nanimaltot(:,j)  = 0.0

            ELSEWHERE ( (nanimal(:,j,1) .GT. 0.0) .AND. (stockingstart(:,j) .EQ. 1))

                deltaanimal(:,j) = MIN (0.0001,(wshtot(:,j) - wshtotgrazing(:,j))/intakemax(:,j))
                nanimaltot(:,j)  = MIN (MAX (0.0, nanimaltot(:,j)  + deltaanimal(:,j)), nanimaltotmax)

            END WHERE
          END IF!manag not cut not graze
        END DO

      ENDIF calc_nanimaltot

! CALCUL 2 :
! Ajout Nicolas VUICHARD pour autogestion
! si autogestion = 2 --> Animaux
!-------------------------------------------------

!070703 AIG à confirmer   
! Les animaux sont sortis de la parcelle si la biomasse disponible devient inférieure à 
! min_grazing = 0.2 kg MS / m²    
! * stocking rate  = 1 animal/ha on condition that shoot biomass is greater 
! than min_grazing + 0.05 (with min_grazing = 0.2 kg MS / m²)
! * else we consider there is not enough biomass to feed animals and grazing 
! stop or not begin: stocking rate  = 0 animal/ha 
! nanimaltot: stocking rate h(1...ntocking) (animal/m²) *!      

        IF (f_autogestion .EQ. 2) THEN
        ! AIG 23/07/2010, min_grazing à changer pour BM_threshold
        DO j=2,nvm
           IF (is_grassland_manag(j) .AND. (.NOT.is_grassland_cut(j)).AND. &
                (.NOT.is_grassland_grazed(j)))THEN

            WHERE (wshtot(:,j) .GE. (BM_threshold_turnout))

                delai_ugb(:,j) = delai_ugb(:,j) + 1
                ! Potentialy I can put animals, if delai_ugb >=0
                WHERE (delai_ugb(:,j) .GE. 0)
                  ugb(:,j) = 1 ! animals are in
                  WHERE (compte_pature(:,j).LE.10)
                    compt_ugb(:,j)  = compt_ugb(:,j) + 1
                    nanimaltot(:,j) = sr_ugb(:,j)
                  ELSEWHERE
                    nanimaltot(:,j)=0.0
                  END WHERE
                ENDWHERE
            ELSEWHERE (wshtot(:,j) .LT. BM_threshold)
                ! AIG 23/07/2010, min_grazing à changer pour BM_threshold
                ! A la sortie des animaux sauvegarde des donnée a écrire dans le fichier Yield


                Autogestion_out(:,j,1)=Local_autogestion_out(:,j,1)
                Autogestion_out(:,j,2)=Local_autogestion_out(:,j,2)

                nanimaltot(:,j) = 0.0
                !compt_ugb(:)           = 0
                !Quand les animaux sont sortis on initialise delai_ugb au temps minimum 
                !separant la nouvelle entrée en pature               
                !delai_ugb = -15    ! RL 23 July 2010            
                ugb(:,j) = 0 ! animals are moved out

            END WHERE
          END IF!manag not cut not graze
        END DO



          DO j=2,nvm
            DO i=1,npts
              IF ((nanimaltot_prec(i,j)>0.0).AND.(nanimaltot(i,j).EQ.0.0)) THEN
                delai_ugb(i,j) = -15
              ENDIF
            ENDDO
         ENDDO
                       
        END IF

        IF (f_postauto .EQ. 1) THEN
        ! AIG 23/07/2010, min_grazing à changer pour BM_threshold

            WHERE (wshtot(:,mgraze_C3) .GE. (BM_threshold_turnout))

                delai_ugb(:,mgraze_C3) = delai_ugb(:,mgraze_C3) + 1
                ! Potentialy I can put animals, if delai_ugb >=0
                WHERE (delai_ugb(:,mgraze_C3) .GE. 0)
                  ugb(:,mgraze_C3) = 1 ! animals are in
                  WHERE (compte_pature(:,mgraze_C3).LE.10)
                    compt_ugb(:,mgraze_C3)  = compt_ugb(:,mgraze_C3) + 1
                    nanimaltot(:,mgraze_C3) = sr_ugb(:,mgraze_C3)
                  ELSEWHERE
                    nanimaltot(:,mgraze_C3)=0.0
                  END WHERE
                ENDWHERE
            ELSEWHERE (wshtot(:,mgraze_C3) .LT. BM_threshold)
                ! AIG 23/07/2010, min_grazing à changer pour BM_threshold
                ! A la sortie des animaux sauvegarde des donnée a écrire dans le
                ! fichier Yield
                Autogestion_out(:,mgraze_C3,1)=Local_autogestion_out(:,mgraze_C3,1)
                Autogestion_out(:,mgraze_C3,2)=Local_autogestion_out(:,mgraze_C3,2)

                nanimaltot(:,mgraze_C3) = 0.0
                !compt_ugb(:)           = 0
                !Quand les animaux sont sortis on initialise delai_ugb au temps
                !minimum 
                !separant la nouvelle entrée en pature               
                !delai_ugb = -15    ! RL 23 July 2010            
                ugb(:,mgraze_C3) = 0 ! animals are moved out
            END WHERE

            WHERE (wshtot(:,mgraze_C4) .GE. (BM_threshold_turnout))

                delai_ugb(:,mgraze_C4) = delai_ugb(:,mgraze_C4) + 1
                ! Potentialy I can put animals, if delai_ugb >=0
                WHERE (delai_ugb(:,mgraze_C4) .GE. 0)
                  ugb(:,mgraze_C4) = 1 ! animals are in
                  WHERE (compte_pature(:,mgraze_C4).LE.10)
                    compt_ugb(:,mgraze_C4)  = compt_ugb(:,mgraze_C4) + 1
                    nanimaltot(:,mgraze_C4) = sr_ugb(:,mgraze_C4)
                  ELSEWHERE
                    nanimaltot(:,mgraze_C4)=0.0
                  END WHERE
                ENDWHERE
            ELSEWHERE (wshtot(:,mgraze_C4) .LT. BM_threshold)
                ! AIG 23/07/2010, min_grazing à changer pour BM_threshold
                ! A la sortie des animaux sauvegarde des donnée a écrire dans le
                ! fichier Yield
                Autogestion_out(:,mgraze_C4,1)=Local_autogestion_out(:,mgraze_C4,1)
                Autogestion_out(:,mgraze_C4,2)=Local_autogestion_out(:,mgraze_C4,2)

                nanimaltot(:,mgraze_C4) = 0.0
                !compt_ugb(:)           = 0
                !Quand les animaux sont sortis on initialise delai_ugb au temps
                !minimum 
                !separant la nouvelle entrée en pature               
                !delai_ugb = -15    ! RL 23 July 2010            
                ugb(:,mgraze_C4) = 0 ! animals are moved out
            END WHERE


          DO j=2,nvm
            DO i=1,npts
              IF ((nanimaltot_prec(i,j)>0.0).AND.(nanimaltot(i,j).EQ.0.0)) THEN
                delai_ugb(i,j) = -15
              ENDIF
            ENDDO
         ENDDO

                   
        END IF


! JCMODIF for differen sr_ugb given varied threshold
! with 1 LSU of 250 gDM and stop grazing with 0.8 * 250 g DM
! with < 1 LSU of 2*2^(1-sr_ugb*10000)*sr_ugb*10000*125
! e.g., 0.5 LSU 180 gDM  0.1 LSU 46 gDM
! 0.01 LSU 5 gDM

        IF (f_postauto .EQ. 5) THEN

          able_grazing(:,mgraze_C3) = sr_ugb(:,mgraze_C3) * 10000.0 * 250.0 * &
                 2.0**(1.0-(sr_ugb(:,mgraze_C3)*10000.0))/1000.0
          able_grazing(:,mgraze_C4) = sr_ugb(:,mgraze_C4) * 10000.0 * 250.0 * &
                 2.0**(1.0-(sr_ugb(:,mgraze_C4)*10000.0))/1000.0
!print *,'able_grazing', able_grazing(301:320,mgraze_C3)
          ! > 1 LSU/ha using 0.25 kgDM
          WHERE (sr_ugb(:,mgraze_C3) .GE. 0.0001)
            WHERE (wshtot(:,mgraze_C3) .GE. (min_grazing + 0.05))

              delai_ugb(:,mgraze_C3) = delai_ugb(:,mgraze_C3) +1
              WHERE (delai_ugb(:,mgraze_C3) .GE. 0)
                ugb(:,mgraze_C3) = 1
              ENDWHERE

            ELSEWHERE (wshtot(:,mgraze_C3) .LT. (min_grazing - 0.075))
               Autogestion_out(:,mgraze_C3,1)=Local_autogestion_out(:,mgraze_C3,1)
                Autogestion_out(:,mgraze_C3,2)=Local_autogestion_out(:,mgraze_C3,2)
                nanimaltot (:,mgraze_C3) = 0.0
                ugb(:,mgraze_C3)           = 0
                delai_ugb(:,mgraze_C3) = -15
            END WHERE

         ELSEWHERE (sr_ugb(:,mgraze_C3) .GE. 0.00002 .and. &
              sr_ugb(:,mgraze_C3) .LT. 0.0001)
            WHERE (wshtot(:,mgraze_C3) .GE. able_grazing(:,mgraze_C3))

              delai_ugb(:,mgraze_C3) = delai_ugb(:,mgraze_C3) +1
              WHERE (delai_ugb(:,mgraze_C3) .GE. 0)
                ugb(:,mgraze_C3) = 1
              ENDWHERE

            ELSEWHERE (wshtot(:,mgraze_C3) .LT. able_grazing(:,mgraze_C3)*0.5)
               Autogestion_out(:,mgraze_C3,1)=Local_autogestion_out(:,mgraze_C3,1)
                Autogestion_out(:,mgraze_C3,2)=Local_autogestion_out(:,mgraze_C3,2)
                nanimaltot (:,mgraze_C3) = 0.0
                ugb(:,mgraze_C3)           = 0
                delai_ugb(:,mgraze_C3) = -15
            END WHERE
          ELSEWHERE (sr_ugb(:,mgraze_C3) .LT. 0.00002)
            WHERE (wshtot(:,mgraze_C3) .GE. able_grazing(:,mgraze_C3))

              delai_ugb(:,mgraze_C3) = delai_ugb(:,mgraze_C3) +1
              WHERE (delai_ugb(:,mgraze_C3) .GE. 0)
                ugb(:,mgraze_C3) = 1
              ENDWHERE

            ELSEWHERE (wshtot(:,mgraze_C3) .LT. able_grazing(:,mgraze_C3)*0.3)
               Autogestion_out(:,mgraze_C3,1)=Local_autogestion_out(:,mgraze_C3,1)
                Autogestion_out(:,mgraze_C3,2)=Local_autogestion_out(:,mgraze_C3,2)
                nanimaltot (:,mgraze_C3) = 0.0
                ugb(:,mgraze_C3)           = 0
                delai_ugb(:,mgraze_C3) = -15
            END WHERE
          ENDWHERE
            IF (tjulian .GT. tseasonendmin) THEN
              WHERE (snowfall_daily(:) .GT. 1e-3)
                nanimaltot (:,mgraze_C3) = 0.0
                ugb(:,mgraze_C3)           = 0
              ENDWHERE
            ENDIF
            WHERE (ugb(:,mgraze_C3) .EQ. 1)
                compt_ugb(:,mgraze_C3)  = compt_ugb(:,mgraze_C3) + 1
              WHERE (sr_ugb(:,mgraze_C3) .GT. 0.00002)
                nanimaltot (:,mgraze_C3) = sr_ugb(:,mgraze_C3)
              ELSEWHERE
                nanimaltot (:,mgraze_C3) = 0.00002
              ENDWHERE
            END WHERE
          ! > 1 LSU/ha using 0.25 kgDM
          WHERE (sr_ugb(:,mgraze_C4) .GE. 0.0001)
            WHERE (wshtot(:,mgraze_C4) .GE. (min_grazing + 0.05))

              delai_ugb(:,mgraze_C4) = delai_ugb(:,mgraze_C4) +1
              WHERE (delai_ugb(:,mgraze_C4) .GE. 0)
                ugb(:,mgraze_C4) = 1
              ENDWHERE

            ELSEWHERE (wshtot(:,mgraze_C4) .LT. (min_grazing - 0.075))
                Autogestion_out(:,mgraze_C4,1)=Local_autogestion_out(:,mgraze_C4,1)
                Autogestion_out(:,mgraze_C4,2)=Local_autogestion_out(:,mgraze_C4,2)
                nanimaltot (:,mgraze_C4) = 0.0
                ugb(:,mgraze_C4)           = 0
                delai_ugb(:,mgraze_C4) = -15
            END WHERE
         ELSEWHERE (sr_ugb(:,mgraze_C4) .GE. 0.00002 .and. &
              sr_ugb(:,mgraze_C4) .LT. 0.0001)
            WHERE (wshtot(:,mgraze_C4) .GE. able_grazing(:,mgraze_C4))

              delai_ugb(:,mgraze_C4) = delai_ugb(:,mgraze_C4) +1
              WHERE (delai_ugb(:,mgraze_C4) .GE. 0)
                ugb(:,mgraze_C4) = 1
              ENDWHERE

            ELSEWHERE (wshtot(:,mgraze_C4) .LT. able_grazing(:,mgraze_C4)*0.5)
                Autogestion_out(:,mgraze_C4,1)=Local_autogestion_out(:,mgraze_C4,1)
                Autogestion_out(:,mgraze_C4,2)=Local_autogestion_out(:,mgraze_C4,2)
                nanimaltot (:,mgraze_C4) = 0.0
                ugb(:,mgraze_C4)           = 0
                delai_ugb(:,mgraze_C4) = -15
            END WHERE
          ELSEWHERE (sr_ugb(:,mgraze_C4) .LT. 0.00002)
            WHERE (wshtot(:,mgraze_C4) .GE. able_grazing(:,mgraze_C4))

              delai_ugb(:,mgraze_C4) = delai_ugb(:,mgraze_C4) +1
              WHERE (delai_ugb(:,mgraze_C4) .GE. 0)
                ugb(:,mgraze_C4) = 1
              ENDWHERE

            ELSEWHERE (wshtot(:,mgraze_C4) .LT. able_grazing(:,mgraze_C4)*0.3)
                Autogestion_out(:,mgraze_C4,1)=Local_autogestion_out(:,mgraze_C4,1)
                Autogestion_out(:,mgraze_C4,2)=Local_autogestion_out(:,mgraze_C4,2)
                nanimaltot (:,mgraze_C4) = 0.0
                ugb(:,mgraze_C4)           = 0
                delai_ugb(:,mgraze_C4) = -15
            END WHERE
          ENDWHERE
            IF (tjulian .GT. tseasonendmin) THEN
              WHERE (snowfall_daily(:) .GT. 1e-3)
                nanimaltot (:,mgraze_C4) = 0.0
                ugb(:,mgraze_C4)           = 0
              ENDWHERE
            ENDIF
            WHERE (ugb(:,mgraze_C4) .EQ. 1)
                compt_ugb(:,mgraze_C4)  = compt_ugb(:,mgraze_C4) + 1
              WHERE (sr_ugb(:,mgraze_C4) .GT. 0.00002)
                nanimaltot (:,mgraze_C4) = sr_ugb(:,mgraze_C4)
              ELSEWHERE
                nanimaltot (:,mgraze_C4) = 0.00002
              ENDWHERE
            END WHERE

        ENDIF
!end gmjc
    IF (f_autogestion .EQ. 2) THEN
      DO j=2,nvm
         IF (is_grassland_manag(j) .AND. (.NOT.is_grassland_cut(j)).AND. &
              (.NOT.is_grassland_grazed(j)))THEN

          IF(ugb(1,j).NE.ugb_last(1,j)) THEN
            IF ((ugb(1,j).EQ.1)) THEN
              print*, 'Animaux in'
            ELSE
              print*, 'Animaux out'
            ENDIF
          ENDIF
        END IF!manag not cut not graze
      END DO

    ENDIF
    IF (f_postauto .EQ. 1) THEN
       IF(ugb(1,mgraze_C3).NE.ugb_last(1,mgraze_C3)) THEN
          IF ((ugb(1,mgraze_C3).EQ.1)) THEN
             print*, 'Animaux in'
          ELSE
             print*, 'Animaux out'
          ENDIF
       ENDIF
    ENDIF
    ! Mise a jour de tanimal, danimal, BCS(Y/M) et PI(Y/M) et des valeurs intiales pour le premier 
    ! chargement en cas d'autogestion
    ! Renseignements des variables du fichier management pour ecriture de ce dernier en fin de
    ! simulation
      IF (f_autogestion.EQ.2) THEN
        DO j=2,nvm
           IF (is_grassland_manag(j) .AND. (.NOT.is_grassland_cut(j)).AND.&
                (.NOT.is_grassland_grazed(j)))THEN

            DO i=1,npts
             !Nous sommes sur une entrée en paturage, on initialise les valeurs de simulation et on sauvegarde
             !les données pour ecriture management
               IF((nanimaltot_prec(i,j).EQ.0).AND.(nanimaltot(i,j).NE.0).AND.&
                    (compte_pature(i,j).LE.10)) THEN      
                 !nous sommes limites à 10 periodes de paturage
                 compte_pature(i,j)=compte_pature(i,j)+1
                  print *, "compte pature : ", compte_pature(i,j)
                 IF(compte_pature(i,j).GT.10) THEN
                    compte_pature(i,j)=10
                 ENDIF
                 BCScow(i,j,1)=autogestion_BCScow(i,j,1)
                 BCScow(i,j,2)=autogestion_BCScow(i,j,2)
                 Wanimalcow(i,j,1)=autogestion_weightcow(i,j,1)
                 Wanimalcow(i,j,2)=autogestion_weightcow(i,j,2)
                 AGEcow(i,j,1)=autogestion_AGEcow(i,j,1)+tjulian /30
                 AGEcow(i,j,2)=autogestion_AGEcow(i,j,2)+tjulian /30
                 Fday_pasture(i,j)=tjulian 

                 autogestion_init(i,j)=1

                 PIYcow(i,j,compte_pature(i,j))=Wanimalcow(i,j,1)
                 PIMcow(i,j,compte_pature(i,j))=Wanimalcow(i,j,2)
                 BCSYcow(i,j,compte_pature(i,j))=BCScow(i,j,1)
                 BCSMcow(i,j,compte_pature(i,j))=BCScow(i,j,2)
                 AGE_cow_P(i,j,compte_pature(i,j))=AGEcow(i,j,1)
                 AGE_cow_M(i,j,compte_pature(i,j))=AGEcow(i,j,2)
                 nanimal(i,j,compte_pature(i,j))=nanimaltot(i,j)
                 tanimal(i,j,compte_pature(i,j))=tjulian 
             ENDIF
             !cas d'une sortie de paturage
             IF(nanimaltot_prec(i,j).NE.0.AND.nanimaltot(i,j).EQ.0) THEN
                 print *, "compte pature : ", compte_pature(i,j)
                 danimal(i,j,compte_pature(i,j))=tjulian -tanimal(i,j,compte_pature(i,j))
                 !on sauvegarde les poids et BCS des vaches pour la prochaine entré en paturage
                 autogestion_BCScow(i,j,1)=BCScow(i,j,1)
                 autogestion_BCScow(i,j,2)=BCScow(i,j,2)
                 autogestion_weightcow(i,j,1)=Wanimalcow(i,j,1)
                 autogestion_weightcow(i,j,2)=Wanimalcow(i,j,2)
             ENDIF
           ENDDO !i
         END IF!manag not cut not graze
       END DO
      ELSE IF (f_postauto.EQ.1 .OR. f_postauto .EQ. 5) THEN
         DO i=1,npts
             !Nous sommes sur une entrée en paturage, on initialise les valeurs
             !de simulation et on sauvegarde
             !les données pour ecriture management
            IF((nanimaltot_prec(i,mgraze_C3).EQ.0).AND.&
                 (nanimaltot(i,mgraze_C3).NE.0).AND.(compte_pature(i,mgraze_C3).LE.10))THEN
                 !nous sommes limites à 10 periodes de paturage
                 compte_pature(i,mgraze_C3)=compte_pature(i,mgraze_C3)+1
                  print *, "compte pature : ", compte_pature(i,mgraze_C3)
                 IF(compte_pature(i,mgraze_C3).GT.10) THEN
                    compte_pature(i,mgraze_C3)=10
                 ENDIF
                 BCScow(i,mgraze_C3,1)=autogestion_BCScow(i,mgraze_C3,1)
                 BCScow(i,mgraze_C3,2)=autogestion_BCScow(i,mgraze_C3,2)
                 Wanimalcow(i,mgraze_C3,1)=autogestion_weightcow(i,mgraze_C3,1)
                 Wanimalcow(i,mgraze_C3,2)=autogestion_weightcow(i,mgraze_C3,2)
                 AGEcow(i,mgraze_C3,1)=autogestion_AGEcow(i,mgraze_C3,1)+tjulian /30
                 AGEcow(i,mgraze_C3,2)=autogestion_AGEcow(i,mgraze_C3,2)+tjulian /30
                 Fday_pasture(i,mgraze_C3)=tjulian

                 autogestion_init(i,mgraze_C3)=1

                 PIYcow(i,mgraze_C3,compte_pature(i,mgraze_C3))=Wanimalcow(i,mgraze_C3,1)
                 PIMcow(i,mgraze_C3,compte_pature(i,mgraze_C3))=Wanimalcow(i,mgraze_C3,2)
                 BCSYcow(i,mgraze_C3,compte_pature(i,mgraze_C3))=BCScow(i,mgraze_C3,1)
                 BCSMcow(i,mgraze_C3,compte_pature(i,mgraze_C3))=BCScow(i,mgraze_C3,2)
                 AGE_cow_P(i,mgraze_C3,compte_pature(i,mgraze_C3))=AGEcow(i,mgraze_C3,1)
                 AGE_cow_M(i,mgraze_C3,compte_pature(i,mgraze_C3))=AGEcow(i,mgraze_C3,2)
                 nanimal(i,mgraze_C3,compte_pature(i,mgraze_C3))=nanimaltot(i,mgraze_C3)
                 tanimal(i,mgraze_C3,compte_pature(i,mgraze_C3))=tjulian
             ENDIF
             !cas d'une sortie de paturage
             IF(nanimaltot_prec(i,mgraze_C3).NE.0.AND.nanimaltot(i,mgraze_C3).EQ.0) THEN
                 print *, "compte pature : ", compte_pature(i,mgraze_C3)
                 danimal(i,mgraze_C3,compte_pature(i,mgraze_C3))=tjulian-tanimal(i,mgraze_C3,compte_pature(i,mgraze_C3))
                 !on sauvegarde les poids et BCS des vaches pour la prochaine
                 !entré en paturage
                 autogestion_BCScow(i,mgraze_C3,1)=BCScow(i,mgraze_C3,1)
                 autogestion_BCScow(i,mgraze_C3,2)=BCScow(i,mgraze_C3,2)
                 autogestion_weightcow(i,mgraze_C3,1)=Wanimalcow(i,mgraze_C3,1)
                 autogestion_weightcow(i,mgraze_C3,2)=Wanimalcow(i,mgraze_C3,2)
             ENDIF

             IF((nanimaltot_prec(i,mgraze_C4).EQ.0).AND.&
                  (nanimaltot(i,mgraze_C4).NE.0).AND.(compte_pature(i,mgraze_C4).LE.10))THEN
                 !nous sommes limites à 10 periodes de paturage
                 compte_pature(i,mgraze_C4)=compte_pature(i,mgraze_C4)+1
                  print *, "compte pature : ", compte_pature(i,mgraze_C4)
                 IF(compte_pature(i,mgraze_C4).GT.10) THEN
                    compte_pature(i,mgraze_C4)=10
                 ENDIF
                 BCScow(i,mgraze_C4,1)=autogestion_BCScow(i,mgraze_C4,1)
                 BCScow(i,mgraze_C4,2)=autogestion_BCScow(i,mgraze_C4,2)
                 Wanimalcow(i,mgraze_C4,1)=autogestion_weightcow(i,mgraze_C4,1)
                 Wanimalcow(i,mgraze_C4,2)=autogestion_weightcow(i,mgraze_C4,2)
                 AGEcow(i,mgraze_C4,1)=autogestion_AGEcow(i,mgraze_C4,1)+tjulian/30
                 AGEcow(i,mgraze_C4,2)=autogestion_AGEcow(i,mgraze_C4,2)+tjulian/30
                 Fday_pasture(i,mgraze_C4)=tjulian

                 autogestion_init(i,mgraze_C4)=1

                 PIYcow(i,mgraze_C4,compte_pature(i,mgraze_C4))=Wanimalcow(i,mgraze_C4,1)
                 PIMcow(i,mgraze_C4,compte_pature(i,mgraze_C4))=Wanimalcow(i,mgraze_C4,2)
                 BCSYcow(i,mgraze_C4,compte_pature(i,mgraze_C4))=BCScow(i,mgraze_C4,1)
                 BCSMcow(i,mgraze_C4,compte_pature(i,mgraze_C4))=BCScow(i,mgraze_C4,2)
                 AGE_cow_P(i,mgraze_C4,compte_pature(i,mgraze_C4))=AGEcow(i,mgraze_C4,1)
                 AGE_cow_M(i,mgraze_C4,compte_pature(i,mgraze_C4))=AGEcow(i,mgraze_C4,2)
                 nanimal(i,mgraze_C4,compte_pature(i,mgraze_C4))=nanimaltot(i,mgraze_C4)
                 tanimal(i,mgraze_C4,compte_pature(i,mgraze_C4))=tjulian
             ENDIF
             !cas d'une sortie de paturage
             IF(nanimaltot_prec(i,mgraze_C4).NE.0.AND.&
                  nanimaltot(i,mgraze_C4).EQ.0)THEN
                 print *, "compte pature : ", compte_pature(i,mgraze_C4)
                 danimal(i,mgraze_C4,compte_pature(i,mgraze_C4))=tjulian-tanimal(i,mgraze_C4,compte_pature(i,mgraze_C4))
                 !on sauvegarde les poids et BCS des vaches pour la prochaine
                 !entré en paturage
                 autogestion_BCScow(i,mgraze_C4,1)=BCScow(i,mgraze_C4,1)
                 autogestion_BCScow(i,mgraze_C4,2)=BCScow(i,mgraze_C4,2)
                 autogestion_weightcow(i,mgraze_C4,1)=Wanimalcow(i,mgraze_C4,1)
                 autogestion_weightcow(i,mgraze_C4,2)=Wanimalcow(i,mgraze_C4,2)
             ENDIF


         ENDDO !i

      ENDIF

    END IF n_day !n_day
    !Flag gestation and calf computation
    gestation=0
    calf=0
    tempTjulian=int(Tjulian*100)
    tempTjulian=tempTjulian/100
    DO j=2,nvm
      DO i=1,npts
        IF (tempTjulian .GE. tcalving(i,j)) THEN

     !84 est 365 moins la durée de gestation(280j) 
          IF (tempTjulian - tcalving(i,j) .GE. 84) THEN
            gestation(i,j)=1
          ENDIF
          IF (tempTjulian-tcalving(i,j) .LE. age_sortie_calf(i,j)+1) THEN
            calf(i,j)=1
          ENDIF
        ELSE
           IF (tempTjulian+365-tcalving(i,j) .GE. 84 .and. &
                tempTjulian+365-tcalving(i,j) .LE. 365) THEN
            gestation(i,j)=1
          ENDIF
          IF (365-(tcalving(i,j)-tempTjulian).LT. age_sortie_calf(i,j)+1) THEN
            calf(i,j)=1
          ENDIF
        ENDIF
      ENDDO
    ENDDO
    WHERE (nanimaltot.EQ.0)
      calf=0     
      gestation=0
    END WHERE   

    IF (type_animal.NE.2) THEN
      calf=0
      wanimalcalf=0.0
    ENDIF


   ! dans le cas autogestion, le calcul du poids d  u veau lorque les animaux commence le paturage
   ! est estimé par un modèle
   IF(type_animal.EQ.2) THEN
    DO j=2,nvm
      IF (f_autogestion.EQ.2) THEN
            DO i=1,npts
               IF (nanimaltot_prec(i,j).EQ.0.AND.&
                    nanimaltot(i,j).GT.0.AND.calf(i,j).EQ.1) THEN
                   IF(tjulian.GT.tcalving(i,j)) THEN
                      CALL estime_weightcalf(tjulian-tcalving(i,j),Wcalfborn(i,j),Wanimalcalf(i,j))
                   ELSE
                      CALL estime_weightcalf(365+tjulian-tcalving(i,j),Wcalfborn(i,j),Wanimalcalf(i,j))
                   ENDIF
                   PICcow(i,j,compte_pature(i,j))=Wanimalcalf(i,j)
                   ENDIF
                IF (tjulian.EQ.tcalving(i,j)) THEN
                   Wanimalcalf(i,j)=Wcalfborn(i,j)
                ENDIF
            ENDDO
      ELSE
           DO i=1,npts
             IF (calf(i,j) .EQ. 1 .AND. calfinit(i,j) .EQ. 0) THEN
                 Wanimalcalf(i,j)=Wanimalcalfinit(i,j)
                 calfinit(i,j)=1
             ENDIF
           ENDDO
       ENDIF
     ENDDO
   ENDIF


   WHERE(nanimaltot.GT.0)
      AGE_animal(:,:,1)=AGEcow(:,:,1)+(tjulian-Fday_pasture(:,:))/30
      AGE_animal(:,:,2)=AGEcow(:,:,2)+(tjulian-Fday_pasture(:,:))/30
   ENDWHERE
   nanimaltot_prec=nanimaltot


!---------------------
! Milk Production (MP)
! Just the potential MP for dairy cows
!---------------------

   IF(type_animal.EQ.1) THEN    ! Dairy cows
       !dans le cas dairy, on ne calcule que la production potentielle
       !necessaire au calcul de la complémentation et de la NEL totale
       !la production de lait du module dairy est fonction de l'ingéré

       CALL Potentiel_dairy_d(npts,tjulian,Nweeklact,NweekGest,MPwmax,MPwcow2)
       !Affectation necessaire pour le calcul de la complémentation
       !le vrai potentiel est calculé apres car necessité de l'ingestion totale

       MPcow2=MPwcow2

   ELSEIF(type_animal.EQ.2) THEN ! Suckler cows

       CALL Milk_Animal_cow(               &
       npts, dt                            ,&
       nanimaltot,tjulian,NEBcow_prec       ,&
       MPcow2,MPcow,MPwcow2,&
       MPcowC, MPcowN              ,&
       MPcowCsum, MPcowNsum, milkanimalsum,milkKG)

   ELSEIF(type_animal.EQ.4.OR.type_animal.EQ.5)THEN ! Heifers
       MPcow2=0.
       MPcow=0.
       MPwcow2=0.
       MPcowC=0.
       MPcowN=0.
       MPcowCsum=0.
       MPcowNsum=0.
       milkanimalsum=0.
       milkKG=0.
       nWeeklact=0.
       nWeekGest=0.
    ENDIF


!---------------------
! intake capacity (IC) 
!--------------------- 
! Cow intake capacity  (young/primiparous and old/multiparous)
    IF(type_animal.EQ.1) THEN       !dairy
      CALL intake_capacity_cow_d(&
      npts,2,   &
      MPwcow2       ,&
      BCScow, wanimalcow, nanimaltot, ICcow,&
      AGE_animal, nWeekLact,nWeekGest)
    ELSEIF(type_animal.EQ.2)THEN    !suckler
      CALL intake_capacity_cow(&
          npts,   wanimalcow  , &
          MPwcow2, BCScow     , &
          nanimaltot, ICcow)
    ELSEIF(type_animal.EQ.4.OR.type_animal.EQ.5) THEN
      CALL intake_capacity_heifer(npts, type_animal, Wcalfborn, wanimalcow, ICcow)
    ENDIF

! Cow average IC
!------------------
! C'est cette capacite d'ingestion qui sera utilisee pour le calcul 
! des processus de selection animale avec le nouveau module

    IC_tot = ICcow(:,:,1) * pyoung(:,:) + ICcow(:,:,2) * (1-pyoung(:,:))

! Calf IC
!---------------
! MPwcow2 and BCScow must be here but not use in the calf case

    IF(type_animal.EQ.2) THEN
      CALL intake_capacity_calves(&
       npts,   wanimalcalf,&
       nanimaltot,tjulian, ICcalf)
    ENDIF

    WHERE (calf.EQ.0)
      ICcalf=0
    ENDWHERE

!----------------------------
! Dry matter ingestion (DMI)
!----------------------------

    IF(type_animal.EQ.1) THEN    ! Dairy cows (primiparous and multiparous)

        CALL Grazing_intake_cow_d(    &
             npts, 2                 ,&
             ntot,nanimaltot,DNDF    ,&
             NDF,ICcow,tadmin,tadmoy ,&
             DMIcowanimal            ,&
             OMD, wshtot, FVh,tmoy_14,&
             BM_threshold)

    ELSEIF(type_animal.EQ.2) THEN ! Suckler cows

        ! DMI of young cows
        CALL Grazing_intake_cow(       &
             npts, type_animal, wshtot,&
             tadmin,nanimaltot,DNDF   ,&
             NDF,ICcow(:,:,1)           ,&
             DMIcowanimal(:,:,1)        ,&
             OMD, tadmoy, FVh, ntot   ,&
             tmoy_14, BM_threshold)

        ! DMI of mature cows
        CALL Grazing_intake_cow(       &
             npts, type_animal, wshtot,&
             tadmin,nanimaltot,DNDF   ,&
             NDF,ICcow(:,:,2)           ,&
             DMIcowanimal(:,:,2)        ,&
             OMD, tadmoy, FVh, ntot   ,&
             tmoy_14, BM_threshold)

        ! DMI of calves
        !----------------------------------
        CALL Grazing_intake_cow(       &
             npts, type_animal, wshtot,&
             tadmin,nanimaltot,DNDF   ,&
             NDF,ICcalf               ,&
             DMIcalfanimal,OMD, tadmoy,&
             FVh, ntot,tmoy_14        ,&
             BM_threshold)

        !integration of cumulated value for calves 
        !   (grazing_intake_complementation is never called for calves variables     

             DMIcalf=DMIcalfanimal*nanimaltot
          DO j=2,nvm
             CALL Euler_funct (npts,dt,DMIcalfanimal(:,j), DMIcalfanimalsum(:,j))
             CALL Euler_funct (npts,dt,DMIcalf(:,j), DMIcalfsum(:,j))
             CALL Euler_funct (npts,dt,DMIcalf(:,j)*(n(:,j)+fn(:,j)),DMIcalfnsum(:,j))
          END DO

    ELSEIF(type_animal.EQ.4.OR.type_animal.EQ.5) THEN ! Heifers

       CALL Grazing_intake_cow(        &
             npts, type_animal, wshtot,&
             tadmin,nanimaltot,DNDF   ,&
             NDF,ICcow(:,:,1)           ,&
             DMIcowanimal(:,:,1)        ,&
             OMD, tadmoy, FVh, ntot   ,&
             tmoy_14, BM_threshold)

             !Pour l'appel de grazing_intake_complementation
             !la dimension 2 sera remise a zero dans grazing_intake_complementation
             ICcow(:,:,2)=ICcow(:,:,1)
             DMIcow(:,:,2)=DMIcow(:,:,1)

    ENDIF


  !---------------------------------------
  ! Energetic content of the herbage (NEL)
  !---------------------------------------

    CALL Calcul_NEL_herbage(npts,OMD, NELherbage)

  !---------------------------------------
  ! Energy required for cow - Necessary for auto-supplementation calculation
  !---------------------------------------
  !Si entrée en paturage alors MPcow2_prec = MPwcow2
    DO j=2,nvm
      DO k=1,nstocking
        DO i=1,npts
          IF (tanimal(i,j,k).EQ.tjulian.AND.f_autogestion.NE.2) THEN
            MPcow2_prec(i,j,1)=MPwcow2(i,j,1)
            MPcow2_prec(i,j,2)=MPwcow2(i,j,2)
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ! AIG 04/07/2010
  ! On calcule les besoins en energie pour realiser la production de lait potentielle (et non relle)
  ! On doit donc passer en entree de la subroutine MPwcow2 tout le temps     
    CALL Calcul_NER_cow(npts,2,wanimalcow,wcalfborn, Age_animal, nweekgest, MPcow2_prec,NER,NEGcow,NEMcow)

  ! MODULE COMPLEMENTATION
  ! Complementation with herbage and concentrate in management or
  ! auto-complementation with herbage for suckler cow and concentrate for dairy cow
  !---------------------------------   

  ! Dans le cas des dairy, la production de lait n'est pas encore calculée, on prend donc la 
  ! la production de lait au pas de temps d'avant pour le calcul de la complémentation
    IF(type_animal.EQ.1) THEN
     MPcow2=MPcow2_prec
    ENDIF
    CALL grazing_intake_complementation(npts,dt                                      ,&
                                            DMIcowanimal, FVh, ICcow, FVf          ,&
                                            MPcow2,MPwcow2,Forage_quantity_period  ,&
                                            QIc, NELherbage, EVf,nanimaltot        ,&
                                            DMIcowsum,DMIcowanimalsum              ,&
                                            DMIcow,DMIcowNsum,n,fn,pyoung          ,&
                                            type_animal,intake_tolerance           ,&
                                            Q_max_complement,forage_complementc    ,&
                                            NER,forage_complementn,NEIcow,NEMcow   ,&
                                            NEIh,NEIf,NEIc,NEGcow,f_complementation,&
                                            DMIc,DMIf)

   ! Update of cattle Variables(old & young cows + calf)
   !-------------------------------------
    WHERE (nanimaltot.EQ.0)
      intake_animal=0.0
      intake=0.0
      OMD=0.0
      ! AIG et MG 06/02/2010
      intakemax=0.0
    ELSEWHERE
      intake_animal=DMIcalfanimal(:,:)+DMIcowanimal(:,:,1)*pyoung+DMIcowanimal(:,:,2)*(1-pyoung)
      intake=DMIcalf+DMIcow(:,:,1)+DMIcow(:,:,2)
      intakesum=DMIcowsum(:,:,1)+DMIcowsum(:,:,2)+DMIcalfsum(:,:)
      intakensum=DMIcalfnsum+DMIcowNsum(:,:,1)+DMIcowNsum(:,:,2)
     ! AIG et MG 06/02/2010 calcul de l'intakemax qui sera utilisé dans plante
     ! pour le calcul des préférences alimentaires des animaux
     intakemax = ICcow(:,:,1)*pyoung + ICcow(:,:,2)*(1-pyoung)+ ICcalf
    ENDWHERE

    DO j=2,nvm
      CALL Euler_funct (npts,dt,intake_animal(:,j), intake_animalsum(:,j))
    END DO

    CALL variablesPlantes(&
       npts,biomass,&
       c,n,intake_animal,intakemax,&
       AnimalDiscremineQualite)


    CALL chg_plante(&
       npts, dt, biomass  , &
       c, n,leaf_frac     , &
       wsh, wshtot        , &
       nanimaltot, intake_animal, &
       trampling,intake, &
       NDF,DNDF,DNDFI, &
       grazing_litter)

!    CALL variablesPlantes(&
!       npts,biomass,NDF,DNDF,DNDFI,&
!       c,n,intake_animal,intakemax,&
!       AnimalDiscremineQualite)


!---------------------------------------------------------
! Possible and observed Milk Production (MPpos and MPobs)
! For dairy cows only
!--------------------------------------------------------
   !
    WHERE(nanimaltot.GT.0.0.AND.MPcow2(:,:,1).GT.0.0.AND.&
         type_animal.eq.1.AND.f_complementation.EQ.4)
       Qic(:,:,1)=DMIc(:,:,1)/MPcow2(:,:,1)
    ENDWHERE

    WHERE(nanimaltot.GT.0.0.AND.MPcow2(:,:,2).GT.0.0.AND.&
         type_animal.eq.1.AND.f_complementation.EQ.4)
       Qic(:,:,2)=DMIc(:,:,2)/MPcow2(:,:,2)
    ENDWHERE

    IF(type_animal.EQ.1) THEN !Dairy cows

       CALL calcul_NEI_cow_d(npts,2,MPcow2_prec,DMIcowanimal,NELherbage  ,&
                                  EVf,Forage_quantity_period     ,&
                                  EVc,Qic,NEIcow,NEMcow,NEIh,NEIf,&
                                  NEIc)

       WHERE(BCScow_prev(:,:,1).EQ.0)
            deltaBCS(:,:,1)=0
       ELSEWHERE
            deltaBCS(:,:,1)=BCScow(:,:,1)-BCScow_prev(:,:,1)
       ENDWHERE

       WHERE(BCScow_prev(:,:,2).EQ.0)
            deltaBCS(:,:,2)=0
       ELSEWHERE
            deltaBCS(:,:,2)=BCScow(:,:,2)-BCScow_prev(:,:,2)
       ENDWHERE

    CALL Milk_Animal_cow_d(                        &
       npts, dt                                  ,&
       nanimaltot,tjulian                        ,&
       MPcow2,MPcow,MPwcow2                      ,&
       MPcowC, MPcowN                            ,&
       MPcowCsum, MPcowNsum, milkanimalsum,milkKG,&
       NWeekLact, NWeekGest,PEmax,PEpos,deltaBCS ,&
       MPpos,NEIcow,NEMcow,NEGcow,MPcow2_prec,MPwCow2)

       ! Une fois la quantité de lait produite, si les vaches laitières sont complémentées en concentré alors 
       ! il faut calculé la quantité Qic de concentré par litre de lait qui permet de faire les bilan d'energie
    ENDIF

    !On remet a jour QIc
    WHERE(nanimaltot.GT.0.0.AND.MPcow2(:,:,1).GT.0.0.AND.&
         type_animal.eq.1.AND.f_complementation.EQ.4)
       Qic(:,:,1)=DMIc(:,:,1)/MPcow2(:,:,1)
    ENDWHERE

    WHERE(nanimaltot.GT.0.0.AND.MPcow2(:,:,2).GT.0.0.AND.&
         type_animal.eq.1.AND.f_complementation.EQ.4)
       Qic(:,:,2)=DMIc(:,:,2)/MPcow2(:,:,2)
    ENDWHERE


   ! Update of cattle Variables(mature/multi cow of cattle + young/primi of cattle)
    IF(type_animal.EQ.1.OR.type_animal.EQ.2) THEN
      milksum(:,:)   =MPcowsum(:,:,1)+MPcowsum(:,:,2)
      milknsum(:,:)  =MPcowNsum(:,:,1)+MPcowNsum(:,:,2)
      milkcsum(:,:)  =MPcowCsum(:,:,1)+MPcowCsum(:,:,2)
      milkn(:,:)     =MPcowN(:,:,1)+MPcowN(:,:,2)
      milkc(:,:)     =MPcowC(:,:,1)+MPcowC(:,:,2)
    ENDIF


!------------------------  
! Net energy balance (NEB)
!------------------------
    IF(type_animal.EQ.1) THEN
    !NEB of dairy cows
    !------------------
    CALL balance_energy_cow_d(npts,2,dt,&
          MPcow2,MPwcow2,MPpos,&
          BCScow,BCScow_prev,AGE_animal,wanimalcow,nanimaltot)


    ELSEIF(type_animal.EQ.2) THEN
      !NEB of suckler cows
      !------------------
      !Young cows    
      CALL balance_energy_cow(npts,dt          ,&
         DMIcowanimal(:,:,1),MPcow2(:,:,1)         ,&
         0, BCScow(:,:,1),tjulian,wanimalcow(:,:,1),nanimaltot,&
         NEBcow(:,:,1), NELherbage, EVf(:,:),DMIf(:,:,1),&
         EVc(:,:),Qic(:,:,1), NEIcow(:,:,1), NEIh(:,:,1),&
         NEIf(:,:,1), NEIc(:,:,1),& ! to check
         NEPgestcow(:,:,1), NEPlactcow(:,:,1)      ,&
         NEPcow(:,:,1), NEMcow(:,:,1), NER(:,:,1))
      !Mature cows 
      CALL balance_energy_cow(npts,dt          ,&
         DMIcowanimal(:,:,2),MPcow2(:,:,2)         ,&
         1, BCScow(:,:,2),tjulian,wanimalcow(:,:,2),nanimaltot,&
         NEBcow(:,:,2), NELherbage, EVf(:,:), DMIf(:,:,2),&
         EVc(:,:),Qic(:,:,2), NEIcow(:,:,2), NEIh(:,:,2), &
         NEIf(:,:,2), NEIc(:,:,2),& ! to check
         NEPgestcow(:,:,2), NEPlactcow(:,:,2)      ,&
         NEPcow(:,:,2), NEMcow(:,:,2), NER(:,:,2))

      !NEB of suckler calves
      !------------------     
      CALL balance_energy_calf(npts,dt        ,&
         DMIcalfanimal,milkKG,nanimaltot      ,&
         wanimalcalf, NELherbage,NEIherbagecalf ,&
         NEImilkcalf, NEIcalf, NEMcalf, NEGcalf)


    ELSEIF(type_animal.EQ.4.OR.type_animal.EQ.5) THEN
      !NEB of heifers
      !------------------        
      CALL balance_energy_heifer(npts,dt,nanimaltot,&
                                 DMIcowanimal(:,:,1),NELherbage,&
                                 EVf(:,:),DMIf(:,:,1),&
                                 wanimalcow(:,:,1),NEIcow(:,:,1),&
                                 NEIh(:,:,1), NEIf(:,:,1),type_animal)
    ENDIF
    NEBcow_prec=NEBcow
    nel=NELherbage


    DO j=2,nvm
      CALL Euler_funct (npts,dt,intake(:,j)*nel(:,j),nelgrazingsum(:,j))
    ENDDO


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!ADD FROM Animaux_main_dynamic_post_plant 
  !!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !!!!!! In order to get the variables that needed by Respiration_Methane and Urine_Faeces
  !!!!!! we need to calculate new grazingn and grazingc using intake from above
  !!!!!! So we call modified cal_grazing which from MODULE applic_plant to get variables needed
    CALL cal_grazing(&
       npts                  , &
       nanimaltot            , &
       intake_animal         , &
       wsh                   , &
       wshtot                , &
       c                     , &
       n                     , &
       fn                    , &
       Substrate_grazingwc  , &
       Substrate_grazingwn  , &
       grazingcstruct        , &
       grazingnstruct        , &
       intake)

      !-----------------------------------------------------------  
      ! CARBON NITROGEN BALANCE
      !-----------------------------------------------------------


      WHERE (nanimaltot.NE.0)
         grazingn  = grazingnstruct + Substrate_grazingwn
         grazingc  = grazingcstruct + Substrate_grazingwc
      ELSEWHERE
         grazingn=0
         grazingc=0
      ENDWHERE
     DO j=2,nvm
       CALL Euler_funct (npts,dt,grazingn(:,j), grazingnsum(:,j))
       CALL Euler_funct (npts, dt, grazingc(:,j), grazingcsum(:,j))
     ENDDO
     WanimalMOYcow = (Wanimalcow(:,:,1)*pyoung + &
          wanimalcow(:,:,2)*(1-pyoung) + wanimalcalf)

      !-------------------------------- 
      !Respiration and  CH4 emission   
      !--------------------------------  
      IF(f_CH4_methode) THEN
      ! Calcul des emissions de methane selon N Vuichard
           CALL Respiration_Methane_cow(&
                 npts,  grazingc, &
                 nanimaltot, DNDFI, wanimalMOYcow,&
                 ranimal, methane)
      ELSE
      ! Calcul des emissions de methane selon Vermorel et al 2008
           CALL Respiration_Methane_cow_2(npts,2,&
                 type_animal,OMD,NEIh,NEIf,NEIc,&
                 grazingc,nanimaltot,pyoung,&
                 ranimal,methane,CH4animal,&
                 MPcow2, forage_complementc,&
                 f_complementation)

      ENDIF


       WHERE (nanimaltot.EQ.0)
           methane_ani=0
       ELSEWHERE
          methane_ani=methane/nanimaltot
       ENDWHERE
     DO j=2,nvm
        CALL Euler_funct (npts, dt, ranimal(:,j), ranimalsum(:,j))
        !!! @equation animaux::ranimalsum
        CALL Euler_funct (npts, dt, methane(:,j), Methanesum(:,j))
        !!! @equation animaux::Methanesum
        CALL Euler_funct (npts, dt, methane_ani(:,j), Methane_aniSum(:,j))
        !!! @equation animaux::Methane_aniSum
     ENDDO
      !------------------
      !Excreta  
      !------------------
        CALL Urine_Faeces_cow(&
           npts, grazingn, grazingc,&
           forage_complementc,&
           forage_complementn, nanimaltot ,&
           urineN, faecesN, &
           urineC, faecesC)
     DO j=2,nvm
        CALL Euler_funct (npts,dt,urineN(:,j),urineNsum(:,j))
        CALL Euler_funct (npts,dt,urineC(:,j),urineCsum(:,j))
        CALL Euler_funct (npts,dt,faecesN(:,j),faecesNsum(:,j))
        CALL Euler_funct (npts,dt,faecesC(:,j),faecesCsum(:,j))
     ENDDO



    !!!History write
    CALL xios_orchidee_send_field("GRAZINGC",grazingc)
    CALL xios_orchidee_send_field("NANIMALTOT",nanimaltot)
    CALL xios_orchidee_send_field("INTAKE_ANIMAL",intake_animal)
    CALL xios_orchidee_send_field("INTAKE",intake)
    CALL xios_orchidee_send_field("TRAMPLING",trampling)
    CALL xios_orchidee_send_field("CT_DRY",ct_dry)
!    CALL xios_orchidee_send_field("INTAKE_ANIMAL_LITTER",intake_animal_litter)
!    CALL xios_orchidee_send_field("INTAKE_LITTER",intake_litter)
!    CALL xios_orchidee_send_field("SR_WILD",sr_wild)
    CALL xios_orchidee_send_field("MILK",milk)
    CALL xios_orchidee_send_field("MILKC",milkc)
    CALL xios_orchidee_send_field("METHANE",Methane)
    CALL xios_orchidee_send_field("RANIMAL",ranimal)
    CALL xios_orchidee_send_field("URINEC",urinec)
    CALL xios_orchidee_send_field("FAECESC",faecesc)
    CALL xios_orchidee_send_field("GRAZED_FRAC",grazed_frac)
    CALL xios_orchidee_send_field("NB_ANI",nb_ani)
    CALL xios_orchidee_send_field("IMPORT_YIELD",import_yield)
    CALL xios_orchidee_send_field("NB_GRAZINGDAYS",nb_grazingdays)
    CALL xios_orchidee_send_field("OUTSIDE_FOOD",outside_food)

    !grazed
    CALL histwrite_p(hist_id_stomate ,'GRAZINGC',itime ,grazingc ,npts*nvm, horipft_index) 
    CALL histwrite_p(hist_id_stomate ,'GRAZINGCSUM',itime ,grazingcsum ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'NANIMALTOT',itime ,nanimaltot  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'INTAKE_ANIMAL' ,itime ,intake_animal  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'INTAKE'    ,itime ,intake     ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'INTAKESUM' ,itime ,intakesum  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'TRAMPLING' ,itime ,trampling  ,npts*nvm, horipft_index)
!gmjc for avoid grazing domestic over wet soil
    CALL histwrite_p(hist_id_stomate ,'CT_DRY' ,itime ,ct_dry  ,npts*nvm, horipft_index) 
    !milk NEW ANIMAL MODULE put in histwrite_p_cow_part1

    CALL histwrite_p(hist_id_stomate ,'MILKSUM'   ,itime ,milksum    ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'MILKCSUM'  ,itime ,milkcsum   ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'MILKC'     ,itime ,milkc      ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'MILKN'     ,itime ,milkn      ,npts*nvm, horipft_index)
 
    CALL histwrite_cow_Part1(npts,DMicowanimal(:,:,1),DMIcowanimal(:,:,2),DMIcalfanimal, &
        pyoung,OMD,MPcow2,NEBcow, NEIcow, nanimaltot, type_animal,MPwcow2,MPpos,DMIc,DMIf)
    
    !methane & respiration
    CALL histwrite_p(hist_id_stomate ,'METHANE',itime ,Methane ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'METHANE_ANI',itime ,Methane_ani ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'RANIMALSUM',itime ,ranimalsum ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'METHANESUM',itime ,MethaneSum ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'RANIMAL'   ,itime ,ranimal    ,npts*nvm, horipft_index)

    CALL histwrite_cow_Part2(npts,CH4animal(:,:,1),CH4animal(:,:,2))

    !farces and urine
    CALL histwrite_p(hist_id_stomate ,'FAECESNSUM',itime ,faecesnsum ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'FAECESCSUM',itime ,faecescsum ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'URINECSUM' ,itime ,urinecsum  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'URINENSUM' ,itime ,urinensum  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'NEL'       ,itime ,nel        ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'URINEN'    ,itime ,urinen     ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'URINEC'    ,itime ,urinec     ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'FAECESC'   ,itime ,faecesc    ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'FAECESN'   ,itime ,faecesn    ,npts*nvm, horipft_index)

    CALL histwrite_p(hist_id_stomate ,'GRAZED_FRAC' ,itime ,grazed_frac  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'NB_ANI' ,itime ,nb_ani  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'IMPORT_YIELD' ,itime ,import_yield  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'EXTRA_FEED' ,itime ,extra_feed  ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'COMPT_UGB',itime ,compt_ugb ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'NB_GRAZINGDAYS',itime ,nb_grazingdays ,npts*nvm, horipft_index)
    CALL histwrite_p(hist_id_stomate ,'AMOUNT_YIELD',itime ,amount_yield,npts*nvm,horipft_index)
    CALL histwrite_p(hist_id_stomate ,'CONSUMP',itime ,consump,npts*nvm,horipft_index)
    CALL histwrite_p(hist_id_stomate ,'ADD_NB_ANI',itime ,add_nb_ani,npts*nvm,horipft_index)
    CALL histwrite_p(hist_id_stomate ,'OUTSIDE_FOOD',itime ,outside_food,npts*nvm,horipft_index)

!
  END SUBROUTINE  Animaux_main_dynamic





  !********************************************
  !********************************************
  ! SUBROUTINE OF cow ANIMAL MODEL
  !********************************************
  !********************************************

  !----------------------------------
  ! 1 - intake capacity 
  !----------------------------------
  !*suckler Cow
   SUBROUTINE intake_capacity_cow( &
      npts, wanimalcow, MPwcow2,BCScow  , &
      nanimaltot, ICcow)

     INTEGER, INTENT(in)                               :: npts
     ! Number of spatial points (-)
     REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: wanimalcow
     ! Animal liveweight (kg/animal) (young:1, adult:2)
     REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: MPwcow2
     ! Daily potential milk production per animal for primiparous or multiparous cows (kg/animal/d)
     REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: BCScow
     ! Body score condition cow (young in first, and adult in second) (/5)
     REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: nanimaltot
     ! Stocking rate (animal m-2)
     REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: ICcow
     ! Cow intake capacity of primiparous or multiparous cows(kg/animal/d)

      INTEGER                                           :: i,j !for loop

      ICcow= 3.2+0.015*wanimalcow+0.25*MPwcow2-(0.002*wanimalcow*((BCScow-2.5)))
    DO j=2,nvm
      DO i=1,npts
        IF (nanimaltot(i,j) .EQ. 0.0) THEN
          ICcow(i,j,:)= REAL(0.0,r_std )      
        ENDIF         
      ENDDO  
    END DO
   ENDSUBROUTINE intake_capacity_cow

  ! Suckler Calf
  
   SUBROUTINE intake_capacity_calves(&
      npts,   wanimalcalf  ,&
      nanimaltot, tjulian, ICcalf)

     INTEGER, INTENT(in)                               :: npts
     ! Number of spatial points (-)
     REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)         :: wanimalcalf
     ! Calf liveweigth (kg/animal)
     REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)         :: nanimaltot
     ! Stocking rate (animal m-2)
     INTEGER(i_std ), INTENT(in)                          :: tjulian
     ! Julian day (-) 
     REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)        :: ICcalf
     ! Calf intake capacity  (kg/animal/d)

     INTEGER, DIMENSION(npts,nvm)                          :: dsevrage
     ! Julian day of the suckling calf period
      
      INTEGER                                           :: i,j !for loop

      dsevrage=tcalving+tsevrage
    DO j=2,nvm
      DO i=1,npts
      IF (tjulian.GT.dsevrage(i,j)) THEN
         ICcalf(i,j) = 0.0345*(wanimalcalf(i,j)**0.9)
      ELSE 
         IF (dsevrage(i,j).GT.365) THEN
            IF (tjulian.GT.dsevrage(i,j)-365.AND.tjulian.LT.tcalving(i,j)) THEN
               ICcalf(i,j)=0.0345*(wanimalcalf(i,j)**0.9)
            ELSE  
               ICcalf(i,j)= 0.0559*exp(5.28*(1-exp(-0.00703*wanimalcalf(i,j))))
            ENDIF
         ELSE
            ICcalf(i,j)= 0.0559*exp(5.28*(1-exp(-0.00703*wanimalcalf(i,j))))  
         ENDIF
      ENDIF    
      ENDDO
    END DO
      WHERE (nanimaltot.EQ.REAL(0.0,r_std ))
         ICcalf=REAL(0.0,r_std )
      ENDWHERE

   ENDSUBROUTINE intake_capacity_calves
    
  ! Dairy Cow
  SUBROUTINE intake_capacity_cow_d(&
    npts,npta,   &
    MPwcow2       ,&
    BCS, wanimalcow, nanimaltot, IC_animal,&
    AGE_animal, nWeekLact,nWeekGest)
    
    INTEGER, INTENT(in)                               :: npts
    ! Number of spatial points (-)
    INTEGER, INTENT(in)                               :: npta
    ! equal 2 when cow (Young and old) and 1 when calf
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: MPwcow2
    ! Daily potential milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: BCS
    ! Body Condition Score (for cow only /5)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: wanimalcow
    ! Animal liveweight (kg/animal) (young:1, adult:2)
    REAL(r_std ), DIMENSION(npts,nvm)     , INTENT(in)    :: nanimaltot
    ! Stocking rate (animal m-2)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)   :: IC_animal
    ! intake Capacity (Kg)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: AGE_animal
    ! Animal age in case of simulation of dairy cows (months)
    REAL(r_std ), DIMENSION(npts,nvm)     , INTENT(in)    :: nWeekLact
    ! Lactation week (in weeks from calving) 
    REAL(r_std ), DIMENSION(npts,nvm)     , INTENT(in)    :: nWeekGest
    ! Gestation week (in weeks from mating)
         
    REAl(r_std ),DIMENSION(npts,nvm,npta)                 :: IL
    ! Lactation Index
    REAL(r_std ),DIMENSION(npts,nvm)                      :: IG
    ! Gestation Index
    REAL(r_std ),DIMENSION(npts,nvm,npta)                 :: IM
    ! Maturity Index

    !Lactation Indice computation
    IL(:,:,1)=0.6+(0.4)*(1-exp(-0.16*NWeekLact))
    IL(:,:,2)=0.7+(0.3)*(1-exp(-0.16*NWeekLact))
    IG=0.8+0.2*(1-exp(-0.25*(40-NWeekGest)))
    IM=-0.1+1.1*(1-exp(-0.08*AGE_animal))

    Ic_animal(:,:,1)= (13.9+(0.015*(Wanimalcow(:,:,1)-600))+&
         (0.15*MPwcow2(:,:,1))+(1.5*(3-BCS(:,:,1))))*IL(:,:,1)*IG*IM(:,:,1)   
    Ic_animal(:,:,2)= (13.9+(0.015*(Wanimalcow(:,:,2)-600))+&
         (0.15*MPwcow2(:,:,2))+(1.5*(3-BCS(:,:,2))))*IL(:,:,2)*IG*IM(:,:,2)   
    
    !Ingestion allaitante - test 
    !Ic_animal(:,1)=3.2+0.015*Wanimalcow(:,1)+0.25*MPwcow2(:,1)-(0.002*wanimalcow(:,1)*((BCS(:,1)-2.5)))
    !Ic_animal(:,2)=3.2+0.015*Wanimalcow(:,2)+0.25*MPwcow2(:,2)-(0.002*wanimalcow(:,2)*((BCS(:,2)-2.5)))
    !print*, Ic_animal(:,1)
    !print*, Ic_animal(:,2)
    
    WHERE (nanimaltot .EQ. 0.0) 
       Ic_animal(:,:,1)=0.      
       Ic_animal(:,:,2)=0.      
    END WHERE         
  
    
  ENDSUBROUTINE intake_capacity_cow_d
  
  ! Heifer
  ! Equations from INRA feed tables 2007 p.75
  !------------------------------------------
  SUBROUTINE intake_capacity_heifer(&
             npts, type_animal,winit,wanimalcow,IC_animal)
    INTEGER, INTENT(in)                              :: npts
    ! Number of spatial points (-)
    INTEGER, INTENT(in)                              :: type_animal
    ! 1: Dairy cows, 2: Suckler cows, 3: Old module, 4: Dairy heifers, 5 : Suckler heifers
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)        :: winit
    ! Initial live weigth of heifer
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)        :: wanimalcow
    ! Animal liveweight (kg/animal) (young:1, adult:2) 
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)       :: IC_animal
    ! Heifer intake capacity
                
    ! variable local            
    REAL(r_std ), DIMENSION(npts,nvm)                    :: P1
    ! Parameter for IC calculation 
    REAL(r_std ), DIMENSION(npts,nvm)                    :: itype
    ! Parameter for IC calculation  
    
    itype=0.
    P1=0.
    
    WHERE(winit.LT.150)
        P1=0.2
    ELSEWHERE(winit.LT.300)
        P1=0.1
    ENDWHERE  
    
    IF(type_animal.EQ.1) THEN
        itype=0.039   ! Dairy heifers
    ELSE
        itype=0.03275 ! Suckler heifers
    ENDIF
    
   IC_animal=itype*(wanimalcow**0.9)+ P1
   !             
  ENDSUBROUTINE intake_capacity_heifer
  
  
  !----------------------------------
  ! 2 - intake
  !----------------------------------
  
  SUBROUTINE Grazing_intake_cow(&
     npts, type_animal, wshtot ,&
     tadmin,nanimaltot,DNDF    ,&
     NDF,IC                    ,&     
     DMIanimal                 ,&
     OMD, tadmoy, FVh, ntot    ,&
     tmoy_14, BM_threshold)

    ! declarations :
    
    INTEGER, INTENT(in)                          :: npts
    ! Number of spatial points (-)
    INTEGER, INTENT(in)                          :: type_animal
    ! 1 or 2 or 4 or 5= > new module animal
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)    :: wshtot
    ! Shoot structural dry matter (kg m-2)
    REAL(r_std ), DIMENSION(npts), INTENT(in)    :: tadmin
    ! Daily minimum temperature
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout) :: nanimaltot
    ! Stocking rate (animal m-2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)    :: DNDF
    ! fraction of digestible fibres in total fibres (-)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)    :: NDF
    ! fraction of fibres in the intake(-)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)    :: IC
    ! intake capacity (Kg)    
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)   :: DMIanimal
    ! Dry Matter intake of a cow/calf (Kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)   :: OMD
    ! Digestible organic matter in the intake(kg/kg)
    REAL(r_std ), DIMENSION(npts), INTENT(in)    :: tadmoy
    ! Daily average temperature (K)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)   :: FVh
    ! Herbage Fill Value (UE)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)    :: ntot
    ! nitrogen substrate concentration in plant,(kg n/kg)
    REAL(r_std ), DIMENSION(npts), INTENT(in)    :: tmoy_14
    ! 14 day running average of daily air temperature (K)
    REAL(r_std ),                  INTENT(in)    :: BM_threshold
    ! Biomass threshold above which animals are moved out the paddock (kg/m2)
    !implicit variables intent(in) :
    ! - AnimalqintakeM : intake parameter (m2 m-2)
  
    !Local variables
    INTEGER                                       :: i,j
    REAL(r_std ), DIMENSION(npts,nvm)                 :: NDFnd
    ! fraction of non digestible fibres in the intake(g/Kg)
    REAL(r_std ), DIMENSION(npts)                 :: temperature_effect_OMD
    ! temperature effect on organic matter digestibility (-)
    
  
    ! Fraction of non digestible fibres in the intake(g/Kg)
    !------------------------- 
        NDFnd=NDF*(1-DNDF)*1000
                
    ! Digestible organic matter in the intake (kg/kg)
    !------------------------- 
        OMD=(89.49-0.1102*NDFnd)/100
        
     !Temperature effect of herbage digestible organic matter
     !-------------------------     
        temperature_effect_OMD=min(0.1,max(-0.1,(tmoy_14-t_seuil_OMD)*0.00645))
      DO j=2,nvm
        OMD(:,j)=max(0.4,min(1.0, OMD(:,j) - temperature_effect_OMD))  
      END DO          
    ! Herbage fill value of the diet
    !------------------------- 
    IF (type_animal.EQ.2) THEN
        FVh=95/(-13.9+145*OMD) ! suckler cows
    ELSE
    ! Adapté de l'equation QIB des tables INRA 2007 p.177 
    ! sous hypothèse de prairies permanentes
    ! et d'un coefficient de MS de 20%
    ! MAT[g/kg]*6.25*1000=ntot[kgN/kg]
        FVh=95/(6.44+65.5*OMD+700.0*ntot+13.58)! suckler or dairy heifers
    END IF         
    
    ! Herbage dry matter intake without supplementation
    !-------------------------
   DO j=2,nvm
!     DO i=1,npts           
!JCMODIF new threshold
!         IF(((wshtot(i,j).GT.BM_threshold).OR.f_complementation.EQ.4).and.(nanimaltot(i,j).NE.0)) THEN      
      WHERE(((wshtot(:,j).GT.able_grazing(:,j)).OR.&
           f_complementation.EQ.4).and.(nanimaltot(:,j).NE.0))
!ENDJCMODIF
        !Dry Matter intake of a cow/calf
!JCMODIF
!           DMIanimal(:,j)=(IC(:,j)/FVh(:,j))*(1-exp(-0.0012*wshtot(i,j)*10000))             
           DMIanimal(:,j)=IC(:,j)
!ENDJCMODIF 
!            IF (f_temperature_DMI)THEN
!                WHERE ((tadmoy(:)>298.15).and.(tadmin(:)>295.15))
!                   DMIanimal(:,j)=DMIanimal(:,j)*(1-0.02*(tadmoy(:)-298.15))  
!                ENDWHERE
!            ENDIF                            
         ELSEWHERE               
            DMIanimal(:,j) = 0.0
            !06/02/2010 AIG & MG
            WHERE (nanimaltot(:,j).NE.0.and.f_autogestion.NE.2)                           
                nanimaltot(:,j) = 0.0        
!                print*, 'WARNING : unsufficient biomass -> cows have been moved out'
            ENDWHERE   
         ENDWHERE   
!     ENDDO
   END DO    
  ENDSUBROUTINE Grazing_intake_cow

  
  !dairy
  SUBROUTINE Grazing_intake_cow_d(&
     npts, npta                  ,&                               
     ntot,nanimaltot,DNDF        ,&
     NDF,IC,tadmin,tadmoy        ,&                         
     DMIanimal, OMD, wshtot, FVh ,&
     tmoy_14,BM_threshold)

    ! declarations :
    
    INTEGER, INTENT(in)                                 :: npts
    ! Number of spatial points (-)
    INTEGER, INTENT(in)                                 :: npta
    ! equal 2 for primi and multipare
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)           :: wshtot
    ! Shoot structural dry matter (kg m-2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)           :: ntot
    ! nitrogen substrate concentration in plant,(kg n/kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout)        :: nanimaltot
    ! Stocking rate (animal m-2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)           :: DNDF
    ! fraction of digestible fibres in total fibres (-)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)           :: NDF
    ! fraction of fibres in the intake(-)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)      :: IC
    ! intake capacity (Kg)    
    REAL(r_std ), DIMENSION(npts), INTENT(in)           :: tadmin
    ! Daily minimum temperature
    REAL(r_std ), DIMENSION(npts), INTENT(in)           :: tadmoy
    ! Daily average temperature
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)     :: DMIanimal
    ! Dry Matter intake of a cow/calf (Kg)

    REAL(r_std ), DIMENSION(npts,nvm)     , INTENT(out)     :: OMD
    ! Digestible organic matter in the intake(kg/kg)
    REAL(r_std ),DIMENSION(npts,nvm)      , INTENT(out)     :: FVh
    ! Herbage fill value (UE)
    REAL(r_std ), DIMENSION(npts), INTENT(in)           :: tmoy_14
    ! 14 day running average of daily air temperature (K)
    REAL(r_std ),                  INTENT(in)           :: BM_threshold
    ! Biomass threshold above which animals are moved out the paddock (kg/m2)
    !Local variables
    REAL(r_std ),DIMENSION(npts,nvm)                        :: NDFnd
    ! fraction of non digestible fibres in the intake(g/Kg)
    !06/02/2010 AIG & MG
    LOGICAL,DIMENSION(npts,nvm)                             :: Bool_movedout
    ! Bolean to decide to move out animal

    INTEGER                                             :: i,j
       
    REAL(r_std ),DIMENSION(npts)                        :: temperature_effect
    ! temperature effect on dry matter intake (-)
    REAL(r_std ),DIMENSION(npts)                        :: temperature_effect_OMD
    ! temperature effect on organic matter digestibility (-)
   
     
!     DO i=1,npts
         WHERE ((f_temperature_DMI.AND.tadmoy(:).GT.298.15).AND.(tadmin(:).GT.295.15))
            temperature_effect(:)= 1-0.02*(tadmoy(:)-298.15)
         ELSEWHERE
            temperature_effect(:)= 1.0
         ENDWHERE
!    END DO     
     
     !bool_movedout=0    
     ! Fraction of non digestible fibres in the intake(g/Kg)
     !-------------------------       
     NDFnd=NDF*(1-DNDF)*1000
        
     ! Herbage digestible organic matter (g/g)
     !-------------------------       
     OMD=(89.49-0.1102*NDFnd)/100
     
     !Temperature effect of herbage digestible organic matter
     !-------------------------     
     temperature_effect_OMD=min(0.1,max(-0.1,(tmoy_14-t_seuil_OMD)*0.00645))
      DO j=2,nvm
        OMD(:,j)=max(0.4,min(1.0, OMD(:,j) - temperature_effect_OMD))
      END DO
        
     ! Herbage fill value (UE)
     !-------------------------  
     ! Adapté de l'equation QIL des tables INRA 2007 p.177 
     ! sous hypothèse de prairies permanentes
     ! et d'un coefficient de MS de 20%
     ! MAT[g/kg]*6.25*1000=ntot[kgN/kg]

     FVh=140/(66.3+65.5*OMD+612.5*ntot+12.52)
             
     !06/02/2010 AIG & MG 
     bool_movedout=.FALSE.
           
    !Cow dry Matter intake    
    !------------------------- 
    !06/02/2010 AIG & MG
    
  DO j=2,nvm 
!JCMODIF new threshold
!     WHERE((nanimaltot(:,j).NE.0).AND.((wshtot(:,j).GT.BM_threshold).OR.(f_complementation.EQ.4)))
     WHERE((nanimaltot(:,j).NE.0).AND.&
          ((wshtot(:,j).GT.able_grazing(:,j)).OR.(f_complementation.EQ.4)))
!ENDJCMODIF 
     !WHERE(nanimaltot.NE.0)  
     ! On calcule l'ingestion avec la limitation de la disponibilité en herbe proposée par 
     ! Jouven et al 2008
!JCMODIF
!        DMIanimal(:,j,1)=(IC(:,j,1)/FVh(:,j))*(1-16.95*exp(-0.00275*wshtot(:,j)*10000))
!        DMIanimal(:,j,2)=(IC(:,j,2)/FVh(:,j))*(1-16.95*exp(-0.00275*wshtot(:,j)*10000))
         DMIanimal(:,j,1)=IC(:,j,1)
         DMIanimal(:,j,2)=IC(:,j,2)
!ENDJCMODIF        
     ! Temperature effect on DMI
     ! (Freer et al 1997)
     !-------------------------   
!        WHERE ((tadmoy>298.15).and.(tadmin>295.15))
!            DMIanimal(:,j,1)=DMIanimal(:,j,1)*temperature_effect
!            DMIanimal(:,j,2)=DMIanimal(:,j,2)*temperature_effect
!        ENDWHERE       
     ELSEWHERE   
        DMIanimal(:,j,1) = 0.0
        DMIanimal(:,j,2) = 0.0
        !06/02/2010 AIG & MG
        !nanimaltot     = 0.0
        bool_movedout(:,j)=.TRUE.  
     ENDWHERE   
    ENDDO
    IF(ANY(DMIanimal(:,:,:).LT.0)) THEN
           STOP "Herbage ingestion is negative"
    ENDIF
    
    !06/02/2010 AIG & MG
  DO j=2,nvm 
!    DO i=1,npts
        ! en autogestion on ne sort qu'en début de journée
        WHERE(bool_movedout(:,j) .AND. nanimaltot(:,j) .NE. 0.0 .AND. f_autogestion .NE. 2)
!           print*,'WARNING : unsufficient biomass -> cows have been moved out. Pixel '
           nanimaltot(:,j)=0.0
           bool_movedout(:,j)=.FALSE.
        ENDWHERE
!    ENDDO
  END DO
    
  ENDSUBROUTINE Grazing_intake_cow_d  
  
  SUBROUTINE grazing_intake_complementation(npts,dt                              ,&
                                            DMIcowanimal, FVh, ICcow, FVf        ,&
                                            MPcow2,MPwcow2,Forage_quantity_period,&
                                            QIc, NELherbage, EVf,nanimaltot      ,&
                                            DMIcowsum,DMIcowanimalsum            ,&
                                            DMIcow,DMIcowNsum,n,fn,pyoung        ,&
                                            type_animal,intake_tolerance         ,&
                                            Q_max_complement,forage_complementc  ,&
                                            NER,forage_complementn,NEI,NEM,NEIh  ,&
                                            NEIf,NEIC,NEG,f_complementation,DMIc ,&
                                            DMIf)
                                            
    INTEGER, INTENT(in)                               :: npts
    ! Number of spatial points (-)
    REAL(r_std ), INTENT(in)                          :: dt
    ! Time step (d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: DMIcowanimal
    ! Daily animal intake for primiparous or multiparous cows(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: FVh
    ! Herbage Fill Value (UE)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: ICcow
    ! Cow intake capacity of primiparous or multiparous cows(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: FVf
    ! forage fill value (Kg)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: MPcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: MPwcow2
    ! Daily potential milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(inout)    :: forage_quantity_period
    ! Daily forage quantity provided to herbivors during the current stocking period (Kg/Animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2)  , INTENT(inout)    :: QIc
    ! Daily concentrate quantity per kg of milk or per kg of lw
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: NELherbage
    ! Energetic content of the herbage (MJ/kg) 
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: EVf
    ! Energetic content of the forage (MJ/Kg)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: nanimaltot
    ! Stocking rate (animal/m²)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: DMIcowsum
    ! Cumulated intake per m2 for primiparous or multiparous cows(kg/m2)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: DMIcowanimalsum
    ! Cumulated animal intake for primiparous or multiparous cows(kg/animal)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: DMIcow
    ! Daily intake per m2 for primiparous or multiparous cows(kg/m2/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: DMIcowNsum
    ! N in daily intake per m2 for primiparous or multiparous cows(kgN/m2)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: n
    ! nitrogen substrate concentration in plant,(kg n/kg)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: fn
    ! nitrogen in structural dry matter
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: pyoung
    ! Fraction of young or primiparous in the cattle (-)
    INTEGER                        , INTENT(in)       :: type_animal
    ! kind of herbivores (1: dairy cows, 2 suckler cows+calf, 3 old module, 4 dairy heifers, 5 suckler heifers)
    REAL(r_std )                   , INTENT(in)       :: intake_tolerance
    ! intake tolerance threshold (-)
    REAL(r_std )                   , INTENT(in)       :: Q_max_complement
    ! Maximum quantity of forage or concentrate to supplement animals when auto-supplementation (kg)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: NER
    ! Net energy requirement (MJ)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(out)      :: forage_complementc
    ! fraction of carbon in Forage + concentrate (kgC/m²/d)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(out)      :: forage_complementn
    ! fraction of nitrogen in Forage + concentrate (kgC/m²/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: NEI
    ! Net energy intake(MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: NEM
    ! Net energy requirements for maintenance(MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: NEIh
    ! Net Energy intake from ingested herbage(MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: NEIf
    ! Net Energy intake from ingested forage(MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: NEIc
    ! Net Energy intake from ingested concentrate(MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: NEG
    ! Net energy required for gestation (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: DMIc
    ! Concentrate intake (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: DMIf
    ! forage intake (kg/animal/d)
  
  !local variables
    REAL(r_std ), DIMENSION(npts,nvm,2)                   :: Shf
    ! substitution rate of herbage by forage in the cow diet (-)
    REAL(r_std ), DIMENSION(npts,nvm,2)                   :: Shc1
    ! substitution rate of herbage by concentrate in the cow diet (-)
    REAL(r_std ), DIMENSION(npts,nvm,2)                   :: Shc2
    ! substitution rate of herbage by concentrate in the cow diet (-)
    REAL(r_std ), DIMENSION(npts,nvm,2)                   :: Shfc
    ! substitution rate of herbage by concentrate in the cow diet (-)
    REAL(r_std ), DIMENSION(npts,nvm,2)                   :: EDhf
    ! substitution rate of herbage by concentrate in the cow diet (-)
    REAL(r_std ), DIMENSION(npts,nvm,2)                   :: A
    ! intermediary variable
  
    REAL(r_std ), DIMENSION(npts,nvm)                     :: ICmoy
    ! Average intake capacity of the cattle [kg MS/animal/d]
    REAL(r_std ), DIMENSION(npts,nvm)                     :: DMImoy
    ! Average dry matter intake of the cattle [kg MS/animal/d] 
  
    REAL(r_std ), DIMENSION(npts,nvm,2)                   :: temp
    ! temporary variable
    INTEGER, INTENT(in)                               :: f_complementation
    ! Flag to activate cow supplementation
  
  INTEGER :: i=0
  INTEGER :: k=0  ! 1 : primipare/young, 2: multipare/mature
   INTEGER :: j 
  DMIc=0.0
  DMIf=0.0
  DO j=2,nvm
     IF(f_complementation.EQ.1.OR.f_complementation.EQ.3) THEN
        !supplementation with forage only or with forage and concentrate
   
     IF(f_complementation.EQ.3) THEN !supplementation with forage and concentrate
      DO i=1,npts
           DO k=1,2
           IF(nanimaltot(i,j).GT.0) THEN       
                 !DMIc(i,j)=QIc(i)*MPcow2(i,j)              
                 DMIc(i,j,k)=QIc(i,j,k)*MPwcow2(i,j,k)
                 EDhf(i,j,k)=(DMIcowanimal(i,j,k)*NELherbage(i,j)/7.12+&
                      Forage_quantity_period(i,j)*EVf(i,j))/(DMIcowanimal(i,j,k)*&
                      FVh(i,j)+Forage_quantity_period(i,j)*FVf(i,j))
                 A(i,j,k)=(0.0004*MPwcow2(i,j,k)**2)+(2.39*(EDhf(i,j,k))**2)-&
                      (0.0452*MPwcow2(i,j,k)*(EDhf(i,j,k)))         
                 Shfc(i,j,k)=0.11+(0.02*DMIc(i,j,k))-(1.13*(EDhf(i,j,k))**2)+&
                      A(i,j,k)*((DMIcowanimal(i,j,k)*FVh(i,j)+Forage_quantity_period(i,j)*&
                      FVf(i,j))/ICcow(i,j,k))
                 DMIcowanimal(i,j,k)=DMIcowanimal(i,j,k)-SHfc(i,j,k)*DMIc(i,j,k)
           ELSE
                 DMIcowanimal(i,j,k)=0.0
           ENDIF             
           ENDDO
      ENDDO
    ENDIF
     DO i=1,npts
       DO k=1,2
           IF(nanimaltot(i,j).GT.0) THEN
              Shf(i,j,k)=((DMIcowanimal(i,j,k)*FVh(i,j))/ICcow(i,j,k))*&
                   (2.2-1.2*(FVh(i,j)/FVf(i,j)))
              DMIcowanimal(i,j,k)=DMIcowanimal(i,j,k)-Shf(i,j,k)*&
                   Forage_quantity_period(i,j)
               DMIf(i,j,k)=Forage_quantity_period(i,j)
           ELSE
               DMIcowanimal(i,j,k)=0.0
           ENDIF
       ENDDO
     ENDDO 
      
    ELSEIF(f_complementation.EQ.2) THEN !supplementation with concentrate only     
          DO i=1,npts
          
            DO k=1,2
                IF(nanimaltot(i,j).GT.0) THEN
                     !DMIc(i,j)=QIc(i)*MPcow2(i,j)
                     DMIc(i,j,k)=QIc(i,j,k)*MPwcow2(i,j,k)
                     A(i,j,k)=(0.0004*MPwcow2(i,j,k)**2)+(2.39*(NELherbage(i,j)/&
                          (7.12*FVh(i,j)))**2)-(0.0452*MPwcow2(i,j,k)*(NELherbage(i,j)/(7.12*FVh(i,j))))
                     Shc1(i,j,k)=0.8+0.01*DMIc(i,j,k)
                     shc2(i,j,k)=0.11+(0.02*DMIc(i,j,k))-(1.13*(NELherbage(i,j)/&
                          (7.12*FVh(i,j)))**2)+A(i,j,k)*((DMIcowanimal(i,j,k)*FVh(i,j))/ICcow(i,j,k))
                     DMIcowanimal(i,j,k)=DMIcowanimal(i,j,k)-min(Shc1(i,j,k),Shc2(i,j,k))&
                          *DMIc(i,j,k)                                     
                ENDIF                       
            ENDDO
          ENDDO  

           
    ELSEIF(f_complementation.eq.4) THEN     !auto-supplementation   
    
            IF(type_animal.EQ.1) THEN     !dairy supplementation with concentrate
               CALL auto_complementation_dairy(npts,dmicowanimal,fvh,iccow,NER,nelherbage, evf,Q_max_complement,DMIc,MPcow2_prec,&
                                               MPwcow2,NEI,NEM,NEIh,NEIf,NEIc,NEG,nanimaltot)                                                        
                                               
            ELSEIF(type_animal.eq.2) THEN !suckler supplementation with forage
               CALL auto_complementation_suckler(npts,dmicowanimal,fvh,iccow,NER    ,&
                                                nelherbage,evf,fvf,Q_max_complement,&
                                                DMIf,nanimaltot,intake_tolerance)
                                                
               Forage_quantity_period(:,:)=DMIf(:,:,1)*pyoung+DMIf(:,:,2)*(1-pyoung)
            ENDIF        
    ENDIF   
  END DO
  WHERE(nanimaltot(:,:).EQ.0)
      DMIc(:,:,1)=0.0
      DMIc(:,:,2)=0.0
      DMIf(:,:,1)=0.0
      DMIf(:,:,2)=0.0
  ENDWHERE    
  
  ! AIG 04/03/2010 Le calcul de l'ingéré par m2 ne prend par en compte la proportion
  ! pyoung pour les génisses
  
  IF(type_animal.EQ.4.OR.type_animal.EQ.5) THEN
    DMIcow(:,:,1) = DMIcowanimal(:,:,1) * nanimaltot(:,:)
    DMIcow(:,:,2) = 0.0
    ICcow(:,:,2)  = 0.0
  ELSE
    DMIcow(:,:,1) = DMIcowanimal(:,:,1) * nanimaltot(:,:) *pyoung(:,:)
    DMIcow(:,:,2) = DMIcowanimal(:,:,2) * nanimaltot(:,:) *(1-pyoung(:,:))
  ENDIF
  DO j=2,nvm 
   CALL Euler_X(npts,2, dt, DMIcow(:,j,:), DMIcowsum(:,j,:))

   CALL Euler_X(npts,2, dt, DMIcowanimal(:,j,:), DMIcowanimalsum(:,j,:))

   temp(:,j,1)=DMIcow(:,j,1)*(n(:,j)+fn(:,j))
   temp(:,j,2)=DMIcow(:,j,2)*(n(:,j)+fn(:,j))
   
   CALL Euler_X(npts,2, dt, temp(:,j,:), DMIcowNsum(:,j,:))
 

   WHERE(nanimaltot(:,j).GT.0.AND.f_complementation.LT.4) 
      forage_complementc(:,j)=0.60*((forage_quantity_period(:,j)+&
           DMIc(:,j,1))*pyoung(:,j) + (forage_quantity_period(:,j)+DMIc(:,j,2))&
           *(1-pyoung(:,j)))*nanimaltot(:,j)
      forage_complementn(:,j)=((fN_forage(:,j)*forage_quantity_period(:,j)+&
           fN_concentrate(:,j)*DMIc(:,j,1))*pyoung(:,j)+ &
           (fN_forage(:,j)*forage_quantity_period(:,j)+&
           fN_concentrate(:,j)*DMIc(:,j,2))*(1-pyoung(:,j)))*nanimaltot(:,j)
   ELSEWHERE(nanimaltot(:,j).GT.0.AND.f_complementation.EQ.4)                    
      forage_complementc(:,j)=0.60*((DMIf(:,j,1)+DMIc(:,j,1))*pyoung(:,j) +&
           (DMIF(:,j,2)+DMIc(:,j,2))*(1-pyoung(:,j)))*nanimaltot(:,j)
      forage_complementn(:,j)=((fN_forage(:,j)*DMIf(:,j,1)+&
           fN_concentrate(:,j)*DMIc(:,j,1))*pyoung(:,j) +&
           (fN_forage(:,j)*DMIf(:,j,2)+fN_concentrate(:,j)*&
           DMIc(:,j,2))*(1-pyoung(:,j)))*nanimaltot(:,j)
   ELSEWHERE    
       forage_complementc(:,j)=0.0
       forage_complementn(:,j)=0.0
   ENDWHERE  

   
   CALL Euler_funct (npts,dt,forage_complementc(:,j),forage_complementcsum(:,j))
   CALL Euler_funct (npts,dt,forage_complementn(:,j),forage_complementnsum(:,j))
  ENDDO     
  ENDSUBROUTINE grazing_intake_complementation
  
  
  
  !Routine permettant de calculer la complémentation automatique des vaches laitières
  
  SUBROUTINE auto_complementation_dairy(npts,DMIcowanimal,FVh,ICcow,NER,NELherbage, EVc,&
                                       Q_max_complement,DMIc,MPcow2,MPwcow2,NEI,NEM,NEIh,&
                                       NEIf,NEIC,NEG,nanimaltot)
                                       
    INTEGER, INTENT(in)                               :: npts
    ! Number of spatial points (-)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: DMIcowanimal
    ! Daily animal intake for primiparous or multiparous cows(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: FVh
    ! Herbage Fill Value (UE)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: ICcow
    ! Cow intake capacity of primiparous or multiparous cows(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: NER
    ! Net energy requirement (MJ)  
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: NELherbage
    ! Energetic content of the herbage (MJ/kg) 
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: EVc
    ! Energetic value of the forage  (MJ/kg) 
    REAL(r_std )                   , INTENT(in)       :: Q_max_complement
    ! Maximum quantity of forage or concentrate to supplement animals when auto-supplementation (kg)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: DMIc
    ! Forage quantity calculated by the model (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: MPcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: MPwcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: NEI
    ! Net energy intake(MJ)
     REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: NEM
     ! Net energy requirements for maintenance (MJ)
     REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: NEIh
     ! Net Energy intake from ingested herbage(MJ)
     REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: NEIf
     ! Net Energy intake from ingested forage(MJ)
     REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: NEIc
     ! Net Energy intake from ingested concentrate(MJ)
     REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: NEG
     ! Net energy required for gestation
     REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: nanimaltot
     ! Stocking rate (animal/m²)

     
     !local variables
     REAL(r_std ), DIMENSION(npts,nvm,2)                   :: Shc1,shc2,shc
     ! Substitution rate of herbage by concentrate in the cow diet (-)
     REAL(r_std ), DIMENSION(npts,nvm,2)                   :: A
     ! Intermediary variable
     REAL(r_std ), DIMENSION(npts,nvm,2)                   :: MPpos_loc
     ! Possible milk production (local) (Kg/UGB)
     REAL(r_std ), DIMENSION(npts,nvm,2)                   :: Qic
     ! Quantité de concentré ingéré par Kg de lait
     REAL(r_std ), DIMENSION(npts,nvm)                     :: EDh
     ! Substitution rate of herbage by concentrate in the cow diet (-)
     REAL(r_std ), DIMENSION(npts,nvm)                     :: temp
     ! Intermediairy variable
     LOGICAL,      DIMENSION(npts,nvm,2)                   :: fin
     ! To stop the iterative algorithm
     REAL(r_std ), DIMENSION(npts,nvm)                     :: ICmoy
     ! Average intake capacity of the cattle [kg MS/animal/d]
     REAL(r_std ), DIMENSION(npts,nvm)                     :: DMImoy
     ! Average dry matter intake of the cattle [kg MS/animal/d] 
     INTEGER     , DIMENSION(npts,nvm)                     :: Loop_count
     ! Counter for loop               
     
     temp(:,:)=0.0
     Loop_count=0.0
     DMIc(:,:,1)=0.5
     DMIc(:,:,2)=0.5
     fin=.FALSE.     
     
     print*, "MG auto"     
 
     WHERE(nanimaltot(:,:).GT.0.0) ! Animals at pasture
        ICmoy(:,:)=(ICcow(:,:,1)+ICcow(:,:,2))/2
        DMImoy(:,:)=(DMIcowanimal(:,:,1)+DMIcowanimal(:,:,2))/2
        !On ne complemente pas au dessus du pourcentage de l'ingere potentiel defini en entree
        WHERE((DMImoy(:,:)/ICmoy(:,:))*FVh(:,:)>intake_tolerance)
            DMIc(:,:,1)=0.0
            DMIc(:,:,2)=0.0
            fin(:,:,1)=.TRUE.
            fin(:,:,2)=.TRUE.
        ENDWHERE
     
     ELSEWHERE                  ! Animals at barn
        DMIc(:,:,1)=0.0
        DMIc(:,:,2)=0.0
        fin(:,:,1)=.TRUE.
        fin(:,:,2)=.TRUE.
     ENDWHERE
     
        
     
     !DO WHILE(NOT(ALL(fin)))  
         Loop_count=Loop_count+1  
         EDh(:,:)=NELherbage(:,:)/(7.12*FVh(:,:))
         A(:,:,1)=(0.0004*MPcow2(:,:,1)**2)+(2.39*EDh(:,:)**2)-&
              (0.0452*MPwcow2(:,:,1)*EDh(:,:))
         A(:,:,2)=(0.0004*MPcow2(:,:,2)**2)+(2.39*EDh(:,:)**2)-&
              (0.0452*MPwcow2(:,:,2)*EDh(:,:))
         shc1(:,:,1)=0.8+0.01*DMIc(:,:,1)
         shc1(:,:,2)=0.8+0.01*DMIc(:,:,2)
         shc2(:,:,1)=0.11+(0.02*DMIc(:,:,1))-(1.13*EDh(:,:)**2)+&
              A(:,:,1)*(DMIcowanimal(:,:,1)*FVh/Iccow(:,:,1))
         shc2(:,:,2)=0.11+(0.02*DMIc(:,:,2))-(1.13*EDh(:,:)**2)+&
              A(:,:,2)*(DMIcowanimal(:,:,2)*FVh/Iccow(:,:,2))
         
         shc(:,:,1)=min(shc1(:,:,1),shc2(:,:,1))
         shc(:,:,2)=min(shc1(:,:,2),shc2(:,:,2))
         
         WHERE(.NOT.(fin(:,:,1)))
            DMIc(:,:,1)=(NER(:,:,1)-DMIcowanimal(:,:,1)*NELherbage(:,:))/&
                 (7.12*EVc(:,:)-shc(:,:,1)*NELherbage(:,:))
         ENDWHERE
         
         WHERE(.NOT.(fin(:,:,2)))
            DMIc(:,:,2)=(NER(:,:,2)-DMIcowanimal(:,:,2)*NELherbage(:,:))/&
                 (7.12*EVc(:,:)-shc(:,:,2)*NELherbage(:,:))
         ENDWHERE
         
         WHERE(((NER(:,:,1)-DMIcowanimal(:,:,1)*NELherbage(:,:)).LT.0.0).OR.&
              ((7.12*EVc(:,:)-shc(:,:,1)*NELherbage(:,:)).LT.0.0))
            DMIc(:,:,1)=0.0
         ENDWHERE
         
         WHERE(((NER(:,:,2)-DMIcowanimal(:,:,2)*NELherbage(:,:)).LT.0.0).OR.&
              ((7.12*EVc(:,:)-shc(:,:,2)*NELherbage(:,:)).LT.0.0))
            DMIc(:,:,2)=0.0
         ENDWHERE        
         
         WHERE(DMIc.GE.Q_max_complement)
               fin=.TRUE.  
               DMIc=Q_max_complement   
         ENDWHERE
         !Faut-il considerer ici la production de lait reelle
         Qic(:,:,1)=DMIc(:,:,1)/MPcow2(:,:,1)
         Qic(:,:,2)=DMIc(:,:,2)/MPcow2(:,:,2)
         
         CALL calcul_NEI_cow_d(npts,2,MPcow2,DMIcowanimal,NELherbage,&
                                      temp,temp,&
                                      EVc,Qic,NEI,NEM,NEIh,NEIf,NEIc)
                                      
         MPpos_loc(:,:,1)=(NEI(:,:,1)-NEM(:,:,1)-NEG(:,:,1))/(0.44*7.12)
         MPpos_loc(:,:,2)=(NEI(:,:,2)-NEM(:,:,2)-NEG(:,:,2))/(0.44*7.12)
         
         ! AIG 04/07/2010
         ! On arrete de complémenter les VL quand la PL possible devient supérieure à la PL potentielle
         !WHERE(MPwcow2.LE.MPcow2)
            !fin=.TRUE.
         !ENDWHERE
         ! Je corrige:
         WHERE(MPpos_loc(:,:,1).GE.MPwcow2(:,:,1))
            fin(:,:,1)=.TRUE.
         ENDWHERE 
         
         WHERE(MPpos_loc(:,:,2).GE.MPwcow2(:,:,2))
            fin(:,:,2)=.TRUE.
         ENDWHERE  
         
         WHERE(Loop_count.GT.100)
             fin(:,:,1)=.TRUE.
             fin(:,:,2)=.TRUE.
         ENDWHERE                                                   
     !ENDDO
                   
    ! AIG 28/07/2010
    ! Sauf erreur de ma part, il faut recalculer la quantite d'herbe (en sortie de la subroutine) 
    ! en lui soustrayant le concentre qui lui est substitue soit:
          
     DMIcowanimal(:,:,1)=DMIcowanimal(:,:,1)-shc(:,:,1)*DMIc(:,:,1)
     DMIcowanimal(:,:,2)=DMIcowanimal(:,:,2)-shc(:,:,2)*DMIc(:,:,2)  
          
          
  ENDSUBROUTINE auto_complementation_dairy
  
  !Routine permettant de calculer la complémentation automatique des vaches allaitantes
  
  SUBROUTINE  auto_complementation_suckler(npts,DMIcowanimal,FVh,ICcow,NER,NELherbage, &
                                           EVf,FVf,Q_max_complement,DMIf,nanimaltot,intake_tolerance)
                                           
    INTEGER, INTENT(in)                               :: npts
    ! Number of spatial points (-)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)    :: DMIcowanimal
    ! Daily animal intake for primiparous or multiparous cows(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: FVh
    ! Herbage Fill Value (UE)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: ICcow
    ! Cow intake capacity of primiparous or multiparous cows(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: NER
    ! Net energy requirement (MJ)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: NELherbage
    ! Energetic content of the herbage (MJ/kg) 
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: EVf
    ! Energetic value of the forage  (MJ/kg) 
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: FVf
    ! Forage vill value  (UE) 
    REAL(r_std )                   , INTENT(in)       :: Q_max_complement
    ! Maximum quantity of forage or concentrate to supplement animals when auto-supplementation (kg)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)      :: DMIf
    ! Forage quantity calculated by the model (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)       :: nanimaltot
    ! Stocking rate (animal/m²)
    REAL(r_std )                   , INTENT(in)       :: intake_tolerance
    ! intake tolerance threshold (-)

     !local variables
    REAL(r_std ), DIMENSION(npts,nvm)                     :: Shf
    ! Substitution rate of herbage by forage in the cow diet (-)
    REAL(r_std ), DIMENSION(npts,nvm)                     :: ICmoy
    ! Average intake capacity of the cattle [Kg MS/UGB]
    REAL(r_std ), DIMENSION(npts,nvm)                     :: DMImoy
    ! Average dry matter intake of tje cattle [Kg MS/UGB] 
  

     WHERE(nanimaltot(:,:).GT.0.0)
        ICmoy(:,:)=(ICcow(:,:,1)+ICcow(:,:,2))/2
        DMImoy(:,:)=(DMIcowanimal(:,:,1)+DMIcowanimal(:,:,2))/2
         
        ! Substitution rate of herbage by forage
        !---------------------------------------   
        ! As DMI/IC ratio are the same beetwen young and mature cow, Shf should be calculated once 
        Shf(:,:)= ((DMIcowanimal(:,:,1)*FVh(:,:))/ICcow(:,:,1))*&
             (2.2-1.2*FVh(:,:)/FVf(:,:))
        
        DMIf(:,:,1)=(NER(:,:,1)-DMIcowanimal(:,:,1)*NELherbage(:,:))/&
             (7.12*EVf(:,:)-SHf(:,:)*NELherbage(:,:))      
          
        DMIf(:,:,2)=(NER(:,:,2)-DMIcowanimal(:,:,2)*NELherbage(:,:))/&
             (7.12*EVf(:,:)-SHf(:,:)*NELherbage(:,:))                  
         
       ! On ne complemente pas les animaux si l'herbe suffit a couvrir les besoins energetiques
         WHERE(DMIf(:,:,1).LT.0.0) 
               DMIf(:,:,1)=0.0
         ENDWHERE   
         
         WHERE(DMIf(:,:,2).LT.0.0) 
               DMIf(:,:,2)=0.0
         ENDWHERE 
         
         !On verifie qu'on ne depasse pas la capacite d'ingestion des animaux
         WHERE (((DMIcowanimal(:,:,1)-Shf(:,:)*DMIf(:,:,1))*FVh(:,:)+&
              DMIf(:,:,1)*FVf(:,:)).gt.ICcow(:,:,1))
            DMIf(:,:,1)=(iccow(:,:,1)-(DMIcowanimal(:,:,1)-&
                 Shf(:,:)*DMIf(:,:,1))*FVh(:,:))/FVf(:,:)    
         ENDWHERE 
         
         WHERE (((DMIcowanimal(:,:,2)-Shf(:,:)*DMIf(:,:,2))*FVh(:,:)+&
              DMIf(:,:,2)*FVf(:,:)).gt.ICcow(:,:,2))    
            DMIf(:,:,2)=(iccow(:,:,2)-(DMIcowanimal(:,:,2)-&
                 Shf(:,:)*DMIf(:,:,2))*FVh(:,:))/FVf(:,:)
         ENDWHERE
         
         !On borne la quantité apportée au maximum defini en entree     
         WHERE(DMIf(:,:,1).GT.Q_max_complement)
               DMIf(:,:,1)=Q_max_complement
         ENDWHERE
         
         WHERE(DMIf(:,:,2).GT.Q_max_complement)
               DMIf(:,:,2)=Q_max_complement
         ENDWHERE 
         
         !On ne complemente pas au dessus du pourcentage de l'ingere potentiel defini en entree
         WHERE(((DMImoy(:,:)/ICmoy(:,:))*FVh(:,:)).GT.intake_tolerance)
               DMIf(:,:,1)=0.0
               DMIf(:,:,2)=0.0
         ENDWHERE
         
     ELSEWHERE
         DMIf(:,:,1)=0.0
         DMIf(:,:,2)=0.0          
     ENDWHERE     
     
     !Actual herbage ingestion
     DMIcowanimal(:,:,1)=DMIcowanimal(:,:,1)-Shf(:,:)*DMIf(:,:,1)
     DMIcowanimal(:,:,2)=DMIcowanimal(:,:,2)-Shf(:,:)*DMIf(:,:,2)
               
  ENDSUBROUTINE
  
  !----------------------------------------------
  ! 3 - Milk_production
  !----------------------------------------------
  ! the milk production is based on Wood equation
  !----------------------------------------------
  SUBROUTINE Milk_Animal_cow(         &
     npts, dt                        ,&
     nanimaltot,tjulian,NEBcow       ,&
     MPcow2,MPcow,MPwcow2            ,&
     MPcowC, MPcowN                  ,&
     MPcowCsum, MPcowNsum, milkanimalsum,milkKG)
     
     
    INTEGER, INTENT(in)                            :: npts
    ! Number of spatial points (-)
    REAL(r_std ), INTENT(in)                       :: dt
    ! Time step (d)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)    :: nanimaltot
    ! Stocking density (animal m-2)
    INTEGER(i_std ),                    INTENT(in)    :: tjulian
    ! Julian day (d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)    :: NEBcow
    ! Net energy Balance (young :1 , adult:2) (MJ) 
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)   :: MPcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)   :: MPcow
    ! Daily milk production per m2 for primiparous or multiparous cows (kg/m2/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)   :: MPwcow2
    ! Daily potential milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)   :: MPcowC
    ! C in daily milk production per m2 for primiparous or multiparous cows (kgC/m2/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)   :: MPcowN
    ! N in daily milk production per m2 for primiparous or multiparous cows (kgN/m2/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)   :: MPcowCsum
    ! Cumulated C in milk production per m2 for primiparous or multiparous cows (kgC/m2)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)   :: MPcowNsum
    ! Cumulated N in milk production per m2 for primiparous or multiparous cows (kgN/m2)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(inout) :: milkanimalsum
    ! Milk product per animal per years (L.(animal.years)-1)    
    REAL(r_std ), DIMENSION(npts,nvm)                  :: milkKG
    ! Daily actual milk production per animal for the whole cattle (kg/animal/d)

    !20/03/2009 AIG & MG
    REAL(r_std ), DIMENSION(npts,nvm)                  :: nWeeklact
    ! Lactation week (in weeks from calving)
    REAL(r_std ), DIMENSION(npts,nvm,2)                :: MPwcow2max
    ! Daily potential milk production per animal for primiparous or multiparous cows at peak of lactation(kg/animal/d)      
    REAL(r_std ), DIMENSION(npts,nvm)                  :: milkanimal_write        
    REAL(r_std ), DIMENSION(npts,nvm)                  :: dsevrage
    ! Julian day of the suckling calf period

    INTEGER                                        :: i,j
    ! for loop


    MPwcow2max=MPwmax
    DO j=2,nvm
      DO i=1,npts
        ! Week of lactation for cows
            IF(tjulian .GE. tcalving(i,j)) THEN
                nWeeklact(i,j) = CEILING((tjulian-REAL(tcalving(i,j))+1)/7)
            ELSE    
            ! si tjulian est inférieur à tcalving on considere le velage de l'annee precedente   
                nWeeklact(i,j) = CEILING((tjulian-(REAL(tcalving(i,j))-365)+1)/7)             
            END IF        
       
            
            dsevrage(i,j)=tcalving(i,j)+tsevrage(i,j)
            IF (dsevrage(i,j) > 365) THEN
               dsevrage(i,j)=dsevrage(i,j)-365
            ENDIF   
       
            IF (dsevrage(i,j).LT.tcalving(i,j)) THEN                
            ! Maximum potential of lactation of a cow
               IF ((nWeeklact(i,j) .LE.43).AND.((tjulian.LT.dsevrage(i,j)).OR.&
                    (tjulian.GT.tcalving(i,j)))) THEN       
                  MPwcow2(i,j,1) = MPwcow2max(i,j,1) * &
                       ( 0.885 * nWeeklact(i,j)**(0.2) * EXP((-0.04) * nWeeklact(i,j)) )
                  MPwcow2(i,j,2) = MPwcow2max(i,j,2) *&
                       ( 0.885 * nWeeklact(i,j)**(0.2) * EXP((-0.04) * nWeeklact(i,j)) )
                ELSE
                    MPcow2(i,j,1) = 0.0    
                    MPcow2(i,j,2) = 0.0
                ENDIF    
            ELSE
                IF ((nWeeklact(i,j).LE.43).AND.((tjulian.GT.tcalving(i,j)).AND.(tjulian.LT.dsevrage(i,j)))) THEN        
                   MPwcow2(i,j,1) = MPwcow2max(i,j,1) * &
                        ( 0.885 * nWeeklact(i,j)**(0.2) * EXP((-0.04) * nWeeklact(i,j)) )
                   MPwcow2(i,j,2) = MPwcow2max(i,j,2) * &
                        ( 0.885 * nWeeklact(i,j)**(0.2) * EXP((-0.04) * nWeeklact(i,j)) )                
                ELSE
                    MPwcow2(i,j,1) = 0.0   
                    MPwcow2(i,j,2) = 0.0   
                ENDIF    
            END IF       

            ! Milk Production of a cow (kg milk/animal/d)      
            ! Après les 3 premiers mois de lactation la production laitière tient compte du bilan énergétique net NEB       
            IF (nWeeklact(i,j) .LE. 12) THEN       
                 MPcow2(i,j,1) = MPwcow2(i,j,1)        
                 MPcow2(i,j,2) = MPwcow2(i,j,2)        
            ELSE        
                MPcow2(i,j,1) = MPwcow2(i,j,1) * ( 1 + 0.01 * NEBcow(i,j,1) )     
                MPcow2(i,j,2) = MPwcow2(i,j,2) * ( 1 + 0.01 * NEBcow(i,j,2) )        
            END IF   
        ENDDO
      ENDDO      
            
        
        milkKG=MPcow2(:,:,1)*pyoung(:,:)+MPcow2(:,:,2)*(1-pyoung(:,:))

        if(ANY(milkKG(:,:).GT.50).OR. ANY(milkKG(:,:).LT.-50)) THEN
           print*, "bug"
        endif   
        
        WHERE (nanimaltot.EQ.0)
            milkKG=0
            MPcow2(:,:,1)=0
            MPcow2(:,:,2)=0
        ENDWHERE    
         
        ! Milk production for all cows (kg milk/d)
        MPcow(:,:,1) = nanimaltot * MPcow2(:,:,1) * pyoung
        MPcow(:,:,2) = nanimaltot * MPcow2(:,:,2) * (1-pyoung)
        
       
        ! Carbon in milk produced by cows (kg milk/d)    
        MPcowC = 0.0588 * MPcow
        
        ! Nitrogen in milk produced by cows (kg milk/d)     
        MPcowN = 0.00517 * MPcow
      DO j=2,nvm     
        CALL Euler_X(npts,2, dt, MPcow(:,j,:) ,   MPcowsum(:,j,:))
        CALL Euler_X(npts,2, dt, MPcowC(:,j,:),   MPcowCsum(:,j,:))
        CALL Euler_X(npts,2, dt, MPcowN(:,j,:),   MPcowNsum(:,j,:))
        CALL Euler_X(npts,2, dt, MPcow2(:,j,:), MPcow2sum(:,j,:))   

        
        milkanimal_write(:,j)=MilkKG(:,j)

        
        CALL Euler_funct (npts, dt, milkanimal_write(:,j), milkanimalsum(:,j))
      ENDDO
     
  ENDSUBROUTINE Milk_animal_cow
  
  
  
  
  !----------------------------------------------
  ! 4 - Balance energy Cow
  !----------------------------------------------
  ! the energy balance for the cow to compute weight
  ! gain or loss, and body condition score gain or loss
  !----------------------------------------------
  
  SUBROUTINE balance_energy_cow(npts,dt,&
      DMIcowanimal,MPcow2,&
      Agecow, BCS,tjulian,wanimalcow,nanimaltot   ,&
      NEB, NELherbage, EVf, Forage_quantity_period, &
      EVc, Qic, NEI, NEIh, NEIf, NEIc,&
      NEPgest, NEPlact, NEP, NEM, NER)
       
    INTEGER, INTENT(in)                         :: npts
    ! Number of spatial points (-)
    REAL(r_std ), INTENT(in)                    :: dt
    ! Time step (d)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: DMIcowanimal
    ! Daily animal intake for primiparous or multiparous cows(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: MPcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    INTEGER,                       INTENT(in)   :: Agecow
    ! 0:young, 1:adult
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout):: BCS
    ! Body Condition Score (for cow only /5)
    INTEGER(i_std ), INTENT(in)                    :: tjulian
    ! Julian day (-) 
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout):: wanimalcow
    ! Animal liveweight (kg/animal) (young:1, adult:2)  
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: nanimaltot
    ! Stocking rate (animal m-2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEB
    ! Net energy balance(MJ)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: NELherbage
    ! Energetic content of the herbage (MJ/kg) 
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: EVf
    ! Energy of the forage based (MJ/Kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: Forage_quantity_period
    ! Forage quantity  (MJ/Kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: EVc
    ! Energy of the concentrate (MJ/Kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: Qic
    ! Concentrate quantity per kg of milk or per kg of LW (MJ/Kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEI
    ! Net energy intake from ingested herbage(MJ)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEIh
    ! Net energy intake from ingested herbage(MJ)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEIf
    ! Net energy intake from ingested forage(MJ)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEIc
    ! Net energy intake from ingested concentrate(MJ)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEPgest
    ! Net energy for gestation (suckler cows)(MJ)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEPlact
    ! Net energy for milk production(MJ)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEP
    ! Net energy for production (MJ)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEM
    ! Net energy for maintenance (MJ)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NER
    ! Total net energy requirements (maintenance and production)(MJ)


  
  !Local variable
    REAL(r_std ), DIMENSION(npts,nvm)               :: NEBcow_calc
    ! tempory variable to Gain or Loss computation


    INTEGER                                     :: jourdepuisvelage
    ! Calving date (-)
    INTEGER                                     :: i,j
    ! for loop
    REAL(r_std )                                :: alpha
    !parametre for NEM computation
    REAL(r_std )                                :: beta = 0.2
    !parametre for NEM computation
    REAL(r_std )                                :: gamma
    !parametre for NEM computation
    REAL(r_std )                                :: delta
    !parametre for NEM computation

  !Certain calcul (notemment les paramétrage de variation du poids et de la BCS) 
  !Dependent du signe de NEB, on est obligé de faire le calcul de façon sclaire 
  !pour chaque valeur des vecteurs ce qui explique le DO... END DO.
    DO j=2,nvm  
      DO i=1,npts
      IF (nanimaltot(i,j).ne.0) THEN
        !NEI compute (Net Energy intake)   
         NEIh(i,j)= DMIcowanimal(i,j)* NELherbage(i,j)
         NEIf(i,j)= Forage_quantity_period(i,j)*7.12*EVf(i,j)
         NEIc(i,j)= Qic(i,j)* MPcow2(i,j)* 7.12*EVc(i,j)   
         NEI(i,j)= NEIh(i,j)+ NEIf(i,j) + NEIc(i,j)
      
        !NEP compute (net energy production (gestation and milk production)  
        !NEPlact(i)=3.20*MPcow2(i)
        NEPlact(i,j)=0.44*7.12*MPcow2(i,j)
        
        jourdepuisvelage=tjulian-tcalving(i,j)
        
        IF (jourdepuisvelage .lt. 0) THEN 
            jourdepuisvelage=365+jourdepuisvelage
        ENDIF   
      
        
        WHERE (gestation.eq.0) 
          NEPgest=0
          
        ELSEWHERE      
          !NEPgest=26.3*exp(-0.0184*(365-jourdepuisvelage))
          NEPgest=3.70*7.12*exp(-0.0184*(365-jourdepuisvelage))
        ENDwhere 
        
        NEP(i,j)=NEPlact(i,j)+NEPgest(i,j)
      
        !NEM compute()  
        
      
        IF (MPcow2(i,j).eq.0) THEN
            !alpha=0.263
            alpha=0.037*7.12
        ELSE
            !alpha=0.291   
            alpha=0.041*7.12
        ENDIF  
       
        
        
        
        !NEM(i)=((alpha+0.099*(BCS(i)-2.5))*wanimalcow(i)**(0.75)*(1+beta))
        NEM(i,j)=((alpha+0.014*7.12*(BCS(i,j)-2.5))*wanimalcow(i,j)**(0.75)*(1+beta))

      
        NEB(i,j)=NEI(i,j)-(NEM(i,j)+NEP(i,j))
        
        NER(i,j)= NEM(i,j)+NEP(i,j)
        
      
        
        !coefficient de reduction des gain et note d'etat
               
        !Determination parameters according to the age of the cow (young or adult)
        ! agecow = 0 for young cows and 1 for mature cows
        IF (agecow.eq.1) THEN
            gamma=0.032
            delta=0.0007
        ELSE
            gamma=0.044
            delta=0.0002
        EndIf               
        
              
        If(NEB(i,j).ge.0) THEN 
            NEBcow_calc(i,j)=NEB(i,j)*gamma
        ELSE
            NEBcow_calc(i,j)=(NEB(i,j)*gamma/0.8)
        ENDIF
        ! Gain or Loss weigth accroding to NEB
        CALL Euler_funct (1, dt, NEBcow_calc(i,j), wanimalcow(i,j))
        
        !wanimalcow between [300..1000]
        IF (wanimalcow(i,j)<300) THEN
           wanimalcow(i,j)=300
        ENDIF
        
        IF (wanimalcow(i,j) > 1000) THEN  
           wanimalcow(i,j)=1000
        ENDIF
        
                
                
        If(NEB(i,j).ge.0) THEN 
            NEBcow_calc(i,j)=NEB(i,j)*delta
        ELSE
            NEBcow_calc(i,j)=(NEB(i,j)*delta/0.8)
        ENDIF

        ! Gain or Loss body score condition acording to NEB
        CALL Euler_funct (1, dt, NEBcow_calc(i,j), BCS(i,j))
        
        !BCS beetween [0..5]
        IF (BCS(i,j) < 0) THEN
        BCS(i,j)=0
        ENDIF
        
        IF (BCS(i,j)>5) THEN
        BCS(i,j)=5
        ENDIF
        

      ENDIF 
    END DO      
  END DO 
    WHERE (nanimaltot.EQ.0)
        BCS=0
        Wanimalcow=0
    ENDWHERE
  ENDSUBROUTINE balance_energy_cow  
  
  
  SUBROUTINE balance_energy_calf(npts,dt ,&
        DMIcowcalf,MPcow2,nanimaltot  ,&
        wanimalcalf, NELherbage,NEIherbage ,&
        NEImilk, NEI, NEM, NEG)
        
    INTEGER, INTENT(in)                         :: npts
    ! Number of spatial points (-)
    REAL(r_std ), INTENT(in)                    :: dt
    ! Time step (d)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: DMIcowcalf
    ! Calf dry matter intake (Kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: MPcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: nanimaltot
    ! Stocking density (animal m-2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout):: wanimalcalf
    ! Calf liveweigth (kg/animal)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: NELherbage
    ! Energetic content of the herbage (MJ/kg) 
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEIherbage
    ! Net energy intake from ingested herbage (MJ/Kg)  
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEImilk
    ! Net Erengy of ngested milk(MJ/Kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEI
    ! Net energy of global intake(MJ/Kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEM
    ! Net energy  metabolic(MJ/Kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NEG
    ! Net energy growth(MJ/Kg)
  
  !Local variable
    REAL(r_std )                                :: beta=0.2
    ! Parameter for NEM computation
    REAL(r_std ), DIMENSION(npts,nvm)               :: NEG_calc
    ! For compute gain weigth
    INTEGER                                     :: i,j
    ! for loop
  
  
  
  !Calcul de NEIforage 
  NEIherbage=DMIcowcalf*NELherbage
  
  !Calcul de NEImilk 
  !NEImilk=2.27*MPcow2
  NEImilk=0.32*7.12*MPcow2
  
  !calcul de NEI : Net Energy Ingested
  NEI=NEIherbage+NEImilk   

  !NEM computation
  !NEM=0.291*wanimalcalf**(0.75)*(1+beta)
  NEM=0.041*7.12*wanimalcalf**(0.75)*(1+beta)
  
  !Net energy for calf growth
  NEG=NEI-NEM
  
  !Only gain, not loss weigth
  DO j=2,nvm
    DO i=1,npts
      IF (NEG(i,j) .le. 0.0) THEN
        NEG(i,j)=0.0
      ENDIF   
    ENDDO
  ENDDO
  ! On met la NEG à 0 quand le poids du veau est nul pour eviter la division par zero
  
  WHERE (nanimaltot.NE.0.0.AND.calf.NE.0.AND.wanimalcalf.NE.0.0)
  
    !NEG_calc=(NEG/(0.309*((wanimalcalf)**0.75)))**(1/1.4)
    NEG_calc=(NEG/(0.0435*7.12*((wanimalcalf)**0.75)))**(1/1.4)
  
  ELSEWHERE
    NEG_calc=0
    NEM=0
    NEI=0
    NEImilk=0
    NEIherbage=0
    NEG=0
    wanimalcalf=0.0
  ENDWHERE

  DO j=2,nvm
  
    !Gain calf weight according to NEG
    CALL Euler_funct(npts, dt, NEG_calc(:,j), wanimalcalf(:,j))     
  ENDDO            
  ENDSUBROUTINE balance_energy_calf
  
  SUBROUTINE balance_energy_cow_d(npts,npta,dt,&
      MPcow2,MPwcow2,MPpos,&
      BCS,BCScow_prev, AGE_animal,&
      wanimalcow,nanimaltot)
       
    INTEGER, INTENT(in)                               :: npts
    ! Number of spatial points (-)
    INTEGER, INTENT(in)                               :: npta
    ! 1 : primiparous cows 2 : multiparous cows
    REAL(r_std ), INTENT(in)                          :: dt
    ! Time step (d)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: MPcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: MPpos
    ! Possible milk production of dairy cows according to the diet (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: MPwcow2
    ! Daily potential milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(inout) :: BCS
    ! Body Condition Score (for cow only /5)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(inout) :: BCScow_prev
    ! Body Condition Score at previsou time step (for cow only /5)
    REAL(r_std ), DIMENSION(npts,nvm,npta),INTENT(in)     :: AGE_animal
    ! Animal age in case of simulation of dairy cows (months)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(inout) :: wanimalcow
    ! Animal liveweight (kg/animal) (young:1, adult:2)  
    REAL(r_std ), DIMENSION(npts,nvm)     , INTENT(in)    :: nanimaltot
    ! Stocking density (animal/m2)

    
  !Local variable
    REAL(r_std ), DIMENSION(npts,nvm,npta)               :: NEBcow_W
    ! Daily variation of cow liveweight (kg/d)
    REAL(r_std ), DIMENSION(npts,nvm,npta)               :: NEBcow_BCS
    ! Daily variation of cow body condition score (/d)


        !----------------------- 
        ! Net Energy available for liveweight and BCS 
        !-----------------------

      WHERE(nanimaltot.NE.0) ! Animals are at pasture
      ! Primiparous cows   
         WHERE((MPwcow2(:,:,1)-MPpos(:,:,1)).LT.0)
            ! Liveweight and body condition increase
             NEBcow_BCS(:,:,1)=(0.44/180)*(MPpos(:,:,1)-MPcow2(:,:,1))
             NEBcow_W(:,:,1)=(0.44/3.5)*(MPpos(:,:,1)-MPcow2(:,:,1))
          ELSEWHERE
             ! Liveweight and body condition decrease
             NEBcow_BCS(:,:,1)=(0.44/240)*(MPpos(:,:,1)-MPcow2(:,:,1))
             NEBcow_W(:,:,1)=(0.44/4.5)*(MPpos(:,:,1)-MPcow2(:,:,1))                               
         ENDWHERE
       ! Multiparous cows  
         WHERE((MPwcow2(:,:,2)-MPpos(:,:,2)).LT.0)
            ! Liveweight and body condition increase
             NEBcow_BCS(:,:,2)=(0.44/180)*(MPpos(:,:,2)-MPcow2(:,:,2))
             NEBcow_W(:,:,2)=(0.44/3.5)*(MPpos(:,:,2)-MPcow2(:,:,2))
          ELSEWHERE
             ! Liveweight and body condition decrease
             NEBcow_BCS(:,:,2)=(0.44/240)*(MPpos(:,:,2)-MPcow2(:,:,2))
             NEBcow_W(:,:,2)=(0.44/3.5)*(MPpos(:,:,2)-MPcow2(:,:,2))                               
         ENDWHERE
        
       
         WHERE (BCS(:,:,1).LT.0)
             BCS(:,:,1)=0
         ELSEWHERE(BCS(:,:,1).GT.5)
             BCS(:,:,1)=5
         ENDWHERE    
           
         WHERE (BCS(:,:,2).LT.0)
             BCS(:,:,2)=0
         ELSEWHERE(BCS(:,:,2).GT.5)
             BCS(:,:,2)=5
         ENDWHERE         
         
    ELSEWHERE  
    ! Animals are at barn      
       BCS(:,:,1)=0
       BCS(:,:,2)=0
       Wanimalcow(:,:,1)=0
       Wanimalcow(:,:,2)=0 
       NEBcow_BCS(:,:,1)=0
       NEBcow_BCS(:,:,2)=0             
       NEBcow_W(:,:,1)=0
       NEBcow_W(:,:,2)=0
    ENDWHERE 
      
    !Liveweight integration
 
    
    !We save the previous BCS
    BCScow_prev=BCS
    
    
  ENDSUBROUTINE balance_energy_cow_d
  
  
  SUBROUTINE balance_energy_heifer(&
             npts,dt,nanimaltot,DMIheifer,NELherbage,&
             EVf,Forage_quantity_period, wanimalcow,&
             NEI, NEIh, NEIf, type_animal)

    INTEGER, INTENT(in)                               :: npts
    ! Number of spatial points (-)
    INTEGER, INTENT(in)                               :: type_animal
    ! 1: Dairy cows, 2: Suckler cows, 3: Old module, 4: Dairy heifers, 5 : Suckler heifers
    REAL(r_std ), INTENT(in)                          :: dt
    ! Time step (d)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)         :: nanimaltot
    ! StockRate of cattle
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)         :: DMIheifer
    ! Dry Matter intake of a cow/calf (Kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)         :: NELherbage
    ! Energetic content of the herbage (MJ/kg) 
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)         :: EVf
    ! Energy of the forage based (MJ/Kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)         :: Forage_quantity_period
    ! Forage quantity (MJ/Kg)
    
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(inout)      :: wanimalcow
    ! Animal liveweight (kg/animal) (young:1, adult:2)  
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)        :: NEI
    ! Energy of the forage based on SEBIEN model(MJ/Kg)                        
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)        :: NEIh
    ! Net Energy intake from ingested herbage(MJ)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)        :: NEIf
    ! Net Energy intake from ingested forage(MJ)

    REAL(r_std ), DIMENSION(npts,nvm)                     :: NEIheifer_W
    ! temporary variable to Gain or Loss computation
    ! These parameters come from INRA tables 2007p. + J. Agabriel UMR URH Theix
    REAL(r_std ), DIMENSION(npts,nvm)                     :: alpha
    ! Coefficient for linear regression : NEI[UFL]/LW[kg]^0.75=alpha * LWG[kg/d]^1.4 + beta
    REAL(r_std ), DIMENSION(npts,nvm)                     :: beta
    ! Coefficient for linear regression : NEI[UFL]/LW[kg]^0.75=alpha * LWG[kg/d]^1.4 + beta
    REAL(r_std ), DIMENSION(npts,nvm)                     :: denominateur
    ! intermediary variable 
  INTEGER                                     :: j

    IF(type_animal.EQ.4) THEN ! Dairy heifers
        alpha=0.0348
        beta =0.0446   
    ELSE ! Suckler heifers (type_animal=5)
        alpha=0.0498
        beta =0.0269 
    ENDIF
    
    denominateur=7.12*(wanimalcow)**0.75
    
    ! Net Energy intake      
    WHERE((nanimaltot.NE.0).AND.(denominateur.GT.0))
         NEIh(:,:)= DMIheifer(:,:)*NELherbage
         NEIf(:,:)= Forage_quantity_period(:,:)*7.12*EVf(:,:)    
         NEI(:,:)= NEIh(:,:) + NEIf(:,:)
         NEIheifer_W=(max(0.001,((NEI(:,:)/denominateur-beta)/alpha)))**0.71
    ELSEWHERE  
    ! no grazing period      
       Wanimalcow(:,:)=0.
       NEI(:,:)=0.
       NEIheifer_W=0.
    ENDWHERE
   DO j=2,nvm   
     CALL Euler_funct (1, dt, NEIheifer_W(:,j), wanimalcow(:,j))
   ENDDO 
    
  ENDSUBROUTINE balance_energy_heifer
    
  !----------------------------------
  ! 4 - Respiration & Methane loss
  !----------------------------------
  
  ! Methane emissions were previously calculated as a fixed proportion of the 
  ! ingested carbon (Minonzio, 1998);
  ! Methan-Emissionen der schweizerischen Landwirtschaft
  ! G Minonzio, A Grub, J Fuhrer - Schriftenreihe Umwelt, 1998
  ! In reality, the main factors responsible for CH4 production are not only the amount
  ! but also the quality of the diet (fibres). Cf. Vuichard Thesis
  
  SUBROUTINE Respiration_Methane_cow(&
     npts,grazingc, &
     nanimaltot, DNDFI, Wanimal,&
     R_cow, CH4_cow)

    ! Declarations:
    INTEGER, INTENT(in)                        :: npts
    ! Number of spatial points (-)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)  :: grazingc
    ! C flux associated to grazing (kg C m-2 d-1)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)  :: nanimaltot
    ! Stocking density (animal m-2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)  :: DNDFI
    ! Amount of digestible neutral detergent fiber in the intake (kg d-1)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)  :: Wanimal
    ! Animal life weight (kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out) :: R_cow
    ! Animal respiration (kg C / m²)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out) :: CH4_cow
    ! Enteric methane emission (Kg C / m²)

   !implicit variables intent(in) :
   ! - franimal : Fraction of grazingc respired (-)
   ! - ch4toc   : parameter for the calculation of enteric methane emission

    ! Animal respiration
    !----------------------------------
    ! From grazingc, the fraction franimal is respired
    ! franimal = 0.5 *!
    
    R_cow = franimal*grazingc

    ! Enteric methane emission
    !----------------------------------
    ! ach4   = 0.0002867 (kg CH4 (kg life weight)-1 d-1)
    ! bch4   = 0.000045  (kg CH4 (kg life weight)-1 d-1)
    ! ch4toc = 0.75 * ! parameter for the calculation of enteric methane emission
    
    WHERE (nanimaltot .GT. 0.0)
    
        WHERE((aCH4 + bCH4 * DNDFI) .GE. 0.0)
        
        !(2) p88 equation (1)
        ! Inversion de ach4 & bch4

            CH4_cow = (ach4 + bch4 * DNDFI)*wanimal*ch4toc*nanimaltot      
        
        ELSEWHERE 
           
            CH4_cow = 0.0

        END WHERE
        
    ELSEWHERE
    
        CH4_cow = 0.0
        
    END WHERE       
    

  END SUBROUTINE Respiration_Methane_cow  
  
 
 SUBROUTINE Respiration_Methane_cow_2(npts, npta, type_animal, OMD,NEIh,NEIf,NEIc,grazingc,nanimaltot,&
                                      panimaltot,R_cow,CH4,CH4animal, MPcow2, forage_complementc, f_complementation)                    
 
   INTEGER, INTENT(in)                              :: npts
   ! Number of spatial points (-)
   INTEGER, INTENT(in)                              :: npta
   !  equals 2 when cow (young/primipare and mature/multipare) and 1 when calf
   INTEGER, INTENT(in)                              :: type_animal
   ! 1: Dairy cows, 2: Suckler cows, 3: Old module, 4: Dairy heifers, 5 : Suckler heifers
   REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)        :: OMD
   ! Digestible organic matter in the intake(kg/kg)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)   :: NEIh        
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)   :: NEIf        
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)   :: NEIc        
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)        :: grazingc
    ! C flux associated to grazing (kg C m-2 d-1)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)        :: nanimaltot

    ! Stocking rate (animal m-2)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)        :: panimaltot
    ! proportion of primipare
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)       :: R_cow
    ! Daily animal respiration (kg C m-2 d-1)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)       :: CH4


    ! Daily enteric methane production (kg C/m2/d); 
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)  :: CH4animal
    ! Daily enteric methane production for young or mature cows (kg C/m2/d); 
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)   :: MPcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm)     , INTENT(inout):: forage_complementc
    ! C flux associated to complemtation with forage and concentrate (kg C m-2 d-1)
    INTEGER, INTENT(in)                              :: f_complementation
    ! Flag to activate cow complementation
                                                                        
    
    
    REAL(r_std ), DIMENSION(npts,nvm)                    :: dE
    ! Energy digestibility (%)
    REAL(r_std ), DIMENSION(npts,nvm)                    :: Ymh
    ! CH4 conversion factor, per cent of  metabolizable energy in ingested herbage 
    REAL(r_std ), DIMENSION(npts,nvm,npta)               :: Ymfc
    ! CH4 conversion factor, per cent of  metabolizable energy in ingested forage+concentrate
    REAL(r_std ), DIMENSION(npts,nvm,npta)               :: CH4h
    ! Daily enteric methane production from ingested herbage  (kg C animal-1 d-1)
    REAL(r_std ), DIMENSION(npts,nvm,npta)               :: CH4fc
    ! Daily enteric methane production from ingested forage and concentrate (kg C animal-1 d-1)                                                    
                                                                
    INTEGER :: i,j,k


    IF(type_animal.EQ.1) THEN                !!! for dairy cows !!!
       ! Tables INRA p. 173 Fourrages verts graminées et légumineuses
       ! dE et OMD en %
       dE=0.957*OMD*100-0.068
       Ymh=-0.238*dE+27.67                   ! herbage
       Ymfc(:,:,1)=12.5+0.17*(15-MPcow2(:,:,1))  ! forage (& concentrate)
       Ymfc(:,:,2)=12.5+0.17*(15-MPcow2(:,:,2))  ! forage (& concentrate)
     DO j=2,nvm 
       DO i=1,npts
          DO k=1,npta
             IF( MPcow2(i,j,k).LT.15.0) THEN
            ! Methane from ingested forage and concentrate(kg C/m2/d)           
                CH4fc(i,j,k)=((8.25+0.07*(NEIf(i,j,k)+NEIc(i,j,k))/k_CH4)/55.65)*&
                     ch4toc*nanimaltot(i,j)            
             ELSE    
                CH4fc(i,j,k)=(Ymfc(i,j,k)*(NEIf(i,j,k)+NEIc(i,j,k))/(5565*k_CH4))*&
                     ch4toc*nanimaltot(i,j)            
             ENDIF
          ENDDO
       ENDDO
     ENDDO   
    ELSE  !!! for suckler cows or heifers !!!
       Ymh = 12                            ! herbage
       Ymfc(:,:,:)= 15                       ! forage (& concentrate)
       ! Methane from ingested forage and concentrate(kg C/m2/d)
       CH4fc(:,:,1)=Ymfc(:,:,1)*(NEIf(:,:,1)+NEIc(:,:,1))/(5565*k_CH4)*&
            ch4toc*nanimaltot
       CH4fc(:,:,2)=Ymfc(:,:,2)*(NEIf(:,:,2)+NEIc(:,:,2))/(5565*k_CH4)*&
            ch4toc*nanimaltot
    ENDIF   
    
  ! Methane from ingested herbage (kg C/m2/d)
    
    CH4h(:,:,1)=Ymh*NEIh(:,:,1)/(5565*k_CH4)*ch4toc*nanimaltot
    CH4h(:,:,2)=Ymh*NEIh(:,:,2)/(5565*k_CH4)*ch4toc*nanimaltot
    
  ! Methane from young or mature cows (kg C/m2/d)
  
    IF (f_complementation>0) THEN   ! Cows are supplemented
       CH4animal(:,:,1)=CH4h(:,:,1)+CH4fc(:,:,1)
       CH4animal(:,:,2)=CH4h(:,:,2)+CH4fc(:,:,2)      
    ELSE                            ! Cows are only fed with grazed herbage
       CH4animal(:,:,1)=CH4h(:,:,1)
       CH4animal(:,:,2)=CH4h(:,:,2)
       CH4fc(:,:,1)=0.0
       CH4fc(:,:,2)=0.0
       forage_complementc=0.0
    ENDIF           
       
    
  ! Total methane (kg C/m2/d)

    CH4(:,:)=(CH4h(:,:,1)+CH4fc(:,:,1))*panimaltot+(CH4h(:,:,2)+&
         CH4fc(:,:,2))*(1-panimaltot)
    
  ! Animal respiration(kg C/m2/d)
    
    R_cow=franimal*(grazingc +forage_complementc)
    

 END SUBROUTINE

 
 
 
 SUBROUTINE Urine_Faeces_cow(&
     npts,grazingn, grazingc    ,&
     forage_complementc, forage_complementn,&
     nanimaltot, urinen, faecesn,urinec, faecesc)

   INTEGER, INTENT(in)                        :: npts
   ! Number of spatial points (-)
   REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)  :: grazingn
   ! N flux associated to grazing (kg N m-2 d-1)
   REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)  :: grazingc
   ! C flux associated to grazing (kg C m-2 d-1)
   REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)  :: forage_complementc
   ! C flux associated to forage anc complementation (kg C m-2 d-1)
   REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)  :: forage_complementn
   ! N flux associated to forage anc complementation (kg C m-2 d-1)
   
   REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)  :: nanimaltot
   ! Stocking rate (animal m-2)
   REAL(r_std ), DIMENSION(npts,nvm), INTENT(out) :: urinen
   ! urine N flux (kg N m-2 d-1)
   REAL(r_std ), DIMENSION(npts,nvm), INTENT(out) :: faecesn
   ! faeces N lux (kg N m-2 d-1)
   REAL(r_std ), DIMENSION(npts,nvm), INTENT(out) :: urinec
   ! urine C flux (kg C m-2 d-1)
   REAL(r_std ), DIMENSION(npts,nvm), INTENT(out) :: faecesc
   ! faeces C flux (kg C m-2 d-1)

   !implicit variable intent(in) :
   !- fnurine : Fraction of N in excreta not volatilised, that is in urineN (Menzi et al 1997) (-) 

    ! Local variables
   REAL(r_std ), DIMENSION(npts,nvm) :: excretan
   ! Total N excreta (kg N m-2 d-1)


    WHERE (nanimaltot(:,:).NE.0) 
    
        !urine and faeces
        !(thornley 1998)


        ! Total N excreta
        !----------------------------------
        ! is given by the difference between grazing N and the N converted into milk *!
        
        excretan = grazingn + forage_complementn - milkn


        ! urine N flux
        !---------------------------------- 
        ! equation (4.4d) de "Grassland dynamics" Thornley
        ! fnurine = 0.6 *!
        
        urinen   = fnurine*excretan

        ! faeces N flux
        !---------------------------------- *!
        
        faecesn  = (1.0 - fnurine)*excretan
  
        
        ! yearly values
        
        ! c respired and in excreta
        ! équation (4.4e) de "grassland dynamics" thornley
          

        ! urine C flux
        !----------------------------------
        ! 12/28:urea C:2N ratio *!
        
        urinec  = fnurine*excretan*12.0/28.0
  

        ! faeces C flux
        !----------------------------------
        ! C in faeces is given by the difference between grazingC and the sum of all the
        ! other output C fluxes *!
        
        faecesc = &
           grazingc + &            ! C flux associated to grazing
           forage_complementc - &  ! C flux associated to forage anc complementation
           milkc      - &          ! Fraction of 0.00588 for C of milk production
           ranimal    - &          ! Animal respiration
           methane    - &          ! Enteric methane emission
           urinec                  ! urine C flux
   ELSE WHERE
        urinen(:,:)=0      
        faecesn(:,:)=0
        urinec(:,:)=0
        faecesc(:,:)=0
   ENDWHERE    
           
     
       
    ! yearly values
  END SUBROUTINE Urine_Faeces_cow
  
  
  
  
  SUBROUTINE Calcul_NEL_herbage(npts,OMD, NELherbage)
    INTEGER, INTENT(in)                         :: npts          ! Number of spatial points (-)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)   :: OMD           ! Digestible organic matter in the intake(kg/kg)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(out)  :: NELherbage    ! Energetic content of the herbage (MJ/kg) 
    
        !NELherbage=11.2*OMD-1.83 ! Equation prenant en compte Fourrages verts et foin [Jouven et al.2008]
        NELherbage=10.78*OMD-1.69 ! Equation adaptée par R. Baumont pour prendre en compte l'ensemble des fourrages verts
        
  ENDSUBROUTINE Calcul_NEL_herbage
  
  
  
  SUBROUTINE histwrite_cow_Part1(npts,DMIyoung,DMImature,DMicalf,pyoung_in,OMD,MPcow2,NEBcow, NEIcow, nanimaltot,type_animal,&
                                 MPwCow2,MPpos, DMIc, DMIf)
    INTEGER, INTENT(in)                             :: npts
    ! Number of spatial points (-)
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)       :: DMIyoung
    ! Ingested dry matter for calf (Kg/d)         
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)       :: DMImature
    ! Ingested dry matter for calf (Kg/d)         
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)       :: DMIcalf
    ! Daily calf intake per m2 (Kg/d)          
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)       :: pyoung_in
    ! Ingested dry matter for calf (Kg/d)         
    REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)       :: OMD
    ! Digestible organic matter in the intake(kg/kg)
    
    REAL(r_std ), DIMENSION(npts,nvm)                   :: BCScows
    ! Average BCS of cattle
    REAL(r_std ), DIMENSION(npts,nvm)                   :: Weightcows

    ! Average weight of cattle 
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)     :: MPcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)     :: NEBcow
    ! Net energy Balance (young :1 , adult:2) (MJ) 
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)     :: NEIcow
    ! Net energy intake (MJ)
    REAL(r_std ), DIMENSION(npts,nvm)                   :: nanimaltot
    ! Stocking density (animal/m2)
    INTEGER, INTENT(in)                             :: type_animal
    ! 1 or 2 or 4 or 5= > new module animal
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)     :: MPwcow2
    ! Daily potential milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)     :: MPpos
    ! Possible milk production of dairy cows according to the diet (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)     :: DMIc
    ! Concentrate intake (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)     :: DMIf
    ! forage intake (kg/animal/d)

    !Local variable
    
    REAL(r_std ), DIMENSION(npts,nvm)                   :: Milk_animal
    

      CALL histwrite_p(hist_id_stomate, 'BCSyoung'      ,itime , BCScow(:,:,1)     ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'BCSmature'     ,itime , BCScow(:,:,2)     ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'Weightyoung'   ,itime , wanimalcow(:,:,1) ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'Weightmature'  ,itime , wanimalcow(:,:,2) ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'Weightcalf'    ,itime , wanimalcalf     ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'MPyoung'       ,itime , MPcow2(:,:,1)     ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'MPmature'      ,itime , MPcow2(:,:,2)     ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'MPwyoung'      ,itime , MPwcow2(:,:,1)    ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'MPwmature'     ,itime , MPwcow2(:,:,2)    ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'MPposyoung'    ,itime , MPpos(:,:,1)      ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'MPposmature'   ,itime , MPpos(:,:,2)      ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'NEByoung'      ,itime , NEBcow(:,:,1)     ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'NEBmature'     ,itime , NEBcow(:,:,2)     ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'NEIyoung'      ,itime , NEIcow(:,:,1)     ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'NEImature'     ,itime , NEIcow(:,:,2)     ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'DMIcyoung'     ,itime , DMIc(:,:,1)       ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'DMIcmature'    ,itime , DMIc(:,:,2)       ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'DMIfyoung'     ,itime , DMIf(:,:,1)       ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'DMIfmature'    ,itime , DMIf(:,:,2)       ,npts*nvm, horipft_index)    
      
      !condition car ces variables sont dejà ecrite dans la fonction milk animal pour l'ancien module
      IF((type_animal.NE.3).AND.(type_animal.NE.6)) THEN
         Milk_animal=MPcow2(:,:,1)*pyoung+MPcow2(:,:,2)*(1-pyoung)
       
         CALL histwrite_p(hist_id_stomate, 'milk'          ,itime , Milk_animal*nanimaltot,npts*nvm, horipft_index )
         CALL histwrite_p(hist_id_stomate, 'milkanimal'    ,itime , Milk_animal,npts*nvm, horipft_index )
         CALL histwrite_p(hist_id_stomate, 'milkanimalsum' ,itime , milkanimalsum             ,npts*nvm, horipft_index )
      ENDIF
      
      !Affichage de variables locales à Main_cow
      CALL histwrite_p(hist_id_stomate, 'DMIyoung'      ,itime , DMIyoung            ,npts*nvm, horipft_index )
      CALL histwrite_p(hist_id_stomate, 'DMImature'     ,itime , DMImature           ,npts*nvm, horipft_index )
      CALL histwrite_p(hist_id_stomate, 'DMIcalf'       ,itime , DMIcalf             ,npts*nvm, horipft_index )
      CALL histwrite_p(hist_id_stomate, 'OMD'           ,itime , OMD                 ,npts*nvm, horipft_index )
      
      !Affichage de variables locales à la routine
      BCScows=BCScow(:,:,1)*pyoung_in + BCScow(:,:,2)*(1-pyoung_in)
      Weightcows=wanimalcow(:,:,1)*pyoung_in+wanimalcow(:,:,2)*(1-pyoung_in)
      
      CALL histwrite_p(hist_id_stomate, 'Weightcows'    ,itime , Weightcows          ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'BCScows'       ,itime , BCScows             ,npts*nvm, horipft_index)

  ENDSUBROUTINE histwrite_cow_Part1
  
  SUBROUTINE histwrite_cow_Part2(npts,CH4young, CH4mature)
    INTEGER, INTENT(in)                             :: npts                 ! Number of spatial points (-)
    REAL(r_std ), DIMENSION(npts,nvm)                   :: CH4young             ! 
    REAL(r_std ), DIMENSION(npts,nvm)                   :: CH4mature            ! 
 
      CALL histwrite_p(hist_id_stomate, 'CH4young'      ,itime , CH4young            ,npts*nvm, horipft_index)
      CALL histwrite_p(hist_id_stomate, 'CH4mature'      ,itime , CH4mature           ,npts*nvm, horipft_index)
  ENDSUBROUTINE histwrite_cow_Part2
          
  !Cette fonction permet d'estimer le poids du veau a partir d'un certain age et d'un poids de naissance
  !cela sert dans le cas ou la mise a l'herbe des animaux est activé par l'autogestion alors que le veau n'est pas encore 
  !sortie masi qeu le prochain velage n'a pas eu lieu.
  !Confert document module animal "silver peace" pour elaboration du modèle    
  SUBROUTINE estime_weightcalf(age_calf, weight_init, liveweight_calf)
     REAL(r_std ), INTENT(in)  :: age_calf     ! Age of calf
     REAL(r_std ), INTENT(in)  :: weight_init  ! Initial weight of calf
     REAL(r_std ), INTENT(out) :: liveweight_calf  ! weight of calf

     REAL(r_std )              :: a1
     REAL(r_std )              :: a2               
     REAL(r_std )              :: b1
     REAL(r_std )              :: b2
     REAL(r_std )              :: c1
     
     a1=2.38668*1E-05
     a2=-0.002090876
     b1=-0.00752016
     b2=1.453736796
     c1=0.109332016
     
     liveweight_calf=((a1*weight_init+a2)*age_calf**2)&
                    +((b1*weight_init+b2)*age_calf)&
                    + (c1+1)*weight_init                                      
  ENDSUBROUTINE estime_weightcalf
  
!Fonction permettant de verifier la cohérence du fichier management
!Retour : 0 - Ok
!         1 - Chevauchement de periode de paturage
INTEGER function Verif_management(npts,nstocking,tanimal,danimal)
  INTEGER, INTENT(in)                                    :: npts
  ! Number of spatial points (-)
  INTEGER, INTENT(in)                                    :: nstocking
  ! Number of spatial points (-)
  REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(in)    :: tanimal
  ! Beginning of the grazing period    h (1,..,nstocking) (d)
  
  REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(in)    :: danimal
  ! Lenght of the grazing period    h (1,..,nstocking) (d)        
    !Local
    INTEGER, DIMENSION(npts,nvm)   :: cumule_periode
    INTEGER                    :: J 
    INTEGER                    :: h
    INTEGER                    :: retour=0

    !On verifie qu'il n'y a aucune periode de mise a l'here des animaux qui se chevauchent

    !on parcours les 360 jours
    !On regarde si il y a cumule de periode, si oui STOP RUN       
        DO J=1,365 
        cumule_periode  = 0
        h  = 1

            !DO WHILE(h .LT. nstocking)
               WHERE((J .GE. tanimal(:,:,h)) .AND. &
                    (J .LT. (tanimal(:,:,h) + danimal(:,:,h))))
         
                 cumule_periode = cumule_periode + 1

            END WHERE
                      h  = h  + 1
            !END DO
            IF(ANY(cumule_periode.GE.2)) THEN
                retour=1
            ENDIF           
            h = 1
            cumule_periode=0
        END DO    
    Verif_management=retour
end function Verif_management  



!Cette fonction est appelée a chaque entrée en paturage afin de calculer
!la perte d'etat max d'une vache laitière pour la période considérée

SUBROUTINE calcul_perte_etat(npts,tjulian,BCScow,MPwmax,tcalving,PEmax) 
    
  INTEGER, INTENT(in)                                    :: npts
  ! Number of spatial points (-)
  INTEGER(i_std ), INTENT(in)                               :: tjulian
  ! Julian day
  REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)            :: BCScow
  ! Body Condition Score (for cow only /5)
  REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)            :: MPwmax
  ! Maximum of theoretical milk production (kg/animal/d)
  REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)              :: tcalving
  ! Calving date (d) 
  REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)           :: PEmax
  ! Perte d'etat maximale des vaches laitières sur la periode de paturage 
   
  REAL(r_std ), DIMENSION(npts,nvm)                          :: nWeeklact
  ! Lactation week (in weeks from calving)
 
  WHERE(tjulian .GE. tcalving)
       nWeeklact = CEILING((tjulian-REAL(tcalving))/7+1)
  ELSEWHERE    
 ! si tjulian est inférieur à tcalving on considere le velage de l'annee precedente   
       nWeeklact = CEILING((tjulian-(REAL((tcalving)-365)))/7+1)                
  ENDWHERE
  
  ! Dans les cas ou la definition des conditions d'entree en paturage sont en dehors du 
  ! domaine de validite de l'equation, PEmax peut etre positif
  ! On borne dans ce cas la perte d'etat max a zero car celle ci doit être signee negativement


  PEmax(:,:,1)=0.52615+7*0.0042*nWeekLact(:,:)-&
       0.01416*MPwmax(:,:,1)-0.3644*BCScow(:,:,1)
  PEmax(:,:,2)=0.66185+7*0.0042*nWeekLact(:,:)-&
       0.01416*MPwmax(:,:,2)-0.3644*BCScow(:,:,2)

     WHERE (PEmax(:,:,1).GT.0.0)
        PEmax(:,:,1)=0.0
     ENDWHERE
   
     WHERE (PEmax(:,:,2).GT.0.0)
         PEmax(:,:,2)=0.0
     ENDWHERE   
     
ENDSUBROUTINE calcul_perte_etat


  
! Fonction permettant de savoir si les animaux paturent au jour J
! Retour : 1:si des animaux sont en paturage au jour J
!          0:sinon
SUBROUTINE in_management(npts,nstocking,tanimal,danimal,tjulian,retour)
  INTEGER, INTENT(in)                                    :: npts
  ! Number of spatial points (-)
  INTEGER, INTENT(in)                                    :: nstocking
  ! Number of spatial points (-)
  REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(in)    :: tanimal
  ! Beginning of the grazing period    h (1,..,nstocking) (d)
  REAL(r_std ), DIMENSION(npts,nvm,nstocking), INTENT(in)    :: danimal
  ! Lenght of the grazing period    h (1,..,nstocking) (d)        
  INTEGER(i_std ),                            INTENT(in)    :: tjulian
  ! Julian day (-) 
    INTEGER, DIMENSION(npts,nvm),                INTENT(out)   :: retour
    INTEGER :: h
    INTEGER, dimension(npts,nvm) :: cumule_periode
    cumule_periode  = 0
    h  = 1
    retour=0
            !DO WHILE(h .LT. nstocking)
               WHERE((tjulian .GE. tanimal(:,:,h)) .AND. &
                    (tjulian .LT. (tanimal(:,:,h) + danimal(:,:,h))))
         
                 cumule_periode = cumule_periode + 1

            END WHERE
                      h  = h  + 1
            !END DO
            WHERE(cumule_periode.EQ.1) 
                retour=1
            ENDWHERE           

END SUBROUTINE in_management  
 


!----------------------------------------
! SUBROUTINES DU MODULE ANIMAL LAITIER
!----------------------------------------
  
  SUBROUTINE Calcul_NER_cow(npts,npta,wanimalcow,wcalfborn, Age_animal, nweekgest, MPwcow2,NER,NEGcow,NEMcow)
    INTEGER, INTENT(in)                               :: npts
    ! Number of spatial points (-)
    INTEGER, INTENT(in)                               :: npta
    ! 
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: wanimalcow
    ! Animal liveweight (kg/animal) (young:1, adult:2) 
    REAL(r_std ), DIMENSION(npts,nvm)     , INTENT(in)    :: Wcalfborn
    ! Calf liveweigth at birth (kg/animal)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: AGE_animal
    ! Animal age in case of simulation of dairy cows (months)
    REAL(r_std ), DIMENSION(npts,nvm),      INTENT(in)    :: Nweekgest
    ! Gestation week (in weeks from mating)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: MPwcow2
    ! Daily potential milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)   :: NER
    ! Total net energy required (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)   :: NEGcow
    ! Net energy required for gestation (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)   :: NEMcow
    ! Net energy required for gestation (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,npta)                :: NEPlact                 ! Net energy required for milk prduction (MJ)
    
      
         !initialialisation
         !
          NER(:,:,1)=0
          NER(:,:,2)=0
          
         !calcul de besoin d'energie pour la production de lait
         ! AIG 04/07/2010 On calcule les besoins en énergie pour réaliser la production de lait POTENTIELLE
         ! NEPlact(:,1)=0.44*7.12*MPcow2(:,1)
         ! NEPlact(:,2)=0.44*7.12*MPcow2(:,2)
          NEPlact(:,:,2)=0.44*7.12*MPwcow2(:,:,1)
          NEPlact(:,:,2)=0.44*7.12*MPwcow2(:,:,2)
         !calcul de besoin pour la gestation
         WHERE (nweekgest.LE.40)
            NEGcow(:,:,1)=7.12*(3.25-0.08*Age_animal(:,:,1) + &
                 0.00072*wcalfborn(:,:)*exp(0.116*nweekgest(:,:)))
            NEGcow(:,:,2)=7.12*(3.25-0.08*Age_animal(:,:,2) + &
                 0.00072*wcalfborn(:,:)*exp(0.116*nweekgest(:,:)))
         ENDWHERE
         
         !calcul des besoin pour l'entretiens
          NEMcow(:,:,1)=7.12*0.041*(wanimalcow(:,:,1)**0.75)*(1+0.2)
          NEMcow(:,:,2)=7.12*0.041*(wanimalcow(:,:,2)**0.75)*(1+0.2)            
          
          NER=NEPlact+NEGcow+NEMcow
  ENDSUBROUTINE Calcul_NER_cow
  
  
  !--------------------------
  ! Net Energy requirements
  !--------------------------
  SUBROUTINE calcul_NEI_cow_d(npts,npta,MPcow2,DMIcowanimal,NELherbage,&
                                  EVf,Forage_quantity_period       ,&
                                  EVc,Qic,NEI,NEM,NEIh,NEIf,NEIc)
                                  
    INTEGER, INTENT(in)                               :: npts
    ! Number of spatial points (-)
    INTEGER, INTENT(in)                               :: npta
    ! 
      REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)       :: MPcow2
      ! Daily actual milk production per animal for primiparous or multiparous cows at previous time step (kg/animal/d)
      REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: DMIcowanimal
      ! Daily animal intake for primiparous or multiparous cows(kg/animal/d)
      REAL(r_std ), DIMENSION(npts,nvm)     , INTENT(in)    :: NELherbage
      ! Energetic content of the herbage (MJ/kg) 
      REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)         :: EVf
      ! Energy of the forage based (MJ/Kg)
      REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)         :: Forage_quantity_period
      ! Forage quantity  (MJ/Kg)
      REAL(r_std ), DIMENSION(npts,nvm), INTENT(in)         :: EVc
      ! Energy of the concentrate (MJ/Kg)
      REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(in)    :: Qic
      ! Concentrate quantity per kg of milk or per kg of LW (MJ/Kg)
      REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)   :: NEI
      ! Net energy intake(MJ/Kg)
      REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)   :: NEM
      ! Net energy intake(MJ/Kg)
      REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)   :: NEIh
      ! Net Energy intake from ingested herbage(MJ)
      REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)   :: NEIf
      ! Net Energy intake from ingested forage(MJ)
      REAL(r_std ), DIMENSION(npts,nvm,npta), INTENT(out)   :: NEIc
      ! Net Energy intake from ingested concentrate(MJ)
    
         ! Net Energy intake
         
           ! Primiparous cows
         
         NEIh(:,:,1)= DMIcowanimal(:,:,1)*NELherbage
         NEIf(:,:,1)= Forage_quantity_period(:,:)*7.12*EVf(:,:)
         NEIc(:,:,1)= Qic(:,:,1)*MPcow2(:,:,1)*EVc(:,:)
         
           ! Multiparous cows
         NEIh(:,:,2)= DMIcowanimal(:,:,2)*NELherbage
         NEIf(:,:,2)= Forage_quantity_period(:,:)*7.12*EVf(:,:)
         NEIc(:,:,2)= Qic(:,:,2)*MPcow2(:,:,2)*EVc(:,:)
                
         NEI(:,:,1)=NEIh(:,:,1)+NEIf(:,:,1)+NEIc(:,:,1)
         NEI(:,:,2)=NEIh(:,:,2)+NEIf(:,:,2)+NEIc(:,:,2)
         
         ! Net energy for maintenance 
         
         NEM(:,:,1)=7.12*0.041*(wanimalcow(:,:,1)**0.75)*(1+0.2)
         NEM(:,:,2)=7.12*0.041*(wanimalcow(:,:,2)**0.75)*(1+0.2)
         
         ! Net energy for gestation
         ! Attention la gestation ne dure que 9 mois (280j) donc on ne calcule les besoins de gestation
         ! que pour nweekgest compris entre 0 et 40   
         
         
  ENDSUBROUTINE Calcul_NEI_cow_d
  
  !----------------------------------
  ! Potential milk production (MPpot)
  !----------------------------------
    
  SUBROUTINE Potentiel_dairy_d(npts,tjulian,nweekLact,nweekGest,MPwcow2max,MPwcow2)
  
    INTEGER, INTENT(in)                             :: npts
    ! Number of spatial points (-)
    INTEGER(i_std ),                    INTENT(in)     :: tjulian
    ! Julian day (d)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(out)    :: nWeeklact
    ! Lactation week (in weeks from calving)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(out)    :: nWeekGest
    ! Gestation week (in weeks from mating)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)     :: MPwcow2max
    ! Daily potential milk production per animal for primiparous or multiparous cows at peak of lactation(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)    :: MPwcow2
    ! Daily potential milk production per animal for primiparous or multiparous cows (kg/animal/d)
     
    ! Lactation and gestation weeks
    !------------------------------

        WHERE(tjulian .GE. tcalving)
            nWeeklact = CEILING((tjulian-REAL(tcalving))/7+1)
            nWeekGest = CEILING((tjulian-80-REAL(tcalving))/7+1)
        ELSEWHERE    
        ! si tjulian est inférieur à tcalving on considere le velage de l'annee precedente   
            nWeeklact = CEILING((tjulian-(REAL((tcalving)-365)))/7+1)
            nWeekGest = CEILING((tjulian-80-(REAL((tcalving)-365)))/7+1)                 
        ENDWHERE 
                             
        WHERE(nWeekGest.LT.0)
            nweekGest=0
        ELSEWHERE(nWeekgest.GT.40) 
        ! On considere une gestation de 9 mois soit pas plus de 40 semaines soit 280j
            nweekgest=0
        ENDWHERE 
        
        MPwcow2(:,:,1)=MPwcow2max(:,:,1)*(1.084-(0.7*exp(-0.46*nWeeklact(:,:)))-&
             (0.009*nWeeklact(:,:))-(0.69*exp(-0.16*(45-nweekgest(:,:)))))   
        MPwcow2(:,:,2)=MPwcow2max(:,:,2)*(1.047-(0.69*exp(-0.90*nWeeklact(:,:)))-&
             (0.0127*nWeeklact(:,:))-(0.5*exp(-0.12*(45-nweekgest(:,:)))))   
                  
  ENDSUBROUTINE Potentiel_dairy_d
  
  
  
  SUBROUTINE Milk_Animal_cow_d(                &
     npts, dt                                  ,&
     nanimaltot,tjulian                        ,&
     MPcow2,MPcow,MPwcow2                      ,&
     MPcowC, MPcowN                            ,&
     MPcowCsum, MPcowNsum, milkanimalsum,milkKG,&
     NWeekLact, NWeekGest,PEmax,PEpos,deltaBCS ,&
     MPpos,NEIcow,NEMcow,NEGcow,MPcow2_prec    ,&
     MPpot)
     
    INTEGER, INTENT(in)                              :: npts
    ! Number of spatial points (-)
    REAL(r_std ), INTENT(in)                         :: dt
    ! Time step (d)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(in)      :: nanimaltot
    ! Stocking density (animal m-2)
    INTEGER(i_std ),                    INTENT(in)      :: tjulian
    ! Julian day (d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)     :: MPcow2
    ! Daily actual milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)     :: MPcow
    ! Daily milk production per m2 for primiparous or multiparous cows (kg/m2/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)     :: MPwcow2
    ! Daily potential milk production per animal for primiparous or multiparous cows (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)     :: MPcowC
    ! C in daily milk production per m2 for primiparous or multiparous cows (kgC/m2/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)     :: MPcowN
    ! N in daily milk production per m2 for primiparous or multiparous cows (kgN/m2/d)
    
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)     :: MPcowCsum
    ! Cumulated C in milk production per m2 for primiparous or multiparous cows (kgC/m2)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)     :: MPcowNsum
    ! Cumulated N in milk production per m2 for primiparous or multiparous cows (kgN/m2)
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(inout)   :: milkanimalsum
    ! Milk production per animal and per year (L.(animal.year)-1)    
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(out)     :: nWeeklact
    ! Lactation week (in weeks from calving)
    
    REAL(r_std ), DIMENSION(npts,nvm)  , INTENT(out)     :: nWeekGest
    ! Gestation week (in weeks from mating)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)      :: PEmax
    ! Perte d'etat maximale des vaches laitières sur la periode de paturage 
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)   :: PEpos
    ! Perte d'etat possible des vaches laitières au jour j
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)      :: deltaBCS
    ! Body condition score variation between two consecutive time steps (-)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)     :: MPpos
    ! Possible milk production of dairy cows according to the diet (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)      :: NEIcow
    ! Total net energy intake (1:young, 2:adult) (MJ) 
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)      :: NEMcow
    ! Net energy for maintenance (young :1 , adult:2) (MJ)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(in)      :: NEGcow
    ! Net energy for gestation (dairy cows)(young :1 , adult:2) (MJ) 
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(inout)   :: MPcow2_prec
    ! Daily actual milk production per animal for primiparous or multiparous cows at previous time step (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2), INTENT(out)     :: MPpot
    ! Potential milk production (kg/d)

    REAL(r_std ), DIMENSION(npts,nvm)                   :: milkKG
    ! Daily actual milk production per animal for the whole cattle (kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm,2)                 :: MR
    ! Milk response (-)
    REAL(r_std ), DIMENSION(npts,nvm,2)                 :: RF
    ! Remobilisation fraction (-)
    REAL(r_std ), DIMENSION(npts,nvm)                   :: Fremob
    ! facteur de remobilisation (fonction de la lactation)
    REAL(r_std ), DIMENSION(npts,nvm,2)                 :: MPwcow2max
    ! Daily potential milk production per animal for primiparous or multiparous cows at peak of lactation(kg/animal/d)
    REAL(r_std ), DIMENSION(npts,nvm)                   :: milkanimal_write
    ! Milk production per animal and per day (kg animal-1 d-1)
    REAL(r_std ), DIMENSION(npts,nvm,2)                 :: min_NEB
    ! minimum value of NEB for milk production calculation
    INTEGER                                         :: i,k,j
    ! For loop
    
    MPwcow2max(:,:,1)=MPwmax(:,:,1)
    ! potential milk production of primiparous cows (kg) 
    MPwcow2max(:,:,2)=MPwmax(:,:,2)
    ! potential milk production of multiparous cows (kg)
        
    !Calcul de la production de lait possible
    ! AIG June 2010 To avoid that possible milk production could be negative   
    MPpos(:,:,1)=max(0.0,(NEIcow(:,:,1)-NEMcow(:,:,1)-NEGcow(:,:,1))/(0.44*7.12))
    MPpos(:,:,2)=max(0.0,(NEIcow(:,:,2)-NEMcow(:,:,2)-NEGcow(:,:,2))/(0.44*7.12))
        
   
           ! Lactation and gestation weeks
           !------------------------------

            WHERE(tjulian .GE. tcalving)
                nWeeklact = CEILING((tjulian-REAL(tcalving))/7+1)
                nWeekGest = CEILING((tjulian-80-REAL(tcalving))/7+1)
            ELSEWHERE    
            ! si tjulian est inférieur à tcalving on considere le velage de l'annee precedente   
                nWeeklact = CEILING((tjulian-(REAL((tcalving)-365)))/7+1)
                nWeekGest = CEILING((tjulian-80-(REAL((tcalving)-365)))/7+1)                 
            ENDWHERE
                          
            WHERE(nWeekGest.LT.0)
                nweekGest=0
            ELSEWHERE(nWeekgest.GT.40) 
            ! On considere une gestation de 9 mois soit pas plus de 40 semaines soit 280j
                nweekgest=0
            ENDWHERE
            
            !
                
            WHERE(nWeeklact(:,:).GE.20)
               Fremob(:,:)=0.66*(1-0.02*(nWeekLact(:,:)-20))
            ELSEWHERE
               Fremob(:,:)=0.66
            ENDWHERE
            
            ! Potential milk production for young and mature cows (kg/animal)
            !----------------------------------------------------
            MPpot(:,:,1)=MPwcow2max(:,:,1)*(1.084-(0.7*exp(-0.46*nWeeklact))-&
                 (0.009*nWeeklact)-(0.69*exp(-0.16*(45-nweekgest))))   
            MPpot(:,:,2)=MPwcow2max(:,:,2)*(1.047-(0.69*exp(-0.90*nWeeklact))-&
                 (0.0127*nWeeklact)-(0.5*exp(-0.12*(45-nweekgest))))   

            ! Possible remobilisation of body reserves 
            !---------------------------------------
            PEpos(:,:,1)=PEpos(:,:,1)-deltaBCS(:,:,1)
            PEpos(:,:,2)=PEpos(:,:,2)-deltaBCS(:,:,2)    
            
            DO k=1,2
                WHERE((MPpos(:,:,k)-MPpot(:,:,k).LT.0).AND.(PEmax(:,:,k).NE.0))
                    RF(:,:,k)= PEpos(:,:,k)/PEmax(:,:,k)
                ELSEWHERE 
                    RF(:,:,k)=0   
                ENDWHERE
            ENDDO
            
            ! Milk response (-)
            !---------------
            
            MR(:,:,1)=Fremob(:,:)*RF(:,:,1)
            MR(:,:,2)=Fremob(:,:)*RF(:,:,2)
            
            
            ! Observed milk production of dairy cows (Kg[milk]/animal/d)
            !-----------------------------------------------------------
                WHERE(nWeeklact .LE.43)                                        
                
                    WHERE((MPpos(:,:,1)-MPpot(:,:,1)).LT.0.0)
                    ! AIG June 2010 to avoid that milk production could be negative
                       !MPcow2(:,1)=min(MPpot(:,1),max(0.0,MPpos(:,1)-MR(:,1)*(MPpos(:,1)-MPpot(:,1))))
                       MPcow2(:,:,1)=max(0.0,MPpos(:,:,1)-MR(:,:,1)*&
                            (MPpos(:,:,1)-MPpot(:,:,1)))
                    ELSEWHERE
                       MPcow2(:,:,1)=MPpot(:,:,1)   
                    ENDWHERE                 
                
                    
                    WHERE((MPpos(:,:,2)-MPpot(:,:,2)).LT.0.0)
                    ! AIG June 2010 to avoid that milk production could be negative
                       !MPcow2(:,2)=min(MPpot(:,2),max(0.0,MPpos(:,2)-MR(:,2)*(MPpos(:,2)-MPpot(:,2))))
                       MPcow2(:,:,2)=max(0.0,MPpos(:,:,2)-MR(:,:,2)*&
                            (MPpos(:,:,2)-MPpot(:,:,2)))
                    ELSEWHERE
                       MPcow2(:,:,2)=MPpot(:,:,2)   
                    ENDWHERE                                                
                
                ELSEWHERE
                    MPwcow2(:,:,1)= 0.0    
                    MPwcow2(:,:,2)= 0.0
                    MPcow2(:,:,1) = 0.0    
                    MPcow2(:,:,2) = 0.0
                    MPpos(:,:,1)  = 0.0
                    MPpos(:,:,2)  = 0.0
                ENDWHERE
                    
          
        MPcow2_prec=MPcow2   

        milkKG=MPcow2(:,:,1)*pyoung+MPcow2(:,:,2)*(1-pyoung)
              
              
        WHERE (nanimaltot.EQ.0)
            milkKG=0.0
            MPcow2(:,:,1)=0.0
            MPcow2(:,:,2)=0.0
            MPpos(:,:,1)=0.0
            MPpos(:,:,2)=0.0
        ENDWHERE    
         
        ! Daily milk production per m2 for primiparous or multiparous cows (kg/m2/d)
        !----------------------------------------------------------------
        MPcow(:,:,1) = nanimaltot * MPcow2(:,:,1) * pyoung
        MPcow(:,:,2) = nanimaltot * MPcow2(:,:,2) * (1-pyoung)
        
       
        ! C in MPcow (kgC/m2/d)
        !----------------------    
        MPcowC = 0.0588 * MPcow
        
        ! N in MPcow (kgN/m2/d)
        !----------------------    
        MPcowN = 0.00517 * MPcow
      DO j=2,nvm     
        CALL Euler_X(npts,2, dt, MPcow(:,j,:) ,   MPcowsum(:,j,:))
        CALL Euler_X(npts,2, dt, MPcowC(:,j,:),   MPcowCsum(:,j,:))
        CALL Euler_X(npts,2, dt, MPcowN(:,j,:),   MPcowNsum(:,j,:))
        CALL Euler_X(npts,2, dt, MPcow2(:,j,:),   MPcow2sum(:,j,:))   

!        milk_write=MPcow(:,1)+MPcow(:,2)
        milkanimal_write(:,j)=MilkKG(:,j)

        
        CALL Euler_funct (npts, dt, milkanimal_write(:,j), milkanimalsum(:,j))
      ENDDO   
  ENDSUBROUTINE Milk_animal_cow_d

END MODULE grassland_grazing