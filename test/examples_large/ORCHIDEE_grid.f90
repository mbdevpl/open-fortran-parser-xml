
!! This module define variables for the grid to gathered points.
!!
!! @call sechiba_main
!! @Version : $Revision: 4357 $, $Date: 2017-05-19 10:02:22 +0200 (ven. 19 mai 2017) $
!! 
!< $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/branches/ORCHIDEE-MICT/ORCHIDEE/src_global/grid.f90 $
!< $Date: 2017-05-19 10:02:22 +0200 (ven. 19 mai 2017) $
!< $Revision: 4357 $
!!
!! @author Marie-Alice Foujols, Jan Polcher and Martial Mancip
!!
!! This module archives and makes available for all ORCHIDEE routine the information on the grid
!! being used. 3 types of grids are foreseen :
!! - Regular longitude latitude grid : This is the default and mostly used for global applications.
!! - Regular X/Y grid : this is a typical grid for regional models and requires a projection method
!!                      to go from X/y to lon/lat.
!! - unstructures grid : This is a general grid where each cell is a polygone. It prepares ORCHIDEE
!!                       for DYNAMICO.
!!
!! The subroutines have the following role :
!!  grid_init : this routine will provide the dimensions needed to allocate the memory and the
!!              characteristics of the grid.
!!
!!  grid_stuff : This subroutine provides the grid details for all land points. Obviously depending
!!               on the grid type different level of information need to be provided.
!!
!f90doc MODULEgrid
MODULE grid

  USE grid_var
  USE defprec
  USE constantes
  USE mod_orchidee_para

  USE haversine
  USE module_llxy

  USE ioipsl
  USE netcdf

  IMPLICIT NONE
  !
  !=================================================================================
  !
  ! Horizontal grid information
  !
  !=================================================================================

  ! Global map or not.
  ! There is little chance that if iim <=2 and jjm <= 2 that we have global grid.
  ! Furthermore using the second line allows to avoid pole problems for global grids
  LOGICAL, SAVE                                     :: global = .TRUE.
!$OMP THREADPRIVATE(global)

  ! PARAMETERS
  ! default resolution (m)
  REAL(r_std), PARAMETER :: default_resolution = 250000.
  !
  ! VARIABLES
  !
  !-
  !- Variable to help describe the grid
  !- once the points are gathered.
  !-
  !! Limits of the domain
  REAL(r_std), SAVE                                   :: limit_west, limit_east, &
       &                                                limit_north, limit_south
!$OMP THREADPRIVATE(limit_west, limit_east, limit_north, limit_south)
  !-
  !! Geographical coordinates
  REAL(r_std), ALLOCATABLE, DIMENSION (:,:), SAVE     :: lalo
!$OMP THREADPRIVATE(lalo)
  !! index  of land points
  INTEGER, ALLOCATABLE, DIMENSION (:), SAVE           :: ilandindex,jlandindex
!$OMP THREADPRIVATE(ilandindex, jlandindex)
  !- 
  !! Fraction of continents.
  REAL(r_std), ALLOCATABLE, DIMENSION (:), SAVE       :: contfrac
!$OMP THREADPRIVATE(contfrac)
  !
  ! indices of the NbNeighb neighbours of each grid point 
  ! (1=Northern most vertex and then in clockwise order)
  ! Zero or negative index means that this neighbour is not a land point
  INTEGER(i_std), ALLOCATABLE, DIMENSION (:,:), SAVE  :: neighbours
!$OMP THREADPRIVATE(neighbours)
  !
  ! Heading of the direction out of the grid box either through the vertex 
  ! of the mid-segment of the polygon.
  !
  REAL(r_std), ALLOCATABLE, DIMENSION(:,:), SAVE      :: headings
!$OMP THREADPRIVATE(headings)
  !
  ! Length of segments of the polygon.
  !
  REAL(r_std), ALLOCATABLE, DIMENSION(:,:), SAVE      :: seglength
!$OMP THREADPRIVATE(seglength)
  !
  ! Area of the grid box
  !
  REAL(r_std), ALLOCATABLE, DIMENSION(:), SAVE        :: area
!$OMP THREADPRIVATE(area)
  !
  ! Coordinats of the vertices
  !
  REAL(r_std), ALLOCATABLE, DIMENSION(:,:,:), SAVE    :: corners
!$OMP THREADPRIVATE(corners)
  !
  ! Resolution remains a temporary variable until the merge of the
  ! re-interfacing of the interpolation by Lluis. One this is done
  ! Resolution will be replaced in the model either by area or seglength.
  !
  REAL(r_std), ALLOCATABLE, DIMENSION (:,:), SAVE     :: resolution
!$OMP THREADPRIVATE(resolution)
  !
  !
  !
  ! Get the direction of the grid
  !
  CHARACTER(LEN=2), DIMENSION(2), SAVE, PRIVATE      :: grid_dir
!$OMP THREADPRIVATE(grid_dir)
  !
  INTEGER(i_std), PARAMETER :: MAX_DOMAINS=1
  !
  type (proj_info), SAVE, dimension(1:MAX_DOMAINS) :: proj_stack
  !
  real(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:)   :: dxwrf, dywrf
  !
  !
  !=================================================================================
  !
  ! Calendar information
  !
  !=================================================================================
  !
  ! The calendar
  CHARACTER(LEN=20), SAVE               :: calendar_str
!$OMP THREADPRIVATE(calendar_str)
  !
  ! The date
  REAL(r_std), SAVE                     :: in_julian
!$OMP THREADPRIVATE(in_julian)
  ! Diff with day 0
  REAL(r_std), SAVE                     :: julian_diff
!$OMP THREADPRIVATE(julian_diff)
  !
  INTEGER(i_std), SAVE                  :: year, month, day
!$OMP THREADPRIVATE(year, month, day)
  REAL(r_std), SAVE                     :: sec
!$OMP THREADPRIVATE(sec)
  !
  ! month_len (d)
  INTEGER(i_std), SAVE                  :: month_len
!$OMP THREADPRIVATE(month_len)
  !
  ! year length (d)
  INTEGER(i_std), SAVE                  :: year_length=0
!$OMP THREADPRIVATE(year_length)
  !
  ! Ration between calendar year in days (ie 360d or 365d ...) to gregorian year length 
  REAL(r_std), SAVE :: year_spread
!$OMP THREADPRIVATE(year_spread)
  !
  !
  INTERFACE grid_tolola
     MODULE PROCEDURE grid_tolola_scal, grid_tolola_1d, grid_tolola_2d
  END INTERFACE grid_tolola

  INTERFACE grid_toij
     MODULE PROCEDURE grid_toij_scal, grid_toij_1d, grid_toij_2d
  END INTERFACE grid_toij
  !
CONTAINS
  !
  !f90doc CONTAINS
  ! 
  !
!!  =============================================================================================================================
!! SUBROUTINE:    grid_init
!!
!>\BRIEF	  Initialization of grid description distributed by this module to the rest of the model.
!!
!! DESCRIPTION:	  Routine which provides the dimension of the grid (number of land points) as well as the 
!!                grid characteristics (type and name) so that the memory can be allocated.
!!
!!                This subroutine is called by intersurf_main_2d or any driver of the model.
!!
!! \n
!_ ==============================================================================================================================
!!
  SUBROUTINE grid_init ( npts, nbseg, gtype, gname, isglobal )
    !
    ! 0 interface
    !
    IMPLICIT NONE
    !
    ! 0.1 input  !
    !
    ! Domain size
    INTEGER(i_std), INTENT(in)                                 :: npts     !! Number of local continental points
    INTEGER(i_std), INTENT(in)                                 :: nbseg    !! number of segments of the polygone of the mesh
    CHARACTER(LEN=*), INTENT(in)                               :: gtype    !! Type of grid
    CHARACTER(LEN=*), INTENT(in)                               :: gname    !! Name of the grid
    LOGICAL, OPTIONAL                                          :: isglobal
    !
    !
    !
    CHARACTER(LEN=20)                                          :: gtype_lower
    !
    ! Verify the information passed and save it in the global variables of the model.
    !
    gtype_lower = gtype
    CALL strlowercase(gtype_lower)

    IF ( INDEX(gtype_lower, "reglonlat") > 0) THEN
       IF ( nbseg /= 4 ) THEN
          CALL ipslerr(3, "grid_init", "This regular Lon/lat grid should have 4 segments", &
               &       "per horizontal grid box","")
       ELSE
          NbSegments=4
       ENDIF
       GridType="RegLonLat"
       GridName=gridname
       IF ( PRESENT(isglobal) ) THEN
          global = isglobal
       ELSE
          global = .TRUE.
       ENDIF
    ELSE IF ( INDEX(gtype_lower, "regxy") > 0) THEN
       IF ( nbseg /= 4 ) THEN
          CALL ipslerr(3, "grid_init", "This regular X/Y grid should have 4 segments", &
               &       "per horizontal grid box","")
       ELSE
          NbSegments=4
       ENDIF
       GridType="RegXY"
       GridName=gridname
       IF ( PRESENT(isglobal) ) THEN
          global = isglobal
       ELSE
          global = .FALSE.
       ENDIF
    ELSE IF ( INDEX(gtype_lower, "unstruct") > 0) THEN
       NbSegments=nbseg
       GridType="UnStruct"
       GridName=gridname
       IF ( PRESENT(isglobal) ) THEN
          global = isglobal
       ELSE
          global = .TRUE.
       ENDIF
    ELSE
       CALL ipslerr(3, "grid_init", "unrecognized grid type.",&
            &       "It has to be either reglatlon, regxy or unstruct","")
    ENDIF
    !
    !  Create the internal coordinate table
    !
    IF ( (.NOT.ALLOCATED(lalo))) THEN
       ALLOCATE(lalo(npts,2))
       lalo(:,:) = val_exp
    ENDIF
    !-
    !- Store variable to help describe the grid
    !- once the points are gathered.
    !-
    NbNeighb=2*NbSegments
    IF ( (.NOT.ALLOCATED(neighbours))) THEN
       ALLOCATE(neighbours(npts,NbNeighb))
       neighbours(:,:) = -999999
    ENDIF
    IF ( (.NOT.ALLOCATED(headings))) THEN
       ALLOCATE(headings(npts,NbNeighb))
       headings(:,:) = val_exp
    ENDIF
    IF ( (.NOT.ALLOCATED(seglength))) THEN
       ALLOCATE(seglength(npts,NbSegments))
       seglength(:,:) = val_exp
    ENDIF
    IF ( (.NOT.ALLOCATED(corners))) THEN
       ALLOCATE(corners(npts,NbSegments,2))
       corners(:,:,:) = val_exp
    ENDIF
    IF ( (.NOT.ALLOCATED(area))) THEN
       ALLOCATE(area(npts))
       area(:) = val_exp
    ENDIF
    !
    ! TEMPORARY
    !
    IF ( (.NOT.ALLOCATED(resolution))) THEN
       ALLOCATE(resolution(npts,2))
       resolution(:,:) = val_exp
    ENDIF
    !
    !- Store the fraction of the continents only once so that the user
    !- does not change them afterwards.
    !
    IF ( (.NOT.ALLOCATED(contfrac))) THEN
       ALLOCATE(contfrac(npts))
       contfrac(:) = val_exp
    ENDIF
    !
    ! Allocation of index coordinates ... 
    ! JP : these are global fields and should perhaps be allocated somewhere else.
    IF (.NOT. ALLOCATED(ilandindex)) THEN
       ALLOCATE(ilandindex(nbp_glo),jlandindex(nbp_glo))
       ilandindex(:) = -10000000
       jlandindex(:) = -10000000
    ENDIF
    !
  END SUBROUTINE grid_init
!!
!!
!!  =============================================================================================================================
!! FUNCTION grid_set
!!
!>\BRIEF	     subroutine to set global grid parameters present on all procs
!!
!! DESCRIPTION:	  
!!                
!!
!!              
!!
!! \n
!_ ==============================================================================================================================
!!
  SUBROUTINE grid_set_glo(arg_nbp_lon,arg_nbp_lat,arg_nbp_glo)
    IMPLICIT NONE

    INTEGER(i_std), INTENT(IN) :: arg_nbp_lon
    INTEGER(i_std), INTENT(IN) :: arg_nbp_lat
    INTEGER(i_std), INTENT(IN),OPTIONAL :: arg_nbp_glo
    iim_g=arg_nbp_lon
    jjm_g=arg_nbp_lat
    IF (PRESENT(arg_nbp_glo)) nbp_glo=arg_nbp_glo
  END SUBROUTINE grid_set_glo
!!  =============================================================================================================================
!! FUNCTION grid_set/allocate_glo
!!
!>\BRIEF	 subroutines to allocate variables present on all procs
!!
!! DESCRIPTION:	  
!!                
!!
!!              
!!
!! \n
!_ ==============================================================================================================================
!!
  SUBROUTINE grid_allocate_glo(nbseg)
    !
    IMPLICIT NONE
    ! 0.1 input  !
    !
    ! Domain size
    INTEGER(i_std), INTENT(in)                                 :: nbseg    !! number of segments of the polygone of the mesh
    !
    ! In case the allocation of the grid is called before the initialisation,
    ! we already set the number of segments.
    ! This will be done properly in grid_init.
    !
    IF ( NbSegments < 3 ) THEN
       NbSegments = nbseg
       NbNeighb=2*NbSegments
    ENDIF
    !
    !
    ALLOCATE(neighbours_g(nbp_glo,NbNeighb))
    ALLOCATE(headings_g(nbp_glo,NbNeighb))
    ALLOCATE(seglength_g(nbp_glo,NbSegments))
    ALLOCATE(corners_g(nbp_glo,NbSegments,2))
    ALLOCATE(area_g(nbp_glo))
    !
    ! TEMPORARY
    !
    ALLOCATE(resolution_g(nbp_glo,2))
    !
    ! Allocate other variables
    !
    ALLOCATE(lalo_g(nbp_glo,2), contfrac_g(nbp_glo),index_g(nbp_glo))
    ALLOCATE(lon_g(iim_g, jjm_g), lat_g(iim_g, jjm_g), zlev_g(iim_g, jjm_g))
    !
  END SUBROUTINE grid_allocate_glo
!!
!!  =============================================================================================================================
!! SUBROUTINE:    grid_stuff
!!
!>\BRIEF	  transfers the global horizontal grid information to ORCHIDEE in the case of grid regular in Longitude
!!                and Latitude.
!!
!! DESCRIPTION:	  
!!                
!!
!!                This subroutine is called by intersurf_main_2d or any driver of the model.
!!
!! \n
!_ ==============================================================================================================================
!!
  SUBROUTINE grid_stuff (npts_glo, iim, jjm, grid_lon, grid_lat, kindex, contfrac_tmp)
    !
    ! 0 interface
    !
    IMPLICIT NONE
    !
    ! 0.1 input  !
    
    ! Domain size
    INTEGER(i_std), INTENT(in)                                 :: npts_glo
    ! Size of cartesian grid
    INTEGER(i_std), INTENT(in)                                 :: iim, jjm
    ! Longitudes on cartesian grid
    REAL(r_std), DIMENSION(iim,jjm), INTENT(in)                :: grid_lon
    ! Latitudes on cartesian grid
    REAL(r_std), DIMENSION(iim,jjm), INTENT(in)                :: grid_lat
    ! Index of land point on 2D map (in local position)
    INTEGER(i_std), DIMENSION(npts_glo), INTENT(in)            :: kindex
    ! The fraction of continent in the grid box [0-1]
    REAL(r_std), DIMENSION(npts_glo), OPTIONAL, INTENT(in)     :: contfrac_tmp
    !
    !
    ! =========================================================================
    
    IF ( printlev >= 4 ) WRITE(numout,*) 'Entering grid_stuff'

    ! default resolution
    IF ( printlev >=2 ) WRITE(numout,*) 'grid stuff: default resolution (m): ',default_resolution
    !
    !-
    IF (is_root_prc) THEN
       !
       CALL grid_topolylist(GridType, NbSegments, npts_glo, iim, jjm, grid_lon, grid_lat, kindex, &
            &               global, corners_g, neighbours_g, headings_g, seglength_g, area_g, ilandindex, jlandindex)
       !
       IF (PRESENT(contfrac_tmp)) THEN
          !
          ! Transfer the contfrac into the array managed in this module. 
          !
          contfrac_g(:) = contfrac_tmp(:)
       ENDIF
       !
    ENDIF
    !
    ! With this the description of the grid is complete and the information
    ! can be scattered to all processors.
    !
    CALL grid_scatter()
    !
    CALL bcast(neighbours_g)
    CALL bcast(resolution_g)
    !
    IF ( printlev >= 3 ) WRITE(numout,*) 'Leaving grid_stuff'
    
  END SUBROUTINE grid_stuff
!!
!!  =============================================================================================================================
!! SUBROUTINE:    grid_topolylist
!!
!>\BRIEF	  This routine transforms a regular grid into a list of polygons which are defined by the following 
!!                quantities :
!!
!!                corners : the n vertices of the polugon in longitude and latitude
!!                neighbours : the neighbouring land grid box for each of the vertices and segments
!!                headings : the direction in which the neighbour is
!!                seglength : the lenght of each segment
!!                area : the area of the polygon
!!                ilindex, jlindex : provides the i,j coordinates of the mesh in the global grid.
!!
!! DESCRIPTION:	  
!!
!! \n
!_ ==============================================================================================================================
!!
  SUBROUTINE grid_topolylist(gtype, nbseg, nland, iim, jjm, grid_lon, grid_lat, kindex, &
       &                     globalg, corners_loc, neighbours_loc, headings_loc, seglength_loc, &
       &                     area_loc, ilindex_loc, jlindex_loc)
    !
    ! 0 interface
    !
    IMPLICIT NONE
    !
    ! 0.1 input  !
    ! Grid type
    CHARACTER(LEN=20), INTENT(in)                        :: gtype
    ! Number of segments for each polygon
    INTEGER(i_std), INTENT(in)                           :: nbseg
    ! Number of land points on the grid
    INTEGER(i_std), INTENT(in)                           :: nland
    ! Size of cartesian grid
    INTEGER(i_std), INTENT(in)                           :: iim, jjm
    ! Longitudes on cartesian grid
    REAL(r_std), DIMENSION(iim,jjm), INTENT(in)          :: grid_lon
    ! Latitudes on cartesian grid
    REAL(r_std), DIMENSION(iim,jjm), INTENT(in)          :: grid_lat
    ! Index of land point on 2D map (in local position)
    INTEGER(i_std), DIMENSION(nland), INTENT(in)         :: kindex
    !
    ! 0.2 Output
    !
    LOGICAL, INTENT(out)                                  :: globalg
    !
    REAL(r_std), DIMENSION(nland,nbseg,2), INTENT(out)    :: corners_loc
    INTEGER(i_std), DIMENSION(nland,nbseg*2), INTENT(out) :: neighbours_loc
    REAL(r_std), DIMENSION(nland,nbseg*2), INTENT(out)    :: headings_loc
    REAL(r_std), DIMENSION(nland,nbseg), INTENT(out)      :: seglength_loc
    REAL(r_std), DIMENSION(nland), INTENT(out)            :: area_loc
    INTEGER(i_std), DIMENSION(nland), INTENT(out)         :: ilindex_loc, jlindex_loc
    !
    ! 0.3 Local variables
    !
    INTEGER(i_std)                        :: i, is, iss
    REAL(r_std), DIMENSION(nland,2)       :: center
    REAL(r_std)                           :: maxdellon, mindellon, maxlon, minlon
    REAL(r_std), DIMENSION(nland,nbseg*2) :: lonpoly, latpoly
    !
    IF ( INDEX(gtype,"RegLonLat") > 0 ) THEN
       !
       ! If we are in regular Lon Lat, then we test just the longitude and see if we span 0-360deg.
       !
       maxdellon=MAXVAL(ABS(grid_lon(1:iim-1,1)-grid_lon(2:iim,1)))
       mindellon=MINVAL(ABS(grid_lon(1:iim-1,1)-grid_lon(2:iim,1)))
       maxlon=MAXVAL(grid_lon(1:iim,1))
       minlon=MINVAL(grid_lon(1:iim,1))
       !
       ! test if it could be a global grid on 0 -> 360
       !
       IF ( minlon > 0 .AND. maxlon > 180 ) THEN
          IF ( (minlon - maxdellon/2.0 ) <= 0 .AND. (maxlon + maxdellon/2.0) >= 360) THEN
             globalg = .TRUE.
          ELSE
             globalg = .FALSE.
          ENDIF
       !
       ! Test if it could be a -180 to 180 grid
       !
       ELSE IF ( minlon < 0 .AND. maxlon > 0 ) THEN
          IF ( (minlon - maxdellon/2.0 ) <= -180 .AND. (maxlon + maxdellon/2.0) >= 180) THEN
             globalg = .TRUE.
          ELSE
             globalg = .FALSE.
          ENDIF
       !
       ! If neither condition is met then it cannot be global.
       !
       ELSE
          globalg = .FALSE.
       ENDIF
    ELSE IF ( gtype == "RegXY" ) THEN
       !
       ! The hypothesis is that if we are in RegXY then we are not global
       !
       globalg = .FALSE.
    ELSE
       STOP "Unknown grid"
    ENDIF
    !
    ! 2.0 Transform the grid into a list of polygones while keeping the neighbour relations
    !     between these polygones.
    !
    !     Each polygone starts with a vertex and alternates vertices and mid-points of segments.
    !
    IF (nland == 1) THEN
       CALL haversine_singlepointploy(iim, jjm, grid_lon, grid_lat, nland, kindex, global, &
     &                              nbseg, lonpoly, latpoly, center, &
     &                              neighbours_loc, ilindex_loc, jlindex_loc)
    ELSE IF ( INDEX(gtype, "RegLonLat") > 0 ) THEN
       CALL haversine_reglatlontoploy(iim, jjm, grid_lon, grid_lat, nland, kindex, global, &
     &                              nbseg, lonpoly, latpoly, center, &
     &                              neighbours_loc, ilindex_loc, jlindex_loc)
    ELSE IF ( INDEX(gtype, "RegXY") > 0 ) THEN
       CALL haversine_regxytoploy(iim, jjm, grid_lon, grid_lat, nland, kindex, proj_stack, &
     &                              nbseg, lonpoly, latpoly, center, &
     &                              neighbours_loc, ilindex_loc, jlindex_loc)
    ELSE
       STOP "Unknown grid"
    ENDIF
    !
    ! Save the longitude and latitudes nbseg corners (=vertices) of the polygones
    !
    DO i=1,nland
       DO is=1,nbseg
          iss=(is-1)*2+1
          corners_loc(i,is,1) = lonpoly(i,iss)
          corners_loc(i,is,2) = latpoly(i,iss)
       ENDDO
    ENDDO
    !
    ! Get the heading normal to the 4 segments and through the 4 corners.
    ! 
    CALL haversine_polyheadings(nland, nbseg, lonpoly, latpoly, center, headings_loc)
    !
    ! Order the points of the polygone in clockwise order Starting with the northern most
    !
    CALL haversine_polysort(nland, nbseg, lonpoly, latpoly, headings_loc, neighbours_loc)
    !
    ! Compute the segment length and area.
    ! For the RegLonLat we have specific calculations for seglength and area.
    ! For projected regular grids we use the great cicle assumption for the segments
    ! but the projected area.
    ! For unstructured grid we use the most general routines.
    !
    IF ( INDEX(gtype, "RegLonLat") > 0 ) THEN
       CALL haversine_laloseglen(nland, nbseg, lonpoly, latpoly, seglength_loc)
       CALL haversine_laloarea(nland, nbseg, seglength_loc, area_loc)
    ELSE IF ( INDEX(gtype, "RegXY") > 0 ) THEN
       CALL haversine_polyseglen(nland, nbseg, lonpoly, latpoly, seglength_loc)
       CALL haversine_xyarea(nland, nbseg, ilindex_loc, jlindex_loc, dxwrf, dywrf, area_loc)
    ELSE
       CALL haversine_polyseglen(nland, nbseg, lonpoly, latpoly, seglength_loc)
       CALL haversine_polyarea(nland, nbseg, lonpoly, latpoly, area_loc)
    ENDIF
    ! Compute the area

    !
  END SUBROUTINE grid_topolylist
  !!
!!
!!
!!  =============================================================================================================================
!! SUBROUTINE:    grid_scatter
!!
!>\BRIEF	  Scatter the grid information so that each processor knows the characteristics of the grid it works on.
!!
!! DESCRIPTION:	  
!!                
!!
!!                The grid information has been computed for the entire grid on the root processor. Now we give each processor
!!                the information of the piece of the grid it works on. This concerns the following variables describing the grid :
!!                - area
!!                - resolution
!!                - neighbours
!!                - contfrac    : fraction of continent
!!
!!                Should ilandindex and jlandindex not b initialized, we catch-up here. This field is the same on all processors.
!!
!!                TODO :
!!                This code should get the grid describing fields as arguments and then writem into the *_g variables on
!!                root_prc before scattering. This would allow to compute the grid characteristics in any subroutine
!!                fore calling grid_scatter.
!!
!!      
!!
!! \n
!_ ==============================================================================================================================
!!
!!
  SUBROUTINE grid_scatter()
    !
    !
    INTEGER(i_std)  :: i, ip, jp
    !
    IF ( MAXVAL(ilandindex) < 0 .AND. MAXVAL(jlandindex) < 0 ) THEN
       DO i = 1, nbp_glo          
          !
          ! 1 find numbers of the latitude and longitude of each point
          !
          
          ! index of latitude
          jp = INT( (index_g(i)-1) /iim_g ) + 1
          
          ! index of longitude
          ip = index_g(i) - ( jp-1 ) * iim_g
          !
          ! Save this information for usage in other modules.
          !
          ilandindex(i)=ip
          jlandindex(i)=jp
          !
       ENDDO       
    ENDIF
    !
    CALL scatter(neighbours_g, neighbours)
    CALL scatter(contfrac_g, contfrac)
    CALL scatter(headings_g, headings)
    CALL scatter(seglength_g, seglength)
    CALL scatter(corners_g, corners)
    CALL scatter(area_g, area)
    !
    ! TEMPORARY section for resolution
    !
    IF ( is_root_prc) THEN
       IF ( INDEX(GridType,"Reg") > 0 ) THEN
          resolution_g(:,1) = (seglength_g(:,1)+seglength_g(:,3))/2.0
          resolution_g(:,2) = (seglength_g(:,2)+seglength_g(:,4))/2.0
       ELSE
          CALL ipslerr(3, "grid_scatter", "unsupported grid type.",&
               &       "As long as resolution has not been replaced,",&
               &       "ORCHIDEE cannot run on anything other than regular grids.")
       ENDIF
    ENDIF
    CALL scatter(resolution_g, resolution)

    !
    !
    IF ( printlev >=4 ) THEN
       WRITE(numout,*) 'grid_scatter  > seglength  = ', seglength(1,:)
       WRITE(numout,*) 'grid_scatter  > neighbours  = ', neighbours(1,:)
       WRITE(numout,*) 'grid_scatter  > contfrac  = ', contfrac(1)
       WRITE(numout,*) 'grid_scatter  > area  = ', area(1)
    ENDIF
    !
  END SUBROUTINE grid_scatter
!!
!!
!!  =============================================================================================================================
!! SUBROUTINE:    grid_initproj
!!
!>\BRIEF	  Routine to initialise the projection
!!
!! DESCRIPTION:	  
!!                
!!
!!                This subroutine is called by the routine whichs ets-up th grid on which ORCHIDEE is to run.
!!                The aim is to set-upu the projection so that all the grid variables needed by ORCHIDEE can
!!                be computed in grid_stuff_regxy
!!
!! \n
!_ ==============================================================================================================================
!!
!!
  SUBROUTINE grid_initproj (fid, iim, jjm)
    !
    !
    ! 0 interface
    !
    IMPLICIT NONE
    !
    ! 0.1 input  !
    !
    ! Domain size
    INTEGER(i_std), INTENT(in)                                  :: fid
    INTEGER(i_std), INTENT(in)                                  :: iim, jjm
    !
    ! 0.2 Local variables
    !
    INTEGER(i_std)             :: current_proj, idom, iret, lonid, latid, numLons, numLats
    INTEGER, DIMENSION(nf90_max_var_dims) :: dimIDs
    REAL(r_std)                :: user_stand_lon, user_truelat1, user_truelat2, user_dxkm, user_dykm
    REAL(r_std)                :: user_dlat, user_dlon, user_known_x, user_known_y, user_known_lat, user_known_lon
    REAL(r_std), DIMENSION(16) :: corner_lons, corner_lats
    !
    INTEGER(i_std)             :: iv, i, j
    CHARACTER(LEN=20)          :: varname
    REAL(r_std)                :: dx, dy, dtx, dty, coslat
    REAL(r_std), ALLOCATABLE, DIMENSION (:)    :: LON, LAT
    REAL(r_std), ALLOCATABLE, DIMENSION (:,:)  :: mapfac_x, mapfac_y
    !
    !
    ! Only one domain is possible for the moment
    !
    idom=1
    CALL map_init(proj_stack(idom))
    !
    ! Does ORCHIDEE have the same Earth Radius as the map projection ?
    !
    IF ( ABS(R_Earth-EARTH_RADIUS_M) > 0.1 ) THEN
       WRITE(*,*) "Earth Radius in WRF : ", EARTH_RADIUS_M
       WRITE(*,*) "Earth Radius in ORCHIDEE : ", R_Earth
       CALL ipslerr (3,'grid_initproj','The Earth radius is not the same in the projection module and ORCHIDEE',&
            & " ", " ")
    ENDIF
    !
    ! Get parameters of the projection from the netCDF file
    !
    iret = NF90_GET_ATT(fid, NF90_GLOBAL, "MAP_PROJ", current_proj)
    !
    iret = NF90_GET_ATT(fid, NF90_GLOBAL, "STAND_LON",  user_stand_lon)
    iret = NF90_GET_ATT(fid, NF90_GLOBAL, "TRUELAT1", user_truelat1)
    iret = NF90_GET_ATT(fid, NF90_GLOBAL, "TRUELAT2", user_truelat2)
    !
    iret = NF90_GET_ATT(fid, NF90_GLOBAL, "DX", user_dxkm)
    iret = NF90_GET_ATT(fid, NF90_GLOBAL, "DY", user_dykm)
    user_dlat = undef
    user_dlon = undef
    !
    IF ( current_proj == PROJ_LATLON ) THEN
       !
       iret = NF90_inq_VARID(fid, "XLONG_M",lonid)
       iret = NF90_INQUIRE_VARIABLE(fid, lonid, dimids = dimIDs)
       iret = NF90_INQUIRE_DIMENSION(fid, dimIDs(1), len = numLons)
       iret = NF90_INQUIRE_DIMENSION(fid, dimIDs(2), len = numLats)
       ALLOCATE(LON(numLons))
       iret = NF90_GET_VAR(fid, lonid, LON(:), start = (/ 1, 1, 1 /), count = (/ numLons, 1, 1 /))
       
       iret = NF90_inq_VARID(fid, "XLAT_M",latid)
       ALLOCATE(LAT(numLats))
       iret = NF90_GET_VAR(fid, latid, LAT(:), start = (/ 1, 1, 1 /), count = (/ 1, numLats, 1 /))
       
       user_dlon = (LON(numLons) - LON(1)) / (numLons - 1)
       user_dlat = (LAT(numLats) - LAT(1)) / (numLats - 1)
       
       DEALLOCATE(LON,LAT)

    ENDIF
    ! Unable to know from where to get the information
    user_known_x = 1
    user_known_y = 1
    !
    iret = NF90_GET_ATT(fid, NF90_GLOBAL, "corner_lats", corner_lats)
    iret = NF90_GET_ATT(fid, NF90_GLOBAL, "corner_lons", corner_lons)
    user_known_lat = corner_lats(1)
    user_known_lon = corner_lons(1)
    !
    ! Read mapfactor, land mask and orography
    !
    !
    ! Allocation
    !
    ALLOCATE(mapfac_x(iim,jjm))
    ALLOCATE(mapfac_y(iim,jjm))
    ALLOCATE(dxwrf(iim,jjm))
    ALLOCATE(dywrf(iim,jjm))
    !
    varname = "MAPFAC_MX"
    iret = NF90_INQ_VARID (fid, varname, iv)
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr (3,'WRFdomain_Read',"Could not find variable ", varname," ")
    ELSE
       iret = NF90_GET_VAR (fid,iv,mapfac_x)
    ENDIF
    varname = "MAPFAC_MY"
    iret = NF90_INQ_VARID (fid, varname, iv)
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr (3,'WRFdomain_Read',"Could not find variable ", varname," ")
    ELSE
       iret = NF90_GET_VAR (fid,iv,mapfac_y)
    ENDIF
    !
    ! Initilize the projection
    !
    if (current_proj == PROJ_LATLON) then
       call map_set(current_proj, proj_stack(idom), &
            lat1=user_known_lat, &
            lon1=user_known_lon, &
            knowni=user_known_x, &
            knownj=user_known_y, &
            latinc=user_dlat, &
            loninc=user_dlon, &
            r_earth=R_Earth)
       
    else if (current_proj == PROJ_MERC) then
       call map_set(current_proj, proj_stack(idom), &
            truelat1=user_truelat1, &
            lat1=user_known_lat, &
            lon1=user_known_lon, &
            knowni=user_known_x, &
            knownj=user_known_y, &
            dx=user_dxkm, &
            r_earth=R_Earth)
       
    else if (current_proj == PROJ_CYL) then
       call ipslerr(3,"grid_initproj",'Should not have PROJ_CYL as projection for',&
            'source data in push_source_projection()', " ")
       
    else if (current_proj == PROJ_CASSINI) then
       call ipslerr(3,"grid_initproj",'Should not have PROJ_CASSINI as projection for', &
            'source data in push_source_projection()', " ")
       
    else if (current_proj == PROJ_LC) then
       call map_set(current_proj, proj_stack(idom), &
            truelat1=user_truelat1, &
            truelat2=user_truelat2, &
            stdlon=user_stand_lon, &
            lat1=user_known_lat, &
            lon1=user_known_lon, &
            knowni=user_known_x, &
            knownj=user_known_y, &
            dx=user_dxkm, &
            r_earth=R_Earth)
       
    else if (current_proj == PROJ_ALBERS_NAD83) then
       call map_set(current_proj, proj_stack(idom), &
            truelat1=user_truelat1, &
            truelat2=user_truelat2, &
            stdlon=user_stand_lon, &
            lat1=user_known_lat, &
            lon1=user_known_lon, &
            knowni=user_known_x, &
            knownj=user_known_y, &
            dx=user_dxkm, &
            r_earth=R_Earth)
       
    else if (current_proj == PROJ_PS) then
       call map_set(current_proj, proj_stack(idom), &
            truelat1=user_truelat1, &
            stdlon=user_stand_lon, &
            lat1=user_known_lat, &
            lon1=user_known_lon, &
            knowni=user_known_x, &
            knownj=user_known_y, &
            dx=user_dxkm, &
            r_earth=R_Earth)
       
    else if (current_proj == PROJ_PS_WGS84) then
       call map_set(current_proj, proj_stack(idom), &
            truelat1=user_truelat1, &
            stdlon=user_stand_lon, &
            lat1=user_known_lat, &
            lon1=user_known_lon, &
            knowni=user_known_x, &
            knownj=user_known_y, &
            dx=user_dxkm, &
            r_earth=R_Earth)
       
    else if (current_proj == PROJ_GAUSS) then
       call map_set(current_proj, proj_stack(idom), &
            lat1=user_known_lat, &
            lon1=user_known_lon, &
            nlat=nint(user_dlat), &
            loninc=user_dlon, &
            r_earth=R_Earth)
       
    else if (current_proj == PROJ_ROTLL) then
       call ipslerr(3 ,"grid_initproj",'Should not have PROJ_ROTLL as projection for', &
            'source data in push_source_projection() as not yet implemented', '')
    end if
    !
    ! Transform the mapfactors into dx and dy to be used for the description of the polygons and
    ! interpolations.
    !
    DO i=1,iim
       DO j=1,jjm
          !
          IF (proj_stack(idom)%code /= PROJ_LATLON ) THEN
             dx = proj_stack(idom)%dx
             ! Some projections in WRF do not store dy, in that case dy=dx.
             IF ( proj_stack(idom)%dy > 0 ) THEN
                dy = proj_stack(idom)%dy
             ELSE
                dy = proj_stack(idom)%dx
             ENDIF
             dxwrf(i,j) = dx/mapfac_x(i,j)
             dywrf(i,j) = dy/mapfac_y(i,j)
          ELSE
             !
             ! The LatLon projection is also a special case as here it is not the dx and dy
             ! which are stored in the projection file but the increments in Lon and Lat.
             !
             dtx = proj_stack(idom)%loninc
             dty = proj_stack(idom)%latinc
             coslat = COS(lat(j) * pi/180. )
             dxwrf(i,j) = dtx * pi/180. * R_Earth * coslat
             dywrf(i,j) = dty * pi/180. * R_Earth
             !
          ENDIF
          !
       ENDDO
    ENDDO
    !
  END SUBROUTINE grid_initproj
!
!
!
!=========================================================================================
!
  SUBROUTINE grid_tolola_scal (ri, rj, lon, lat)
    !
    !
    ! Argument
    REAL(r_std), INTENT(in)      :: ri, rj
    REAL(r_std), INTENT(out)     :: lon, lat
    !
    !
    IF ( proj_stack(1)%code < undef_int ) THEN
       !
       CALL ij_to_latlon(proj_stack(1), ri, rj, lat, lon)
       !
    ELSE
       CALL ipslerr(3, "grid_tolola_scal", "Projection not initilized"," "," ")
    ENDIF
    !
  END SUBROUTINE grid_tolola_scal
!
!=========================================================================================
!
  SUBROUTINE grid_tolola_1d (ri, rj, lon, lat)
    !
    !
    ! Argument
    REAL(r_std), INTENT(in), DIMENSION(:)      :: ri, rj
    REAL(r_std), INTENT(out), DIMENSION(:)     :: lon, lat
    !
    ! Local
    INTEGER                            :: i, imax
    !
    imax=SIZE(lon)
    !
    IF ( proj_stack(1)%code < undef_int ) THEN
       DO i=1,imax
          !
          CALL ij_to_latlon(proj_stack(1), ri(i), rj(i), lat(i), lon(i))
          !
       ENDDO
    ELSE
       CALL ipslerr(3, "grid_tolola_1d", "Projection not initilized"," "," ")
    ENDIF
    !
  END SUBROUTINE grid_tolola_1d
!
!=========================================================================================
!
  SUBROUTINE grid_tolola_2d (ri, rj, lon, lat)
    !
    !
    ! Argument
    REAL(r_std), INTENT(in), DIMENSION(:,:)      :: ri, rj
    REAL(r_std), INTENT(out), DIMENSION(:,:)     :: lon, lat
    !
    ! Local
    INTEGER                            :: i, imax, j, jmax
    !
    imax=SIZE(lon,DIM=1)
    jmax=SIZE(lon,DIM=2)
    !
    IF ( proj_stack(1)%code < undef_int ) THEN
       DO i=1,imax
          DO j=1,jmax
             !
             CALL ij_to_latlon(proj_stack(1), ri(i,j), rj(i,j), lat(i,j), lon(i,j))
             !
          ENDDO
       ENDDO
    ELSE
       CALL ipslerr(3, "grid_tolola_2d", "Projection not initilized"," "," ")
    ENDIF
    !
  END SUBROUTINE grid_tolola_2d
!
!=========================================================================================
!
  SUBROUTINE grid_toij_scal (lon, lat, ri, rj)
    !
    !
    ! Argument
    REAL(r_std), INTENT(in)     :: lon, lat
    REAL(r_std), INTENT(out)    :: ri, rj
    !
    !
    IF ( proj_stack(1)%code < undef_int ) THEN
       !
       CALL latlon_to_ij(proj_stack(1), lat, lon, ri, rj)
       !
    ELSE
       CALL ipslerr(3, "grid_toij_scal", "Projection not initilized"," "," ")
    ENDIF
    !
  END SUBROUTINE grid_toij_scal
!
!=========================================================================================
!
  SUBROUTINE grid_toij_1d (lon, lat, ri, rj)
    !
    !
    ! Argument
    REAL(r_std), INTENT(in), DIMENSION(:)     :: lon, lat
    REAL(r_std), INTENT(out), DIMENSION(:)    :: ri, rj
    !
    ! Local
    INTEGER                            :: i, imax
    !
    imax=SIZE(lon)
    !
    IF ( proj_stack(1)%code < undef_int ) THEN
       DO i=1,imax
          !
          CALL latlon_to_ij(proj_stack(1), lat(i), lon(i), ri(i), rj(i))
          !
       ENDDO
    ELSE
       CALL ipslerr(3, "grid_toij_1d", "Projection not initilized"," "," ")
    ENDIF
    !
  END SUBROUTINE grid_toij_1d
!
!=========================================================================================
!
  SUBROUTINE grid_toij_2d (lon, lat, ri, rj)
    !
    !
    ! Argument
    REAL(r_std), INTENT(in), DIMENSION(:,:)     :: lon, lat
    REAL(r_std), INTENT(out), DIMENSION(:,:)    :: ri, rj
    !
    ! Local
    INTEGER                            :: i, imax, j, jmax
    !
    imax=SIZE(lon,DIM=1)
    jmax=SIZE(lon,DIM=2)
    !
    IF ( proj_stack(1)%code < undef_int ) THEN
       DO i=1,imax
          DO j=1,jmax
             !
             CALL latlon_to_ij(proj_stack(1), lat(i,j), lon(i,j), ri(i,j), rj(i,j))
             !
          ENDDO
       ENDDO
    ELSE
       CALL ipslerr(3, "grid_toij_2d", "Projection not initilized"," "," ")
    ENDIF
    !
  END SUBROUTINE grid_toij_2d
!
!
!=========================================================================================
!
!
END MODULE grid