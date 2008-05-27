module PotentialModule
   use NumberKinds
   use OutputWriterModule
   use LoggerModule
   implicit none
  
   private
   public Potential, New, Delete, assignment(=), OutputProperties
   public SetPotentialForm, GetPotentialForm, ValueAtCoord
   public HARMONIC_POTENTIAL, MORSE_POTENTIAL, LJ_POTENTIAL
   save
  
   type Potential
      private
      integer(KINT)                                     :: potentialForm  
   end type
   
   ! Overloaded procedure interfaces
   interface New               
      module procedure NewPrivate
   end interface
     
   interface Delete               
      module procedure DeletePrivate
   end interface
  
   interface OutputProperties  
      module procedure OutputPropertiesPrivate
   end interface

   ! Definitions
   integer(KINT), parameter                             :: HARMONIC_POTENTIAL  = 1
   integer(KINT), parameter                             :: MORSE_POTENTIAL     = 2
   integer(KINT), parameter                             :: LJ_POTENTIAL        = 3

   ! Logging
   type (Logger)                                        :: log
   integer(KINT), parameter                             :: LOGGING_LEVEL = WARNING_LOGGING_LEVEL

contains

   ! ------------------------
   ! Logging.
   ! ------------------------
   subroutine InitLogger()
      ! This routine is called from the constructor to initialize the Logger.
      logical, save                                     :: loggerInitialized = .false.
      if ( .not. loggerInitialized ) then
         call New(log, LOGGING_LEVEL)
         loggerInitialized = .true.
      end if
   end subroutine

   ! ------------------------
   ! Standard ADT Methods. Construction, Destruction, Copying, and Assignment.
   ! ------------------------
   subroutine NewPrivate(self,form)
      type (Potential), intent(out)                     :: self
      integer(KINT), intent(in)                         :: form
      self%potentialForm = form
      call InitLogger
   end subroutine
  
   subroutine DeletePrivate(self)
      type (Potential), intent(inout)                   :: self
   end subroutine
  
   ! ------------------------
   ! Accessors.
   ! ------------------------
   subroutine SetPotentialForm(self, form)
      type (Potential), intent(inout)                   :: self
      integer(KINT), intent(in)                         :: form
      self%potentialForm = form
   end subroutine

   function GetPotentialForm(self)
      type (Potential), intent(in)                      :: self
      integer(KINT)                                     :: GetPotentialForm
      GetPotentialForm = self%potentialForm
   end function

   ! ------------------------
   ! Other methods.
   ! ------------------------
   function ValueAtCoord(self, coord)
      type (Potential), intent(in)                      :: self
      real(KREAL), intent(in)                           :: coord
      real(KREAL)                                       :: valueAtCoord
      call LogMessage(log, TRACE_LOGGING_LEVEL, 'Entered ValueAtCoord')
    
      select case (self%potentialForm)
      case (HARMONIC_POTENTIAL)
         valueAtCoord = (coord-1)**2
         call LogMessage(log, DEBUG_LOGGING_LEVEL, 'Harmonic potential with value', valueAtCoord)
      case (MORSE_POTENTIAL)
         valueAtCoord = (1-exp(-coord+1))**2
         call LogMessage(log, DEBUG_LOGGING_LEVEL, 'Morse potential with value:', valueAtCoord)
      case (LJ_POTENTIAL) 
         valueAtCoord = 4*(1/(coord**12) - 1/(coord**6))
         call LogMessage(log, DEBUG_LOGGING_LEVEL, '12/6 Lennard Jones potential with value:', valueAtCoord)
      case default
         stop 'Invalid potential form'
      end select
    
      call LogMessage(log, TRACE_LOGGING_LEVEL, 'Exiting ValueAtCoord')
   end function
  
   ! ------------------------
   ! Output properties.
   ! ------------------------
   subroutine OutputPropertiesPrivate(self, writer)
      type (Potential), intent(in)                      :: self
      type (OutputWriter), intent(inout)                :: writer
 
      select case (self%potentialForm)
      case (HARMONIC_POTENTIAL)
         call Write(writer, 'Potential Type', 'Harmonic')
      case (MORSE_POTENTIAL)
         call Write(writer, 'Potential Type', 'Morse')
      case (LJ_POTENTIAL)
         call Write(writer, 'Potential Type', 'Lennard-Jones')
      case default
         stop 'Invalid potential form'
      end select

   end subroutine

end module


