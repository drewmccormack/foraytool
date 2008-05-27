module GaussianDistributorModule
   use NumberKinds
   use OutputWriterModule
   use LoggerModule
   implicit none
  
   private 
   public GaussianDistributor
   public New, Delete, assignment(=), OutputProperties
   public GetStandardDeviation, GenerateRandomValue
   save

   type GaussianDistributor
      private
      real(KREAL)                                       :: standardDeviation 
   end type

   ! Overloaded procedure interfaces
   interface New               
      module procedure NewPrivate, NewCopyPrivate
   end interface
      
   interface Delete               
      module procedure DeletePrivate
   end interface
   
   interface assignment(=)
      module procedure AssignPrivate
   end interface
   
   interface OutputProperties  
      module procedure OutputPropertiesPrivate
   end interface

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
   subroutine NewPrivate(self, standardDeviation)
      type (GaussianDistributor), intent(out)           :: self
      real(KREAL), intent(in)                           :: standardDeviation
      self%standardDeviation = standardDeviation
      call InitLogger
   end subroutine
  
   subroutine NewCopyPrivate(self, other)
      type (GaussianDistributor), intent(out)           :: self
      type (GaussianDistributor), intent(in)            :: other
      self%standardDeviation = other%standardDeviation
   end subroutine
  
   subroutine DeletePrivate(self)
      type (GaussianDistributor), intent(inout)         :: self
   end subroutine
  
   subroutine AssignPrivate(self, other)
      type (GaussianDistributor), intent(inout)         :: self
      type (GaussianDistributor), intent(in)            :: other
      self%standardDeviation = other%standardDeviation
   end subroutine
  
   ! ------------------------
   ! Accessors.
   ! ------------------------
   subroutine SetStandardDeviation(self, standardDeviation)
      type (GaussianDistributor), intent(inout)         :: self
      real(KREAL), intent(in)                           :: standardDeviation
      self%standardDeviation = standardDeviation
   end subroutine

   function GetStandardDeviation(self)
      type (GaussianDistributor), intent(in)            :: self
      real(KREAL)                                       :: GetStandardDeviation
      GetStandardDeviation = self%standardDeviation
   end function

   ! ------------------------
   ! Other methods.
   ! ------------------------
   function GenerateRandomValue(self) result (coord)
      type (GaussianDistributor), intent(inout)         :: self
      real(KREAL)                                       :: coord,random
      real(KREAL)                                       :: gaussVal
      call LogMessage(log, TRACE_LOGGING_LEVEL, 'Entered GenerateRandomValue')
    
      random = 1.0_KREAL
      gaussVal = 0.0_KREAL
      do while ( random > gaussVal )
         ! Choose random value of coord in a 3 standard deviation range either side of zero
         call random_number(coord)
         coord = 3.0 * self%standardDeviation * (2.0 * coord - 1.0) 
   
         ! Calculate value of gaussian function (normal curve)
         gaussVal = exp(-1.0_KREAL / (2.0 * self%standardDeviation**2) * coord**2)

         ! Generate a second random number, and test it against the gaussian value
         call random_number(random)
      enddo

      call LogMessage(log, DEBUG_LOGGING_LEVEL, 'The random number is', coord)
      call LogMessage(log, TRACE_LOGGING_LEVEL, 'Exiting GenerateRandomValue')
   end function
  
   ! ------------------------
   ! Output properties.
   ! ------------------------
   subroutine OutputPropertiesPrivate(self, writer)
      type (GaussianDistributor), intent(in)            :: self
      type (OutputWriter), intent(inout)                :: writer
    
      ! Write details of ADT instance to the writer
      call Write(writer, 'Standard Deviation', self%standardDeviation)
  
   end subroutine
  
end module


