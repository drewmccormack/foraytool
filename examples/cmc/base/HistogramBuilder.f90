module HistogramBuilderModule
   use NumberKinds
   use OutputWriterModule
   use LoggerModule
   implicit none
   
   private
   public HistogramBuilder, New, Delete, Assignment(=), OutputProperties
   public GetLowerBound, GetUpperBound, GetNumberOfBins, CreateHistogram
   public GetBinCenters, GetBinWidth
   save

   type HistogramBuilder
      private
      real (KREAL)                                      :: lowerBound,upperBound,delta
      integer (KINT)                                    :: numberOfBins
   end type

   ! Overloaded procedure interfaces
   interface New               
      module procedure NewPrivate, NewCopyPrivate
   end interface
   
   interface Delete               
      module procedure DeletePrivate
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
   subroutine NewPrivate(self, lowerBound, upperBound, numberOfBins)
      type (HistogramBuilder), intent(out)              :: self
      real (KREAL), intent(in)                          :: lowerBound, upperBound
      integer (KINT), intent(in)                        :: numberOfBins
      if ( numberOfBins <= 0 ) stop 'Too few bins in HistogramBuilder'
      self%lowerBound = lowerBound
      self%upperBound = upperBound
      self%numberOfBins = numberOfBins
      self%delta = (upperBound - lowerBound) / numberOfBins
      call InitLogger
   end subroutine
  
   subroutine NewCopyPrivate(self, other)
      type (HistogramBuilder), intent(out)              :: self
      type (HistogramBuilder), intent(in)               :: other
      self = other
   end subroutine
  
   subroutine DeletePrivate(self)
      type (HistogramBuilder), intent(inout)            :: self
   end subroutine
  
   ! ------------------------
   ! Accessors.
   ! ------------------------
   function GetLowerBound(self)
      type (HistogramBuilder), intent(in)               :: self
      real(KREAL)                                       :: GetLowerBound
      GetLowerBound = self%lowerBound
   end function

   function GetUpperBound(self)
      type (HistogramBuilder), intent(in)               :: self
      real(KREAL)                                       :: GetUpperBound
      GetUpperBound = self%upperBound
   end function

   function GetNumberOfBins(self)
      type (HistogramBuilder), intent(in)               :: self
      integer(KINT)                                     :: GetNumberOfBins
      GetNumberOfBins = self%numberOfBins
   end function
   
   function GetBinCenters(self) result(centers)
      type (HistogramBuilder), intent(in)               :: self
      real(KREAL)                                       :: centers(self%numberOfBins)
      integer(KINT)                                     :: i
      do i = 1, size(centers)
         centers(i) = self%lowerBound + GetBinWidth(self) * (i - 0.5_KREAL)
      enddo
   end function
   
   function GetBinWidth(self)
      type (HistogramBuilder), intent(in)               :: self
      real(KREAL)                                       :: GetBinWidth
      GetBinWidth = (self%upperBound - self%lowerBound) / self%numberOfBins
   end function
  
   ! ------------------------
   ! Other methods.
   ! ------------------------
   function CreateHistogram(self, values) result(histo)
      type (HistogramBuilder), intent(inout)            :: self
      real(KREAL), intent(in)                           :: values(:)
      integer(KINT)                                     :: i,dummy
      integer(KINT)                                     :: histo(self%numberOfBins)
      call LogMessage(log, TRACE_LOGGING_LEVEL, 'Entered CreateHistogram')
    
      histo = 0
      do i = 1, size(values)
         dummy = ceiling((values(i) - self%lowerBound) / self%delta)
         if ((dummy > 0) .and. (dummy <= self%numberOfBins)) then
            histo(dummy) = histo(dummy) + 1
         endif
      enddo
      
      call LogMessage(log, DEBUG_LOGGING_LEVEL, 'The array of values', histo)
      call LogMessage(log, TRACE_LOGGING_LEVEL, 'Exiting CreateHistogram')
   end function
  
   ! ------------------------
   ! Output properties.
   ! ------------------------
   subroutine OutputPropertiesPrivate(self, writer)
      type (HistogramBuilder), intent(in)               :: self
      type (OutputWriter), intent(inout)                :: writer
    
      ! Write details of ADT instance to the writer
      call Write(writer, 'Lower Bound', self%lowerBound)
      call Write(writer, 'Upper Bound', self%upperBound)
      call Write(writer, 'Number of Bins', self%numberOfBins)
  
   end subroutine
  
end module


