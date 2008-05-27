module TestRunnerModule
   use NumberKinds
   use ParticleEnsembleTests
   use PotentialTests
   use HistogramBuilderTests
   use GaussianDistributorTests
   use InputReaderTests
   implicit none
   
   private
   public TestRunner, New, Delete, Run
   save

   type TestRunner
      private
      integer(KINT)                                     :: dummy  
   end type

   ! Overloaded procedure interfaces
   interface New               
      module procedure NewPrivate               
   end interface
  
   interface Delete            
      module procedure DeletePrivate
   end interface
  
contains

   ! ------------------------
   ! Standard ADT Methods. Construction, Destruction, Copying, and Assignment.
   ! ------------------------
   subroutine NewPrivate(self)
      type (TestRunner), intent(out)                    :: self
      self%dummy = 0 ! This stops compiler issuing a warning
   end subroutine
  
   subroutine DeletePrivate(self)
      type (TestRunner), intent(inout)                  :: self
   end subroutine

   ! ------------------------
   ! Other methods.
   ! ------------------------
   subroutine Run(self)
      type (TestRunner), intent(inout)                  :: self

      print *,'Running Tests'
      call RunParticleEnsembleTests()
      call RunPotentialTests()
      call RunHistogramBuilderTests()
      call RunGaussianDistributorTests()
      call RunInputReaderTests()
      print *

   end subroutine
  
end module


