module GaussianDistributorTests
   use NumberKinds
   use GaussianDistributorModule
   use Testing
   implicit none
  
   private
   public RunGaussianDistributorTests
   save
  
contains
  
   subroutine RunGaussianDistributorTests()
      call TestDistributor()
   end subroutine
  
   subroutine TestDistributor()
      type (GaussianDistributor)                        :: distrib
      real(KREAL)                                       :: stddev, mean, sd, val
      integer(KINT)                                     :: i
      integer(KINT),parameter                           :: NUM_TEST_VALUES = 1000
 
      stddev = 1.0
      call New(distrib, stddev)
      call Assert( GetStandardDeviation(distrib) == stddev, 'standardDerivation was not correctly initialized' )

      ! Test mean and standard deviation
      mean = 0.0
      sd = 0.0
      do i = 1,NUM_TEST_VALUES
         val = GenerateRandomValue(distrib) 
         mean = mean + val
         sd = sd + val**2
      enddo
      call AssertAlmostEqual( mean / NUM_TEST_VALUES, 0.0_KREAL, 0.1_KREAL, 'Mean incorrect' )
      call AssertAlmostEqual( sqrt(sd / NUM_TEST_VALUES), stddev, 0.1_KREAL, 'Standard deviation incorrect' )

      call Delete(distrib)
   end subroutine

end module

