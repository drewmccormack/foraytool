module Testing
   use NumberKinds
   implicit none
   
   private
   public Assert, AssertAlmostEqual
   save

contains

   subroutine Assert(testResult, failureMessage)
      logical, intent(in)                               :: testResult
      character(len=*), intent(in)                      :: failureMessage
      if ( testResult ) then
         write(*,'(a)',advance='no')'.'
      else 
         print *,'Failed Test: ', failureMessage
      endif
   end subroutine

   subroutine AssertAlmostEqual(firstValue, secondValue, tolerance, failureMessage)
      real(KREAL), intent(in)                           :: firstValue, secondValue, tolerance
      character(len=*), intent(in)                      :: failureMessage
      if ( abs(firstValue - secondValue) < tolerance ) then
         write(*,'(a)',advance='no')'.'
      else 
         print *  ! New line
         print *,'--------------------------------'
         print *,'Failed Test: ', failureMessage
         print *,'First value:', firstValue
         print *,'Second value:', secondValue
         print *,'Tolerance:', tolerance
         print *,'--------------------------------'
      endif
   end subroutine

end module

