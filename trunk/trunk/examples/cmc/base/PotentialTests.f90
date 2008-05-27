module PotentialTests
   use NumberKinds
   use PotentialModule
   use Testing
   implicit none
  
   private
   public RunPotentialTests
   save
  
contains
  
   subroutine RunPotentialTests
      call TestHarmonic
      call TestMorse
      call TestLJ
   end subroutine
  
   subroutine TestHarmonic
      type (Potential)                                  :: pot
      integer (KINT)                                    :: potType = HARMONIC_POTENTIAL
      real (KREAL)                                      :: coord
      call New(pot,potType)
      call Assert( GetPotentialForm(pot) == HARMONIC_POTENTIAL, 'potType was not correctly initialized' )
      call AssertAlmostEqual( valueAtCoord(pot,3.0_KREAL), 4.0_KREAL, 1.e-6_KREAL, 'valueAtCoord gave wrong value')
      call Delete(pot)
   end subroutine
   
   subroutine TestMorse
      type (Potential)                                  :: pot
      integer (KINT)                                    :: potType = MORSE_POTENTIAL
      call New(pot,potType)
      call Assert( GetPotentialForm(pot) == MORSE_POTENTIAL, 'potType was not correctly initialized' )
      call AssertAlmostEqual( valueAtCoord(pot,100.0_KREAL), 1._KREAL, 1.e-6_KREAL, 'valueAtCoord gave wrong value')
      call Delete(pot)
   end subroutine

   subroutine TestLJ
      type (Potential)                                  :: pot
      integer (KINT)                                    :: potType = LJ_POTENTIAL
      call New(pot,potType)
      call Assert( GetPotentialForm(pot) == LJ_POTENTIAL, 'potType was not correctly initialized' )
      call AssertAlmostEqual( valueAtCoord(pot,1.0_KREAL), 0._KREAL, 1.e-6_KREAL, 'valueAtCoord gave wrong value')
      call AssertAlmostEqual( valueAtCoord(pot,100.0_KREAL), 0._KREAL, 1.e-6_KREAL, 'valueAtCoord gave wrong value')
      call Delete(pot)
   end subroutine

end module


