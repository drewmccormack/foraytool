module ParticleEnsembleTests
   use NumberKinds
   use ParticleEnsembleModule
   use Testing
   implicit none
  
   private 
   public RunParticleEnsembleTests
   save
  
   real(KREAL), parameter                               :: SMALL = 1e-12_KREAL

contains
  
   subroutine RunParticleEnsembleTests()
      call TestConstruction ()
      call TestAddParticles ()
      call TestAddManyParticles ()
      call TestRemoveAllParticles ()
      call TestCopyConstruction ()
      call TestAssignment ()
   end subroutine
  
   subroutine TestConstruction()
      type (ParticleEnsemble)                           :: pe
      call New(pe, 1.0_KREAL)
      call AssertAlmostEqual( GetParticleMass(pe), 1.0_KREAL, SMALL, &
         'ParticleEnsemble: new: particleMass was not correctly initialized to 1.0' )
      call Assert( GetNumberOfParticles(pe) == 0, &
         'ParticleEnsemble: new: GetNumberOfParticles was not correctly initialized to 0' )
      call Delete(pe)
   end subroutine

   subroutine TestAddParticles()
      type (ParticleEnsemble)                           :: pe
      type (Particle)                                   :: p1, p2

      call New(pe, 1.0_KREAL)
      p1%position = 5.0_KREAL

      call AddParticle(pe, p1)
      call Assert( GetNumberOfParticles(pe) == 1, &   
         'ParticleEnsemble: AddParticle 1: GetNumberOfParticles was not correctly set to 1' )
      p2%position = 0.0_KREAL
      p2 = GetParticleAtIndex(pe, 1)
      call AssertAlmostEqual( p2%position, 5.0_KREAL, SMALL, &
         'ParticleEnsemble: AddParticle 1: position of particle 1 not correct' )

      p1%position = 4.0_KREAL
      call AddParticle(pe, p1)
      call Assert( GetNumberOfParticles(pe) == 2, &
         'ParticleEnsemble: AddParticle 2: GetNumberOfParticles was not correctly set to 2' )
      p2 = GetParticleAtIndex(pe, 1)
      call AssertAlmostEqual( p2%position, 5.0_KREAL, SMALL, &
         'ParticleEnsemble: AddParticle 2: position of particle 1 not correct' )
      p2 = GetParticleAtIndex(pe, 2)
      call AssertAlmostEqual( p2%position, 4.0_KREAL, SMALL, &
         'ParticleEnsemble: AddParticle 2: position of particle 2 not correct' )

      p1%position = 3.0_KREAL
      call AddParticle(pe, p1)
      call Assert( GetNumberOfParticles(pe) == 3, &
         'ParticleEnsemble: AddParticle 3: GetNumberOfParticles was not correctly set to 3' )
      p2 = GetParticleAtIndex(pe, 1)
      call AssertAlmostEqual( p2%position, 5.0_KREAL, SMALL, &
         'ParticleEnsemble: AddParticle 3: position of particle 1 not correct' )
      p2 = GetParticleAtIndex(pe, 2)
      call AssertAlmostEqual( p2%position, 4.0_KREAL, SMALL, &
         'ParticleEnsemble: AddParticle 3: position of particle 2 not correct' )
      p2 = GetParticleAtIndex(pe, 3)
      call AssertAlmostEqual( p2%position, 3.0_KREAL, SMALL, &
         'ParticleEnsemble: AddParticle 3: position of particle 3 not correct' )

      call Delete(pe)
   end subroutine

   subroutine TestAddManyParticles()
      type (ParticleEnsemble)                           :: pe
      type (Particle)                                   :: p1, p2
      integer(KINT)                                     :: i

      call New(pe, 1.0_KREAL)

      do i = 1, 12
         p1%position = i
         call AddParticle(pe, p1)
      end do

      call Assert( GetNumberOfParticles(pe) == 12, &
         'ParticleEnsemble: AddManyParticles: GetNumberOfParticles was not correctly set to 12' )

      p2%position = 0.0_KREAL
      p2 = GetParticleAtIndex(pe, 12)
      call AssertAlmostEqual( p2%position, 12.0_KREAL, SMALL, &
         'ParticleEnsemble: AddManyParticles: position of particle 12 not correct' )

      call Delete(pe)
   end subroutine TestAddManyParticles

   subroutine TestRemoveAllParticles()
      type (ParticleEnsemble)                           :: pe
      type (Particle)                                   :: p

      call New(pe, 1.0_KREAL)

      p%position = 5.0_KREAL
      call AddParticle(pe, p)
      p%position = 4.0_KREAL
      call AddParticle(pe, p)
      p%position = 3.0_KREAL
      call AddParticle(pe, p)
      call RemoveAllPArticles(pe)

      call Assert( GetNumberOfParticles(pe) == 0, &
         'ParticleEnsemble: RemoveAllParticles: GetNumberOfParticles was not correctly set to 0' )

      call Delete(pe)
   end subroutine 

   subroutine TestCopyConstruction ()
      type (ParticleEnsemble)                           :: pe1, pe2
      type (Particle)                                   :: p

      call New(pe1, 1.0_KREAL)

      p%position = 5.0_KREAL
      call AddParticle(pe1, p)
      p%position = 4.0_KREAL
      call AddParticle(pe1, p)
      p%position = 3.0_KREAL
      call AddParticle(pe1, p)
      call New(pe2, pe1)

      call AssertAlmostEqual( GetParticleMass(pe2), 1.0_KREAL, SMALL, &
         'ParticleEnsemble: newCopy: particleMass was not correctly initialized to 1.0' )
      call Assert( GetNumberOfParticles(pe2) == 3, &
         'ParticleEnsemble: newCopy: GetNumberOfParticles was not correctly set to 3' )
      p = GetParticleAtIndex(pe2, 1)
      call AssertAlmostEqual( p%position, 5.0_KREAL, SMALL, &
         'ParticleEnsemble: newCopy: position of particle 1 not correct' )
      p = GetParticleAtIndex(pe2, 2)
      call AssertAlmostEqual( p%position, 4.0_KREAL, SMALL, &
         'ParticleEnsemble: newCopy: position of particle 2 not correct' )
      p = GetParticleAtIndex(pe2, 3)
      call AssertAlmostEqual( p%position, 3.0_KREAL, SMALL, &
         'ParticleEnsemble: newCopy: position of particle 3 not correct' )

      call Delete(pe1)
      call Delete(pe2)
   end subroutine

   subroutine TestAssignment()
      type (ParticleEnsemble)                           :: pe1, pe2
      type (Particle)                                   :: p

      call New(pe1, 1.0_KREAL)

      p%position = 5.0_KREAL
      call AddParticle(pe1, p)
      p%position = 4.0_KREAL
      call AddParticle(pe1, p)
      p%position = 3.0_KREAL
      call AddParticle(pe1, p)
      
      call New(pe2, 2.0_KREAL)
      p%position = 1.0_KREAL
      call AddParticle(pe2, p)
      p%position = 2.0_KREAL
      call AddParticle(pe2, p)

      pe2 = pe1

      call AssertAlmostEqual( GetParticleMass(pe2), 1.0_KREAL, SMALL, &
         'ParticleEnsemble: = : GetParticleMass was not correctly initialized to 1.0' )
      call Assert( GetNumberOfParticles(pe2) == 3, &
         'ParticleEnsemble: = : GetNumberOfParticles was not correctly set to 3' )
      p = GetParticleAtIndex(pe2, 1)
      call AssertAlmostEqual( p%position, 5.0_KREAL, SMALL, &
         'ParticleEnsemble: = : position of particle 1 not correct' )
      p = GetParticleAtIndex(pe2, 2)
      call AssertAlmostEqual( p%position, 4.0_KREAL, SMALL, &
         'ParticleEnsemble: = : position of particle 2 not correct' )
      p = GetParticleAtIndex(pe2, 3)
      call AssertAlmostEqual( p%position, 3.0_KREAL, SMALL, &
         'ParticleEnsemble: = : position of particle 3 not correct' )

      call Delete(pe1)
      call Delete(pe2)
   end subroutine 

end module

