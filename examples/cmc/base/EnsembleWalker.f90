module EnsembleWalkerModule
   use NumberKinds
   use OutputWriterModule
   use LoggerModule
   use PotentialModule
   use ParticleEnsembleModule
   implicit none
  
   private 
   public EnsembleWalker, EnsembleWalkerProperties
   public New, Delete, OutputProperties, OutputResults
   public GetProperties, GetPotential, GetParticleEnsemble
   public GetMeanPotential, PropagateEnsemble
   save
   
   type EnsembleWalkerProperties
      real(KREAL)                                       :: timeStep
      real(KREAL)                                       :: initialReferenceEnergy
      real(KREAL)                                       :: energyConvergenceTolerance
      real(KREAL)                                       :: particleMass
      real(KREAL)                                       :: initialPositionOfParticles
      integer(KINT)                                     :: initialNumberOfParticles
      integer(KINT)                                     :: maxPropagationSteps
      integer(KINT)                                     :: numberOfTransientSteps
      integer(KINT)                                     :: numberOfEnergyBins
   end type

   type EnsembleWalker
      private
      type (EnsembleWalkerProperties)                   :: properties
      type (Potential), pointer                         :: potentialFunction
      type (ParticleEnsemble), pointer                  :: ensemble
      real(KREAL)                                       :: summedReferenceEnergy
      real(KREAL)                                       :: currentReferenceEnergy
      logical                                           :: isConverged     
      integer(KINT)                                     :: numStepsTaken
      integer(KINT)                                     :: numStepsForAverage
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
   subroutine NewPrivate(self, potentialFunction, properties)
      type (EnsembleWalker), intent(out)                :: self
      type (Potential), pointer                         :: potentialFunction
      type (EnsembleWalkerProperties), intent(in)       :: properties
      
      type (Particle)                                   :: p
      integer(KINT)                                     :: i
      
      self%potentialFunction => potentialFunction
      self%properties = properties
      self%currentReferenceEnergy = properties%initialReferenceEnergy
      self%isConverged = .false.
      self%numStepsTaken = 0
      
      ! Create initial ensemble
      allocate(self%ensemble)
      call New(self%ensemble, properties%particleMass)
      p%position = properties%initialPositionOfParticles
      do i = 1, properties%initialNumberOfParticles
         call AddParticle(self%ensemble, p)
      enddo
      
      call InitLogger
      
   end subroutine
  
   subroutine DeletePrivate(self)
      type (EnsembleWalker), intent(inout)              :: self
      call Delete(self%ensemble)
      deallocate(self%ensemble)
   end subroutine
  
   ! ------------------------
   ! Accessors.
   ! ------------------------
   function GetProperties(self)
      type (EnsembleWalker), intent(in)                 :: self
      type (EnsembleWalkerProperties)                   :: GetProperties
      GetProperties = self%properties
   end function
   
   function GetPotential(self)
      type (EnsembleWalker), intent(in)                 :: self
      type (Potential), pointer                         :: GetPotential
      GetPotential => self%potentialFunction
   end function
   
   function GetParticleEnsemble(self)
      type (EnsembleWalker), intent(in)                 :: self
      type (ParticleEnsemble), pointer                  :: GetParticleEnsemble
      GetParticleEnsemble => self%ensemble
   end function
   
   function GetMeanPotential(self)
      ! Calculates the mean potential on the fly
      type (EnsembleWalker), intent(in)              :: self
      real(KREAL)                                    :: GetMeanPotential
      
      type (Particle)                                :: part
      integer(KINT)                                  :: i, n
      real(KREAL)                                    :: pot, potSum
      
      n = GetNumberOfParticles(self%ensemble)
      if ( n == 0 ) stop 'There were no particles in the ensemble in GetMeanPotential'

      potSum = 0.0_KREAL
      do i = 1, n
         part = GetParticleAtIndex(self%ensemble, i)
         pot = ValueAtCoord(self%potentialFunction, part%position)
         potSum = potSum + pot
      enddo
      
      GetMeanPotential =  potSum / n      
   end function
   
   ! ------------------------
   ! Other methods.
   ! ------------------------
   subroutine PropagateEnsemble(self)
      type (EnsembleWalker), intent(inout)              :: self
      
      type (ParticleEnsemble), pointer                  :: newEnsemble
      integer(KINT)                                     :: i, numParticlesLast, numParticles
      real(KREAL)                                       :: lastEnergy, factor, tol, energy
      logical                                           :: converged
      real(KREAL), parameter                            :: ENERGY_DAMPING = 0.1_KREAL
      
      call LogMessage(log, TRACE_LOGGING_LEVEL, 'Entered PropagateEnsemble')
            
      self%summedReferenceEnergy = 0.0_KREAL
      self%numStepsForAverage = 0
      self%isConverged = .false.
      do i = 1, self%properties%maxPropagationSteps
         ! Step each particle, and duplicate or destroy it
         numParticlesLast = GetNumberOfParticles(self%ensemble)
         call StepParticles(self, self%ensemble)
         allocate(newEnsemble)
         call New(newEnsemble, GetParticleMass(self%ensemble))
         call SpawnParticles(self, self%ensemble, newEnsemble)
         call Delete(self%ensemble)
         deallocate(self%ensemble)
         self%ensemble => newEnsemble
         
         ! Update steps taken
         self%numStepsTaken = self%numStepsTaken + 1
         
         ! Update reference energy
         numParticles = GetNumberOfParticles(self%ensemble)
         factor = 1.0_KREAL - real(numParticles) / numParticlesLast
         self%currentReferenceEnergy = self%currentReferenceEnergy + ENERGY_DAMPING * factor / self%properties%timeStep
         
         ! Update reference energy summation
         if ( i >= self%properties%numberOfTransientSteps ) then
            lastEnergy = self%summedReferenceEnergy / max(1, self%numStepsForAverage)
            self%numStepsForAverage = self%numStepsForAverage + 1
            self%summedReferenceEnergy = self%summedReferenceEnergy + self%currentReferenceEnergy
         endif
         
         ! Log progress
         call LogMessage(log, DEBUG_LOGGING_LEVEL, 'Progation Cycle', i)
         call LogMessage(log, DEBUG_LOGGING_LEVEL, 'Number of Particles', numParticles)
         call LogMessage(log, DEBUG_LOGGING_LEVEL, 'Reference Energy', self%currentReferenceEnergy)

         ! Check convergence
         if ( i > self%properties%numberOfTransientSteps ) then
            tol = self%properties%energyConvergenceTolerance
            energy = self%summedReferenceEnergy / self%numStepsForAverage
            self%isConverged = (abs(energy - lastEnergy) <= tol)
            call LogMessage(log, DEBUG_LOGGING_LEVEL, 'Averaged Ref. Energy', &
               self%summedReferenceEnergy / self%numStepsForAverage)
         endif
         
         if ( self%isConverged ) exit
      enddo
      
      call LogMessage(log, TRACE_LOGGING_LEVEL, 'Exiting PropagateEnsemble')
            
   end subroutine
   
   subroutine StepParticles(self, particles)
   
      ! Propagate each particle in the ensemble one step in time.
      
      use GaussianDistributorModule
      type (EnsembleWalker), intent(inout)              :: self
      type (ParticleEnsemble), intent(inout)            :: particles
      
      type (GaussianDistributor)                        :: distributor
      type (Particle)                                   :: p
      real(KREAL)                                       :: step, mass, factor
      integer(KINT)                                     :: i, n
      
      call New(distributor, 1.0_KREAL)
      mass = GetParticleMass(particles)
      factor = sqrt(self%properties%timeStep / mass)
      n = GetNumberOfParticles(particles)
      do i = 1, n
         p = GetParticleAtIndex(particles, i)
         step = GenerateRandomValue(distributor)
         step = step * factor
         p%position = p%position + step
         call SetParticleAtIndex(particles, i, p)
      enddo
      call Delete(distributor)
      
   end subroutine
   
   subroutine SpawnParticles(self, originalEnsemble, resultEnsemble)
   
      ! Duplicates particles from originalEnsemble into resultEnsemble. Particles
      ! may spawn more than one new particle, or disappear altogether. Both
      ! ensembles should already be constructed, but resultEnsemble will usually
      ! be empty initially.
      
      type (EnsembleWalker), intent(inout)              :: self
      type (ParticleEnsemble), intent(in)               :: originalEnsemble
      type (ParticleEnsemble), intent(inout)            :: resultEnsemble
      
      type (Particle)                                   :: p
      integer(KINT)                                     :: n, i, j, numSpawned
      real(KREAL)                                       :: weight, potValue, rand
      
      n = GetNumberOfParticles(originalEnsemble)
      do i = 1, n
         p = GetParticleAtIndex(originalEnsemble, i)
         potValue = ValueAtCoord(self%potentialFunction, p%position)
         weight = exp( (self%currentReferenceEnergy - potValue) * self%properties%timeStep )
         call random_number(rand)
         numSpawned = min( floor(weight + rand), 3 )
         do j = 1, numSpawned
            call AddParticle(resultEnsemble, p)
         enddo
      enddo
      
   end subroutine
  
   ! ------------------------
   ! Output properties.
   ! ------------------------
   subroutine OutputPropertiesPrivate(self, writer)
      type (EnsembleWalker), intent(in), target         :: self
      type (OutputWriter), intent(inout)                :: writer
      type (EnsembleWalkerProperties), pointer          :: pr
    
      pr => self%properties
      call Write(writer, 'Time Step', pr%timeStep)
      call Write(writer, 'Initial Reference Energy', pr%initialReferenceEnergy)
      call Write(writer, 'Energy Convergence', pr%energyConvergenceTolerance)
      call Write(writer, 'Initial Position of Particles', pr%initialPositionOfParticles)
      call Write(writer, 'Initial Number of Particles', pr%initialNumberOfParticles)

      call StartSection(writer, 'Potential Function Properties', 'The potential function used in the calculation.')
      call OutputProperties(self%potentialFunction, writer)
      call EndSection(writer)
      
   end subroutine
   
   subroutine OutputResults(self, writer)
      use HistogramBuilderModule
      type (EnsembleWalker), intent(in), target         :: self
      type (OutputWriter), intent(inout)                :: writer
      
      type (HistogramBuilder)                           :: histoBuilder
      type (Particle)                                   :: part
      real(KREAL)                                       :: lowerBound, upperBound
      real(KREAL), allocatable                          :: positions(:), wavefunc(:)
      integer(KINT)                                     :: numberOfBins, i, n

      if ( self%isConverged ) then
         call Write(writer, 'Calculation Convergence', 'Succeeded')
      else
         call Write(writer, 'Calculation Convergence', 'Failed')
      endif
      
      call Write(writer, 'Propagation Steps', self%numStepsTaken)
      call Write(writer, 'Energy', self%summedReferenceEnergy / self%numStepsForAverage)
      call Write(writer, 'Number of Particles', GetNumberOfParticles(self%ensemble))

      ! Create histogram for the particle wavefunction. Print out the wavefunction.
      n = GetNumberOfParticles(self%ensemble)
      allocate(positions(n))
      do i = 1, n
         part = GetParticleAtIndex(self%ensemble, i)
         positions(i) = part%position
      enddo
      lowerBound = minval(positions)
      upperBound = maxval(positions)
      call New(histoBuilder, lowerBound, upperBound, self%properties%numberOfEnergyBins)
      allocate( wavefunc(self%properties%numberOfEnergyBins) )
      wavefunc = CreateHistogram(histoBuilder, positions)
      wavefunc = wavefunc / max(1.0_KREAL, sum(wavefunc))  
      wavefunc = wavefunc / GetBinWidth(histoBuilder) 
      wavefunc = sqrt(wavefunc)
      call Write(writer, 'Wavefunction', GetBinCenters(histoBuilder), wavefunc )
      call Delete(histoBuilder)
      deallocate(positions, wavefunc)
      
   end subroutine   
  
end module


