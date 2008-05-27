module DMCCalculationModule
   use NumberKinds
   use OutputWriterModule
   use InputReaderModule
   use LoggerModule
   use EnsembleWalkerModule
   use PotentialModule
   implicit none
  
   private 
   public DMCCalculation
   public New, Delete, Run
   save

   type DMCCalculation
      private
      type (EnsembleWalkerProperties), pointer          :: walkerProperties
      type (Potential), pointer                         :: potentialFunction
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
   ! Standard ADT Methods. Construction, Destruction.
   ! ------------------------
   subroutine NewPrivate(self)
      type (DMCCalculation), intent(out)                :: self
      
      type (InputReader)                                :: reader
      type (EnsembleWalkerProperties), pointer          :: p
      logical                                           :: didRead
      integer(KINT)                                     :: inputFileUnit, potForm
      
      ! Initialize ensemble walker properties with defaults
      allocate(self%walkerProperties)
      p => self%walkerProperties
      p%timeStep = 2.0_KREAL
      p%initialReferenceEnergy = 0.0_KREAL
      p%energyConvergenceTolerance = 1.e-8_KREAL
      p%particleMass = 1800.0_KREAL
      p%initialPositionOfParticles = 0.0_KREAL
      p%initialNumberOfParticles = 10000
      p%maxPropagationSteps = 500
      p%numberOfTransientSteps = 150
      p%numberOfEnergyBins = 100
      
      ! Override defaults from input
      inputFileUnit = 70
      open(unit=inputFileUnit, file='cmcinput.txt', action='read', status='old')
      call New(reader, inputFileUnit)
      didRead = Read(reader, 'timestep', p%timestep)
      didRead = Read(reader, 'referenceenergy', p%initialReferenceEnergy)
      didRead = Read(reader, 'energyconvergence', p%energyConvergenceTolerance)
      didRead = Read(reader, 'particlemass', p%particleMass)
      didRead = Read(reader, 'particleposition', p%initialPositionOfParticles)
      didRead = Read(reader, 'numberofparticles', p%initialNumberOfParticles)
      didRead = Read(reader, 'maximumsteps', p%maxPropagationSteps)
      didRead = Read(reader, 'minimumsteps', p%numberOfTransientSteps)
      didRead = Read(reader, 'numenergybins', p%numberOfEnergyBins)
      
      ! Initialize potential
      allocate(self%potentialFunction)
      potForm = HARMONIC_POTENTIAL
      didRead = Read(reader, 'potential', potForm)
      print *,potForm
      call New(self%potentialFunction, potForm)
      
      call Delete(reader)
      close(inputFileUnit)
      
   end subroutine
 
   subroutine DeletePrivate(self)
      type (DMCCalculation), intent(inout)              :: self
      
      call Delete(self%potentialFunction)
      deallocate(self%walkerProperties)
      deallocate(self%potentialFunction)
      
   end subroutine
  
   ! ------------------------
   ! Other methods.
   ! ------------------------
   subroutine Run(self)
      type (DMCCalculation), intent(inout)              :: self
      
      type (EnsembleWalker)                             :: walker
      type (OutputWriter)                               :: writer
      
      call New(writer, 6)    ! Write to standard output

      call New(walker, self%potentialFunction, self%walkerProperties)
      call OutputProperties(walker, writer)
      call PropagateEnsemble(walker)
      call OutputResults(walker, writer)
      call Delete(walker)
      
      call Delete(writer)
      
   end subroutine
  
end module


