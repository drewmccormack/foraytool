program CMC
   use DMCCalculationModule
   implicit none
   type (DMCCalculation)                                :: dmcCalc
   call New(dmcCalc)
   call Run(dmcCalc)
   call Delete(dmcCalc)
end program

