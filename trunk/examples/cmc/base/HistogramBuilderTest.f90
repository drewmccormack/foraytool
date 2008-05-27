module HistogramBuilderTests
   use NumberKinds
   use HistogramBuilderModule
   use Testing
   implicit none
  
   private
   public RunHistogramBuilderTests
   save
  
contains
  
   subroutine RunHistogramBuilderTests
      call TestBuilder
   end subroutine
  
   subroutine TestBuilder
      type (HistogramBuilder)                           :: histoBuilder
      real (KREAL)                                      :: low = -5.0, up = 6.3
      real (KREAL)                                      :: eps = 1.e-10,vals(15)
      integer(KINT)                                     :: i
      integer(KINT),parameter                           :: NUM_BINS = 10
      integer(KINT)                                     :: histo(NUM_BINS)

      call New(histoBuilder,low,up,NUM_BINS)
      vals(1) = 0.3
      vals(2) = 2.7
      vals(3) = 0.3
      vals(4) = 1.5
      vals(5) = 2.2
      vals(6) = 6.9
      vals(7) = 3.7
      vals(8) = 5.2
      vals(9) = 1.1
      vals(11) = 0.4
      vals(12) = 2.6
      vals(13) = 7.7
      vals(14) = 3.3
      vals(15) = 6.5

      call AssertAlmostEqual(GetLowerBound(histoBuilder), low, eps, 'lowerBound was not correctly initialized')
      call AssertAlmostEqual(GetUpperBound(histoBuilder), up, eps, 'upperBound was not correctly initialized')
      call Assert( GetNumberOfBins(histoBuilder) == NUM_BINS, 'numberOfBins was not correctly initialized')

      histo = CreateHistogram(histoBuilder,vals)
      call Assert( histo(1) == 0, 'Histogram value incorrect' )
      call Assert( histo(2) == 0, 'Histogram value incorrect' )
      call Assert( histo(3) == 0, 'Histogram value incorrect' )
      call Assert( histo(4) == 0, 'Histogram value incorrect' )
      call Assert( histo(5) == 4, 'Histogram value incorrect' )
      call Assert( histo(6) == 2, 'Histogram value incorrect' )
      call Assert( histo(7) == 3, 'Histogram value incorrect' )
      call Assert( histo(8) == 2, 'Histogram value incorrect' )
      call Assert( histo(9) == 0, 'Histogram value incorrect' )
      call Assert( histo(10) == 1, 'Histogram value incorrect' )

      call Delete(histoBuilder)
   end subroutine

end module

