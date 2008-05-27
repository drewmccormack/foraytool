module InputReaderTests
   use NumberKinds
   use InputReaderModule
   use Testing
   implicit none
  
   private
   public RunInputReaderTests
   save
  
contains
  
   subroutine RunInputReaderTests()
      call TestReading()
   end subroutine
  
   subroutine TestReading()
      type (InputReader)                                    :: reader
      character(len=*), parameter                           :: tempFile = '/tmp/cmc_testReading.txt'
      integer(KINT)                                         :: intVal
      real(KREAL)                                           :: realVal
      logical                                               :: success
      
      open(unit=7, file=tempFile, status='unknown', action='readwrite')
      write(7, '(a)')'hi:1'
      write(7, '(a)')'test1:1.5'
      write(7, '(a)')'test2: 1.5'
      write(7, '(a)')'1234: 17'
      
      call New(reader, 7)
      success = Read(reader, 'hi', intVal)
      call Assert( success .and. intVal == 1, 'Wrong value for  key hi in InputReaderTests')
      success = Read(reader, 'test2', realVal)
      call Assert( success, 'Read failed for key test2 in InputReaderTests')
      call AssertAlmostEqual( realVal, 1.5_KREAL, 1.e-12_KREAL, 'Wrong value for key test2 in InputReaderTests')
      success = Read(reader, '1234', intVal)
      call Assert( success .and. intVal == 17, 'Wrong value for  key 1234 in InputReaderTests')
      call Delete(reader)
      close(7)
      
   end subroutine

end module

