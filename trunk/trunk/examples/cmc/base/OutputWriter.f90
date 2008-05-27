module OutputWriterModule
   use NumberKinds
   implicit none
  
   private
   public OutputWriter, New, Delete, Write
   public GetFileUnit, StartSection, EndSection
   save
  
   type OutputWriter
      private
      integer(KINT)                                     :: fileUnit
      integer(KINT)                                     :: indentLevel
   end type

   ! Overloaded procedure interfaces
   interface New               
      module procedure NewPrivate
   end interface

   interface Delete               
      module procedure DeletePrivate
   end interface

   interface Write
      module procedure WriteReal, WriteRealArray, WriteInteger, WriteIntegerArray, WriteString
      module procedure WriteRealColumns
   end interface
   
contains

   ! ------------------------
   ! Standard ADT Methods. Construction, Destruction, Copying, and Assignment.
   ! ------------------------
   subroutine NewPrivate(self, fileUnit)
      type (OutputWriter), intent(out)                  :: self
      integer(KINT), intent(in)                         :: fileUnit
      self%fileUnit = fileUnit
      self%indentLevel = 0
   end subroutine

   subroutine DeletePrivate(self)
      type (OutputWriter), intent(inout)                :: self
   end subroutine
   
   ! ------------------------
   ! Accessors.
   ! ------------------------
   function GetFileUnit(self)
      type (OutputWriter), intent(in)                   :: self
      integer(KINT)                                     :: GetFileUnit
      GetFileUnit = self%fileUnit
   end function

   ! --------------------
   ! Other methods.
   ! --------------------
   function FormatWithIndent(self, formatString)
      type (OutputWriter), intent(in)                   :: self
      character(len=*), intent(in)                      :: formatString
      character(len=len(formatString)+10)               :: FormatWithIndent
      if ( self%indentLevel > 0 ) then
         write(FormatWithIndent, '(a,i2,2a)')'(', 4 * self%indentLevel, 'x,', formatString(2:) 
      else
         FormatWithIndent = formatString
      endif
   end function
   
   subroutine WriteReal(self, key, val)
      type (OutputWriter), intent(in)                   :: self
      character(len=*), intent(in)                      :: key
      real(KREAL), intent(in)                           :: val
      character(len=32)                                 :: form
      form = FormatWithIndent(self, '(a,t40,f18.8)')
      write(self%fileUnit, form) key, val
   end subroutine

   subroutine WriteInteger(self, key, val)
      type (OutputWriter), intent(in)                   :: self
      character(len=*), intent(in)                      :: key
      integer(KINT), intent(in)                         :: val
      character(len=32)                                 :: form
      form = FormatWithIndent(self, '(a,t40,i8)')
      write(self%fileUnit, form) key, val
   end subroutine

   subroutine WriteRealArray(self, key, vals)
      type (OutputWriter), intent(in)                   :: self
      character(len=*), intent(in)                      :: key
      real(KREAL), intent(in)                           :: vals(:)
      character(len=32)                                 :: form
      form = FormatWithIndent(self, '(a)')
      write(self%fileUnit, form) key
      form = FormatWithIndent(self, '(6f12.6)')
      write(self%fileUnit, form) vals
   end subroutine
   
   subroutine WriteRealColumns(self, key, vals1, vals2)
      type (OutputWriter), intent(in)                   :: self
      character(len=*), intent(in)                      :: key
      real(KREAL), intent(in)                           :: vals1(:), vals2(:)
      integer(KINT)                                     :: i
      character(len=32)                                 :: form
      form = FormatWithIndent(self, '(a)')
      write(self%fileUnit, form) key
      form = FormatWithIndent(self, '(2f12.6)')
      do i = 1, size(vals1)
         write(self%fileUnit, form) vals1(i), vals2(i)
      enddo
   end subroutine
   
   subroutine WriteIntegerArray(self, key, vals)
      type (OutputWriter), intent(in)                   :: self
      character(len=*), intent(in)                      :: key
      integer(KINT), intent(in)                         :: vals(:)
      character(len=32)                                 :: form
      form = FormatWithIndent(self, '(a)')
      write(self%fileUnit, form) key
      form = FormatWithIndent(self, '(6i12)')
      write(self%fileUnit, form) vals
   end subroutine

   subroutine WriteString(self, key, str)
      type (OutputWriter), intent(in)                   :: self
      character(len=*), intent(in)                      :: key
      character(len=*), intent(in)                      :: str
      character(len=32)                                 :: form
      form = FormatWithIndent(self,'(a,t40,a)')
      write(self%fileUnit, form) key, str
   end subroutine
   
   subroutine StartSection(self, sectionName, description)
      type (OutputWriter), intent(inout)                :: self
      character(len=*), intent(in)                      :: sectionName
      character(len=*), intent(in), optional            :: description
      character(len=32)                                 :: form
      form = FormatWithIndent(self,'(a)')
      write(self%fileUnit, form) sectionName
      self%indentLevel = self%indentLevel + 1
      if ( present(description) ) then
         form = FormatWithIndent(self,'(a)')
         write(self%fileUnit, form) description
      endif
   end subroutine
   
   subroutine EndSection(self)
      type (OutputWriter), intent(inout)                :: self
      self%indentLevel = self%indentLevel - 1
      if ( self%indentLevel < 0 ) stop 'Indentation level dropped below zero in EndSection'
   end subroutine

end module


