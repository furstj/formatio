module formatio

   !! formatio
   !!   
   !! This module provides a simple interface to write MATLAB .mat files.
   !! It is not intended to be a complete implementation of the .mat file
   !! format, but rather a simple way to write data from Fortran to MATLAB.
   !! The module is written in Fortran 2003 and should be compatible with
   !! most compilers.  It has been tested with gfortran 12.
   !!
   !! The module provides a single type, matfile, which is used to open
   !! and write to a .mat file.  The type has a single procedure, write,
   !! which can be used to write a single array to the file.  The array
   !! can be of any rank, and the array name can be any string.  The
   !! array is written as a double precision array, regardless of the
   !! precision of the input array.
   !!
   !! Example:
   !!   use formatio
   !!   type(matfile) :: mf
   !!   real*8, dimension(10) :: x
   !!   x = 1.0
   !!   mf = matfile('test.mat', 'write')
   !!   call mf%write('x', x)
   !!   call mf%close()
   !!
   !! @note The module is not yet complete.  Reading .mat files is not yet implemented. 
   !! moreover, the module supports only double precision arrays.
   !!
   !! 
   !! @note File file structure follows the MATLAB 5.0 MAT-file format
   !! see https://www.mathworks.com/help/pdf_doc/matlab/matfile_format.pdf
   
   implicit none
   private

   type :: matfile
      integer                       :: unit = -1      !! file unit
      character(len=:), allocatable :: action         !! 'read' or 'write'
   contains
      procedure, private :: write_real64
      generic   :: write => write_real64
      procedure :: close => close_matfile
      final     :: finalize_matfile
   end type matfile


   interface matfile
      procedure :: open_matfile
   end interface matfile


   public :: matfile


contains


   function open_matfile(filename, action, description) result(mf)
      use iso_fortran_env, only: int32, int16
      character(len=*), intent(in) :: filename               !! file name
      character(len=*), intent(in) :: action                 !! 'read' or 'write'
      character(len=*), intent(in), optional :: description  !! file description
      type(matfile) :: mf

      select case (action)
       case('read','READ','r','R')
         mf%action = 'read'
         error stop 'matfile: reading is not yet implemented'
       case('write','WRITE','w','W')
         mf%action = 'write'
       case default
         error stop 'matfile: unknown action '// action
      end select


      open(newunit=mf%unit, file=filename, action=mf%action, form='unformatted', &
         access='stream')

      if (mf%action == 'write') then
         call write_file_header(mf, description)
      end if

   end function open_matfile


   subroutine close_matfile(mf)
      use iso_fortran_env, only: int32
      class(matfile), intent(inout) :: mf
      if (mf%unit /= -1) close(mf%unit)
      mf%unit = -1
   end subroutine close_matfile


   subroutine finalize_matfile(mf)
      use iso_fortran_env, only: int32
      type(matfile), intent(inout) :: mf
      call mf%close
   end subroutine finalize_matfile


   subroutine write_file_header(mf, description)
      use iso_fortran_env, only: int32, int16
      type(matfile), intent(in) :: mf
      character(len=*), intent(in), optional :: description
      character(len=116) :: text
      character(len=30) :: date

      call ctime(time(), date)

      if (present(description)) then
         text = 'MATLAB 5.0 MAT-file, Platform: LINUX64, Created on: ' // date // &
            ' ' // description
      else
         text = 'MATLAB 5.0 MAT-file, Platform: LINUX64, Created on: ' // date // &
            ' formatio'
      end if
      write(mf%unit) text

      write(mf%unit) 0_int32, 0_int32 ! subsys data offset
      write(mf%unit) 256_int16          ! version
      write(mf%unit) 'IM'             ! endian indicator
   end subroutine write_file_header


   subroutine write_real64(mf, name, x)
      use iso_fortran_env, only: real64, int32, int64
      class(matfile), intent(in)   :: mf
      real(real64), intent(in)     :: x(..)
      character(len=*), intent(in) :: name
      integer, parameter :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
      integer(int64) :: fpos, dsize

      write(mf%unit) 14_int32, 0_int32
      fpos = ftell(mf%unit)

      write(mf%unit) 6_int32, 8_int32    ! Array flags
      write(mf%unit) 6_int32, 1_int32    ! Array flags

      if (rank(x) == 0) then
         call write_entry_int32(mf%unit, [1])
      else
         call write_entry_int32(mf%unit, shape(x))   
      end if
      call write_entry_string(mf%unit, name)
      call write_entry_real64(mf%unit, x)

      dsize = ftell(mf%unit) - fpos
      call fseek(mf%unit, fpos-4, SEEK_SET)
      write(mf%unit) int(dsize, int32)
      call fseek(mf%unit, 0, SEEK_END)

   end subroutine write_real64


   subroutine write_entry_int32(unit, x)
      use iso_fortran_env, only: int32
      integer, intent(in) :: unit
      integer(int32), intent(in) :: x(:)
      write(unit) 5_int32, int(size(x)*4, int32)
      write(unit) x
      ! pad to 64-bit boundary
      if (mod(size(x),2) == 1) write(unit) 0_int32
   end subroutine write_entry_int32


   subroutine write_entry_string(unit, s)
      use iso_fortran_env, only: int32
      integer, intent(in) :: unit
      character(len=*), intent(in) :: s
      integer(int32) :: i, l

      l = 8*( (len(s) + 7) / 8 )
      write(unit) 1_int32, int(len(s), int32)
      do i = 1, len(s)
         write(unit) s(i:i)
      end do
      do i = len(s)+1, l
         write(unit) char(0)
      end do
   end subroutine write_entry_string


   subroutine write_entry_real64(unit, x)
      use iso_fortran_env, only: int32, real64
      integer, intent(in)      :: unit
      real(real64), intent(in) :: x(..)
      write(unit) 9_int32, int(size(x)*8, int32)
      select rank(x)
       rank(0);  write(unit) x
       rank(1);  write(unit) x
       rank(2);  write(unit) x
       rank(3);  write(unit) x
       rank(4);  write(unit) x
       rank(5);  write(unit) x
       rank(6);  write(unit) x
       rank(7);  write(unit) x
       rank default
         error stop 'matfile: rank > 7 not supported'
      end select
   end subroutine write_entry_real64

end module formatio
