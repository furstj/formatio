# formatio

This module provides a simple interface to write MATLAB .mat files.
It is not intended to be a complete implementation of the .mat file
format, but rather a simple way to write data from Fortran to MATLAB.
The module is written in Fortran 2003 and should be compatible with
most compilers.  It has been tested with gfortran 12.

The module provides a single type, matfile, which is used to open
and write to a .mat file.  The type has a single procedure, write,
which can be used to write a single array to the file.  The array
can be of any rank, and the array name can be any string.  The
array is written as a double precision array, regardless of the
precision of the input array.

Example:

    use formatio
    type(matfile) :: mf

    real*8, dimension(10) :: x
    x = 1.0
    mf = matfile('test.mat', 'write')
    call mf%write('x', x)
    call mf%close()
 
