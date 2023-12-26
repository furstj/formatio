program formatio_ex_save_variables

   use iso_fortran_env, only: real64
   use formatio

   implicit none

   type(matfile) :: mat
   real(real64) :: A(3,3), B(2,2)
   real(real64) :: x(3), z(2,2,2), u

   A = reshape( [1,2,3,4,5,6,7,8,9], [3,3] )
   B = reshape( 10+[1,2,3,4], [2,2] )
   x = 20 + [1,2,3]
   z = reshape( 30 + [1,2,3,4,5,6,7,8], [2,2,2] )
   u = 40

   mat = matfile('save_variables_example.mat', 'w', 'Example code')
   call mat%write('A', A)
   call mat%write('B', B)
   call mat%write('x', x)
   call mat%write('z', z)
   call mat%write('u', u)
   call mat%close()
   
end program formatio_ex_save_variables
