program formatio_ex_save_matrix

   !! Creates a file with single matrix

   use iso_fortran_env, only: real64
   use formatio

   implicit none

   type(matfile) :: mat
   real(real64) :: A(3,3)

   A = reshape( [1,2,3,4,5,6,7,8,9], [3,3] )

   mat = matfile('save_matrix_example.mat', 'w', 'Example code')
   call mat%write('A', A)
   call mat%close()
   
end program formatio_ex_save_matrix
