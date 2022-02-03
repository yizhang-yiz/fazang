#include "test_macro.fi"

program adstack_test
  use, intrinsic :: iso_fortran_env
  use fazang_vari_mod, only : vari, adstack, callstack
  use fazang_test_mod
  implicit none

  type(adstack) x

  EXPECT_EQ(x%head, 1)

  call x%set_val(0.1d0)
  call x%set_adj(0.5d0)

  EXPECT_FLOAT_EQ(x%val(), 0.1d0)
  EXPECT_FLOAT_EQ(x%adj(), 0.5d0)
  EXPECT_EQ(x%head, 1)

  call x%push_head()
  call x%set_val(1.1d0)
  call x%set_adj(1.5d0)
  EXPECT_FLOAT_EQ(x%val(x%head), 1.1d0)
  EXPECT_FLOAT_EQ(x%adj(x%head), 1.5d0)
  EXPECT_EQ(x%head, 2)

  call x%set_adj(1024, 99.d0)
  EXPECT_FLOAT_EQ(x%adj(1024), 99.d0)  

end program adstack_test
