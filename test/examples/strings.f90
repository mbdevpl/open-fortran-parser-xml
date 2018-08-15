program strings

  implicit none

  character(*) :: mystring01a
  character(16) :: mystring01b
  character(len=*) :: mystring02a
  character(len=16) :: mystring02b
  character(len=*, kind=c_char) :: mystring03a
  character(len=16, kind=c_char) :: mystring03b
  character :: mystring04a(*)
  character :: mystring04b(16)
  character(kind=c_char) :: mystring05a(*)
  character(kind=c_char) :: mystring05b(16)
  character(kind=c_char, len=*) :: mystring06a
  character(kind=c_char, len=16) :: mystring06b

end program strings
