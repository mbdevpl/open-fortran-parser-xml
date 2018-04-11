subroutine test_arithmetic(var1, var2, var3)

use mydatatypes, only: float64

implicit none

var1 = 100._float64 * var2

if (var2 >= 0.5_float64 + var3) then
  var2 = var3 * (1._float64 - var2)
end if

end subroutine test_arithmetic
