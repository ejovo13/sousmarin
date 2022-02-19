subroutine minus1(m, n, x)
    integer, intent(in) :: m, n
    double precision, intent(inout) :: x(m, n)
    x = x - 1.0
end subroutine minus1