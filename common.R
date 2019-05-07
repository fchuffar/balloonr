g_ab = function(A, C) {
  b_AC = (A[2]-C[2])/(A[1]-C[1])
  a_AC = A[2] - b_AC*A[1]
  c(a_AC, b_AC)
}

g_int = function(AD, BC) {
  x = (BC[1]-AD[1]) / (AD[2]-BC[2])
  y = AD[2] * x + AD[1]
  c(x,y)
}

g_intv = function(AD, x) {
  y = AD[2] * x + AD[1]
  c(x,y)
}


draw_abline = function(l) {
  abline(a=l[1], b=l[2], col="grey")
}

draw_seg = function(I, J, ...) {
  arrows(I[1], I[2], J[1], J[2], 0, ...)
}
