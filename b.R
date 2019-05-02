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

layout(1, respect=TRUE)
plot(0,0,col=0, xlim=c(-1,16), ylim=c(-1,16))#, xaxt="n", yaxt="n", ylab="", xlab="")

p = list(
  A = c(0 ,5) , 
  B = c(15,5) , 
  C = c(7 ,10), 
  D = c(13,10) 
)


l = list(
  AC = g_ab(p$A, p$C),
  AD = g_ab(p$A, p$D),
  BC = g_ab(p$B, p$C),
  BD = g_ab(p$B, p$D)
)

p$E = g_int(l$AD, l$BC)
p$F = g_int(l$AC, l$BD)

p$G = c(p$E[1], 3)
p$H = c(p$F[1], 3)

l$AG = g_ab(p$A, p$G)
l$BH = g_ab(p$B, p$H)
l$AH = g_ab(p$A, p$H)
l$BG = g_ab(p$B, p$G)

p$I = g_int(l$AG, l$BH)
p$J = g_int(l$AH, l$BG)

p$K = c(p$I[1], p$I[2]-2)

l$AK = g_ab(p$A, p$K)
l$BK = g_ab(p$B, p$K)

p$L = g_intv(l$AK, p$G[1])
p$M = g_intv(l$BK, p$H[1])

l$AM = g_ab(p$A, p$M)
l$BL = g_ab(p$B, p$L)

p$N = g_int(l$AM, l$BL)

p$O = g_int(g_ab(p$C, p$D), g_ab(p$E, p$F))
p$O[2] = p$O[2] 

plotrix::draw.ellipse(p$O[1], p$O[2], a=4, b=4)
  

sapply(l, draw_abline)
sapply(names(p), function(q) {
  points(p[[q]][1], p[[q]][2], pch=4)
  text(p[[q]][1], p[[q]][2], q, pos=2)
})

draw_seg(p$G, p$I)
draw_seg(p$H, p$I)
draw_seg(p$G, p$J)
draw_seg(p$J, p$H)
draw_seg(p$G, p$L)
draw_seg(p$I, p$K)
draw_seg(p$H, p$M)
draw_seg(p$L, p$K)
draw_seg(p$K, p$M)

draw_seg(p$C, p$G)
draw_seg(p$D, p$H)
draw_seg(p$E, p$J)
draw_seg(p$F, p$I)





