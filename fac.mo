
DEF MAIN:nat == ncr(9,3)

DEF ncr(n:nat,r:nat):nat ==  div(fac(n),mul(fac(r),fac(sub(n,r))))

DEF fac(n:nat):nat == IF eq(n,0) THEN 1 ELSE mul(n, fac(sub(n, 1))) FI

