module Complex
type Complex = CNum of (float * float)
let (.+.) (CNum(a,b)) (CNum(c,d)) = CNum (a + c, b + d);;
let (.*.) (CNum(a,b)) (CNum(c,d)) = CNum (a*c - b*d, b*c - a*d);;
let (.-.) (CNum(a,b)) (CNum(c,d)) = CNum (-a + c, -b + d);;
let (./.) (CNum(a,b)) (CNum(c,d)) = let denom = (c*c + d*d) in CNum (a,b).*. CNum (c/denom,-d/denom);;