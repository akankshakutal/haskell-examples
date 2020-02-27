doubleMe x = x * 2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x  = if x > 100
                       then x
                       else doubleMe x

doubleSmallNumber' x  = doubleSmallNumber x + 1

rightTriangles = [ (a,b,c) | c <- [1..], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]

half  x = if even x 
          then Just (div x 2)
          else Nothing

