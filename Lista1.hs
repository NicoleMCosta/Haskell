ehTriangulo a b c = if (a + b > c) && (a + c > b) && (b + c > a) then True else False

tipoTriangulo a b c = if (a + b + c)/3 == a then "equilatero" else if (a /= b) && (b /= c) && (c/= a) then "escaleno" else "isoceles"

triangulo a b c = if ehTriangulo a b c then tipoTriangulo a b c else "nao eh um triangulo"

somaPares' 0 = 0
somaPares' n = n + somaPares(n-2)
somaPares n = if mod n 2 == 0 then somaPares' n else somaPares' (n - 1)

somaPot2m m 0 = m
somaPot2m m n = 2^n * m + somaPot2m m (n - 1)


primo n = primo' n (n-1)
primo' n 1 = True
primo' n d  = if (rem n d == 0) then False else primo' n (d-1)

seriePi n = seriePi' n 1 1
seriePi' n d s = if (4/d) > n then 4/d*s + seriePi' n (d+2) (s*(-1)) else 0
