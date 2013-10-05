import Curves

peano :: Curve -> Curve
peano c = c0 `connect` c1 `connect` c2 `connect` c3 `connect` c4 `connect` c5 `connect` c6 `connect` c7 `connect` c8
  where ch = reflect c Horizontal 0
        cv = reflect c Vertical 0
        chv = reflect ch Vertical 0
        h = height c
        w = width c
        p = 6
        c0 = c `translate` point (0, 0)
        c1 = cv `translate` point (w, h+p)
        c2 = c `translate` point (0, h+p+h+p)
        c3 = ch `translate` point (w+p, h+p+h+p+h )
        c4 = chv `translate` point (w+w+p, h+h+p)
        c5 = ch `translate` point (w+p, h)
        c6 = c `translate` point (w+p+w+p, 0)
        c7 = cv `translate` point (w+p+w+p+w, h+p)
        c8 = c `translate` point (w+p+w+p, h+p+h+p)

peanoN :: Int -> Curve
peanoN 0 = curve (point (0,0)) []
peanoN n
  | n > 0 = peano $ peanoN (n - 1)
  | otherwise = error "Cannot do a negative number of iterations"

peanoTest :: IO()
peanoTest = toFile (peanoN 5) "./Peano.svg"