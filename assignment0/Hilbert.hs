import Curves

hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
  where w = width c
        h = height c
        p = 6

        ch = reflect c Horizontal 0

        c0 = ch `rotate` (-90) `translate` point (w+p+w, h+p+h)
        c1 = c `translate` point (w+p+w, h)
        c2 = c
        c3 = ch `rotate` 90 `translate` point (0, h+p)

hilbertN :: Int -> Curve
hilbertN 0 = curve (point (0,0)) []
hilbertN n
  | n > 0 = hilbert $ hilbertN (n - 1)
  | otherwise = error "Cannot do a negative number of iterations"

hilbertTest :: IO()
hilbertTest = toFile (hilbertN 4) "./Hilbert.svg"