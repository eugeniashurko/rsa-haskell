-- Extended Euclidean Algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).  Note that x or y may be negative.
eGCD :: Integer -> Integer -> (Integer, Integer, Integer)
eGCD a b =
    let
        gcd_func (r1, x1, y1) (r2, x2, y2)
            | r2 == 0 = (r1, x1, y1)
            | otherwise =
                let
                    (q, r) = r1 `quotRem` r2
                in
                    gcd_func (r2, x2, y2) (r, x1-q*x2,y1-q*y2)
        (d, x, y) = gcd_func (a, 1, 0) (b, 0, 1)
    in
        if d < 0
        then (-d, -x, -y)
        else (d, x, y)

-- Modular Multiplicative Inverse
modInv :: Integer -> Integer -> Integer
modInv a m = 
    let (g, i, _) = eGCD a m
    in 
        if g == 1
        then mkPos i
        else error "No inverse exists"
    where 
        mkPos x = 
            if x < 0
                then x + m
                else x

-- Modular Exponentiation
-- Implemented using fast exponentiation
modExp :: Integer -> Integer -> Integer -> Integer
modExp m b k =
    let
        exp a k s
            | k == 0 = s
            | k `mod` 2 == 0 = ((exp (a*a `mod` m)) (k `div` 2)) s
            | otherwise = ((exp (a*a `mod` m)) (k `div` 2)) (s*a `mod` m)
    in exp b k 1

-- RSA keys data definition
data RSAPublicKey = PUB Integer Integer -- (n,e)
data RSAPrivateKey = PRIV Integer Integer -- (n,d)