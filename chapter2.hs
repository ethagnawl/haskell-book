1
(+) 2 1
((1 + 2) * 3) + 100
triple = (*) 3
triple 3 -- 9

-- exercises

-- 1
half x = x / 2 -- becomes
let half x = x / 2

square x = x * x -- becomes
let square x = x * x

-- 2
let func n = 3.14 * n
func (5 * 5) -- 78.5

-- 3
let func n = pi * n
func (5 * 5) -- 78.53981633974483

-- 1
True

-- 2
False

-- 3
True

-- module Learn where
--   x = 10 * 5 + y
--   myResult = x * 5
--   y = 10

-- 1
area x = 3.14 * (x * x)

-- 2
double x = x * 2

-- 3
-- module Fixed where
--   x = 7
--   y = 10
--   f = x + y

-- 1
let x = 5 in x -- 5
let x = 5 in x * x -- 25
let x = 5; y = 6 in x * y -- 30
let x = 3; y = 1000 in x + 3

mult1 = x * y
  where x = 5
        y = 6

mult1 -- 30

mult2 = x * 3 + y
  where x = 10
        y = 22

mult2 -- 52

mult3 = y * 5
  where x = 10
        y = 10 * 5 * x

mult3 -- 2500

mult4 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

mult4

-- 1
result = (==) (2 + 2 * 3 - 1) ((2 + 2 * 3) - 1)

-- 2
result = (==) ((^) 10 $ 1 + 1) (((^) 10) $ (1 + 1))

-- 3
result = (==) (2 ^ 2 * 4 ^ 5 + 1) (((2 ^ 2) * (4 ^ 5)) + 1)

-- 1
True

-- 2
True

-- 3
False

-- 4
False

-- 5
False

-- 1
let z = 7
let y = z + 8
let x = y ^ 2
let waxOn = x * 5

10 + waxOn -- 1135
(+ 10) waxOn -- 1135
(-) 15 waxOn -- -1110
(-) waxOn 15 -- 1110

-- 2
triple = (*) 3

-- 3
triple waxOn -- 3375

-- 4
waxOn = x' * 5
  where x' = x

-- 5
triple = (*) 3

-- 6
waxOff x = triple x
waxOff 5 -- 15
waxOff waxOn -- 3375


