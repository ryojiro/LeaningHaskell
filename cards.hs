module Cards where
  toDigits x
    | x > 0 = toDigits(div x 10) ++ [rem x 10]
    | otherwise = []

  toDigitsRev x = reverse(toDigits(x))

  toList x
    | length(x) > 1 = toList(init(x)) * 10 + last(x)
    | length(x) == 1 = head x

  doubleEveryOther x
    | length(x) >= 2 = doubleEveryOther(init(init(x))) ++ [last(init(x)) * 2] ++ [last(x)]
    | otherwise = x

  sumSingleNum x
    | div x 10 > 0 = sumSingleNum(div x 10) + rem x 10
    | otherwise = x

  sumDigits x
    | length(x) > 1 = sumSingleNum(head(x)) + sumDigits(tail(x))
    | otherwise = sumSingleNum(head(x))

  validate x
     | rem (sumDigits(doubleEveryOther(toDigits(x)))) 10 == 0 = True
     | otherwise = False
