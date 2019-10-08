module Rater.Rating exposing
  ( Rating
  , rate

  , change

  , amount, outOf
  )


type Rating
  = Rating Int Int


rate : Int -> Int -> Rating
rate amt maxAmt =
  let
    adjustedMaxAmt =
      max 1 maxAmt
  in
    Rating (adjustAmount amt adjustedMaxAmt) adjustedMaxAmt


change : Int -> Rating -> Rating
change amt (Rating _ maxAmt) =
  Rating (adjustAmount amt maxAmt) maxAmt


amount : Rating -> Int
amount (Rating amt _) = amt


outOf : Rating -> Int
outOf (Rating _ maxAmt) = maxAmt


-- HELPERS


adjustAmount : Int -> Int -> Int
adjustAmount amt maxAmt =
  min maxAmt (max 0 amt)
