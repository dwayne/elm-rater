module Rater.Rating exposing
  ( Rating
  , Ratio
  , outOf5, outOf
  , ratio
  , rate
  )


type Rating
  = Rating Ratio


type alias Ratio =
  { maxValue : Int
  , value : Int
  }


outOf5 : Int -> Rating
outOf5 =
  outOf 5


outOf : Int -> Int -> Rating
outOf maxValue value =
  let
    clampedMaxValue =
      max 1 maxValue

    clampedValue =
      min clampedMaxValue (max 0 value)
  in
  Rating (Ratio clampedMaxValue clampedValue)


ratio : Rating -> Ratio
ratio (Rating r) =
  r


rate : Int -> Rating -> Rating
rate value (Rating r) =
  Rating { r | value = min r.maxValue (max 0 value) }
