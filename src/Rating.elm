module Rating exposing
  ( Rating
  , new

  , change

  , total, value
  )


type Rating
  = Rating Int Int


new : Int -> Int -> Rating
new v t =
  let
    boundedTotal =
      max 1 t
  in
    Rating (boundValue v boundedTotal) boundedTotal


change : Int -> Rating -> Rating
change v (Rating _ t) =
  Rating (boundValue v t) t


total : Rating -> Int
total (Rating _ t) = t


value : Rating -> Int
value (Rating v _) = v


-- HELPERS


boundValue : Int -> Int -> Int
boundValue v high =
  min high (max 0 v)
