module Ex1_4_ii where


data Three = One | Two | Three deriving (Eq, Show)

to :: Three -> (String, Int)
to t = (show t, length . show $ t)

toA :: Three -> String
toA = show

toB :: Three -> Int
toB = length . show

to' :: Three -> (String, Int)
to' t = (toA t, toB t)
