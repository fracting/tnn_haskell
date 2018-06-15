import Data.Maybe

type SourceWord = Maybe Int

---tnnCell :: (Integral a, Ord a) => (Maybe a, Maybe a) -> (Maybe a, Maybe a)
-- tnnCell (x, y) = if x < y then (x, y) else (y, x)

tnnCell :: (Integral a, Ord a) => (Maybe a, Maybe a) -> (Maybe a, Maybe a)
tnnCell (Nothing, Nothing) = (Nothing, Nothing)
tnnCell (Just 0, Nothing) = (Nothing, Nothing)
tnnCell (Just x, Nothing) = (Just a, Just b)
    where 
        (a, b) = divMod x 10
tnnCell (Just x, Just y) = (Just a, Just b)
    where
        (a, b) = divMod (y*16 + x) 10

scanLine :: (Integral a, Ord a) => Maybe a -> [Maybe a] -> ([Maybe a], Maybe a)
scanLine input_y [] = ([], input_y)
scanLine input_y (input_x:input_xs) = (output_x:output_xs, yn)
    where
        (output_xs, yn) = scanLine output_y input_xs
        (output_x, output_y)  = tnnCell (input_x, input_y)

tnn :: (Integral a, Ord a) => [Maybe a] -> [Maybe a]
tnn xs = case yn of
     Nothing -> []
     Just y  -> (tnn output_xs) ++ [Just y]
     where
         (output_xs, yn) = scanLine Nothing xs

main :: IO ()
main = print $
    catMaybes $ tnn $ fmap Just [11, 4,  2,  3,  1,  10]
