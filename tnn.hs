import Data.Maybe

type SourceWord = Maybe Int
type SourceSentence = [Int]
type TargetWord = Maybe Int
type TargetSentence = [Int]
type TNNCell = (SourceWord, TargetWord) -> (SourceWord, TargetWord)

compareAndSwap :: TNNCell
compareAndSwap (x, y) = if x < y then (x, y) else (y, x)

baseConvCell :: TNNCell
baseConvCell (Nothing, Nothing) = (Nothing, Nothing)
baseConvCell (Just 0, Nothing) = (Nothing, Nothing)
baseConvCell (Just x, Nothing) = (Just a, Just b)
    where 
        (a, b) = divMod x 10
baseConvCell (Just x, Just y) = (Just a, Just b)
    where
        (a, b) = divMod (y*16 + x) 10

scanLine :: TNNCell -> TargetWord -> SourceSentence -> (SourceSentence, TargetWord)
scanLine tnnCell input_y [] = ([], input_y)
scanLine tnnCell input_y (input_x:input_xs) = ((maybeToList output_x)++output_xs, output_y')
    where
        (output_xs, output_y') = scanLine tnnCell output_y input_xs
        (output_x, output_y)  = tnnCell (Just input_x, input_y)

tnn :: TNNCell -> SourceSentence -> TargetSentence
tnn tnnCell xs = case yn of
     Nothing -> []
     Just y  -> (tnn tnnCell output_xs) ++ [y]
     where
         (output_xs, yn) = scanLine tnnCell Nothing xs

sourceSentence = [11, 4, 2, 3, 1, 10]

main :: IO ()
main = do
    print $ tnn baseConvCell sourceSentence
    print $ tnn compareAndSwap sourceSentence

--- base converstion (hex2dec): 0xb4231a(hex) == 11805466(dec)
--- bubble sort : [11, 4, 2, 3, 1, 10] -> [1,2,3,4,10,11]
