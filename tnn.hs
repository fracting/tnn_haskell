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
        (a, b) = divMod (y*8 + x) 10

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

sourceSentence = [5,4,2,7,0,1]
sourceSentence2 = [1,8,1,6,9,7]

main :: IO ()
main = do
    print sourceSentence
    print $ tnn baseConvCell sourceSentence
    print $ tnn compareAndSwap sourceSentence2
    print $ tnn compareAndSwap $ tnn baseConvCell sourceSentence

--- oct2dec: [5,4,2,7,0,1] -> [1,8,1,6,9,7]
--- sort:    [1,8,1,6,9,7] -> [1,1,6,7,8,9]
--- oct2dec . sort: [5,4,2,7,0,1] -> [1,8,1,6,9,7] -> [1,1,6,7,8,9]
