module Helper where

toEitherList :: [Either err val] -> Either err [val]
toEitherList [] = Right []
toEitherList (Right x : xs) = do
  tail <- toEitherList xs
  return $ x : tail
toEitherList (Left e : _) = Left e

printSpaces :: Int -> String
printSpaces n
  | n > 0 = "  " ++ printSpaces (n - 1)
  | otherwise = ""

myMap :: (Int -> a -> b) -> Int -> [a] -> [b]
myMap _ _ [] = []
myMap f n (x : xs) = f n x : myMap f n xs

myMapForJson :: (Int -> Bool -> a -> b) -> Int -> Bool -> [a] -> [b]
myMapForJson _ _ _ [] = []
myMapForJson f n flag (x : xs) = f n flag x : myMapForJson f n flag xs

isEmpty :: [a] -> Bool
isEmpty = null

isSingleton :: [a] -> Bool
isSingleton [a] = True
isSingleton _ = False

unique :: Eq a => [a] -> Bool
unique [] = True
unique [x] = True
unique (x : y : xs) = x /= y && unique (y : xs)