{- Some very basic functions -}

{- Length of a string -}
len :: String -> Int
len [] = 0
len (_:xs) = 1 + len xs

{- Concatenation-}
glue :: String -> String -> String
glue [] ys = ys
glue (a:xs) ys = a : (glue xs ys) 

{- Reverse -}
rev :: String -> String
rev [] = []
rev (a:xs) = glue (rev xs) [a]

{- nth character of a string -}
pos :: String -> Int -> Char
pos [] _ = error("the string is not long enough")
pos (a:_) 0 = a
pos (_:xs) n = pos xs (n-1)

{- initial part of a string-}
invTail :: String -> String
invTail [] = error("no init")
invTail (_:[]) = []
invTail (a:xs) = a : (invTail xs)


{- Some funcionts to get strings related to other strings-}

{- Sufixes -}
suf :: String -> [String]
suf [] = [[]]
suf (a:xs) = (a:xs) : (suf xs)

{- Prefixes -}
pref :: String -> [String]
pref [] = [[]]
pref a = a : pref(invTail a)

{- Substrings -}
sub :: String -> [String]
sub [] = [[]]
sub xs = unique (squash [suf x | x <- (pref xs)])


{- Some other miselaneous auxliar functions-}

{- squash a list of list of elements into a list of elements-}
squash :: (Show a) => [[a]] -> [a]
squash [] = []
squash (a:xs) = a ++ (squash xs)

{- remove repetitions -}
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (a:xs) = (a : unique(removeElem xs a))
    

{- remove repetitions of a given element -}
removeElem :: (Eq a) => [a] -> a -> [a]
removeElem [] _ = []
removeElem (a:xs) c = 
    if a == c
        then removeElem xs c
        else a : (removeElem xs c)
                    