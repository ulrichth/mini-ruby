module Lang.PPDoc where

-- Helper module for pretty printer module
-- Provides functions for concatenating strings while automatically generating a
-- mapping function that translates positions in the generated pretty string to
-- positions provided in the AST. 

-- Position type
type PPos = (Int, Int, Int)

-- Mapping function type
type Converter = PPos -> Maybe PPos

-- Internal mapping function - maps just the file offset to the original position.
-- The getPPMapper functions adds a conversion step from PPos to file offset.
type PPConverter = Int -> Maybe PPos

-- Pretty Doc Datatype. Maintains the generated string and the generated converter function.
-- The operators ~~, ~+~ and ~$~ combine two PPDocs to a new PPDoc.
type PPDoc = [PPPart]

data PPPart = KnownPos PPos String
            | UnknownPos String
            deriving Show


-- Combinator. Combines two PPDocs directly, leaving no space between them.
(~~) :: PPDoc -> PPDoc -> PPDoc
parts1 ~~ parts2 = parts1 ++ parts2

--(UnknownPos s0) ~~ (UnknownPos s1) = (UnknownPos (s0 ++ s1))
--(KnownPos p0 s0 c0) ~~ (UnknownPos s1) = (KnownPos p0 s c0)
--   where s = s0 ++ s1
--(UnknownPos s) ~~ (KnownPos _ s1 c1) = (UnknownPos (s++s1))

--(KnownPos p0 s0 c0) ~~ (KnownPos p1 s1 c1) = (KnownPos p0 s c)


-- Combinator. Combines two PPDocs, adding a space between them. The space character belongs to the
-- left PPDoc.
(~+~) :: PPDoc -> PPDoc -> PPDoc
ds1 ~+~ ds2 = ds1 ~~ (stringToken " ") ~~ ds2

-- Combinator. Combines two PPDocs, adding a line break between them.
(~$~) :: PPDoc -> PPDoc -> PPDoc
ds1 ~$~ ds2 = ds1 ~~ (stringToken "\n") ~~ ds2

-- Converts a Maybe value to a PPDoc, using the provided converter function.
ppMaybe :: (Maybe a) -> (a -> PPDoc) -> PPDoc
ppMaybe Nothing _ = (stringToken "")
ppMaybe (Just x) f = (f x)

-- Converts a list of values to a PPDoc, using the provided converter function on each
-- list element and separating them by a space.
ppList :: [a] -> (a -> PPDoc) -> PPDoc
ppList xs f = foldl (++) [] (map f xs)

-- Converts a string to a PPDoc with unknown position - a combinator will calculate
-- the position by the other given PPDoc.
-- NOTE: The left-most PPDoc needs a position token. This is ensured by inserting
-- posTokens at the beginning.
stringToken :: String -> PPDoc
stringToken s = [(UnknownPos s)]

-- Converts a string and a position to a PPDoc with given position.
lexToken :: PPos -> String -> PPDoc
lexToken (orgx,orgy,orgz) s = [(KnownPos (orgx,orgy,orgz) s)]

-- Inserts position information with an empty string.
posToken :: PPos -> PPDoc
posToken (px,py,pz) = [(KnownPos (px,py,pz) "")]

-- Converts coordinate pair to offset value
convertXYToP :: String -> (Int, Int) -> Maybe Int
convertXYToP s (posx, posy) = conv' s 0 0 0
   where conv' :: String -> Int -> Int -> Int -> Maybe Int
         conv' [] x y p = Nothing
         conv' (c:cs) x y p | x == posx && y == posy = Just p
                            | otherwise =
                               if c == '\n' then (conv' cs 0 (y+1) (p+1))
                                            else (conv' cs (x+1) y (p+1))


-- Retrieves the string from a PPDoc datatype.
getPPString :: PPDoc -> String
getPPString ps = foldl conc "" ps
   where conc :: String -> PPPart -> String
         conc s (UnknownPos s2) = s ++ s2
         conc s (KnownPos _ s2) = s ++ s2

-- Retrieves the mapping function from a PPDoc datatype.
getPPMapper :: PPDoc -> Converter
getPPMapper xs = \(_, y, x) -> (c' (convertXYToP s (x-1, y-1)))
   where s = getPPString xs
   
         c' Nothing = Nothing
         c' (Just p) = Just (c p)
   
         c :: Int -> PPos
         c p = conv xs (0, 1, 1) p
         
         conv :: [PPPart] -> PPos -> Int -> PPos
         conv [] pos _ = error "Could not find position in PPDoc."
         conv ((UnknownPos s):ts) pos p = conv' s pos ts p
         conv ((KnownPos pos s):ts) _ p = conv' s pos ts p
         
         conv' :: String -> PPos -> [PPPart] -> Int -> PPos
         conv' s (px,py,pz) ts p = if p < endp then (px+p, py, pz+p)
                                                     else conv ts (px+endp, py, pz+endp) (p-endp)
            where endp = length s