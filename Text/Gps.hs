module Text.Gps (encode, decode) where

-- TODO: previous-line info encoding with whitespaces
-- *Main Data.Char> [x|x<-['\0'..'\995000'], isSpace x]
--    "\t\n\v\f\r \160\5760\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288"
-- http://en.wikipedia.org/wiki/Whitespace_character

import Data.List (elemIndex)

encode :: String -> String
encode = go (0 :: Int, 0 :: Int)
  where go (l, c) [] = line l . column c $ []
        go (l, c) ('\n' : tail) = '\n' : (line l . column c $ go  (l + 1, 0) tail)
        go (l, c) (h : tail) | h < '\5760' = h : go (l, c + 1) tail
        column c = (colMark:) . encodeNat c
        line l = (lineMark:) . encodeNat l
        colMark = '\12288'
        lineMark = '\8287'

encodeNat :: Int -> String -> String
encodeNat 0 = id
encodeNat n | (more, rest) <- n `quotRem` divisor
            =  ((alphabet!!rest):) . encodeNat more


decodeNat :: String -> Int
decodeNat (head : tail) | Just digit <- head `elemIndex` alphabet = digit + divisor * decodeNat tail
decodeNat _ = 0

decodeNat' :: String -> (Int, String)
decodeNat' (head : tail) | Just digit <- head `elemIndex` alphabet = (digit + divisor * decTail, rest)
  where (decTail, rest) = decodeNat' tail
decodeNat' rest = (0, rest)

alphabet = "\5760\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239"
divisor = length alphabet


decode :: String -> (Int, Int)
decode = go 0
  where go c ('\8287' : rest) = let (l, '\12288' : rest') = decodeNat' rest in (l, decodeNat rest' + c)
        go c ('\n' : rest) = let (l', c') = go 0 rest in (l', c' + c)
        go c (_ : rest) = go (pred c) rest
