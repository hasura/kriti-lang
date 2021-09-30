module Text.Read.Lex.Extended (lexString, lexTemplate) where

import GHC.Base
import GHC.Char
import GHC.Unicode (isSpace)
import Text.ParserCombinators.ReadP
import Text.Read.Lex

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

lexString :: ReadP Lexeme
lexString =
  do
    quoteChar <- char '\''
    body id [quoteChar]
  where
    body f quoteChar =
      do
        (c, esc) <- lexStrItem
        if c /= quoteChar || esc
          then body (f . (c <>)) quoteChar
          else
            let s = f ""
             in return (String s)

    lexStrItem =
      (lexEmpty >> lexStrItem)
        +++ lexCharE

    lexEmpty =
      do
        _ <- char '\\'
        c <- get
        case c of
          '&' -> do return ()
          _ | isSpace c -> do skipSpaces; _ <- char '\\'; return ()
          _ -> do pfail
lexTemplate :: ReadP Lexeme
lexTemplate =
  do
    quoteChar <- char '"'
    body id [quoteChar]
  where
    body f quoteChar =
      do
        (c, esc) <- lexStrItem
        if c /= quoteChar || esc
          then body (f . (c <>)) quoteChar
          else
            let s = f ""
             in return (String s)

    lexStrItem =
      (lexEmpty >> lexStrItem)
        +++ lexCharE

    lexEmpty =
      do
        _ <- char '\\'
        c <- get
        case c of
          '&' -> do return ()
          _ | isSpace c -> do skipSpaces; _ <- char '\\'; return ()
          _ -> do pfail

lexCharE :: ReadP (String, Bool) -- "escaped or not"?
lexCharE =
  do
    c1 <- get
    if c1 == '\\'
      then do c2 <- lexEsc; return (c2, True)
      else do return (pure c1, False)
  where
    lexEsc =
      lexEscChar
        +++ fmap pure lexNumeric
        +++ fmap pure lexCntrlChar
        +++ fmap pure lexAscii

    lexEscChar =
      do
        c <- get
        case c of
          'a' -> return "\a"
          'b' -> return "\b"
          'f' -> return "\f"
          'n' -> return "\n"
          'r' -> return "\r"
          't' -> return "\t"
          'v' -> return "\v"
          '\\' -> return "\\"
          '\"' -> return "\""
          '\'' -> return "\'"
          '{' -> return "\\{"
          '`' -> return "`"
          _ -> pfail

    lexNumeric =
      do
        base <- lexBaseChar <++ return 10
        n <- lexInteger base
        guard (n <= toInteger (ord maxBound))
        return (chr (fromInteger n))

    lexCntrlChar =
      do
        _ <- char '^'
        c <- get
        case c of
          '@' -> return '\^@'
          'A' -> return '\^A'
          'B' -> return '\^B'
          'C' -> return '\^C'
          'D' -> return '\^D'
          'E' -> return '\^E'
          'F' -> return '\^F'
          'G' -> return '\^G'
          'H' -> return '\^H'
          'I' -> return '\^I'
          'J' -> return '\^J'
          'K' -> return '\^K'
          'L' -> return '\^L'
          'M' -> return '\^M'
          'N' -> return '\^N'
          'O' -> return '\^O'
          'P' -> return '\^P'
          'Q' -> return '\^Q'
          'R' -> return '\^R'
          'S' -> return '\^S'
          'T' -> return '\^T'
          'U' -> return '\^U'
          'V' -> return '\^V'
          'W' -> return '\^W'
          'X' -> return '\^X'
          'Y' -> return '\^Y'
          'Z' -> return '\^Z'
          '[' -> return '\^['
          '\\' -> return '\^\'
          ']' -> return '\^]'
          '^' -> return '\^^'
          '_' -> return '\^_'
          _ -> pfail

    lexAscii =
      do
        choice
          [ (string "SOH" >> return '\SOH')
              <++ (string "SO" >> return '\SO'),
            -- \SO and \SOH need maximal-munch treatment
            -- See the Haskell report Sect 2.6

            string "NUL" >> return '\NUL',
            string "STX" >> return '\STX',
            string "ETX" >> return '\ETX',
            string "EOT" >> return '\EOT',
            string "ENQ" >> return '\ENQ',
            string "ACK" >> return '\ACK',
            string "BEL" >> return '\BEL',
            string "BS" >> return '\BS',
            string "HT" >> return '\HT',
            string "LF" >> return '\LF',
            string "VT" >> return '\VT',
            string "FF" >> return '\FF',
            string "CR" >> return '\CR',
            string "SI" >> return '\SI',
            string "DLE" >> return '\DLE',
            string "DC1" >> return '\DC1',
            string "DC2" >> return '\DC2',
            string "DC3" >> return '\DC3',
            string "DC4" >> return '\DC4',
            string "NAK" >> return '\NAK',
            string "SYN" >> return '\SYN',
            string "ETB" >> return '\ETB',
            string "CAN" >> return '\CAN',
            string "EM" >> return '\EM',
            string "SUB" >> return '\SUB',
            string "ESC" >> return '\ESC',
            string "FS" >> return '\FS',
            string "GS" >> return '\GS',
            string "RS" >> return '\RS',
            string "US" >> return '\US',
            string "SP" >> return '\SP',
            string "DEL" >> return '\DEL'
          ]

type Base = Int

type Digits = [Int]

lexInteger :: Base -> ReadP Integer
lexInteger base =
  do
    xs <- lexDigits base
    return (val (fromIntegral base) xs)

lexDigits :: Int -> ReadP Digits
-- Lex a non-empty sequence of digits in specified base
lexDigits base =
  do
    s <- look
    xs <- scan s id
    guard (not (Prelude.null xs))
    return xs
  where
    scan (c : cs) f = case valDig base c of
      Just n -> do _ <- get; scan cs (f . (n :))
      Nothing -> do return (f [])
    scan [] f = do return (f [])

val :: Num a => a -> Digits -> a
val = valSimple

{-# RULES
"val/Integer" val = valInteger
  #-}

{-# INLINE [1] val #-}

-- The following algorithm is only linear for types whose Num operations
-- are in constant time.
valSimple :: (Num a, Integral d) => a -> [d] -> a
valSimple base = go 0
  where
    go r [] = r
    go r (d : ds) = r' `seq` go r' ds
      where
        r' = r * base + fromIntegral d
{-# INLINE valSimple #-}

-- A sub-quadratic algorithm for Integer. Pairs of adjacent radix b
-- digits are combined into a single radix b^2 digit. This process is
-- repeated until we are left with a single digit. This algorithm
-- performs well only on large inputs, so we use the simple algorithm
-- for smaller inputs.
valInteger :: Integer -> Digits -> Integer
valInteger b0 ds0 = go b0 (Prelude.length ds0) $ map fromIntegral ds0
  where
    go _ _ [] = 0
    go _ _ [d] = d
    go b l ds
      | l > 40 = b' `seq` go b' l' (combine b ds')
      | otherwise = valSimple b ds
      where
        -- ensure that we have an even number of digits
        -- before we call combine:
        ds' = if even l then ds else 0 : ds
        b' = b * b
        l' = (l + 1) `quot` 2
    combine b (d1 : d2 : ds) = d `seq` (d : combine b ds)
      where
        d = d1 * b + d2
    combine _ [] = []
    combine _ [_] = errorWithoutStackTrace "this should not happen"

valDig :: (Eq a, Num a) => a -> Char -> Maybe Int
valDig 8 c
  | '0' <= c && c <= '7' = Just (ord c - ord '0')
  | otherwise = Nothing
valDig 10 c = valDecDig c
valDig 16 c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'a' <= c && c <= 'f' = Just (ord c - ord 'a' + 10)
  | 'A' <= c && c <= 'F' = Just (ord c - ord 'A' + 10)
  | otherwise = Nothing
valDig _ _ = errorWithoutStackTrace "valDig: Bad base"

valDecDig :: Char -> Maybe Int
valDecDig c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | otherwise = Nothing

lexBaseChar :: ReadP Int
-- Lex a single character indicating the base; fail if not there
lexBaseChar = do
  c <- get
  case c of
    'o' -> return 8
    'O' -> return 8
    'x' -> return 16
    'X' -> return 16
    _ -> pfail
