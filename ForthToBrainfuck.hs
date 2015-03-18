{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char (ord, isDigit, isSpace)
import Data.Either.Extra
import qualified Data.Map as M
import Data.Function
import Data.List
import System.IO
import Text.Parsec as P

delimiter :: Stream s m Char => ParsecT s u m ()
delimiter = choice
    [ void space
    , void . try $ char '\\' >> space >> manyTill anyChar endOfLine
    , void . try $ char '('  >> space >> manyTill anyChar (try ( space >> char ')' >> space))
    ]

delimiters :: Stream s m Char => ParsecT s u m ()
delimiters = skipMany delimiter

anySymbol :: Stream s m Char => ParsecT s u m String
anySymbol = try $ delimiters >> many1 (satisfy (not . isSpace))

symbol :: Stream s m Char => String -> ParsecT s u m ()
symbol s = do
    t <- anySymbol
    when (t /= s) (unexpected $ "symbol " ++ show t)

natural :: Stream s m Char => ParsecT s u m Int
natural = do
    s <- anySymbol
    if all isDigit s
        then return $ read s
        else parserZero

interger :: Stream s m Char => ParsecT s u m Int
interger = do
    s <- anySymbol
    if all isDigit s || (not (null $ tail s) && head s == '-' && all isDigit (tail s))
        then return $ read s
        else parserZero

data Brainfuck
    = Incr
    | Decr
    | Next
    | Prev
    | Read
    | Write
    | While [Brainfuck]
    deriving (Eq, Ord, Show, Read)

newtype Brainfuck' = Brainfuck' [Brainfuck]
unBrainfuck' :: Brainfuck' -> [Brainfuck]
unBrainfuck' (Brainfuck' xs) = xs
instance Show Brainfuck' where
    show = f . unBrainfuck' where
        f = \ case
            [] -> ""
            Incr  : xs -> '+' : f xs
            Decr  : xs -> '-' : f xs
            Next  : xs -> '>' : f xs
            Prev  : xs -> '<' : f xs
            Read  : xs -> ',' : f xs
            Write : xs -> '.' : f xs
            While body : xs -> "[" ++  f body ++ "]" ++ f xs
instance Read Brainfuck' where
    readsPrec _ = map (first Brainfuck') . f where
        f ('+' : xs) = Incr  <:> f xs
        f ('-' : xs) = Decr  <:> f xs
        f ('>' : xs) = Next  <:> f xs
        f ('<' : xs) = Prev  <:> f xs
        f (',' : xs) = Read  <:> f xs
        f ('.' : xs) = Write <:> f xs
        f ('[' : xs) = concatMap g $ f xs where
            g (body, ']' : ys) = While body <:> f ys
            g _ = []
        f xs = [([], xs)]
        x <:> xs = map (first (x :)) xs

readBrainfuck :: String -> [Brainfuck]
readBrainfuck = unBrainfuck' . read
showBrainfuck :: [Brainfuck] -> String
showBrainfuck = show . Brainfuck'

data StackL
    = Push Int
    | IncrL
    | DecrL
    | ReadL
    | WriteL
    | WhileL [StackL] -- not consume top
    | Dup
    | Drop
    | Nip
    | Over
    | Tuck
    | Swap
    | Rot
    | NotRot
    | Pick Int
    | Roll Int
    | Clear
    | Add Int
    | Sub Int
    | Plus
    | Minus
    | Mult
    | Div
    | Mod
    | DivMod
    | Max
    | Min
    | NotF -- returns -1 as true
    | OneF
    | EqF
    | NeF
    | LeF
    | LtF
    | GeF
    | GtF
    | Emit
    | Key
    | Print
    | Newline
    | PushSpace
    | Space
    | SpaceN
    | WhileF [StackL]
    | WhenL [StackL]
    | IfL [StackL] [StackL]
    | SwitchL [[StackL]] -- [0, 1, 2, .., n or more], SwitchL [ ( -- w .. ), ( -- w .. ), .., ( n -- w .. n ) ] -> ( n -- w .. )
    deriving (Eq, Ord, Show, Read)

stacklTopLevel :: Stream s m Char => ParsecT s u m [StackL]
stacklTopLevel = stacklTopLevel' $ M.fromList
    [ (,) "1+"     [IncrL]
    , (,) "1-"     [DecrL]
    , (,) "emit"   [Emit]
    , (,) "key"    [Key]
    , (,) "dup"    [Dup]
    , (,) "drop"   [Drop]
    , (,) "nip"    [Nip]
    , (,) "over"   [Over]
    , (,) "tuck"   [Tuck]
    , (,) "swap"   [Swap]
    , (,) "rot"    [Rot]
    , (,) "-rot"   [NotRot]
    , (,) "+"      [Plus]
    , (,) "-"      [Minus]
    , (,) "*"      [Mult]
    , (,) "/"      [Div]
    , (,) "mod"    [Mod]
    , (,) "/mod"   [DivMod]
    , (,) "max"    [Max]
    , (,) "min"    [Min]
    , (,) "="      [EqF]
    , (,) "<>"     [NeF]
    , (,) "<="     [LeF]
    , (,) "<"      [LtF]
    , (,) ">="     [GeF]
    , (,) ">"      [GtF]
    , (,) "0="     [NotF]
    , (,) "0<>"    [OneF]
    , (,) "."      [Print]
    , (,) "cr"     [Newline]
    , (,) "bl"     [PushSpace]
    , (,) "space"  [Space]
    , (,) "spaces" [SpaceN]
    ]

stacklTopLevel' :: Stream s m Char => M.Map String [StackL] -> ParsecT s u m [StackL]
stacklTopLevel' e = choice $ map try
    [ do
        a <- symbol ":" >> anySymbol
        prog <- stackl e
        symbol ";"
        stacklTopLevel' (M.insert a prog e)
    , do
        prog <- stackl e
        P.optional (symbol "bye")
        delimiters >> eof
        return prog
    ]

stackl :: Stream s m Char => M.Map String [StackL] -> ParsecT s u m [StackL]
stackl e = concat <$> P.many (try $ stackl1 e)

tillFail :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m [a]
tillFail [] = return []
tillFail (x : xs) = try ((:) <$> x <*> tillFail xs) P.<|> return []

stackl1 :: Stream s m Char => M.Map String [StackL] -> ParsecT s u m [StackL]
stackl1 e = choice $ map try
    [ do
        n <- natural
        f <- choice $ map try
            [ symbol "pick" >> return Pick
            , symbol "roll" >> return Roll
            ]
        return [f n]
    , do
        n <- interger
        return [Push n]
    , do
        s <- symbol "char" >> anySymbol
        return [Push (ord $ head s)]
    , do
        symbol "begin" >> symbol "dup" >> symbol "while"
        body <- stackl e
        symbol "repeat"
        return [WhileL body]
    , do
        symbol "begin"
        cond <- stackl e
        symbol "while"
        body <- stackl e
        symbol "repeat"
        return $ cond ++ [WhileF $ body ++ cond, Drop]
    , do
        symbol "if"
        thn <- stackl e
        symbol "else"
        els <- stackl e
        choice [ symbol "then", symbol "endif" ]
        return [IfL thn els]
    , do
        symbol "if"
        body <- stackl e
        choice [ symbol "then", symbol "endif" ]
        return [WhenL body]
    , do
        symbol "case"
        let recur acc = choice
                [ do
                    acc' <- try $ do
                        n <- natural
                        when (256 <= n) parserZero
                        when (n `M.member` acc) parserZero
                        symbol "of"
                        body <- stackl e
                        symbol "endof"
                        return $ M.insert n body acc
                    recur acc'
                , return acc
                ]
        cass <- recur M.empty
        def <- stackl e
        let maxof = maximum (M.keys cass)
        let def'  = M.fromList (map (\ i -> (i, [Push i] ++ def ++ [Drop])) [0 .. maxof])
        let def'' = M.insert (maxof + 1) def def'
        let cass' = map snd . sortBy (compare `on` fst) . M.toList $ cass `M.union` def''
        symbol "endcase"
        return [SwitchL cass']
    , do
        a <- anySymbol
        case M.lookup a e of
            Just b -> return b
            Nothing -> parserZero
    ]

fromStackL :: [StackL] -> [Brainfuck]
fromStackL = f where
    f = \ case
        [] -> []
        Push n : xs -> Next : replicate (n `mod` 0x100) Incr ++ f xs
        IncrL  : xs -> Incr  : f xs
        DecrL  : xs -> Decr  : f xs
        ReadL  : xs -> Read  : f xs
        WriteL : xs -> Write : f xs
        WhileL body : xs -> While (f body) : f xs
        Dup  : xs -> readBrainfuck "[>+>+<<-]>>[<<+>>-]<" ++ f xs
        Drop : xs -> readBrainfuck "[-]<" ++ f xs
        Nip  : xs -> readBrainfuck "<[-]>[<+>-]<" ++ f xs
        Over : xs -> readBrainfuck "<[>>+>+<<<-]>>>[<<<+>>>-]<" ++ f xs
        Tuck : xs -> readBrainfuck "[>+>+<<-]<[>+<-]>>>[<<<+>>>-]<" ++ f xs
        Swap : xs -> readBrainfuck "[>+<-]<[>+<-]>>[<<+>>-]<" ++ f xs
        Rot    : xs -> readBrainfuck "<<[>>>+<<<-]>[<+>-]>[<+>-]>[<+>-]<" ++ f xs
        NotRot : xs -> readBrainfuck "[>+<-]<[>+<-]<[>+<-]>>>[<<<+>>>-]<" ++ f xs
        Pick n : xs -> pickL n ++ f xs
        Roll n : xs -> rollL n ++ f xs
        Clear : xs -> While [Decr] : f xs
        Add n : xs -> replicate n Incr ++ f xs
        Sub n : xs -> replicate n Decr ++ f xs
        Plus  : xs -> readBrainfuck "[<+>-]<" ++ f xs
        Minus : xs -> readBrainfuck "[<->-]<" ++ f xs
        Mult  : xs -> readBrainfuck "<[>[>+>+<<-]>>[<<+>>-]<<<-]>[-]>[<<+>>-]<<" ++ f xs
        Div : xs -> f $ DivMod : Nip  : xs
        Mod : xs -> f $ DivMod : Drop : xs
        DivMod : xs -> divmod ++ f xs
        -- http://sampi.hatenablog.com/entry/2013/09/28/115426
        Max : xs -> readBrainfuck "[>>+<<-]>+>>>+>>++[<<<<<<<-[>]>>>>>>>-<<<<<<[<]>>-[>]>>>>-<<<[<]>>+>]>[-]<<<-<<[-]<-<<[-]>>>>>>[<<<<<<+>>>>>>-]<<<<<<" ++ f xs
        Min : xs -> readBrainfuck "[>>+<<-]>+>>>+>>+[<<<<<<<-[>]>>>>>>>[-]<<<<<<[<]>>-[>]>>>>[-]<<<[<]>>+>]<<-<<[-]<->>>>[<<<<<<+>>>>>>-]<<<<<<" ++ f xs
        NotF : xs -> readBrainfuck ">-<[>+<[-]]>[<->+]<" ++ f xs
        OneF : xs -> readBrainfuck    "[>-<[-]]>[<->+]<" ++ f xs
        EqF : xs -> f $ Minus : NotF : xs
        NeF : xs -> f $ Minus : OneF : xs
        LeF : xs -> leL ++ f xs
        LtF : xs -> ltL ++ f xs
        GeF : xs -> geL ++ f xs
        GtF : xs -> gtL ++ f xs
        Emit : xs -> f $ WriteL : Drop  : xs
        Key  : xs -> f $ Push 0 : ReadL : xs
        Print : xs -> printL ++ f xs
        Newline : xs -> f $ newlineL ++ xs
        PushSpace : xs -> f $ Push (ord ' ') : xs
        Space : xs -> f $ [PushSpace, Emit] ++ xs
        SpaceN : xs -> readBrainfuck (">" ++ replicate (ord ' ') '+' ++ "<[>.<-]>[-]<<") ++ f xs
        WhileF body : xs -> f $ WhileL (Drop : body) : xs
        WhenL body   : xs -> ifL body [] ++ f xs
        IfL thn els  : xs -> ifL thn els ++ f xs
        SwitchL cs : xs -> switchL cs ++ f xs


divmod :: [Brainfuck]
divmod = concat a where
    -- http://sampi.hatenablog.com/entry/2013/10/01/001136
    b = readBrainfuck "[>>+<<-]>+>>>>>+[<<<<[->+>+<<]>[-<+>]>[-<<<<<-[>]>>>>>>>-<<<<<<[<]>>>>]>+>]<<<<<<<[>]>[>[-<<<+>>>]>>>-<<<<<]>->[-]>>>>>[-]<<[<<<<<+>>>>>-]<<<<<"
    a = -- *workaround* when numerator is 0
        [ f Over
        , ifL'
            b
            (f Clear)
        ]
    f = fromStackL . (: [])

-- if you write 1st-argument-only version, it will be very similar to this without some "<>" or "><"
ifL' :: [Brainfuck] -> [Brainfuck] -> [Brainfuck]
ifL' thn els = readBrainfuck $ concat
    [ ">+<[[-]>-<<"
    , showBrainfuck thn
    , ">]>[-<<"
    , showBrainfuck els
    , ">>]<<"
    ]
ifL :: [StackL] -> [StackL] -> [Brainfuck]
ifL = ifL' `on` fromStackL

-- TODO: fix
switchL :: [[StackL]] -> [Brainfuck]
switchL = readBrainfuck . f 0 where
    f _ [] = g [Drop]
    f 0 (x : xs) = concat [">+<",  f 1 xs,   ">[-<<",                 g x, ">>]<", "<"]
    f n [x]      = concat ["[-",             ">[-<", replicate n '+', g x,  ">]<", "]"]
    f n (x : xs) = concat ["[-", f (n+1) xs, ">[-<<",                 g x, ">>]<", "]"]
    g = showBrainfuck . fromStackL

newlineL :: [StackL]
newlineL = case nativeNewline of
    LF   -> [Push $ ord '\n', Emit]
    CRLF -> [Push $ ord '\r', Emit, Push $ ord '\n', Emit]

forthToBrainfuck :: String -> Either ParseError [Brainfuck]
forthToBrainfuck code = fromStackL <$> parse stacklTopLevel "" code
unsafeForthToBrainfuck :: String -> [Brainfuck]
unsafeForthToBrainfuck = fromRight . forthToBrainfuck

-- ( n -- ) now in brainfuck, print n%128
printL :: [Brainfuck]
printL = unsafeForthToBrainfuck $ unlines
        [ "100 /mod"
        , "dup if"
        , "    48 + emit"
        , "    1"
        , "else"
        , "    drop"
        , "    0"
        , "then"
        , "swap"
        , "10 /mod"
        , "dup if"
        , "    48 + emit"
        , "    nip"
        , "else"
        , "    drop"
        , "    swap if"
        , "        48 emit"
        , "    then"
        , "then"
        , "48 + emit"
        , "space"
        ]

gtL :: [Brainfuck] -- !!max(a-b,0)
gtL = unsafeForthToBrainfuck $ unlines
    [ "begin dup while"
    , "    over"
    , "    if"
    , "        swap 1- swap"
    , "    then"
    , "    1-"
    , "repeat"
    , "drop"
    , "0<>"
    ]
geL :: [Brainfuck]
geL = unsafeForthToBrainfuck "swap <="
leL :: [Brainfuck]
leL = unsafeForthToBrainfuck "> 0="
ltL :: [Brainfuck]
ltL = unsafeForthToBrainfuck "swap >"

pickL :: Int -> [Brainfuck]
pickL n = readBrainfuck $ concat
    [ nprev
    , "[", nnext, ">+>+<<", nprev, "-]"
    , nnext, ">>[", nprev, "<<+>>", nnext, "-]"
    , "<"
    ] where
        nnext = replicate n '>'
        nprev = replicate n '<'

rollL :: Int -> [Brainfuck]
rollL n = readBrainfuck $ concat
    [ nprev
    , "[", nnext, ">+<", nprev, "-]"
    , concat $ replicate (n+1) ">[<+>-]"
    , "<"
    ] where
        nnext = replicate n '>'
        nprev = replicate n '<'

main :: IO ()
main = do
    file <- getContents
    case parse stacklTopLevel "*stdin*" file of
        Right result -> putStrLn . showBrainfuck . fromStackL $ result
        Left  result -> error $ show result
