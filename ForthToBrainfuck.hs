{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Applicative hiding ((<|>))
import Control.Arrow
import Control.Monad
import Data.Char (ord, isDigit, isSpace)
import qualified Data.Map as M
import Data.Functor.Identity (Identity)
import Data.Function
import Data.List
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit
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
    | Raw String
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
            Raw s : xs -> s ++ f xs
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

dropL :: StackL
dropL = RawL "[-]<"

data StackL
    = Push Int
    | ReadL
    | WriteL
    | WhileL [StackL] -- not consume top
    | Pick Int
    | Roll Int
    | WhenL [StackL]
    | IfL [StackL] [StackL]
    | SwitchL [[StackL]] -- [0, 1, 2, .., n or more], SwitchL [ ( -- w .. ), ( -- w .. ), .., ( n -- w .. n ) ] -> ( n -- w .. )
    | RawL String
    deriving (Eq, Ord, Show, Read)

stacklProgram :: Stream s m Char => M.Map String [StackL] -> ParsecT s u m [StackL]
stacklProgram e = choice
    [ stacklFuncDef e >>= stacklProgram
    , stacklTopLevel e
    ]

stacklFuncDef :: Stream s m Char => M.Map String [StackL] -> ParsecT s u m (M.Map String [StackL])
stacklFuncDef e = try $ do
    a <- symbol ":" >> anySymbol
    prog <- stackl e
    symbol ";"
    return $ M.insert a prog e

manyf :: Stream s m t => (a -> ParsecT s u m a) -> a -> ParsecT s u m a
manyf f x = (f x >>= manyf f) <|> return x

stacklLibrary :: Stream s m Char => M.Map String [StackL] -> ParsecT s u m (M.Map String [StackL])
stacklLibrary e = manyf stacklFuncDef e <* delimiters <* eof

stacklTopLevel :: Stream s m Char => M.Map String [StackL] -> ParsecT s u m [StackL]
stacklTopLevel e = try $ do
    prog <- stackl e
    P.optional (symbol "bye")
    delimiters >> eof
    return prog

stackl :: Stream s m Char => M.Map String [StackL] -> ParsecT s u m [StackL]
stackl e = concat <$> P.many (try $ stackl1 e)

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
        return $ cond ++ [WhileL $ dropL : body ++ cond, dropL]
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
        let def'  = M.fromList (map (\ i -> (i, [Push i] ++ def ++ [dropL])) [0 .. maxof])
        let def'' = M.insert (maxof + 1) def def'
        let cass' = map snd . sortBy (compare `on` fst) . M.toList $ cass `M.union` def''
        symbol "endcase"
        return [SwitchL cass']
    , do
        symbol "s`"
        void anyChar
        s <- manyTill anyChar (char '`')
        return [RawL s]
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
        ReadL  : xs -> Read  : f xs
        WriteL : xs -> Write : f xs
        WhileL body : xs -> While (f body) : f xs
        Pick n : xs -> pickL n ++ f xs
        Roll n : xs -> rollL n ++ f xs
        WhenL body   : xs -> ifL body [] ++ f xs
        IfL thn els  : xs -> ifL thn els ++ f xs
        SwitchL cs : xs -> switchL cs ++ f xs
        RawL s : xs -> Raw s : f xs


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
    f _ [] = g [dropL]
    f 0 (x : xs) = concat [">+<",  f 1 xs,   ">[-<<",                 g x, ">>]<", "<"]
    f n [x]      = concat ["[-",             ">[-<", replicate n '+', g x,  ">]<", "]"]
    f n (x : xs) = concat ["[-", f (n+1) xs, ">[-<<",                 g x, ">>]<", "]"]
    g = showBrainfuck . fromStackL

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

data Flag
    = Eval String
    | Stdlib FilePath
    deriving (Eq, Ord, Show, Read)

header :: String -> String
header progName = progName

options :: [OptDescr Flag]
options =
    [ Option "e" ["eval"] (ReqArg Eval "CODE") ""
    , Option [] ["stdlib"] (ReqArg Stdlib "PATH") ""
    ]

fromEval :: Flag -> Maybe String
fromEval (Eval s) = Just s
fromEval _ = Nothing

fromStdlib :: Flag -> Maybe FilePath
fromStdlib (Stdlib s) = Just s
fromStdlib _ = Nothing

readFile' :: FilePath -> IO String
readFile' path =
    withFile path ReadMode $ \ fh -> do
        s <- hGetContents fh
        seq (length s) (return s)

fromHead :: a -> [a] -> a
fromHead x [] = x
fromHead _ (x : _) = x

parseIO :: Stream s Identity t => Parsec s () a -> SourceName -> s -> IO a
parseIO a b c = case parse a b c of
    Right result -> return result
    Left  result -> error $ show result

printUsage :: IO ()
printUsage = do
    progName <- getProgName
    putStr $ usageInfo (header progName) options

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (flags, [], []) -> do
            (name, code) <- case mapMaybe fromEval flags of
                [] -> (,) "*stdin*" <$> getContents
                codes -> return $ (,) "*args*" (unlines codes)
            libfile <- readFile' . fromHead "library.fs" . reverse $ mapMaybe fromStdlib flags
            libobj <- parseIO (stacklLibrary M.empty) "*library*" libfile
            result <- parseIO (stacklProgram libobj) name code
            putStrLn . showBrainfuck $ fromStackL result
        (_, _, []) -> do
            printUsage
            exitFailure
        (_, _, errs) -> do
            putStr $ concat errs
            printUsage
            exitFailure
