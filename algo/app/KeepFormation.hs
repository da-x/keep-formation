{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Control.Exception.Lifted as E
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.ST
import           Control.Monad.State
import           Data.STRef
import           Data.List                (intersperse)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

-- import Control.Monad.Loops

data Buffer = Buffer {
       emacsBuffer :: Text
     , emacsPoint  :: Int
     , actionLog   :: [Text]
     } deriving (Show, Eq)

mkBuffer :: Text -> Int -> Buffer
mkBuffer t p = Buffer t p []

type M r = StateT Buffer IO r
type LineColPos = (Int, Int)
type BufferPos = Int

point :: M BufferPos
point = gets emacsPoint

appendLog :: Text -> M ()
appendLog t =
    modify $ \z -> z { actionLog = t:actionLog z }

gotoChar :: BufferPos -> M Int
gotoChar pos = do
    buffer <- gets emacsBuffer
    curPos <- gets emacsPoint
    if pos >= 0  &&  pos <= T.length buffer
       then do when (curPos /= pos) $ appendLog $ T.concat $ map T.pack ["Moved point ", show curPos, " -> ", show pos]
               modify $ \z -> z { emacsPoint = pos }
               return pos
       else point

gotoChar_ :: BufferPos -> M ()
gotoChar_ = void . gotoChar

saveExcursion :: M a -> M a
saveExcursion act = do
    x <- point
    y <- act
    _ <- gotoChar x
    return y

resolveLineColPos :: LineColPos -> M BufferPos
resolveLineColPos (lineNr, colNr) = do
    Buffer{..} <- get

    return $ runST $ do
        pos <- newSTRef 0
        lineNumR <- newSTRef 1

        forM_ (T.lines emacsBuffer) $ \line -> do
            lineNum <- readSTRef lineNumR
            modifySTRef lineNumR (+1)
            if | lineNum < lineNr  -> do modifySTRef pos (+ (1 + T.length line))
                                         return ()
               | lineNum == lineNr -> do modifySTRef pos (+ colNr)
                                         return ()
               | otherwise         -> return ()
        readSTRef pos

gotoColPos :: LineColPos -> M BufferPos
gotoColPos x = resolveLineColPos x >>= gotoChar

insertText :: Text -> M ()
insertText t = do
    Buffer{..} <- get
    appendLog $ T.concat $ map T.pack ["Added ", show t]
    put $ Buffer {
          emacsBuffer =
                T.concat [ T.take emacsPoint emacsBuffer
                         , t
                         , T.drop emacsPoint emacsBuffer
                         ]
        , emacsPoint = emacsPoint + T.length t
        , actionLog = actionLog
        }

searchForward :: Text -> Maybe BufferPos -> M (Maybe BufferPos)
searchForward niddle mlimit = do
    appendLog $ T.concat $ map T.pack ["searchForward ", show niddle, " ", show mlimit]
    Buffer{..} <- get
    let part = T.drop emacsPoint emacsBuffer
    case T.breakOnAll niddle part of
        [] -> return Nothing
        ((x, _):_) ->
            do let pos = T.length x + emacsPoint
               let farEnd = pos + T.length niddle
               let move = do gotoChar_ pos
                             return $ Just pos
               case mlimit of
                   Nothing -> move
                   Just limit ->
                       if farEnd > limit
                           then return Nothing
                           else move

searchBackward :: Text -> Maybe BufferPos -> M (Maybe BufferPos)
searchBackward niddle mlimit = do
    appendLog $ T.concat $ map T.pack ["searchBackward ", show niddle, " ", show mlimit]

    Buffer{..} <- get
    let part = T.take emacsPoint emacsBuffer
    case T.breakOnAll niddle part of
        [] -> return Nothing
        nonZero ->
            do let (x, _) = last nonZero
               let pos = T.length x
               let farStart = pos
               let move = do gotoChar_ pos
                             return $ Just pos
               case mlimit of
                   Nothing -> move
                   Just limit ->
                       if farStart < limit
                           then return Nothing
                           else move
pointMin :: M Int
pointMin = return 0

pointMax :: M Int
pointMax = do
    Buffer{..} <- get
    return $ T.length emacsBuffer

beginingOfLine ::  M BufferPos
beginingOfLine =
    searchBackward "\n" Nothing >>= \case
        Nothing -> pointMin >>= gotoChar
        Just p -> gotoChar (p + 1)

endOfLine :: M BufferPos
endOfLine =
    searchForward "\n" Nothing >>= \case
        Nothing -> pointMax >>= gotoChar
        Just p -> return p

nextLine :: M BufferPos
nextLine = do
    p <- endOfLine
    gotoChar (p + 1)

previousLine :: M BufferPos
previousLine = do
    p <- beginingOfLine
    gotoChar (p - 1)

data Modification = Insert Text
                  | Remove Int
                    deriving Show

keepFormation :: Modification -> M ()
keepFormation modification = do
    -- Find all lines that could be participating in the formation.
    -- those that are of an equal or greater amount of whitespace
    -- as a prefix.

    x <- beginingOfLine
    liftIO $ print x

    return ()

data FailedTest = FailedTest String
instance E.Exception FailedTest
instance Show FailedTest where
    show (FailedTest msgstr) = "FailedTest: " ++ msgstr

verifyResult :: (Eq a, Show a, Show s, Monad m) => StateT s m a -> a -> s -> m ()
verifyResult a c b = do
    x <- evalStateT a b

    when (x /= c) $ do
        E.throw $ FailedTest $ show b ++ " :: " ++ show x ++ " /= " ++ show c

verifyState :: (Show a, Monad m) => StateT Buffer m a -> Buffer -> Buffer -> m ()
verifyState a c b = do
    x <- execStateT a b
    when (x { actionLog = [] } /= c) $ do
        E.throw $ FailedTest $ show b ++ " :: " ++ show x ++ " /= " ++ show c

data Sample = Sample {
        slBefore :: Text
      , slAfter  :: Text
      , slPos    :: BufferPos
      , slMod    :: Maybe Modification
    } deriving Show

textToSample :: Text -> Sample
textToSample sampleText = runST $ do
    beforeI  <- newSTRef []
    afterI   <- newSTRef []
    textPosI <- newSTRef 0
    posI     <- newSTRef 0
    modI     <- newSTRef Nothing

    forM_ (T.splitOn "\n" sampleText) $ \t -> do
        textPos <- readSTRef textPosI
        let restOfLine      = T.drop 1 t
        let whitespace      = (== ' ')
        let spacePrefix     = T.takeWhile whitespace restOfLine
        let symbol          = T.dropWhile whitespace restOfLine
        let app             = ((:) restOfLine)
        let modpos          = writeSTRef posI $ textPos + T.length spacePrefix
        let decodeRemoval   = writeSTRef modI $ Just $ Remove $ T.length $ T.takeWhile (=='^') symbol
        let decodeInsertion = writeSTRef modI $ Just $ Insert $ read $ T.unpack symbol
        let update          = modifySTRef' textPosI ((+) $ 1 + T.length restOfLine)

        case T.take 1 t of
            "-"   -> update >> modifySTRef' beforeI app
            "+"   -> modifySTRef' afterI app
            "x"   -> modpos >> decodeRemoval
            "i"   -> modpos >> decodeInsertion
            _     -> do update
                        modifySTRef' beforeI app
                        modifySTRef' afterI app

    slBefore  <- fmap join' $ readSTRef beforeI
    slAfter   <- fmap join' $ readSTRef afterI
    slPos     <- readSTRef posI
    slMod     <- readSTRef modI

    return $ Sample{..}
    where
        join' ltext = T.concat $ intersperse "\n" $ reverse ltext

printSample :: Sample -> IO ()
printSample Sample{..} = do
    T.putStrLn ""
    T.putStrLn "-----------------------------------------"
    putStrLn "before:"
    T.putStrLn slBefore
    putStrLn ""
    putStrLn "after:"
    T.putStrLn slAfter
    putStrLn ""
    print slPos
    print slMod
    putStrLn "--"

main :: IO ()
main = do
    let example1 = T.unlines
           [ "case x of"
           , "   Just 2  -> expr"
           , "   Nothing -> other"
           ]
    verifyResult (resolveLineColPos (3, 1)) 30 (mkBuffer example1 0)
    verifyResult (gotoColPos (2, 10) >> searchForward "Just" Nothing) Nothing (mkBuffer example1 0)
    verifyResult (gotoColPos (1, 0) >> searchForward "Just" Nothing) (Just 13) (mkBuffer example1 0)
    verifyState  (gotoColPos (1, 0) >> searchForward "Just" Nothing) (mkBuffer example1 13) (mkBuffer example1 0)

    spec <- T.readFile "spec.md"
    forM_ (zip [(1 :: Int)..] (T.splitOn "\n```\n" spec)) $ \(idx, sampleText) -> do
        x <- runExceptT $ do
            when (idx `mod` 2 == 1) $ throwError Nothing

            let sample@Sample {..} = textToSample sampleText
            mod' <- case slMod of
                    Nothing   -> throwError Nothing
                    Just mod' -> return mod'

            lift $ printSample sample
            buffer <- lift $ case mod' of
                Insert t -> do
                    return $ mkBuffer (T.concat [T.take slPos $ slBefore,
                                               t,
                                               T.drop slPos $ slBefore]) (slPos + T.length t)
                Remove _ -> do
                    return $ mkBuffer slBefore slPos

            state' <- lift $ execStateT (keepFormation mod') buffer
            -- TOOO: Code the algorithm and validate it
            lift $ forM_ (actionLog state') $ T.putStrLn
            return ()

        case x of
            Left Nothing  -> return ()
            Left (Just s) -> error s
            Right ()      -> return ()
