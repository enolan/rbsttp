{-# LANGUAGE RankNTypes, TupleSections #-}
import Control.Concurrent
import Control.Monad
import Data.List
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Process

-- 12 note chromatic scale starting at middle C.
data Note =
    C | Csharp | D | Dsharp | E | F | Fsharp | G | Gsharp | A | Asharp | B
    deriving (Show, Enum)

-- Play a note with a given gain relative to max volume (this should be
-- negative), asynchronously.
playNote :: Int -> Note -> IO ()
playNote g n = void $ createProcess $ shell $
    "sox -n -d --no-show-progress synth 0.3 pluck " ++ show (f n) ++ " gain "
    ++ show g
    where
    f :: Note -> Double
    f n = 261.626 * (2 ** ((fromIntegral $ fromEnum n)/12))

go1 :: IO ((Int, Note) -> IO (), EventNetwork)
go1 = do
    (addH, sendNoteEvent) <- newAddHandler
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            noteEvent <- fromAddHandler addH
            reactimate $ fmap (uncurry playNote) noteEvent
    network <- compile networkDescription
    actuate network
    return (sendNoteEvent, network)

addHandlerFromThread :: (Chan a -> IO ()) -> AddHandler a
addHandlerFromThread writerThread handler = do
    chan <- newChan
    tId1 <- forkIO (writerThread chan)
    tId2 <- forkIO $ forever $ (readChan chan >>= handler)
    return (killThread tId1 >> killThread tId2)

bpmToAddHandler :: Int -> AddHandler ()
bpmToAddHandler x = addHandlerFromThread go
    where go chan = forever $ writeChan chan () >> threadDelay microsecs
          microsecs :: Int
          microsecs = round $ (1/(fromIntegral x) * 60 * 1000000)

goBpm :: Int -> IO EventNetwork
goBpm bpm = do
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            tickEvent <- fromAddHandler (bpmToAddHandler bpm)
            reactimate $ fmap (const $ playNote (negate 5) Fsharp) tickEvent
    network <- compile networkDescription
    actuate network
    return network

-- The last two will sound ugly, but whatever I'm not an actual musician and
-- this is a tutorial.
chordify :: Note -> [Note]
chordify n = let n' = fromEnum n in map (toEnum . (`mod` 12)) [n', n'+1, n'+2]

chordify' :: Event t Note -> Event t Note
chordify' = spill . fmap chordify

goBpmChord :: Int -> IO EventNetwork
goBpmChord bpm = do
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            tickEvent <- fromAddHandler (bpmToAddHandler bpm)
            let noteEvent = chordify' $ fmap (const C) tickEvent
            reactimate $ fmap (uncurry playNote . (negate 5,)) noteEvent
    network <- compile networkDescription
    actuate network
    return network
