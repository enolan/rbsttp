import Control.Concurrent
import Control.Monad
import Data.List
import System.Process

-- 12 note chromatic scale starting at middle C.
data Note =
    C | Csharp | D | Dsharp | E | F | Fsharp | G | Gsharp | A | Asharp | B
    | Cagain
    deriving (Show, Enum)

-- Play a note with a given gain relative to max volume, asynchronously.
playNote :: Int -> Note -> IO ()
playNote g n = void $ createProcess $ shell $
    "sox -n -d --no-show-progress synth 0.3 pluck " ++ show (f n) ++ " gain " ++ show g
    where
    f :: Note -> Double
    f n = 261.626 * (2 ** ((fromIntegral $ fromEnum n)/12))
