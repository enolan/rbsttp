import Control.Concurrent
import Control.Monad
import Data.List
import System.Process

data Note = C2 | C3 | C4 | C5 | C6 deriving (Show, Enum)

-- Play a note, asynchronously.
playNote :: Note -> IO ()
playNote n = void $ createProcess (shell $ "mplayer -really-quiet " ++ show n ++ ".ogg 2>/dev/null")
