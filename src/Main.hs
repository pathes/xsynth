module Main
where
 
import Text.XML.HXT.Core
 
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.WAVE

import Model
import Translator ( transSong )
import Generator ( generateWAVE )

main :: IO ()
main = do
    argv <- getArgs
    song <- runX (
            readDocument [withValidate no
                               , withTrace 0
                               , withRemoveWS yes
                               , withPreserveComment no] (argv !! 0)
            >>> transSong
        )
    maybeWave <- generateWAVE (head song)
    case maybeWave of
        Nothing -> return ()
        Just wave -> putWAVEFile (argv !! 1) wave
