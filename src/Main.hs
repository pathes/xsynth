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


--main :: IO ()
--main = do
--    argv <- getArgs
--    (al, src, dst) <- cmdlineOpts argv
--    [rc]  <- runX (application al src dst)
--    if rc >= c_err
--        then exitWith (ExitFailure (0-1))
--        else exitWith ExitSuccess
 
--cmdlineOpts :: [String] -> IO (SysConfigList, String, String)
--cmdlineOpts argv =
--    return ([withValidate no], argv!!0, argv!!1)
 
--application :: SysConfigList -> String -> String -> IOSArrow b Int
--application cfg src dst =
--    configSysVars cfg
--    >>>
--    readDocument [] src
--    >>>
--    transSong
--    >>>
--    getErrStatus

main = do
    song <- runX (
            readDocument [withValidate no
                               , withTrace 1
                               , withRemoveWS yes
                               , withPreserveComment no] "../examples/example.xml"
            >>> transSong
        )
    maybeWave <- generateWAVE (head song)
    case maybeWave of
        Nothing -> return ()
        Just wave -> putWAVEFile "temp.wav" wave
