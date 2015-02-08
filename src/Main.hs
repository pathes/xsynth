module Main
where
 
import Text.XML.HXT.Core
 
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
 
main :: IO ()
main = do
    argv <- getArgs
    (al, src, dst) <- cmdlineOpts argv
    [rc]  <- runX (application al src dst)
    if rc >= c_err
        then exitWith (ExitFailure (0-1))
        else exitWith ExitSuccess
 
cmdlineOpts :: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv =
    return ([withValidate no], argv!!0, argv!!1)
 
application :: SysConfigList -> String -> String -> IOSArrow b Int
application cfg src dst =
    configSysVars cfg
    >>>
    readDocument [] src
    >>>
    processChildren (processDocumentRootElement `when` isElem)
    >>>
    writeDocument [] dst
    >>>
    getErrStatus
 
processDocumentRootElement  :: IOSArrow XmlTree XmlTree
processDocumentRootElement
    = this         -- substitute this by the real application
