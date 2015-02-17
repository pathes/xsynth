module Generator where

import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Identity

import Data.WAVE
import Data.Int ( Int32 )
import Data.Foldable ( foldrM )
import qualified Data.Set as Set
import qualified Data.Map as Map

import Model
import Validator ( GenState, GenMonad, SynthsMap, VersesMap, verifySong )

samplesPS = 16000
bitrate = 32
header = WAVEHeader 1 samplesPS bitrate Nothing

sound :: Double  -- | Frequency
      -> Int     -- | Samples per second
      -> Double  -- | Lenght of sound in seconds
      -> Int32   -- | Volume, (maxBound :: Int32) for highest, 0 for lowest
      -> [Int32]
sound freq samples len volume = take (round $ len * (fromIntegral samples)) $ 
                         map (round . (* fromIntegral volume)) $
                         map sin [0.0, (freq * 2 * pi / (fromIntegral samples))..]

samples :: [[Int32]]
samples = map (:[]) $ sound 600 samplesPS 3 (maxBound `div` 2)



generateWAVE :: Song -> IO (Maybe WAVE)
generateWAVE song = do
    let
        result = runIdentity $ runErrorT $ runStateT
            (transSong song)
            (Map.empty, Map.empty)
    case result of
        Left err -> do
            putStrLn err
            return Nothing
        Right (wave, _) -> do
            return $ Just wave



transSong :: Song -> GenMonad WAVE
transSong song = do
    -- Do the verification and collect synth & verse maps.
    verifySong song
    return $ WAVE header samples
