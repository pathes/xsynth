module Generator where

import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Identity

import Data.WAVE
import Data.Maybe ( catMaybes )
import Data.Fixed ( mod' )
import Data.Int ( Int32 )
import Data.Foldable ( foldrM )
import qualified Data.Set as Set
import qualified Data.Map as Map

import Model
import Validator ( SynthsMap, VersesMap, verifySong )

-- WAVE samples fragment from one instrument.
data Fragment = Fragment {
    fragmentStart :: Integer,
    fragmentEnd :: Integer,
    fragmentSamples :: [WAVESample]
  } deriving (Show)

-- Map from verse name to its fragments. Useful for memoization.
type VerseFragments = Map.Map String [Fragment]

type GenState = (SynthsMap, VersesMap, VerseFragments)
type GenMonad = StateT GenState (ErrorT String Identity)

samplesPS = 16000
bitrate = 32
header = WAVEHeader 1 samplesPS bitrate Nothing

sps :: Double
sps = fromIntegral samplesPS

generateWAVE :: Song -> IO (Maybe WAVE)
generateWAVE song = do
    let
        result = runIdentity $ runErrorT $ runStateT
            (transSong song)
            (Map.empty, Map.empty, Map.empty)
    case result of
        Left err -> do
            putStrLn err
            return Nothing
        Right (wave, _) -> do
            return $ Just wave



transSong :: Song -> GenMonad WAVE
transSong song = do
    -- Do the verification, collect synth & verse maps, get main Verse.
    let
        result = runIdentity $ runErrorT $ runStateT
            (verifySong song)
            (Map.empty, Map.empty)
    case result of
        Left err ->
            lift $ throwError err
        Right (mainVerse, (synthsMap, versesMap)) -> do
            put (synthsMap, versesMap, Map.empty)
            fragments <- transVerse mainVerse
            let
                fragment = mergeFragments Add fragments
                -- WAVE library offers multichannel format
                samples = map (:[]) $ fragmentSamples fragment
            return $ WAVE header samples

transVerse :: Verse -> GenMonad [Fragment]
transVerse (Verse maybeName instruments usedVerses) = do
    let
        generateFragments :: GenMonad [Fragment]
        generateFragments = do
            instrumentFragments   <- mapM transInstrument instruments
            useVerseFragmentsList <- mapM transUseVerse usedVerses
            return $ instrumentFragments ++ (concat useVerseFragmentsList)

    (synthsMap, versesMap, verseFragments) <- get
    -- If verse is named, it may have been generated
    case maybeName of
        Nothing ->
            generateFragments
        Just name ->
            -- Check if verse fragments were already generated
            case Map.lookup name verseFragments of
                -- Memoized
                Just fragments -> return fragments
                -- Not memoized
                Nothing -> do
                    fragments <- generateFragments
                    -- Memoization
                    put (synthsMap, versesMap, Map.insert name fragments verseFragments)
                    return fragments

transUseVerse :: UseVerse -> GenMonad [Fragment]
transUseVerse (UseVerse name start) = do
    (_, versesMap, _) <- get
    case Map.lookup name versesMap of
        -- It really shouldn't happen.
        Nothing -> lift $ throwError $
            "Invalid reference to verse \"" ++ name ++ "\""
        Just verse -> do
            verseFragments <- transVerse verse
            return $ map (delay $ round $ start * sps) verseFragments

delay :: Integer -> Fragment -> Fragment
delay t (Fragment start end samples) = Fragment (start + t) (end + t) samples

transInstrument :: Instrument -> GenMonad Fragment
transInstrument (Instrument name notes) = do
    (synthsMap, _, _) <- get
    case Map.lookup name synthsMap of
        -- It really shouldn't happen.
        Nothing -> lift $ throwError $
            "Invalid reference to synth \"" ++ name ++ "\""
        Just synth -> do
            noteFragments <- mapM (\note -> transNote synth note 1.0) notes
            return $ mergeFragments Add noteFragments

transNote :: Synth -> Note -> Double -> GenMonad Fragment
transNote (BasicSynth attrs synthType) (Note start length pitch) freqmul = do
    let
        -- http://en.wikipedia.org/wiki/MIDI_Tuning_Standard
        freq :: Double
        freq = freqmul * freqmul' * (2 ** ((pitch - 69)/12.0)) * 440.0
        freqmul' :: Double
        freqmul' = case synthFreqmul attrs of
            Just x -> x
            Nothing -> 1.0
        freq' :: Double
        freq' = freq / sps

        saw :: Double -> Double
        saw x = 2.0 * (x -  fromIntegral (floor (x + 0.5)))

        triangle :: Double -> Double
        triangle x = (2.0 * abs (2.0 * (x - fromIntegral (floor (x + 0.5))))) - 1

        square :: Double -> Double
        square x = if mod' x 2.0 < 1.0 then 1.0 else -1.0

        generator :: [Double]
        generator = case synthType of
            Sine ->     map sin      [0.0, (freq' * 2.0 * pi)..]
            Triangle -> map triangle [0.0, freq'..]
            Saw ->      map saw      [0.0, freq'..]
            Square ->   map square   [0.0, freq'..]

        samples :: [WAVESample]
        samples = take (round $ length * sps) $
                  map (round . (* fromIntegral (maxBound `div` (10 :: Int32)))) $ -- TODO volume
                  generator

        start' :: Integer
        start' = round $ start * sps
        end' :: Integer
        end' = round $ (start + length) * sps

    return $ Fragment start' end' samples

transNote (ComplexSynth attrs behavior synthComponents) note freqmul = do
    let
        freqmul' :: Double
        freqmul' = case synthFreqmul attrs of
            Just x -> x
            Nothing -> 1.0
    fragments <- mapM (\synth -> transNote synth note (freqmul * freqmul')) synthComponents
    return $ mergeFragments behavior fragments

transNote (UseSynth attrs name) note freqmul = do
    let
        freqmul' :: Double
        freqmul' = case synthFreqmul attrs of
            Just x -> x
            Nothing -> 1.0
    (synthsMap, _, _) <- get
    case Map.lookup name synthsMap of
        -- It really shouldn't happen.
        Nothing -> lift $ throwError $
            "Invalid reference to synth \"" ++ name ++ "\""
        Just synth ->
            transNote synth note (freqmul * freqmul')


-- Joins together Fragments, returns one big fragment
mergeFragments :: ComplexSynthBehavior -> [Fragment] -> Fragment
mergeFragments behavior fragments =
    let
        start :: Integer
        start = minimum $ map fragmentStart fragments
        end :: Integer
        end = maximum $ map fragmentEnd fragments
        samples :: [WAVESample]
        samples = take (fromIntegral (end - start)) $ generator start fragments
        generator :: Integer -> [Fragment] -> [WAVESample]
        generator t fragments' =
            let
                head' :: WAVESample
                head' = merge $ catMaybes $ map fragmentHead $ getStarted t fragments'
                tail' :: [WAVESample]
                tail' = generator (t+1) (map (moveForward t) $ removeEnded t fragments')
            in
                head':tail'
        getStarted :: Integer -> [Fragment] -> [Fragment]
        getStarted t fragments' =
            filter (\f -> fragmentStart f <= t) fragments'
        removeEnded :: Integer -> [Fragment] -> [Fragment]
        removeEnded t fragments' =
            filter (\f -> fragmentEnd f >= t) fragments'
        fragmentHead :: Fragment -> Maybe WAVESample
        fragmentHead (Fragment _ _ (x:_)) = Just x
        fragmentHead (Fragment _ _ _) = Nothing
        moveForward :: Integer -> Fragment -> Fragment
        moveForward t fragment = case fragment of
            Fragment _ _ [] -> fragment
            Fragment start' end' (x:xs) -> case start' <= t of
                -- Move forward only if fragment has started.
                True -> Fragment start' end' xs
                False -> fragment
        merge :: [WAVESample] -> WAVESample
        merge [] = 0
        merge samples = case behavior of
            Add      -> sum samples
            Multiply -> product samples
    in
        Fragment start end samples
