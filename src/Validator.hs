module Validator where

import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Identity

import qualified Data.Set as Set
import qualified Data.Map as Map

import Model

type RefsSet   = Set.Set String
type SynthsMap = Map.Map String Synth
type VersesMap = Map.Map String Verse

type DFSState = RefsSet
type DFSMonad = StateT DFSState (ErrorT String Identity)

type ValState = (SynthsMap, VersesMap)
type ValMonad = StateT ValState (ErrorT String Identity)


-- Verifies song. Returns state-error monad having relevant maps in state
-- and returning the main Verse.
verifySong :: Song -> ValMonad Verse
verifySong (Song (Defs synths) (Score verses)) = do
    -- Check for synth id duplicates and create synth part of state
    synthDuplicates synths
    -- Check for verse id duplicates and create verse part of state
    verseDuplicates verses
    -- Check for circular references
    (synthsMap, versesMap) <- get
    synthDFS synths synthsMap
    verseDFS verses versesMap
    -- Make sure that the main Verse exists
    case Map.lookup "main" versesMap of
        Nothing -> lift $ throwError $
            "Main verse not found"
        Just mainVerse -> return mainVerse


-- Checks for synth name duplicates in list of Synths.
-- Also, adds synths to map in state.
synthDuplicates :: [Synth] -> ValMonad ()
synthDuplicates synths = do
    let
        synthDuplicates' :: Synth -> ValMonad ()
        synthDuplicates' synth = do
            case synthId $ attrs synth of
                Nothing -> return ()
                Just name -> do
                    (synthsMap, versesMap) <- get
                    case Map.lookup name synthsMap of
                        Nothing ->
                            put (Map.insert name synth synthsMap, versesMap)
                        Just _ ->
                            lift $ throwError $
                                "Duplicate synth \"" ++ name ++ "\""
            -- Complex synths can contain named subsynths.
            case synth of 
                ComplexSynth _ _ synthComponents ->
                    mapM_ synthDuplicates' synthComponents
                _ -> return ()
    mapM_ synthDuplicates' synths

verseDuplicates :: [Verse] -> ValMonad ()
verseDuplicates verses = do
    let
        verseDuplicates' :: Verse -> ValMonad ()
        verseDuplicates' verse = do
            case verseId verse of
                Nothing -> return ()
                Just name -> do
                    (synthsMap, versesMap) <- get
                    case Map.lookup name versesMap of
                        Nothing ->
                            put (synthsMap, Map.insert name verse versesMap)
                        Just _ ->
                            lift $ throwError $
                                "Duplicate verse \"" ++ name ++ "\""
    mapM_ verseDuplicates' verses

-- Runs DFS on list of Synths. Checks for circular dependencies.
synthDFS :: [Synth] -> Map.Map String Synth -> ValMonad ()
synthDFS synths synthsMap = do
    let
        synthDFS' :: Synth -> DFSMonad ()
        synthDFS' synth = do
            case synth of
                ComplexSynth attrs behavior synthComponents -> do
                    -- Get current state
                    set <- get
                    -- Add element to state if it's named
                    case synthId attrs of
                        Nothing -> return ()
                        Just name -> put $ Set.insert name set
                    -- Try to visit components
                    mapM_ tryVisit synthComponents
                    -- Reverse changes
                    put set
                _ -> return ()
        tryVisit :: Synth -> DFSMonad ()
        tryVisit synth = do
            set <- get
            case synth of
                -- If it's a UseSynth, get the relevant synth from map and visit it
                UseSynth attrs name ->
                    case Set.member name set of
                        True -> lift $ throwError $
                            "Circular reference to synth \"" ++ name ++ "\""
                        False ->
                            -- Get synth from synths map
                            case Map.lookup name synthsMap of
                                Nothing -> lift $ throwError $
                                    "Invalid reference to synth \"" ++ name ++ "\""
                                Just synth' ->
                                    synthDFS' synth'
                -- If it's another complex synth, it may contain UseSynths.
                ComplexSynth _ _ _ ->
                    synthDFS' synth
                -- Otherwise (simple synth) - do nothing.
                _ -> return ()
        result :: Either String ((), DFSState)
        result = runIdentity $ runErrorT $ runStateT
            (mapM_ synthDFS' synths)
            (Set.empty)
    case result of
        Left err -> lift $ throwError err
        Right _ -> return ()

-- Runs DFS on list of Verses. Checks for circular dependencies.
verseDFS :: [Verse] -> Map.Map String Verse -> ValMonad ()
verseDFS verses versesMap = do
    let
        verseDFS' :: Verse -> DFSMonad ()
        verseDFS' (Verse verseId instruments usedVerses) = do
            -- Get current state
            set <- get
            -- Add element to state if it's named
            case verseId of
                Nothing -> return ()
                Just name -> put $ Set.insert name set
            -- Try to visit used verses
            mapM_ tryVisit usedVerses
            -- Reverse changes
            put set
        tryVisit :: UseVerse -> DFSMonad ()
        tryVisit (UseVerse name _) = do
            set <- get
            case Set.member name set of
                True -> lift $ throwError $
                    "Circular reference to verse \"" ++ name ++ "\""
                False ->
                    -- Get verse from verses map
                    case Map.lookup name versesMap of
                        Nothing -> lift $ throwError $
                            "Invalid reference to verse \"" ++ name ++ "\""
                        Just verse ->
                            verseDFS' verse
        result :: Either String ((), DFSState)
        result = runIdentity $ runErrorT $ runStateT
            (mapM_ verseDFS' verses)
            (Set.empty)
    case result of
        Left err -> lift $ throwError err
        Right _ -> return ()
