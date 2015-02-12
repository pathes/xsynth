{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Translator where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs ( NTree )
import Model


-- Utilities

atTag :: ArrowXml cat => String -> cat XmlTree XmlTree
atTag tag = isElem >>> hasName tag

readMaybe :: Read a => String -> Maybe a
readMaybe string = if string /= "" then Just (read string) else Nothing

refMaybe :: String -> Maybe String
refMaybe string = if string /= "" then Just string else Nothing

getChildrenNamed :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
getChildrenNamed tag = (this `when` (atTag tag)) <<< getChildren

getChildNamed :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
getChildNamed = single <<< getChildrenNamed


class Translatable a where
    translate :: ArrowXml cat => cat (NTree XNode) a


instance Translatable Song where
    translate = transSong

transSong :: ArrowXml t => t XmlTree Song
transSong =
    atTag "song" >>>
    proc song -> do
        defs  <- transDefs  <<< getChildNamed "defs"  -< song
        score <- transScore <<< getChildNamed "score" -< song
        returnA -< Song {
                defs = defs,
                score = score
            }


instance Translatable Defs where
    translate = transDefs

transDefs :: ArrowXml t => t XmlTree Defs
transDefs =
    proc defs -> do
        synths <- listA (transSynth <<< getChildren) -< defs
        returnA -< Defs {
                synths = synths
            }


instance Translatable Synth where
    translate = transSynth

transSynth :: ArrowXml t => t XmlTree Synth
transSynth =
    proc synth -> do
        ret <-
            ifA (atTag "useSynth") transUseSynth $
            ifA (atTag "synth")    transComplexSynth $
            ifA (atTag "sine")     (transBasicSynth Sine) $
            ifA (atTag "triangle") (transBasicSynth Triangle) $
            ifA (atTag "saw")      (transBasicSynth Saw) $
            ifA (atTag "square")   (transBasicSynth Square) $
            none
            -< synth
        returnA -< ret
transUseSynth :: ArrowXml t => t XmlTree Synth
transUseSynth =
    proc synth -> do
        -- Fails when there's no "ref" attribute
        ref <- getAttrValue0 "ref" -< synth
        synthAttrs <- transSynthAttrs -< synth
        returnA -< UseSynth {
                attrs = synthAttrs,
                synthRef = ref
            }
transComplexSynth :: ArrowXml t => t XmlTree Synth
transComplexSynth =
    proc synth -> do
        synthAttrs <- transSynthAttrs -< synth
        behavior <- transComplexSynthBehavior -< synth
        returnA -< ComplexSynth {
                attrs = synthAttrs,
                behavior = behavior,
                synthComponents = []
            }
transBasicSynth :: ArrowXml t => BasicSynthType -> t XmlTree Synth
transBasicSynth synthType =
    proc synth -> do
        synthAttrs <- transSynthAttrs -< synth
        returnA -< BasicSynth {
                attrs = synthAttrs,
                synthType = synthType
            }
transComplexSynthBehavior :: ArrowXml t => t XmlTree ComplexSynthBehavior
transComplexSynthBehavior =
    proc synth -> do
        behavior <- getAttrValue "behavior" -< synth
        ret <-
            ifP ((==) "add")      (constA Add) $
            ifP ((==) "multiply") (constA Multiply) $
            ifP ((==) "")         (constA Add) $  -- default behavior
            none -< behavior
        returnA -< ret


instance Translatable SynthAttrs where
    translate = transSynthAttrs

transSynthAttrs :: ArrowXml t => t XmlTree SynthAttrs
transSynthAttrs =
    proc synth -> do
    synthId      <- getAttrValue "id"      -< synth
    synthFreqmul <- getAttrValue "freqmul" -< synth
    synthAmplify <- getAttrValue "amplify" -< synth
    synthPhase   <- getAttrValue "phase"   -< synth
    returnA -< SynthAttrs {
            synthId      = readMaybe synthId,
            synthFreqmul = readMaybe synthFreqmul,
            synthAmplify = readMaybe synthAmplify,
            synthPhase   = readMaybe synthPhase
        }


instance Translatable Score where
    translate = transScore

transScore :: ArrowXml t => t XmlTree Score
transScore =
    proc score -> do
        verses <- listA (transVerse <<< getChildren) -< score
        returnA -< Score {
                verses = verses
            }


instance Translatable Verse where
    translate = transVerse

transVerse :: ArrowXml t => t XmlTree Verse
transVerse =
    atTag "verse" >>>
    proc verse -> do
        verseId     <- getAttrValue "id" -< verse
        instruments <- listA (transInstrument <<< getChildrenNamed "instrument") -< verse
        usedVerses  <- listA (transUseVerse   <<< getChildrenNamed "useVerse")   -< verse
        returnA -< Verse {
                verseId     = refMaybe verseId,
                instruments = instruments,
                usedVerses  = usedVerses
            }


instance Translatable Instrument where
    translate = transInstrument

transInstrument :: ArrowXml t => t XmlTree Instrument
transInstrument =
    proc instrument -> do
        name  <- getAttrValue "name" -< instrument
        notes <- listA (transNote <<< getChildrenNamed "note") -< instrument
        returnA -< Instrument {
                name  = name,
                notes = notes
            }


instance Translatable Note where
    translate = transNote

transNote :: ArrowXml t => t XmlTree Note
transNote =
    proc note -> do
        -- All attributes are mandatory
        noteStart  <- getAttrValue0 "start"  -< note
        noteLength <- getAttrValue0 "length" -< note
        notePitch  <- getAttrValue0 "pitch"  -< note
        returnA -< Note {
                noteStart  = read noteStart,
                noteLength = read noteLength,
                notePitch  = read notePitch
            }


instance Translatable UseVerse where
    translate = transUseVerse

transUseVerse :: ArrowXml t => t XmlTree UseVerse
transUseVerse =
    proc useVerse -> do
        -- Fails when there's no "ref" attribute
        ref   <- getAttrValue0 "ref"   -< useVerse
        start <- getAttrValue  "start" -< useVerse
        returnA -< UseVerse {
                verseRef   = ref,
                verseStart = read start
            }


main = do
    result <- runX (
            readDocument [] "../examples/example2.xml"
            >>> transSong
        )
    print result
