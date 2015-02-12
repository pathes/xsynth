module Model where

type Ref = String

-- defs part

-- common synth attributes
data SynthAttrs = SynthAttrs
  { synthId :: Maybe Ref
  , synthFreqmul :: Maybe Double
  , synthAmplify :: Maybe Double
  , synthPhase :: Maybe Double
  } deriving (Show, Eq)

data BasicSynthType = Sine | Triangle | Saw | Square
    deriving (Show, Eq)
data ComplexSynthBehavior = Add | Multiply
    deriving (Show, Eq)

data Synth =
    BasicSynth
      { attrs :: SynthAttrs
      , synthType :: BasicSynthType
      }
  | ComplexSynth
    { attrs :: SynthAttrs
    , behavior :: ComplexSynthBehavior
    , synthComponents :: [Synth]
    }
  | UseSynth 
    { attrs :: SynthAttrs
    , synthRef :: Ref
    }
  deriving (Show, Eq)

data Defs = Defs
  { synths :: [Synth]
  } deriving (Show, Eq)


-- score part

data Note = Note
  { noteStart :: Double
  , noteLength :: Double
  , notePitch :: Double
  } deriving (Show, Eq)

-- instrument - use of synth in verse
data Instrument = Instrument
  { name :: Ref
  , notes :: [Note]
  } deriving (Show, Eq)

data UseVerse = UseVerse
  { verseRef :: Ref
  , verseStart :: Double
  } deriving (Show, Eq)

data Verse = Verse
  { verseId :: Maybe Ref
  , instruments :: [Instrument]
  , usedVerses :: [UseVerse]
  } deriving (Show, Eq)

data Score = Score
  { verses :: [Verse]
  } deriving (Show, Eq)


-- song part

data Song = Song
  { defs :: Defs
  , score :: Score
  } deriving (Show, Eq)
