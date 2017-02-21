
data TypCk = TypCk
  deriving (Eq, Ord, Enum, Show, Read)

instance PhaseIndex TypCk where
  mkPhase     = TypCk
  phaseName _ = "Type Checker"