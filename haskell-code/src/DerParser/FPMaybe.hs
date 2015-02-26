module DerParser.FPMaybe where

data FPMaybe a = Decided { fpMaybe :: a } | Undecided { fpMaybe :: a }
  deriving (Eq, Show)

isDecided :: FPMaybe a -> Bool
isDecided (Decided _) = True
isDecided _           = False

makeDecided :: FPMaybe a -> FPMaybe a
makeDecided v = Decided $ fpMaybe v

instance Functor FPMaybe where
  fmap f (Decided a) = Decided $ f a
  fmap f (Undecided a) = Undecided $ f a

data FPMaybeType = DecidedType | UndecidedType
  deriving (Eq, Show)

fpMaybeType :: FPMaybe a -> FPMaybeType
fpMaybeType (Decided _) = DecidedType
fpMaybeType (Undecided _) = UndecidedType
