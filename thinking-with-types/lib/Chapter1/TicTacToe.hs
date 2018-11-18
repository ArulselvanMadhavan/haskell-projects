module Chapter1.TicTacToe where

data TicTacToe a = TicTacToe
  { topLeft   :: a
  , topCenter :: a
  , topRight  :: a
  , midLeft   :: a
  , midCenter :: a
  , midRight  :: a
  , botLeft   :: a
  , botCenter :: a
  , botRight  :: a
  } deriving (Eq, Show)

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard = TicTacToe Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing
                       Nothing


data Three = One | Two | Three
  deriving (Eq, Ord, Enum, Bounded, Show)

newtype TicTacToe' a = TicTacToe'
  { board :: Three -> Three -> a
  }

emptyBoard' :: TicTacToe' (Maybe Bool)
emptyBoard' = TicTacToe' $ const $ const Nothing
