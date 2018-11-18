{-# LANGUAGE DataKinds #-}
module Chapter2.Playground where
import           Data.Proxy
import           GHC.TypeLits

data Bool
  = True
  | False
  deriving (Eq, Show)

{-
kind Bool
  = 'True   -- Promoted Data Constructor
  | 'False  -- Promoted Data Constructor
-}

data UserType = NormalUser | Admin

data User = User
  { userAdminToken :: Maybe (Proxy 'Admin)
  }

doSensitiveThings :: Proxy 'Admin -> IO ()
doSensitiveThings _ =
  putStrLn "Doing sensitive things"
