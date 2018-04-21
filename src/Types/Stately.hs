module Types.Stately where

import Control.Monad.State
import qualified Data.Map as M
import Types.Expressions

type Vars = M.Map String Expr

initialState :: Vars
initialState = M.fromList []

addVar :: String -> Expr -> State Vars ()
addVar s e = state $ \v -> ((), M.insert s e v)

