module X where

import Data.SOP
import Data.Kind
import Data.Tagged
import Data.RBR
import Data.RBR.Internal
import GHC.TypeLits

-- | This is a universe of labels.
data Cutie

-- | This is the typing judgement over all universes. It says that a given label
-- in a universe has a specific type of values it is bound to refer to.
type family Typing (sort ∷ κ) (label ∷ Symbol) ∷ Type
type instance Typing Cutie "catling" = Int
type instance Typing Cutie "whelp" = String

-- | We do not need to remember the value types — given a universe, typing
-- allows us to compute them from labels. This is otherwise the same as `Data.RBR.Map`.
data Map' symbol = E'
             | N' Color (Map' symbol) symbol (Map' symbol)
    deriving (Show,Eq)

-- | We use typing to compute value types from labels. This is otherwise the
-- same as `Data.RBR.Map`.
data Record' f sort (t :: Map' Symbol)  where
    Empty' :: Record' f sort E'
    Node'  :: Record' f sort left -> f (Typing sort k) -> Record' f sort right -> Record' f sort (N' color left k right)

-- | An example.
recordOfCuties ∷ Record' I Cutie (N' R E' "catling" (N' R E' "whelp" E'))
recordOfCuties = Node' Empty' (I 1) (Node' Empty' (I "woof") Empty')
