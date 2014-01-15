module Types.Item(
  Item(..) 
)where

import Data.Universe

-- | All passive items in the game
data Item =  Ankh         -- | Special, only in Black Market
           | Paste
           | Necronomicon -- | Special, only in City of Gold
           | Cape         -- | 100% drop from Vampires
           | ClimbingGloves 
           | Compass
           | Hedjet       -- | Special, only in Maoi Head
           | Jetpack      -- | 100% in psychic presence levels
           | Kapala       -- | Special, only get from enough favor
           | PitchersMitt 
           | Spectacles
           | SpikeShoes
           | SpringShoes
           | UdjatEye     -- | Special, only get from opening gold chest
           | VladsAmulet  -- | Special, only show up on Hell level with Vlad
           | VladsCape    -- | Special, 100% drop from Vlad
             deriving (Bounded, Enum, Eq)

instance Show Item where
  show i = case i of
    ClimbingGloves -> "climbing gloves"
    Ankh           -> "ankh"
    Paste          -> "paste"
    Necronomicon   -> "necronomicon"
    Cape           -> "cape"
    Compass        -> "compass"
    Hedjet         -> "hedjet"
    Jetpack        -> "jetpack"
    Kapala         -> "kapala"
    PitchersMitt   -> "pitcher's mitt"
    Spectacles     -> "spectacles"
    SpikeShoes     -> "spike shoes"
    SpringShoes    -> "spring shoes"
    UdjatEye       -> "udjat eye"
    VladsAmulet    -> "Vlad's amulet"
    VladsCape      -> "Vlad's cape"
    
instance Universe Item where
  universe = enumFrom Ankh