{-# LANGUAGE TemplateHaskell #-}
module Types.Item(
  Item(..),
  randItem,
  randKaliItem
)where

import Data.Universe
import Random.Probability
import Data.List((\\))

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
           | Parachute 
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
    Ankh           -> "the ankh"
    Paste          -> "some paste"
    Necronomicon   -> "the necronomicon"
    Cape           -> "a cape"
    Compass        -> "a compass"
    Hedjet         -> "the hedjet"
    Jetpack        -> "a jetpack"
    Kapala         -> "the kapala"
    PitchersMitt   -> "a pitcher's mitt"
    Parachute      -> "a parachute"
    Spectacles     -> "spectacles"
    SpikeShoes     -> "spike shoes"
    SpringShoes    -> "spring shoes"
    UdjatEye       -> "the udjat eye"
    VladsAmulet    -> "Vlad's amulet"
    VladsCape      -> "Vlad's cape"
    
instance Universe Item where
  universe = enumFrom Ankh

-- Random shop/wall item (weighted)
-- NB. This is only for passive upgrades 
randItem :: MonadRandom m => m Item
randItem = fromList $ 
     withWeight 1 [Jetpack] 
  ++ withWeight 2 [Cape]
  ++ withWeight 5 [ClimbingGloves, PitchersMitt, SpikeShoes, SpringShoes]
  ++ withWeight 8 [Paste, Compass, Spectacles, Parachute]

-- Get a random item from Kali based on what the Player has already
-- If the player has all of the kali items, just pick one at random.
-- TODO(maybe): This is a little redundant given the above, maybe refactor later.
randKaliItem :: MonadRandom m => [Item] -> m Item
randKaliItem itms = if null xs then randItem else fromList xs
  where xs = withWeight 1 ([Jetpack] \\ itms) 
          ++ withWeight 2 ([Cape] \\ itms)
          ++ withWeight 5 
            ([ClimbingGloves, PitchersMitt, SpikeShoes, SpringShoes] \\ itms)
          ++ withWeight 8 ([Paste, Compass, Spectacles, Parachute] \\ itms)
