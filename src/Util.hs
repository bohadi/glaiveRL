module Util ( 
    XY(..)
  , sym
) where

import Data.Tuple(swap)
import Data.Maybe(fromJust)
import qualified Data.Map.Strict as Map

type XY = (Int, Int)

sym :: String -> Char
sym s = fromJust $ Map.lookup s glyphs

glyphs = Map.fromList $ fmap swap [
    -- player
    ('₩', "pc")       
    -- terrain
  , (' ', "dirt")
  , ('░', "grass")
  , ('▒', "rock1")
  , ('▓', "rock2")
  , ('█', "rock3")
    -- walls
  , ('┇', "prisonV") , ('┅', "prisonH")
  , ('┃', "V")  , ('│', "v")
  , ('━', "H")  , ('─', "h")
  , ('╋', "VH") , ('┼', "vh")
  , ('┓', "DL") , ('┐', "dl")
  , ('┛', "UL") , ('┘', "ul")
  , ('┏', "DR") , ('┌', "dr")
  , ('┗', "UR") , ('└', "ur")
  , ('┳', "DH") , ('┬', "dh")
  , ('┻', "UH") , ('┴', "uh")
  , ('┣', "VR") , ('├', "vr")
  , ('┫', "VL") , ('┤', "vl")
    -- enemy
  , ('ㄆ', "beast1") , ('ㄉ', "beast2") , ('ガ', "beast3") 
  , ('ゆ', "ogre") , ('わ', "ogrechief")                            -- beasts

  , ('ϡ', "wingedling") , ('Ϡ', "winged") 
  , ('ѫ', "spiderling") , ('Ѫ', "spider")
  , ('∝', "hatchling") , ('∞', "hatched") , ('№', "eggbearer")      -- spiders

  , ('ى', "serpentling") , ('گ', "serpent") 
  , ('は', "nagawar") , ('ば', "nagamage") , ('ぽ', "nagapriest")   -- snakes

  , ('ぇ', "zomling") , ('え', "zombie")
  , ('ㄓ', "skeleton") , ('ㄤ', "headless") , ('ネ', "geist")            
  , ('₤', "lich")                                                   -- undead

  , ('め', "wyrm_sleep") , ('ぬ', "wyrm_ground")
  , ('ま', "wyrm_flight") , ('み', "wyrm_death")                    -- dragon
    -- npc
  , ('☻', "npc1") , ('☺', "npc2")
  , ('♂', "male") , ('♀', "female")
    -- weapon
  , ('|', "glaive") , ('†', "glaiveSilv") , ('‡', "glaiveGold")
    -- loot
  , ('▤', "chest1")
  , ('∘', "item1") , ('∙', "item2")
  , ('∗', "jewel") , ('¤', "gem")
  , ('ǒ', "ring")  , ('ǫ', "amulet")
  , ('◎', "crown")  
    -- misc
  , ('Ψ', "sconce")
  , ('▣', "switch")
  , ('‰', "gears")
  , ('ש', "harp") , ('♪', "music1") , ('♫', "music2")
    -- effects
  , ('ϟ', "bolt")  , ('Ϟ', "arc")
  , ('ж', "flame") , ('Ж', "fire")
  , ('☼', "frost") , ('※', "ice")
  , ('§', "whirl")
  , ('✠', "heal")
  ]
