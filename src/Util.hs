module Util (
    XY
  , Name
  , Glyph, GlyphID
  , sym
  , isPathableGlyph
) where

import Data.Tuple(swap)
import Data.Maybe(fromJust)
import qualified Data.Map.Strict as Map

type XY   = (Int, Int)
type Name = String

isPathableGlyph :: Glyph -> Bool
isPathableGlyph c
  | c == sym "dirt"  = True
  | c == sym "grass" = True
  | c == sym "stone" = True
  | otherwise        = False

type Glyph   = Char
type GlyphID = String

sym :: GlyphID -> Glyph
sym s = fromJust $ Map.lookup s glyphs
glyphs = Map.fromList $ fmap swap [
    -- player
    ('₩', "pc")
    -- terrain
  , (' ', "dirt")
  , ('░', "grass")
  , ('▒', "stone")
  , ('▓', "unused")
  , ('█', "rock")
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
    -- features
  , ('▤', "chest")
  , ('Ψ', "sconce")
  , ('▣', "switch") , ('□', "switched1") , ('▪', "switched2")
  , ('‰', "gears")
  , ('ש', "harp") , ('♪', "music1") , ('♫', "music2")
  , ('₽', "fishing rod")
    -- enemy
  , ('ㄆ', "beastA") , ('ㄉ', "beastB") , ('ガ', "beastC")
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
  , ('☻', "westnr") , ('☺', "eastnr")
  , ('♂', "male") , ('♀', "female")
    -- weapon
  , ('|', "glaive") , ('†', "glaiveSilv") , ('‡', "glaiveGold")
    -- loot
  , ('∘', "item1") , ('∙', "item2")
  , ('∗', "jewel") , ('¤', "gem")
  , ('ǒ', "ring")  , ('ǫ', "amulet")
  , ('◎', "crown")
    -- effects
  , ('ϟ', "bolt")  , ('Ϟ', "arc")
  , ('ж', "flame") , ('Ж', "fire")
  , ('☼', "frost") , ('※', "ice")
  , ('§', "whirl")
  , ('✠', "heal")
  ]
