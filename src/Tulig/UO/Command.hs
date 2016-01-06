module Tulig.UO.Command where

import Tulig.UO.Base
import Tulig.UO.Types

cliDrag :: Int -> UO ()
cliDrag i = executeCommand IgnoreResult "CliDrag" [(UOInteger i)] >> return ()

drag :: Int -> Int -> UO ()
drag i amt = executeCommand IgnoreResult "Drag" [(UOInteger i), (UOInteger amt)] >> return ()

-- drop container
dropC :: Int -> Int -> Int -> UO ()
dropC contId x y =
  executeCommand IgnoreResult "DropC" [(UOInteger contId), (UOInteger x), (UOInteger y)] >> return ()

dropC' :: Int -> UO ()
dropC' contId = dropC contId (-1) (-1)

-- drop ground
dropG :: Int -> Int -> Int -> UO ()
dropG x y z =
  executeCommand IgnoreResult "DropG" [(UOInteger x), (UOInteger y), (UOInteger z)] >> return ()

-- drop paper doll
dropPD :: UO [UOArg]
dropPD = executeCommand IgnoreResult "DropPD" []

exMsg :: Int -> Int -> Int -> String -> UO [UOArg]
exMsg ident font color str =
  executeCommand IgnoreResult "ExMsg" [(UOInteger ident), (UOInteger font), (UOInteger color), (UOString str)]

-- TODO: getCont

data UOItem = UOItem { uoItemId :: Int
                     , uoItemType :: Int
                     , uoItemKind :: Int
                     , uoItemContId :: Int
                     , uoItemX :: Int
                     , uoItemY :: Int
                     , uoItemZ :: Int
                     , uoItemStack :: Int
                     , uoItemRep :: Int
                     , uoItemColor :: Int }
            deriving (Show)

-- Get's an item given an index. All return values should be UOArgs so
-- this will first convert UOArgs to Ints and then build a UOItem
-- record.
-- TODO Probably want to return Either with an error.
getItem :: Int -> UO UOItem
getItem i = do
  res <- executeCommand ReturnResult "GetItem" [(UOInteger i)]
  case (map uoArgToInteger res) of
    [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10] ->
      return (UOItem f1 f2 f3 f4 f5 f6 f7 f8 f9 f10)
    x -> error $ "Invalid number of uo items returned: " ++ show x

data UOJournalEntry = UOJournalEntry { uoJournalEntryLine :: String
                                     , uoJournalEntryColor :: Int }
                    deriving (Show)

getJournal :: Int -> UO (Maybe UOJournalEntry)
getJournal i = do
  res <- executeCommand ReturnResult "GetJournal" [(UOInteger i)]
  return $ case res of
    [f1, f2] -> Just (UOJournalEntry (uoArgToString f1) (uoArgToInteger f2))
    _ -> Nothing

-- TODO GetShop

data UOLock = UOLockUp
            | UOLockDown
            | UOLockLocked
            deriving (Show)

uoLockFromInt :: Int -> UOLock
uoLockFromInt 1 = UOLockDown
uoLockFromInt 2 = UOLockLocked
uoLockFromInt _ = UOLockUp

uoLockToInt :: UOLock -> Int
uoLockToInt UOLockUp = 0
uoLockToInt UOLockDown = 1
uoLockToInt UOLockLocked = 2

data UOSkill = UOSkill { uoSkillNorm :: Int
                       , uoSkillReal :: Int
                       , uoSkillCap :: Int
                       , uolock :: UOLock }
               deriving (Show)

getSkill :: String -> UO (Maybe UOSkill)
getSkill skill = do
  res <- executeCommand ReturnResult "GetSkill" [(UOString skill)]
  return $ case (map uoArgToInteger res) of
    [f1, f2, f3, f4] -> Just (UOSkill f1 f2 f3 (uoLockFromInt f4))
    _ -> Nothing

hideItem :: Int -> UO ()
hideItem i = executeCommand IgnoreResult "HideItem" [(UOInteger i)] >> return ()

keyWithMods :: String -> [UOModKey] -> UO ()
keyWithMods k modKeys =
  executeCommand IgnoreResult "Key" [ UOString k
                            , UOBool (MK_CTRL `elem` modKeys)
                            , UOBool (MK_ALT `elem` modKeys)
                            , UOBool (MK_SHIFT `elem` modKeys)] >> return ()

key :: UOKey -> UO ()
key k = keyWithMods k []

macro :: Int -> Int -> String -> UO [UOArg]
macro par1 par2 str = executeCommand IgnoreResult "Macro" [(UOInteger par1), (UOInteger par2), (UOString str)]

macroP1 :: Int -> UO [UOArg]
macroP1 par1 = macro par1 0 ""

macroP2 :: Int -> Int -> UO [UOArg]
macroP2 par1 par2 = macro par1 par2 ""

macroS :: Int -> String -> UO [UOArg]
macroS par1 str = macro par1 0 str

move :: Int -> Int -> Int -> Int -> UO Bool
move x y acc timeout = do
  res <- executeCommand ReturnResult "Move" [(UOInteger x), (UOInteger y), (UOInteger acc), (UOInteger timeout)]
  return (moveResult res)
  where moveResult (x1:_) = uoArgToBoolean x1
        moveResult _ = False

msg :: String -> UO ()
msg str = executeCommand IgnoreResult "Msg" [(UOString str)] >> return ()

pathfind :: Int -> Int -> Int -> UO ()
pathfind x y z =
  executeCommand IgnoreResult "Pathfind" [(UOInteger x), (UOInteger y), (UOInteger z)] >> return ()

popup :: Int -> UO ()
popup nId =
  executeCommand IgnoreResult "Popup" [(UOInteger nId)] >> return ()

data UOProperty = UOProperty { uoPropertyName :: String
                             , uoPropertyInfo :: String }

getProperty :: Int -> UO (Maybe UOProperty)
getProperty i = do
  res <- executeCommand ReturnResult "Property" [(UOInteger i)]
  return $ case (map uoArgToString res) of
    [f1, f2] -> Just (UOProperty f1 f2)
    _ -> Nothing

renamePet :: Int -> String -> UO ()
renamePet nId name =
  executeCommand IgnoreResult "RenamePet" [(UOInteger nId), (UOString name)] >> return ()

scanItems :: Bool -> UO Int
scanItems visibleOnly =
  executeCommand ReturnResult "ScanItems" [(UOBool visibleOnly)] >>= return . scanItemsResult
  where scanItemsResult (x:_) = uoArgToInteger x
        scanItemsResult _ = 0

data UOJournalInfo = UOJournalInfo { uoJournalInfoNewRef :: Int
                                   , uoJournalInfoCount :: Int }
                   deriving (Show)

scanJournal :: Int -> UO (Maybe UOJournalInfo)
scanJournal oldRef = do
  res <- executeCommand ReturnResult "ScanJournal" [(UOInteger oldRef)]
  return $ case (map uoArgToInteger res) of
    [f1, f2] -> Just (UOJournalInfo f1 f2)
    _ -> Nothing

-- TODO setshop

skillLock :: String -> UOLock -> UO ()
skillLock skill lock =
  executeCommand IgnoreResult "SkillLock" [(UOString skill), (UOInteger (uoLockToInt lock))] >> return ()

statBar :: Int -> UO ()
statBar i = executeCommand IgnoreResult "StatBar" [(UOInteger i)] >> return ()

statLock :: UOStat -> UOLock -> UO ()
statLock stat lock =
  executeCommand IgnoreResult "StatLock" [ (UOString (uoStatToString stat))
                                         , (UOInteger (uoLockToInt lock))] >> return ()

sysMessage :: String -> Int -> UO ()
sysMessage str color =
  executeCommand IgnoreResult "SysMessage" [(UOString str), (UOInteger color)] >> return ()

tileCnt :: Int -> Int -> UO Int
tileCnt x y = do
  res <- executeCommand ReturnResult "TileCnt" [(UOInteger x), (UOInteger y)]
  return $ case (map uoArgToInteger res) of
    [f1] -> f1
    _ -> 0

tileCnt' :: Int -> Int -> UOFacet -> UO Int
tileCnt' x y facet = do
  res <- executeCommand ReturnResult "TileCnt" [ (UOInteger x)
                                               , (UOInteger y)
                                               , (UOInteger (uoFacetToInt facet))]
  return $ case (map uoArgToInteger res) of
    [f1] -> f1
    _ -> 0

data UOTile = UOTile { uoTileType :: Int
                     , uoTileZ :: Int
                     , uoTileName :: String
                     , uoTileFlags :: String }

tileGet :: Int -> Int -> Int -> UO (Maybe UOTile)
tileGet x y i = do
  res <- executeCommand ReturnResult "TileGet" (map UOInteger [x, y, i])
  return $ case res of
    [f1, f2, f3, f4] -> Just (UOTile
                              (uoArgToInteger f1)
                              (uoArgToInteger f2)
                              (uoArgToString f3)
                              (uoArgToString f4))
    _ -> Nothing

tileInit :: Bool -> UO Bool
tileInit noOverrides = do
  res <- executeCommand ReturnResult "TileInit" [(UOBool noOverrides)]
  return $ case res of
    (x:_) -> (uoArgToBoolean x)
    _ -> False
