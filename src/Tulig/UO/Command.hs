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

-- TODO GetJournal
-- TODO GetShop

data UOSkillLock = Up
                 | Down
                 | Locked
                 deriving (Show)

uoSkillLockFromInt :: Int -> UOSkillLock
uoSkillLockFromInt 1 = Down
uoSkillLockFromInt 2 = Locked
uoSkillLockFromInt _ = Up

data UOSkill = UOSkill { uoSkillNorm :: Int
                       , uoSkillReal :: Int
                       , uoSkillCap :: Int
                       , uoSkillLock :: UOSkillLock }
               deriving (Show)

getSkill :: String -> UO (Maybe UOSkill)
getSkill skill = do
  res <- executeCommand ReturnResult "GetSkill" [(UOString skill)]
  return $ case (map uoArgToInteger res) of
    [f1, f2, f3, f4] -> Just (UOSkill f1 f2 f3 (uoSkillLockFromInt f4))
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

-- TODO popup
-- TODO property
-- TODO renamepet
-- TODO scanitems

scanItems :: Bool -> UO Int
scanItems visibleOnly =
  executeCommand ReturnResult "ScanItems" [(UOBool visibleOnly)] >>= return . scanItemsResult
  where scanItemsResult (x:_) = uoArgToInteger x
        scanItemsResult _ = 0

-- TODO scanjournal
-- TODO setshop
-- TODO skilllock
-- TODO statbar
-- TODO statlock

sysMessage :: String -> Int -> UO ()
sysMessage str color =
  executeCommand IgnoreResult "SysMessage" [(UOString str), (UOInteger color)] >> return ()

-- TODO TileCnt
-- TODO TileGet
-- TODO TileInit
