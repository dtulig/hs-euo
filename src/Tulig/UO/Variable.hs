module Tulig.UO.Variable where

import Tulig.UO.Base

getAR :: UO Int
getAR = getVarInt "AR"

getBackpackID :: UO Int
getBackpackID = getVarInt "BackpackID"

getCR :: UO Int
getCR = getVarInt "CR"

getCharDir :: UO Int
getCharDir = getVarInt "CharDir"

getCharID :: UO Int
getCharID = getVarInt "CharID"

getCharName :: UO String
getCharName = getVarStr "CharName"

getCliCnt :: UO Int
getCliCnt = getVarInt "CliCnt"

getCharPosX :: UO Int
getCharPosX = getVarInt "CharPosX"

getCharPosY :: UO Int
getCharPosY = getVarInt "CharPosY"

getCharPosZ :: UO Int
getCharPosZ = getVarInt "CharPosZ"

getCharStatus :: UO String
getCharStatus = getVarStr "CharStatus"

getCliLang :: UO String
getCliLang = getVarStr "CliLang"

getCliLeft :: UO Int
getCliLeft = getVarInt "CliLeft"

getCliLogged :: UO Bool
getCliLogged = getVarBool "CliLogged"

getCliTop :: UO Int
getCliTop = getVarInt "CliTop"

getCliVer :: UO String
getCliVer = getVarStr "CliVer"

getCliXRes :: UO Int
getCliXRes = getVarInt "CliXRes"

getCliYRes :: UO Int
getCliYRes = getVarInt "CliYRes"

getContID :: UO Int
getContID = getVarInt "ContID"

getContKind :: UO Int
getContKind = getVarInt "ContKind"

getContName :: UO String
getContName = getVarStr "ContName"

getContPosX :: UO Int
getContPosX = getVarInt "ContPosX"

getContPosY :: UO Int
getContPosY = getVarInt "ContPosY"

getContSizeX :: UO Int
getContSizeX = getVarInt "ContSizeX"

getContSizeY :: UO Int
getContSizeY = getVarInt "ContSizeY"

getContType :: UO Int
getContType = getVarInt "ContType"

getCursKind :: UO Int
getCursKind = getVarInt "CursKind"

getDex :: UO Int
getDex = getVarInt "Dex"

getER :: UO Int
getER = getVarInt "ER"

getEnemyHits :: UO Int
getEnemyHits = getVarInt "EnemyHits"

getEnemyID :: UO Int
getEnemyID = getVarInt "EnemyID"

getFR :: UO Int
getFR = getVarInt "FR"

getFollowers :: UO Int
getFollowers = getVarInt "Followers"

getInt :: UO Int
getInt = getVarInt "Int"

getLHandID :: UO Int
getLHandID = getVarInt "LHandID"

getLLiftedID :: UO Int
getLLiftedID = getVarInt "LLiftedID"

getLLiftedType :: UO Int
getLLiftedType = getVarInt "LLiftedType"

getLLiftedKind :: UO Int
getLLiftedKind = getVarInt "LLiftedKind"

getLObjectID :: UO Int
getLObjectID = getVarInt "LObjectID"

getLObjectType :: UO Int
getLObjectType = getVarInt "LObjectType"

getLShard :: UO Int
getLShard = getVarInt "LShard"

getLSkill :: UO Int
getLSkill = getVarInt "LSkill"

getLSpell :: UO Int
getLSpell = getVarInt "LSpell"

getLTargetID :: UO Int
getLTargetID = getVarInt "LTargetID"

getLTargetKind :: UO Int
getLTargetKind = getVarInt "LTargetKind"

getLTargetTile :: UO Int
getLTargetTile = getVarInt "LTargetTile"

getLTargetX :: UO Int
getLTargetX = getVarInt "LTargetX"

getLTargetY :: UO Int
getLTargetY = getVarInt "LTargetY"

getLTargetZ :: UO Int
getLTargetZ = getVarInt "LTargetZ"

getLuck :: UO Int
getLuck = getVarInt "Luck"

getMana :: UO Int
getMana = getVarInt "Mana"

getMaxDmg :: UO Int
getMaxDmg = getVarInt "MaxDmg"

getMaxFol :: UO Int
getMaxFol = getVarInt "MaxFol"

getMaxHits :: UO Int
getMaxHits = getVarInt "MaxHits"

getMaxMana :: UO Int
getMaxMana = getVarInt "MaxMana"

getMaxStam :: UO Int
getMaxStam = getVarInt "MaxStam"

getMaxStats :: UO Int
getMaxStats = getVarInt "MaxStats"

getMaxWeight :: UO Int
getMaxWeight = getVarInt "MaxWeight"

getMinDmg :: UO Int
getMinDmg = getVarInt "MinDmg"

getNextCPosX :: UO Int
getNextCPosX = getVarInt "NexCPosX"

getNextCPosY :: UO Int
getNextCPosY = getVarInt "NexCPosY"

getPR :: UO Int
getPR = getVarInt "PR"

getRHandID :: UO Int
getRHandID = getVarInt "RHandID"

getSex :: UO Int
getSex = getVarInt "Sex"

getShard :: UO String
getShard = getVarStr "Shard"

getStamina :: UO Int
getStamina = getVarInt "Stamina"

getStatBar :: UO Int
getStatBar = getVarInt "StatBar"

getStr :: UO Int
getStr = getVarInt "Str"

getSysMsg :: UO String
getSysMsg = getVarStr "SysMsg"

getTargCurs :: UO Bool
getTargCurs = getVarBool "TargCurs"

getTP :: UO Int
getTP = getVarInt "TP"

getWeight :: UO Int
getWeight = getVarInt "Weight"

setCliNr :: Int -> UO ()
setCliNr i = setVarInt "CliNr" i

setCliLeft :: Int -> UO ()
setCliLeft i = setVarInt "CliLeft" i

setCliTop :: Int -> UO ()
setCliTop i = setVarInt "CliTop" i

setCliXRes :: Int -> UO ()
setCliXRes i = setVarInt "CliXRes" i

setCliYRes :: Int -> UO ()
setCliYRes i = setVarInt "CliYRes" i

setContPosX :: Int -> UO ()
setContPosX i = setVarInt "ContPosX" i

setContPosY :: Int -> UO ()
setContPosY i = setVarInt "ContPosY" i

setLHandID :: Int -> UO ()
setLHandID i = setVarInt "LHandID" i

setLObjectID :: Int -> UO ()
setLObjectID i = setVarInt "LObjectID" i

setLShard :: Int -> UO ()
setLShard i = setVarInt "LShard" i

setLSkill :: Int -> UO ()
setLSkill i = setVarInt "LSkill" i

setLSpell :: Int -> UO ()
setLSpell i = setVarInt "LSpell" i

setLTargetID :: Int -> UO ()
setLTargetID i = setVarInt "LTargetID" i

setLTargetKind :: Int -> UO ()
setLTargetKind i = setVarInt "LTargetKind" i

setLTargetTile :: Int -> UO ()
setLTargetTile i = setVarInt "LTargetTile" i

setLTargetX :: Int -> UO ()
setLTargetX i = setVarInt "LTargetX" i

setLTargetY :: Int -> UO ()
setLTargetY i = setVarInt "LTargetY" i

setLTargetZ :: Int -> UO ()
setLTargetZ i = setVarInt "LTargetZ" i

setNextCPosX :: Int -> UO ()
setNextCPosX i = setVarInt "NextCPosX" i

setNextCPosY :: Int -> UO ()
setNextCPosY i = setVarInt "NextCPosY" i

setRHandID :: Int -> UO ()
setRHandID i = setVarInt "RHandID" i

setTargCurs :: Bool -> UO ()
setTargCurs b = setVarBool "TargCurs" b
