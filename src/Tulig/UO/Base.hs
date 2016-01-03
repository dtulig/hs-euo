module Tulig.UO.Base where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)

import Tulig.UO.Foreign
import Tulig.UO.Types (UOFunctionResult(..))

data UOClient = UOClient { handle :: Int }

type UO = ReaderT UOClient IO

connect :: IO UOClient
connect = do
  h <- open
  _ <- executeFunction h ReturnResult UOSet [(UOString "CliNr"), (UOInteger 1)]
  return (UOClient h)

data UOArg = UOInteger Int
           | UODouble Double
           | UOBool Bool
           | UOString String
           | UONil

getArg :: Int -> Int -> IO UOArg
getArg h i = do
  t <- getType h i
  case t of
    1 -> (getBoolean h i) >>= (return . UOBool)
    3 -> (getInteger h i) >>= (return . UOInteger)
    4 -> (getString h i) >>= (return . UOString)
    _ -> return UONil

data UOFunc = UOCommand String
            | UOEvent String
            | UOGet
            | UOSet

pushFunction :: Int -> UOFunc -> IO ()
pushFunction h (UOCommand s) = pushStrVal h "Call" >> pushStrVal h s
pushFunction h (UOEvent s) = pushStrVal h s
pushFunction h UOGet = pushStrVal h "Get"
pushFunction h UOSet = pushStrVal h "Set"

pushArg :: Int -> UOArg -> IO ()
pushArg h (UOInteger i) = pushInteger h i
pushArg h (UODouble d) = pushDouble h d
pushArg h (UOBool b) = pushBoolean h b
pushArg h (UOString s) = pushStrVal h s
pushArg h UONil = pushNil h

executeCommand :: UOFunctionResult -> String -> [UOArg] -> UO [UOArg]
executeCommand ignoreResult name args = do
  h <- asks handle
  res <- liftIO $ executeFunction h ignoreResult (UOCommand name) args
  return res

executeFunction :: Int -> UOFunctionResult -> UOFunc -> [UOArg] -> IO [UOArg]
executeFunction h ignoreResult func args =
  -- Erase the stack.
  setTop h 0 >>
  -- Push function name onto the stack.
  pushFunction h func >>
  -- Push arguments onto the stack.
  mapM_ (pushArg h) args >>
  -- Execute.
  execute h >>
  -- Build result from FFI calls.
  buildResult ignoreResult
  where buildResult IgnoreResult = return []
        buildResult ReturnResult = do
          -- Read how many items are on the stack, pull them off.
          cnt <- getTop h
          mapM (getArg h) [1..cnt]

uoArgToString :: UOArg -> String
uoArgToString (UOString s) = s
uoArgToString (UOInteger i) = show i
uoArgToString (UODouble d) = show d
uoArgToString (UOBool b) = show b
uoArgToString UONil = ""

uoArgToInteger :: UOArg -> Int
uoArgToInteger (UOInteger i) = i
uoArgToInteger (UODouble d) = truncate d
uoArgToInteger (UOBool b)
  | b = 1
  | otherwise = 0
uoArgToInteger _ = 0

uoArgToBoolean :: UOArg -> Bool
uoArgToBoolean (UOInteger i)
  | i == 1 = True
  | otherwise = False
uoArgToBoolean (UODouble d)
  | d == 1.0 = True
  | otherwise = False
uoArgToBoolean (UOString s)
  | s == "True" || s == "1" = True
  | otherwise = False
uoArgToBoolean (UOBool b) = b
uoArgToBoolean _ = False

getFirstUOArgSafe :: [UOArg] -> UOArg
getFirstUOArgSafe (x:_) = x
getFirstUOArgSafe _ = UONil

getVar :: String -> UO UOArg
getVar name = do
  h <- asks handle
  res <- liftIO $ executeFunction h ReturnResult UOGet [(UOString name)]
  return (getFirstUOArgSafe res)

getVarStr :: String -> UO String
getVarStr name = (getVar name) >>= return . uoArgToString

getVarInt :: String -> UO Int
getVarInt name = (getVar name) >>= return . uoArgToInteger

getVarBool :: String -> UO Bool
getVarBool name = (getVar name) >>= return . uoArgToBoolean

setVar :: String -> UOArg -> UO ()
setVar name v = do
  h <- asks handle
  _ <- liftIO $ executeFunction h ReturnResult UOSet [(UOString name), v]
  return ()

setVarStr :: String -> String -> UO ()
setVarStr name s = setVar name (UOString s)

setVarInt :: String -> Int -> UO ()
setVarInt name i = setVar name (UOInteger i)

setVarBool :: String -> Bool -> UO ()
setVarBool name b = setVar name (UOBool b)
