{-# LANGUAGE ForeignFunctionInterface #-}
module Tulig.UO.Foreign where

import Foreign.C

-- Return version of the dll.
foreign import stdcall "Version" c_Version :: IO CInt
getVersion :: IO CInt
getVersion = c_Version

-- Returns a window handle.
foreign import stdcall "Open" c_Open :: IO CInt
open :: IO Int
open = c_Open >>= (return . fromIntegral)

-- Essential queries for function execution.
foreign import stdcall "Close" c_Close :: CInt -> IO CInt
close :: Int -> IO Int
close h = c_Close (fromIntegral h) >>= (return . fromIntegral)

foreign import stdcall "Execute" c_Execute :: CInt -> IO CInt
execute :: Int -> IO Int
execute h = c_Execute (fromIntegral h) >>= (return . fromIntegral)

foreign import stdcall "GetTop" c_GetTop :: CInt -> IO CInt
getTop :: Int -> IO Int
getTop h = c_GetTop (fromIntegral h) >>= (return . fromIntegral)

foreign import stdcall "GetType" c_GetType :: CInt -> CInt -> IO CInt
getType :: Int -> Int -> IO Int
getType h i = c_GetType (fromIntegral h) (fromIntegral i) >>= (return . fromIntegral)

foreign import stdcall "Insert" c_Insert :: CInt -> CInt -> IO ()
insert :: Int -> Int -> IO ()
insert h i = c_Insert (fromIntegral h) (fromIntegral i)

-- Functions for pushing values onto the stack.
foreign import stdcall "PushNil" c_PushNil :: CInt -> IO ()
pushNil :: Int -> IO ()
pushNil h = c_PushNil (fromIntegral h)

foreign import stdcall "PushBoolean" c_PushBoolean :: CInt -> CInt -> IO ()
pushBoolean :: Int -> Bool -> IO ()
pushBoolean h b = c_PushBoolean (fromIntegral h) (fromBool b)
  where fromBool True = 1
        fromBool _ = 0

foreign import stdcall "PushInteger" c_PushInteger :: CInt -> CInt -> IO ()
pushInteger :: Int -> Int -> IO ()
pushInteger h i = c_PushInteger (fromIntegral h) (fromIntegral i)

foreign import stdcall "PushDouble" c_PushDouble :: CInt -> CDouble -> IO ()
pushDouble :: Int -> Double -> IO ()
pushDouble h d = c_PushDouble (fromIntegral h) (realToFrac d)

foreign import stdcall "PushStrRef" c_PushStrRef :: CInt -> CString -> IO ()
pushStrRef :: Int -> String -> IO ()
pushStrRef h s = newCString s >>= c_PushStrRef (fromIntegral h)

foreign import stdcall "PushStrVal" c_PushStrVal :: CInt -> CString -> IO ()
pushStrVal :: Int -> String -> IO ()
pushStrVal h s = do
  cStr <- newCString s
  c_PushStrVal (fromIntegral h) cStr

foreign import stdcall "PushValue" c_PushValue :: CInt -> CInt -> IO ()
pushValue :: Int -> Int -> IO ()
pushValue h b = c_PushValue (fromIntegral h) (fromIntegral b)

-- Functions for getting values off the stack.
foreign import stdcall "GetBoolean" c_GetBoolean :: CInt -> CInt -> IO CInt
getBoolean :: Int -> Int -> IO Bool
getBoolean h i = c_GetBoolean (fromIntegral h) (fromIntegral i) >>= (return . toBoolean)
  where toBoolean 1 = True
        toBoolean _ = False

foreign import stdcall "GetInteger" c_GetInteger :: CInt -> CInt -> IO CInt
getInteger :: Int -> Int -> IO Int
getInteger h i = c_GetInteger (fromIntegral h) (fromIntegral i) >>= (return . fromIntegral)

foreign import stdcall "GetDouble" c_GetDouble :: CInt -> CInt -> IO CDouble
getDouble :: Int -> Int -> IO Double
getDouble h i = c_GetDouble (fromIntegral h) (fromIntegral i) >>= (return . realToFrac)

foreign import stdcall "GetString" c_GetString :: CInt -> CInt -> IO CString
getString :: Int -> Int -> IO String
getString h i = c_GetString (fromIntegral h) (fromIntegral i) >>= peekCString >>= return

-- Misc functions for managing the stack.
foreign import stdcall "Remove" c_Remove :: CInt -> CInt -> IO ()
remove :: Int -> Int -> IO ()
remove h i = c_Remove (fromIntegral h) (fromIntegral i)

foreign import stdcall "SetTop" c_SetTop :: CInt -> CInt -> IO ()
setTop :: Int -> Int -> IO ()
setTop h i = c_SetTop (fromIntegral h) (fromIntegral i)

foreign import stdcall "Mark" c_Mark :: CInt -> IO ()
mark :: Int -> IO ()
mark h = c_Mark (fromIntegral h)

foreign import stdcall "Clean" c_Clean :: CInt -> IO ()
clean :: Int -> IO ()
clean h = c_Clean (fromIntegral h)
