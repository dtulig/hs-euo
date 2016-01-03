module Tulig.UO.Types where

data UOFunctionResult = IgnoreResult
                      | ReturnResult

data UOModKey = MK_CTRL
              | MK_ALT
              | MK_SHIFT
                deriving (Show, Eq)

type UOKey = String

-- A-Z
uoK_A :: UOKey
uoK_A = "A"
uoK_B :: UOKey
uoK_B = "B"
uoK_C :: UOKey
uoK_C = "C"
uoK_D :: UOKey
uoK_D = "D"
uoK_E :: UOKey
uoK_E = "E"
uoK_F :: UOKey
uoK_F = "F"
uoK_G :: UOKey
uoK_G = "G"
uoK_H :: UOKey
uoK_H = "H"
uoK_I :: UOKey
uoK_I = "I"
uoK_J :: UOKey
uoK_J = "J"
uoK_K :: UOKey
uoK_K = "K"
uoK_L :: UOKey
uoK_L = "L"
uoK_M :: UOKey
uoK_M = "M"
uoK_N :: UOKey
uoK_N = "N"
uoK_O :: UOKey
uoK_O = "O"
uoK_P :: UOKey
uoK_P = "P"
uoK_Q :: UOKey
uoK_Q = "Q"
uoK_R :: UOKey
uoK_R = "R"
uoK_S :: UOKey
uoK_S = "S"
uoK_T :: UOKey
uoK_T = "T"
uoK_U :: UOKey
uoK_U = "U"
uoK_V :: UOKey
uoK_V = "V"
uoK_W :: UOKey
uoK_W = "W"
uoK_X :: UOKey
uoK_X = "X"
uoK_Y :: UOKey
uoK_Y = "Y"
uoK_Z :: UOKey
uoK_Z = "Z"

-- 1-9
uoK_1 :: UOKey
uoK_1 = "1"
uoK_2 :: UOKey
uoK_2 = "2"
uoK_3 :: UOKey
uoK_3 = "3"
uoK_4 :: UOKey
uoK_4 = "4"
uoK_5 :: UOKey
uoK_5 = "5"
uoK_6 :: UOKey
uoK_6 = "6"
uoK_7 :: UOKey
uoK_7 = "7"
uoK_8 :: UOKey
uoK_8 = "8"
uoK_9 :: UOKey
uoK_9 = "9"

-- F1-F12
uoK_F1 :: UOKey
uoK_F1 = "F1"
uoK_F2 :: UOKey
uoK_F2 = "F2"
uoK_F3 :: UOKey
uoK_F3 = "F3"
uoK_F4 :: UOKey
uoK_F4 = "F4"
uoK_F5 :: UOKey
uoK_F5 = "F5"
uoK_F6 :: UOKey
uoK_F6 = "F6"
uoK_F7 :: UOKey
uoK_F7 = "F7"
uoK_F8 :: UOKey
uoK_F8 = "F8"
uoK_F9 :: UOKey
uoK_F9 = "F9"
uoK_F10 :: UOKey
uoK_F10 = "F10"
uoK_F11 :: UOKey
uoK_F11 = "F11"
uoK_F12 :: UOKey
uoK_F12 = "F12"

-- Special Keys
uoK_ESC :: UOKey
uoK_ESC = "ESC"
uoK_BACK :: UOKey
uoK_BACK = "BACK"
uoK_TAB :: UOKey
uoK_TAB = "TAB"
uoK_ENTER :: UOKey
uoK_ENTER = "ENTER"
uoK_PAUSE :: UOKey
uoK_PAUSE = "PAUSE"
uoK_CAPSLOCK :: UOKey
uoK_CAPSLOCK = "CAPSLOCK"
uoK_SPACE :: UOKey
uoK_SPACE = "SPACE"
uoK_PGDN :: UOKey
uoK_PGDN = "PGDN"
uoK_PGUP :: UOKey
uoK_PGUP = "PGUP"
uoK_END :: UOKey
uoK_END = "END"
uoK_HOME :: UOKey
uoK_HOME = "HOME"
uoK_LEFT :: UOKey
uoK_LEFT = "LEFT"
uoK_RIGHT :: UOKey
uoK_RIGHT = "RIGHT"
uoK_UP :: UOKey
uoK_UP = "UP"
uoK_DOWN :: UOKey
uoK_DOWN = "DOWN"
uoK_PRNSCR :: UOKey
uoK_PRNSCR = "PRNSCR"
uoK_INSERT :: UOKey
uoK_INSERT = "INSERT"
uoK_DELETE :: UOKey
uoK_DELETE = "DELETE"
uoK_NUMLOCK :: UOKey
uoK_NUMLOCK = "NUMLOCK"
uoK_SCROLLLOCK :: UOKey
uoK_SCROLLLOCK = "SCROLLLOCK"
