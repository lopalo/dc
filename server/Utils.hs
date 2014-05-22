module Utils (delPrefix) where

import Data.String.Utils (split, join)


delPrefix :: String -> String -> String -> String
delPrefix delimiter prefix str =
    case delimiter `split` str of
        h:rest | h == prefix-> delimiter `join` rest

