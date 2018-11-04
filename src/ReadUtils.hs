{-# LANGUAGE OverloadedStrings #-}

module ReadUtils where

import           Data.List  (isPrefixOf)
import           Data.Maybe (catMaybes, isJust)

-- helper fn for deriving Read instances for use with readsPrec.
readMatch :: [(a, String)] -> String -> [(a, String)]
readMatch cases str =
  let
    mapper (t, s) = takePrefixSub t s str
    matched = catMaybes $ map mapper cases
  in
    if (length matched > 0)
    then [head matched]
    else []


-- takePrefix sub prefix str = Maybe rest
takePrefixSub :: a -> String -> String -> Maybe (a, String)
takePrefixSub substitute prefix str =
  if prefix `isPrefixOf` str
    then Just (substitute, drop (length prefix) str)
    else Nothing

-- -- Example
-- data Proto = Https | Http
--   deriving (Eq)

-- instance Show Proto where
--   show Https = "https://"
--   show Http  = "http://"

-- instance Read Proto where
--   readsPrec _ str =
--     readMatch [(Http, "http://"), (Https, "https://")] str
