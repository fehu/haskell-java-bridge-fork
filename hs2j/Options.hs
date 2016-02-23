{-# LANGUAGE Haskell2010
    , TemplateHaskell
 #-}

module Options where

import Data.Name
import Data.NamedRecord
import Data.Word

-- The options as used in the application later on.
-- This is a named record, see Data.NamedRecord for
-- more information.
record' "Options"
    `has` "optShowHelp" := False
    `has` "optShowVersion" := False
    `has` "optClasspath" := ["."]
    `has` "optSearchDepth" := (100 :: Word32)
    `has` "optTargetDirectory" := "bindings"
    `has` "optVerbose" := False
    `has` "optCompleteSE6" := False


