{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main
  (main) where


import ActionSpaces
import Analytics
import Parameterization
import Strategies
import Types

import OpenGames.Engine.Engine

-- 1. main executable
main  = do
  parameterSearchModelBasic
  parameterSearchModelBasic2
  parameterSearchModelBayesian
