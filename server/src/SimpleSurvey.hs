module SimpleSurvey
  ( main,
  )
where

import qualified SimpleSurvey.Init as Init

main :: IO ()
main = Init.runApp
