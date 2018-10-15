{-# LANGUAGE OverloadedStrings #-}

module Festral.WWW.TestGUI (
    indexRespond
)
where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid
import Festral.Builder.Meta
import Festral.Files
import Festral.Config
import Data.List.Split
import Data.List
import qualified Data.ByteString.UTF8 as BSU
import System.Directory

indexRespond opts config r ["add_test"] = r $ addTest
indexRespond opts config r query = r $ index query

index x = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p><a href=\"tests\">Tests</a></p>"
    , "<p><a href=\"files\">Reports</a></p>"
    , "<p><a href=\"deploys\">Deploys</a></p>"
    ]

addTest = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    ["<p> Target: <select>"
    ,"<option value=\"KantM2\">KantM2</option>"
    ,"<option value=\"KantM1\">KantM1</option>"
    ,"<option value=\"rpi3\">Raspberri Pi 3</option>"
    ,"</select></p>"
    ,"<p> "
    ]
