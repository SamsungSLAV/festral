{-
 - Copyright (c) 2018 Samsung Electronics Co., Ltd All Rights Reserved
 -
 - Author: Uladzislau Harbuz <u.harbuz@samsung.com>
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -      http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License
 -}

{-# LANGUAGE OverloadedStrings #-}

module Festral.WWW.TestGUI (
    indexRespond
)
where

import Network.Wai
import Network.HTTP.Types (status200, status404)
import Blaze.ByteString.Builder (copyByteString)
import Paths_Festral (version)
import Data.Version (showVersion)
import qualified Data.ByteString.UTF8 as BS

indexRespond opts config r query = r $ index query

index x = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat
    $ map copyByteString
    [ "<p><a href=\"tests\">Tests</a></p>"
    , "<p><a href=\"files\">Files</a></p>"
    , "<p><a href=\"deploys\">Deploys</a></p>"
    , "<hr><h6>Festral v."
    , BS.fromString $ showVersion version
    , "</h6>"
    ]
