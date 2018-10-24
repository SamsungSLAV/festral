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

-- |Module for generating yaml configs for Weles from templated yamls
module Festral.Template (
    generateFromTemplate,
    TemplateType(..)
) where

import Data.List.Split
import Data.List

-- |Type represents types of templated fields of the yaml
data TemplateType
    = URI String                -- ^Url of the file extracted from its parameter string
    | Latest_URI String         -- ^URI for the latest built package with given name
    | RPMInstallCurrent String  -- ^Install package with given name by rpm from current build
    | RPMInstallLatest String   -- ^Install package with given name by rpm from all builds
    | FileContent FilePath      -- ^Put in this place content of the specified file
    | BuildTable String         -- ^Insert into this place HTML build report table with given id
    | TestTable String          -- ^Insert into this place HTML test report table with given id
    | Exec String               -- ^Execute command
    | ExecLog String String     -- ^Execute command and write to the log file

-- |Generate yaml file from template using function given as second parameter
-- for resolve templated fields
generateFromTemplate :: String -> (TemplateType -> IO String) -> IO String
generateFromTemplate yaml extractor = do
    fmap concat $ sequence $ map (\str -> if isInfixOf "TEMPLATE" str
                                            then extract extractor (words str)
                                            else return str) $ splitOn "##" yaml

extract :: (TemplateType -> IO String) -> [String] -> IO String
extract extractor ("TEMPLATE_URL":url:_) = extractor (URI url)
extract extractor ("TEMPLATE_LATEST":url:_) = extractor (Latest_URI url)
extract extractor ("TEMPLATE_RPM_INSTALL_CURRENT":packagename:_)
    = extractor (RPMInstallCurrent packagename)
extract extractor ("TEMPLATE_RPM_INSTALL_LATEST":packagename:_)
    = extractor (RPMInstallLatest packagename)
extract extractor ("TEMPLATE_FILE":filename:_)
    = extractor (FileContent filename)
extract extractor ("TEMPLATE_BUILD_TABLE":id:_) = extractor (BuildTable id)
extract extractor ("TEMPLATE_TEST_TABLE":id:_) = extractor (TestTable id)
extract extractor ("TEMPLATE_RUN_TEST":cmd)
    = extractor (ExecLog (unwords cmd) "/tmp/test.log")
extract extractor ("TEMPLATE_RUN":cmd)
    = extractor (Exec $ unwords cmd)
extract _ _ = return ""
