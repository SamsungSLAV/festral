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
    | BuildTable                -- ^Insert into this place HTML build report table
    | TestTable                 -- ^Insert into this place HTML test report table

-- |Generate yaml file from template using function given as second parameter for resolve templated fields
generateFromTemplate :: String -> (TemplateType -> IO String) -> IO String
generateFromTemplate yaml extractor = do
    fmap concat $ sequence $ map (\str -> if isInfixOf "TEMPLATE" str then extract extractor (words str) else return str) $ splitOn "##" yaml

extract :: (TemplateType -> IO String) -> [String] -> IO String
extract extractor ("TEMPLATE_URL":url:_) = extractor (URI url)
extract extractor ("TEMPLATE_LATEST":url:_) = extractor (Latest_URI url)
extract extractor ("TEMPLATE_RPM_INSTALL_CURRENT":packagename:_) = extractor (RPMInstallCurrent packagename)
extract extractor ("TEMPLATE_RPM_INSTALL_LATEST":packagename:_) = extractor (RPMInstallLatest packagename)
extract extractor ("TEMPLATE_FILE":filename:_) = extractor (FileContent filename)
extract extractor ("TEMPLATE_BUILD_TABLE":_) = extractor BuildTable
extract extractor ("TEMPLATE_TEST_TABLE":_) = extractor TestTable
extract _ _ = return ""
