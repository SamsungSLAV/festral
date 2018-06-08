-- |Module for generating yaml configs for Weles from templated yamls
module Festral.Weles.YamlTemplate (
    generateFromTemplate,
    TemplateType(..)
) where

import Data.List.Split
import Data.List

-- |Type represents types of templated fields of the yaml
data TemplateType = 
    URL String -- ^Url of the file extracted from its parameter string

-- |Generate yaml file from template using function given as second parameter for resolve templated fields
generateFromTemplate :: String -> (TemplateType -> String) -> String
generateFromTemplate yaml extractor = concat $ map (\str -> if isInfixOf "TEMPLATE" str then extract extractor (words str) else str) $ splitOn "##" yaml

extract :: (TemplateType -> String) -> [String] -> String
extract extractor ("TEMPLATE_URL":url:_) = extractor (URL url)
extract _ _ = ""
