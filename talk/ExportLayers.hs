{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath)
import Text.XML as Xml
import Text.XML.Lens
import Data.Default
import Control.Error
import Control.Monad.Trans
import Control.Monad (when)
import Data.Text (Text)
import Data.Maybe (catMaybes)

main = runEitherT $ do
     process "fret.svg" "fret1.svg" ["molecule", "molecule-label"]
     process "fret.svg" "fret2.svg" ["molecule", "donor", "donor-label"]
     process "fret.svg" "fret3.svg" ["molecule", "donor", "donor-label", "acceptor", "acceptor-label"]
     process "fret.svg" "fret4.svg" ["molecule", "donor", "acceptor"]

type LayerLabel = Text

inkscape :: Name -> Name
inkscape = _nameNamespace ?~ "http://www.inkscape.org/namespaces/inkscape"

svg :: Name -> Name
svg = _nameNamespace ?~ "http://www.w3.org/2000/svg"
 
process :: FilePath -> FilePath -> [LayerLabel] -> EitherT String IO ()
process inFile outFile showLayers = do
    doc <- lift $ Xml.readFile def inFile
    let notFound = filter (\l->l `notElem` allLayers doc) showLayers
    when (not $ null notFound)
        $ lift $ putStrLn $ "couldn't find layers: "++show notFound
    lift $ Xml.writeFile def outFile (filterLayers showLayers doc)

allLayers :: Document -> [LayerLabel]
allLayers doc = 
       catMaybes $
       doc ^.. root
             . entire . filtered (views name (== svg "g"))
             . attributeIs (inkscape "groupmode") "layer"
             . attrs . at (inkscape "label")

filterLayers :: [LayerLabel] -> Document -> Document
filterLayers showLayers doc =             
    let match el = (el ^. attrs . at (inkscape "label")) `notElem` map Just showLayers
    in doc
       & root
       . entire . filtered (views name (== svg "g"))
       . attributeIs (inkscape "groupmode") "layer"
       . filtered match
       . attrs . at "style" ?~ "display:none"
