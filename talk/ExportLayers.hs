{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import Prelude hiding (FilePath)
import qualified Filesystem.Path as Path
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString)
import qualified Text.XML as Xml
import Text.XML.Lens
import Data.Default
import Data.Monoid ((<>))
import Control.Error
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Control.Monad (when, forM)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Text as T

data Slide = FigureSlide String FilePath (Document -> Document)
           | TextSlide String String

fslide :: String -> FilePath -> (Document -> Document) -> Writer [Slide] ()
fslide title figure transform = tell [FigureSlide title figure transform]

slide :: String -> FilePath -> Writer [Slide] ()
slide title figure = tell [FigureSlide title figure id]

tslide :: String -> String -> Writer [Slide] ()
tslide title text = tell [TextSlide title text]

title = "An open-source toolchain for fluorescence spectroscopy"

main = runEitherT $ do
    figures <- forM (zip [0..] slides) $ \(n, FigureSlide title figure transform)->do
        let figName = decodeString $ "slide-"++show n
            outName = Path.addExtension figName "pdf"
        process figure (Path.addExtension figName "svg") transform
        return $ unlines [ ""
                         , "## "++title
                         , ""
                         , "![]("++encodeString outName++")"
                         ]
    liftIO $ writeFile "slides.mkd" (concat $ ["# "++title]++figures)


slides :: [Slide] 
slides = execWriter $ do
     -- introduce single molecule measurement
     -- We want to be able to see individual molecules, like this one here
     fslide "A single-molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "molecule-label"]
     -- however molecules are quite small. How do we see them?
     fslide "A single-molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "molecule-label", "scale-bar"]

     -- One powerful approach is to attach fluorescent probe
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "fluorophore-label"]
     -- this is a molecule which we can shine light on to excite
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "fluorophore-label", "donor-exc-photon"]
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "excited-donor"]
     -- later the molecule will decay, resulting in light which we can detect
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-em-photon"]
     -- Even a single fluorescent probe is sufficient to measure
     -- molecular size, characterize reaction kinetics, and more

     -- Let's say, though, that we want to do something a bit more sophisticated:
     -- measure the distance between two points on our molecule
     fslide "Measuring a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "molecule-distance"]
     -- we can do this by adding a second probe of a different color to our molecule
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-exc-photon", "acceptor"]
     -- now we can excite one of our dyes
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "excited-donor", "acceptor"]
     -- Sometimes the dye will transfer
     -- (donate) its energy to the other by a process known as Fӧrster
     -- transfer. After this a photon will be emitted by the acceptor dye.
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-label", "acceptor", "acceptor-label", "energy-transfer"]
     -- The probability of energy transfer is higher when the dyes are close together
     -- giving us a measurement of distance
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "excited-acceptor"]
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "acceptor-em-photon", "acceptor"]

     -- FRET efficiency depends upon distance, stoichastic
     slide "Measuring an intramolecular distance" "conformations.svg"

     -- Typically these experiments are done with an apparatus like this with the
     -- molecule diffusing in solution. We get short bursts of fluorescence
     fslide "A fluorescence experiment" "fret-setup.svg"
       $ showOnlyLayers ["background", "labels", "hardware", "fret-inset", "fret-labels", "detector-label", "legend"]
     
     -- we want to go from photons
     fslide "An inverse problem" "photons-to.svg"
       $ showOnlyLayers ["photons"]
     -- to distance
     fslide "An inverse problem" "photons-to.svg"
       $ showOnlyLayers ["photons", "arrow", "distribution", "distance-label"]
     -- or barring that, something related to distance
     fslide "An inverse problem" "photons-to.svg"
       $ showOnlyLayers ["photons", "arrow", "distribution", "fret-eff-label"]

     -- Great, so we can measure distances, right?
     fslide "A fluorescence experiment" "fret-setup.svg"
       $ showOnlyLayers ["background", "hardware", "fret-inset"]
     -- The devil is in the details: our data is flawed
     fslide "Errors abound" "fret-setup.svg"
       $ showOnlyLayers ["background", "fret-inset", "fret-artifacts"]

     fslide "Errors abound" "fret-setup.svg"
       $ showOnlyLayers ["background", "fret-inset", "fret-artifacts"]

     -- We need to correct for these effects
     fslide "Corrections" "fret-setup.svg"
       $ highlightLayers ["hardware"]


type LayerLabel = Text

inkscape :: Name -> Name
inkscape = _nameNamespace ?~ "http://www.inkscape.org/namespaces/inkscape"

svg :: Name -> Name
svg = _nameNamespace ?~ "http://www.w3.org/2000/svg"
 
process :: FilePath -> FilePath -> (Document -> Document) -> EitherT String IO ()
process inFile outFile transform = do
    doc <- lift $ Xml.readFile def inFile
    --let notFound = filter (\l->l `notElem` allLayers doc) showLayers
    --when (not $ null notFound) $ lift $ putStrLn $ "couldn't find layers: "++show notFound
    lift $ Xml.writeFile def outFile (transform doc)

allLayers :: Document -> [LayerLabel]
allLayers doc = 
       catMaybes $
       doc ^.. root
             . entire . filtered (views name (== svg "g"))
             . attributeIs (inkscape "groupmode") "layer"
             . attrs . at (inkscape "label")

traverseGroups :: Traversal' Document Element
traverseGroups =
    root
    . entire . filtered (views name (== svg "g"))
    . attributeIs (inkscape "groupmode") "layer"

showAllGroups :: Document -> Document
showAllGroups = traverseGroups . attrs . at "style" .~ Nothing

showOnlyLayers :: [LayerLabel] -> Document -> Document
showOnlyLayers showLayers doc =             
    let match el = (el ^. attrs . at (inkscape "label")) `notElem` map Just showLayers
    in showAllGroups doc
       & traverseGroups
       . filtered match
       . attrs . at "style" ?~ "display:none"

highlightLayers :: [LayerLabel] -> Document -> Document
highlightLayers showLayers doc =             
    let match el = (el ^. attrs . at (inkscape "label")) `notElem` map Just showLayers
    in doc
       & traverseGroups
       . filtered match
       . style "opacity" ?~ "0.3"

type StyleAttr = Text

style :: StyleAttr -> Lens' Element (Maybe Text)
style s = attribute "style" . non T.empty . style' . at s

style' :: Iso' Text (M.Map StyleAttr Text)
style' = iso to from
  where
    to = M.fromList . map (T.partition (== ':')) . T.splitOn ";" 
    from = T.intercalate ";" . map (\(k,v)->k<>":"<>v) . M.toList
        
