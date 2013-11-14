{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import Prelude hiding (FilePath)

import qualified Filesystem.Path as Path
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString, toText)

import qualified Text.XML as Xml
import Text.XML.Lens

import Data.Default
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)

import Control.Error hiding (note)
import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Control.Monad (when, forM)

import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Slide = FigureSlide Text FilePath (Document -> Document)
           | TextSlide Text Text
           | Note Text

fslide :: Text -> FilePath -> (Document -> Document) -> Writer [Slide] ()
fslide title figure transform = tell [FigureSlide title figure transform]

slide :: Text -> FilePath -> Writer [Slide] ()
slide title figure = tell [FigureSlide title figure id]

tslide :: Text -> Text -> Writer [Slide] ()
tslide title text = tell [TextSlide title text]

note :: Text -> Writer [Slide] ()
note _ = return ()

title = "An open-source toolchain for fluorescence spectroscopy"

main = runEitherT $ do
    writeSlides slides "slides.mkd"
    liftIO $ T.writeFile "notes.mkd" $ formatNotes slides
    
formatNotes :: [Slide] -> Text
formatNotes = T.unlines . concatMap formatSlide . zip [1..]
  where
    slidePlaceholder n title = 
      [ "# slide "<>T.pack (show n)<>": "<>title
      , ""
      ]
    formatSlide (n, FigureSlide title _ _) = slidePlaceholder n title
    formatSlide (n, TextSlide title _) = slidePlaceholder n title
    formatSlide (n, Note text) =
      [ " * "<>text
      ]

writeSlides :: [Slide] -> FilePath -> EitherT String IO ()
writeSlides slides outName = do 
    figures <- forM (zip [1..] slides) $ \(n, slide)->
      case slide of
        FigureSlide title figure transform -> do
          let figName = decodeString $ "slide-"<>show n
          process figure (Path.addExtension figName "svg") transform
          figOutName <- fmapLT (const "Invalid file name") $ hoistEither
                        $ toText $ Path.addExtension figName "pdf"
          return $ T.unlines [ ""
                             , "## "<>title
                             , ""
                             , "![]("<>figOutName<>")"
                             ]
        TextSlide title text ->
          return $ T.unlines [ ""
                             , "## "<>title
                             , ""
                             , text
                             ]
        otherwise -> return ""
    liftIO $ T.writeFile (encodeString outName) (T.concat $ ["# "<>title]++figures)


slides :: [Slide] 
slides = execWriter $ do
     -- introduce single molecule measurement
     note "We want to be able to see individual molecules, like this one here"
     fslide "A single-molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "molecule-label"]
     
     note "however molecules are quite small. How do we see them?"
     fslide "A single-molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "molecule-label", "scale-bar"]

     note "One powerful approach is to attach fluorescent probe"
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "fluorophore-label"]

     note "this is a molecule which we can shine light on to excite"
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "fluorophore-label", "donor-exc-photon"]
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "excited-donor"]
     
     
     note "later the molecule will decay, resulting in light which we can detect"
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-em-photon"]

     note "Even a single fluorescent probe is sufficient to measure molecular size, characterize reaction kinetics, and more"

     note "but let's say, though, that we want to do something a bit more sophisticated:"
     note "measure the distance between two points on our molecule"
     fslide "Measuring a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "molecule-distance"]

     note "we can do this by adding a second probe of a different color to our molecule"
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-exc-photon", "acceptor"]

     note "now we can excite one of our dyes"
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "excited-donor", "acceptor"]

     note "Sometimes the dye will transfer (donate) its energy to the other by a process known as Fӧrster transfer"
     note "After this a photon will be emitted by the acceptor dye."
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-label", "acceptor", "acceptor-label", "energy-transfer"]
     
     note "The probability of energy transfer is higher when the dyes are close together giving us a measurement of distance"
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "excited-acceptor"]
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "acceptor-em-photon", "acceptor"]

     note "FRET efficiency depends upon distance, stoichastic"
     slide "Measuring an intramolecular distance" "conformations.svg"

     note "Typically these experiments are done with an apparatus like this"
     note "Labelled molecule diffusing in solution"
     note "We get short bursts of fluorescence"
     fslide "A fluorescence experiment" "fret-setup.svg"
       $ showOnlyLayers ["background", "labels", "hardware", "fret-inset", "fret-labels", "detector-label", "legend"]
     
     note "we want to go from photons"
     fslide "An inverse problem" "photons-to.svg"
       $ showOnlyLayers ["photons"]
     note "to distance"
     fslide "An inverse problem" "photons-to.svg"
       $ showOnlyLayers ["photons", "arrow", "distribution", "distance-label"]
     note "or barring that, something related to distance"
     fslide "An inverse problem" "photons-to.svg"
       $ showOnlyLayers ["photons", "arrow", "distribution", "fret-eff-label"]
     note "but once we have this arrow, we've solved our problem"

     note "Great, so we can measure distances, right?"
     fslide "A fluorescence experiment" "fret-setup.svg"
       $ showOnlyLayers ["background", "hardware", "fret-inset"]
     note "The devil is in the details: our data is flawed"
     fslide "Errors abound" "fret-setup.svg"
       $ showOnlyLayers ["background", "fret-inset", "fret-artifacts"]

     note "We need to correct for these effects"
     
     note "fluorescence spectroscopic tools unique view on geometry of single molecule systems"
     note "but quantitative analysis of the experimental results is challenging"
     note "My submission consists of a set of open-source tools built on open platforms for the collection, manipulation, analysis of fluorescence spectroscopy data."

     note "We offer an open-source photon timestamping instrument built on off-the-shelf hardware."
     note "As opposed to most commercial, instruments, the device is flexible enough to enable FRET variants such as alternating excitation which enables more sophisticated correction for the artifacts discussed above"
     note "Additionally, the hardware is orders of magnitude cheaper than equally capable commercial options."
     fslide "Contribution: acquisition hardware" "fret-setup.svg"
       $ highlightLayers 0.2 ["hardware"]
       . showOnlyLayers ["background", "hardware", "fret-inset"]
     
     note "Also provide easy-to-use acquisition software."
     note "Can be easily integrated into existing systems, enabling scanning and other applications"
     tslide "Contribution: Acquisition software" "![](../timetag-ui.png)"

     note "Data analysis tools provide both easy-to-use command line interfaces as well as libraries for incorporation into existing analysis pipelines"
     note "Analysis tools work not only with data from our own instrument, but also with commercial formats"
     note "Integrates fluidly with Python toolchain"
     tslide "Contribution: Low-level data manipulation tools" "![](../burstfind.png)"
     note "Thorough documentation"
     tslide "Contribution: Low-level data manipulation tools" "![](photon-tools-docs.pdf)"
     
     note ""
     --tslide "Contribution: End-to-end FRET analysis tools" "![](fret-analysis.png)"
     
     -- 
     -- burst detection
     
     note "Statistical inference tools enable novel analyses"
     

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
       . style "display" ?~ "none"

highlightLayers :: Double -> [LayerLabel] -> Document -> Document
highlightLayers opacity showLayers doc =             
    let match el = (el ^. attrs . at (inkscape "label")) `notElem` map Just showLayers
    in doc & traverseGroups
           . filtered match
           . style "opacity" ?~ T.pack (show opacity)

type StyleAttr = Text

style :: StyleAttr -> Lens' Element (Maybe Text)
style s = attribute "style" . non T.empty . style' . at s

style' :: Iso' Text (M.Map StyleAttr Text)
style' = iso to from
  where
    splitKeyValue x = case T.splitOn ":" x of
                        [k,v]     -> M.singleton k v
                        otherwise -> M.empty
    to = M.unions . map splitKeyValue . T.splitOn ";" 
    from = T.intercalate ";" . map (\(k,v)->k<>":"<>v) . M.toList
        
