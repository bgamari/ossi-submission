{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import Prelude hiding (FilePath)

import qualified Filesystem.Path as Path
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString, toText)

import qualified Text.XML as Xml
import Text.XML.Lens

import Data.Default
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Data.Traversable (mapAccumL)

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
note text = tell [Note text]

numberSlides :: [Slide] -> [(Int, Slide)]
numberSlides = snd . mapAccumL f 1
  where
    f n s@(Note _) = (n, (n, s))
    f n s          = (n+1, (n, s))

title = "An open-source toolchain for fluorescence spectroscopy"

main = runEitherT $ do
    writeSlides slides "slides.mkd"
    liftIO $ T.writeFile "notes.mkd" $ formatNotes slides
    
formatNotes :: [Slide] -> Text
formatNotes = T.unlines . concatMap formatSlide . numberSlides
  where
    slidePlaceholder n title = 
      [ ""
      , "# slide "<>T.pack (show n)<>": "<>title
      , ""
      ]
    formatSlide (n, FigureSlide title _ _) = slidePlaceholder n title
    formatSlide (n, TextSlide title _) = slidePlaceholder n title
    formatSlide (n, Note text) =
      [ " * "<>text
      ]

writeSlides :: [Slide] -> FilePath -> EitherT String IO ()
writeSlides slides outName = do 
    figures <- forM (numberSlides slides) $ \(n, slide)->
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
    liftIO $ T.writeFile (encodeString outName) 
      $ T.unlines [ "---"
                  , "title: "<>title
                  , "author:"
                  , "- Ben Gamari\\inst{1}"
                  , "- Laura Dietz\\inst{2}"
                  , "- Lori Goldner \\inst{1}"
                  , "---"
                  ]
        <> T.concat figures


slides :: [Slide] 
slides = execWriter $ do
     -- introduce single molecule measurement
     note "Work for Lori Goldner"
     note "Will discuss open source tools I developed for analysis of single-molecule fluorescence data"
     note "We want to be able to study structure of individual molecules"
     note "Want to learn about shape, reactions with other molecules"
     fslide "A single-molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "molecule-label"]
     
     note "however molecules are quite small. How do we see them?"
     note "too small for brightfield microscopy"
     fslide "A single-molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "molecule-label", "scale-bar"]

     note "One powerful approach is to attach fluorescent probe"
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "fluorophore-label"]

     note "this is a molecule which we can shine light"
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "fluorophore-label", "donor-exc-photon"]
     note "to excite"
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "excited-donor"]
     
     note "later the molecule will decay, resulting in a photon which we can detect"
     fslide "Seeing a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-em-photon"]

     note "Even this single labelling is sufficient to learn many things about our system: molecular size, learn reaction kinetics"

     note "but there are some details singly labelled approach isn't very sensitive to"
     note "let's say our molecule has multiple conformational states"
     note "for a given set of environmental conditions, in which conformation is the molecule"
     note "measure the distance between two points on our molecule"
     fslide "Measuring a single molecule" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "molecule-distance"]

     note "we can do this by adding a second probe of a different color to our molecule"
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "acceptor"]
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-exc-photon", "acceptor"]

     note "now we can excite one of our dyes"
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "excited-donor", "acceptor"]

     note "Sometimes the dye will transfer (donate) its energy to the other by a process known as Fӧrster transfer"
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "excited-donor", "donor-label", "acceptor", "acceptor-label", "energy-transfer"]
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "donor-label", "excited-acceptor", "acceptor-label", "energy-transfer"]
     
     note "The probability of energy transfer is higher when the dyes are close together giving us a measurement of distance"
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "excited-acceptor"]

     note "After this a photon will be emitted by the acceptor dye."
     note "For each detected photon, we can distinguish which fluorophore produced it"
     fslide "Measuring an intramolecular distance" "fret.svg"
       $ showOnlyLayers ["molecule", "donor", "acceptor-em-photon", "acceptor"]

     note "as FRET efficiency depends upon distance, we have a stoichastic method for probing intramolecular geometry"
     note "in the case of the extended molecule, we find a low probability of energy transfer, and therefore observe more donor photons"
     slide "Measuring an intramolecular distance" "conformations.svg"

     note "but this is a stoichastic measurement"
     note "often information starved; while we may collect millions of photons, each only encodes a small amount of information"
     note "we want to go from photons"
     fslide "An inverse problem" "photons-to.svg"
       $ showOnlyLayers ["photons"]
     note "to distance"
     note "we want this arrow"
     fslide "An inverse problem" "photons-to.svg"
       $ showOnlyLayers ["photons", "arrow", "distribution", "distance-label"]
     --note "or barring that, something related to distance"
     --fslide "An inverse problem" "photons-to.svg"
     --  $ showOnlyLayers ["photons", "arrow", "distribution", "fret-eff-label"]
     note "but once we have this arrow, we've solved our problem"
     note "let's consider how these observations are collected"

     note "Typically these experiments are conducted with an apparatus like this"
     note "Labelled molecule under microscope"
     note "Excited with laser"
     note "Detect fluorescence photons with photon counting detectors"
     note "Record each arrival time with timing hardware"
     fslide "A fluorescence experiment" "fret-setup.svg"
       $ showOnlyLayers ["background", "labels", "hardware", "fret-inset", "fret-labels", "detector-label", "legend", "acceptor"]

     note "The devil is in the details: our instrument introduces numerous errors"
     note "detectors lose photons"
     note "optics are imperfect at distinguishing donor and acceptor photons"
     fslide "Errors abound" "fret-setup.svg"
       $ showOnlyLayers ["background", "fret-inset", "acceptor", "energy-transfer", "acceptor-em-photon", "fret-artifacts"]
     note "In addition, we have the challenge of photophysics"
     note "fluorophores will eventually die, changing the observed FRET efficiency"
     note "These are just some of the effects for which we must correct for quantitatively accurate measurement"
     note "lack of common open-source analysis toolkit means a great deal of duplicated work, many practitioners lack access to sophisticated tools, hinders reproducibility"
     fslide "Errors abound" "fret-setup.svg"
       $ showOnlyLayers ["background", "fret-inset", "dead-acceptor"]
     
     note "My submission consists of a set of open-source tools built on open platforms for the collection, manipulation, analysis of fluorescence spectroscopy data."

     note "We offer an open-source photon timestamping instrument built on off-the-shelf hardware."
     note "As opposed to most commercial instruments, the device is flexible enough to enable FRET variants such as alternating excitation which enables more sophisticated correction for the artifacts discussed above"
     note "Additionally, the hardware is much cheaper than equally capable commercial options."
     fslide "Contribution: Acquisition hardware" "fret-setup.svg"
       $ highlightLayers 0.2 ["hardware"]
       . showOnlyLayers ["background", "hardware", "fret-inset", "acceptor"]
     
     note "Also provide easy-to-use acquisition software."
     note "Can be easily integrated into existing systems, enabling scanning and other applications"
     tslide "Contribution: Acquisition software" "![](../timetag-ui.png)"

     note "Data analysis tools provide both easy-to-use command line interfaces as well as libraries for incorporation into existing analysis pipelines"
     note "Analysis tools work not only with data from our own instrument, but also with commercial formats"
     note "Integrates fluidly with Python toolchain"
     tslide "Contribution: Low-level data manipulation tools" "![](../burstfind.png)"
     tslide "Contribution: Low-level data manipulation tools" "![](plot-bins.pdf)"
     note "Thorough documentation"
     tslide "Contribution: Low-level data manipulation tools" "![](photon-tools-docs.pdf)"
   
     {-
     fslide "FRET in solution" "diffusion.svg" ["background", "frame1"] 
     fslide "FRET in solution" "diffusion.svg" ["background", "frame2"] 
     fslide "FRET in solution" "diffusion.svg" ["background", "frame3"] 
     fslide "FRET in solution" "diffusion.svg" ["background", "frame4"] 
     -}

     note "End-to-end analysis tools for several common fluorescence techniques"
     note "Provides estimation of correction parameters"
     note "Provides a variety of statistical diagnostics to evaluate result significance"
     tslide "Contribution: End-to-end FRET analysis pipeline" "![](fret-analysis.png)"
     
     --tslide "Contribution: Probabilistic inference framework" ""
     
     note "fluorescence spectroscopic tools unique view on geometry of single molecule systems"
     note "Statistical inference tools enable novel analyses"
     tslide "Summary" $ T.unlines
       [ "![](summary.pdf)"
       , ""
       , " * We provide,"
       , "     * an open photon timetagging instrument"
       , "     * software for data acquisition"
       , "     * tools for analysis of FRET and other fluorescence experiments"
       , " "
       , " * <http://goldnerlab.physics.umass.edu/wiki/HardwareAndSoftware>"
       , " * <http://github.com/bgamari>"
       , " * <bgamari@physics.umass.edu>"
       , ""
       , "![](languages.pdf)"
       ]
      
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
allLayers doc = catMaybes $ doc ^.. traverseGroups . attrs . at (inkscape "label")

traverseGroups :: Traversal' Document Element
traverseGroups =
    root
    . entire . filtered (views name (== svg "g"))
    . attributeIs (inkscape "groupmode") "layer"

showAllGroups :: Document -> Document
showAllGroups = traverseGroups . attrs . at "style" .~ Nothing

hideLayers :: [LayerLabel] -> Document -> Document
hideLayers layers doc =             
    let match el = (el ^. attrs . at (inkscape "label")) `elem` map Just layers
    in showAllGroups doc
       & traverseGroups
       . filtered match
       . style "display" ?~ "none"

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
        
