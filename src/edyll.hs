--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Web.Template.DirList as DL
import           Hakyll.Web.Sass (sassCompilerWith)
import           Text.Sass.Options

--------------------------------------------------------------------------------

saasOptions = SassOptions
      { sassPrecision         = 5
      , sassOutputStyle       = SassStyleNested
      , sassSourceComments    = False
      , sassSourceMapEmbed    = False
      , sassSourceMapContents = False
      , sassOmitSourceMapUrl  = False
      , sassIsIndentedSyntax  = False
      , sassIndent            = "  "
      , sassLinefeed          = "\n"
      , sassInputPath         = Nothing
      , sassOutputPath        = Nothing
      , sassPluginPaths       = Nothing
      , sassIncludePaths      = Just ["./scss/foundation/", "./scss/"]
      , sassSourceMapFile     = Nothing
      , sassSourceMapRoot     = Nothing
      , sassFunctions         = Nothing
      , sassHeaders           = Nothing
      , sassImporters         = Nothing
      }


main :: IO ()
main = hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "scss/*.scss" $ do
        route $ setExtension "css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompilerWith saasOptions)

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "swf/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "mp3/*" $ do
        route   idRoute
        compile copyFileCompiler

    match ( "templates/*" .&&.
            complement "templates/index.html" ) $
      compile templateCompiler

    -- Compile page elements
    match ( (fromGlob "pages/**.md")
            .||. (fromGlob "pages/**.html") ) $ do
      compile pandocCompiler
    
    match "templates/index.html" $ do
      route $ constRoute "index.html"
      compile $ do
        md <- getMetadata =<< getUnderlying
        pages <- loadAll "pages/**"
        let indexCtx =
              DL.dirListField
                (DL.metadataConfiguration md DL.defaultConfiguration)
                "pages" defaultContext
                (return pages)
              `mappend` defaultContext
              
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= relativizeUrls

