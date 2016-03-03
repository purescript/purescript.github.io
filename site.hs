--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend, (<>))
import Data.Foldable (for_)
import Data.String (fromString)
import Hakyll


  --------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match ("img/**" .||. "js/**" .||. "css/**" .||. "CNAME") $ do
    route idRoute
    compile copyFileCompiler

  create [".nojekyll"] $ do
    route idRoute
    compile (makeItem ("" :: String))

  match "index.html" $ do
    let ctx = field "body" (return . itemBody) <> baseCtx (Just "home")
    route idRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

  for_ ["learn", "projects", "download"] $ \subsection -> do
    let ctx = baseCtx (Just subsection)
    match (fromGlob (subsection <> "/index.html")) $ do
      route $ idRoute
      compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "learn/*/*.markdown" $ do
    let ctx = postCtx (Just "learn")
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
navItems :: [ (String, String, String) ]
navItems =
  [ ("home", "Home", "/")
  , ("download", "Download", "/download/")
  , ("learn", "Learn", "/learn/")
  , ("projects", "Projects", "/projects/")
  ]

nav :: Maybe String -> String
nav activeSubsection =
  concat $
    [ "<ul>\n" ]
    <> map render navItems
    <> [ "</ul>\n" ]
  where
  render :: (String, String, String) -> String
  render (name, text, url) =
    concat
      [ "<li><a href=\"" , url , "\""
      , if activeSubsection == Just name then " class=\"active\"" else ""
      , ">"
      , text
      , "</a></li>"
      , "\n"
      ]

baseCtx :: Maybe String -> Context String
baseCtx activeSubsection =
  constField "nav" (nav activeSubsection) <>
  maybe mempty (constField "subsection") activeSubsection <>
  defaultContext

postCtx :: Maybe String -> Context String
postCtx activeSubsection =
  dateField "date" "%B %e, %Y" <>
  baseCtx activeSubsection
