--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import GHC.IO.Encoding
import Hakyll
import System.FilePath


--------------------------------------------------------------------------------
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "PureScript Community Blog"
    , feedDescription = "A collection of posts about PureScript written by its authors and users"
    , feedAuthorName  = "The PureScript Community"
    , feedAuthorEmail = "test@example.com"
    , feedRoot        = "http://purescript.github.io"
    }

main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8
    hakyll $ do

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    create ["css/style.css"] $ do
        route idRoute
        compile $ do
            items <- loadAll "css/_*"
            makeItem $ concatMap (compressCss . itemBody) (items :: [Item String])

    match "posts/*" $ do
        route indexRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= slashUrlsCompiler
        
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` constField "description" ""
            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderRss myFeedConfiguration feedCtx posts

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= slashUrlsCompiler

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

indexRoute :: Routes
indexRoute = customRoute removeDatePrefix 
  `composeRoutes` gsubRoute ".markdown" (const "/index.html")
  
removeDatePrefix :: Identifier -> FilePath
removeDatePrefix ident = replaceFileName file (drop 11 $ takeFileName file)
  where file = toFilePath ident 

slashUrlsCompiler :: Item String -> Compiler (Item String)
slashUrlsCompiler item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r -> fmap slashUrls item

slashUrls :: String -> String 
slashUrls = fileLinks . withUrls convert
  where
    convert = replaceAll "/index.html" (const "/")
    fileLinks = replaceAll "/files/" (const "/files/")

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
