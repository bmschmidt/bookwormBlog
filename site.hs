{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import Hakyll
import Text.Pandoc
import Text.Pandoc.Walk (walk,query)
import Data.Monoid (mappend)
import qualified Data.Map as M
import Network.HTTP.Base (urlEncode)

--------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  `mappend` mathCtx
  `mappend` teaserField "teaser" "content"
  `mappend` defaultContext

mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $ if "mathjax" `M.member` metadata
           then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
           else ""

archiveCtx posts =
  listField "posts" postCtx (return posts)
  `mappend` constField "title" "Archives"
  `mappend` defaultContext

indexCtx posts =
  listField "posts" postCtx (return posts)
  `mappend` constField "title" "Home"
  `mappend` defaultContext

--------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------

static :: Rules ()
static = do
  match "fonts/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "img/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "js/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "bookwormD3/**" $ do
    route idRoute
    compile $ copyFileCompiler

pages :: Rules ()
pages = do
  match "pages/*" $ do
    route $ setExtension "html"
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/page.html" postCtx
      >>= relativizeUrls

posts :: Rules ()
posts = do
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ compiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= relativizeUrls

archive :: Rules ()
archive = do
  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" (archiveCtx posts)
        >>= relativizeUrls


navbar :: Rules ()
navbar = do
  create ["templates/navbar.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "pages/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/navbar.html" (archiveCtx posts)
        >>= relativizeUrls

index :: Rules ()
index = do
  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" (indexCtx posts)
        >>= relativizeUrls
{-
rss :: Rules()
rss = do
  create ["atom.xml"] $ do
    route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` constField "description" "This is the post description"
        posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
        renderRss myFeedConfiguration feedCtx posts
-}

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------
--
--------------------------------------------------------------------

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Ben's Bookworm Blog"
    ,  feedDescription = "Examples of dynamic Bookworm charts in a re-usable bootstrap framework."
    , feedAuthorName  = "Ben Schmidt"
    , feedAuthorEmail = "bmschmidt@gmail.com"
    , feedRoot        = "http://blog.bookworm.benschmidt.org/"
    }



--------------------------------------------------------------------
-- Custom Pandoc Filters
--------------------------------------------------------------------

replaceCharacter :: Char -> Char -> [Char] -> [Char]
replaceCharacter s r value = do
  map (\c -> if c==s then r; else c) value

lookupMetadata:: String -> [(String,String)] -> String

getSvgAttr :: String -> String
getSvgAttr "width" = "600px"
getSvgAttr "height" = "600px"
getSvgAttr "background" = "grey"
getSvgAttr x = ""

bookwormFormatBlock :: Block -> Block
bookwormFormatBlock (CodeBlock (codeblock,["bookworm"],keyvals) code) = do
  let proxy = (CodeBlock (codeblock,["json"],keyvals) code)
  let cleancode = replaceCharacter '\n' ' ' $ replaceCharacter '"' '\'' code
  let clickFunction = "\"bookwormSVG(this," ++ cleancode ++ ")\""

  let svg = RawInline (Format "html") $ "<svg style=\"background:grey;width:" ++ width ++ "\" onclick=" ++ clickFunction ++ "></svg>"
  let attr = ("",["bookworm"],[])
  Div attr [proxy,Plain [svg]]
  
bookwormFormatBlock x = x

swapBookwormBlocks :: Pandoc -> Pandoc
swapBookwormBlocks (Pandoc meta blocks) = do
  let newblocks = walk bookwormFormatBlock blocks
  Pandoc meta newblocks

--------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------

compiler :: Compiler (Item String)
compiler = pandocCompilerWithTransform defaultHakyllReaderOptions pandocOptions swapBookwormBlocks
pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

cfg :: Configuration
cfg = defaultConfiguration

main :: IO ()
main = hakyllWith cfg $ do
  static
  pages
  posts
  archive
  index
  templates
  pages
