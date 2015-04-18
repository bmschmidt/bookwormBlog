{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Stephen Diehl 2013, Ben Schmidt 2015 
-- License   :  MIT
-- Maintainer:  bmschmidt@gmail.com
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

-- Bookworm-specific functions
import Bookworm

---------------
-- Temporary --
---------------

--matcher :: Rules()
--matcher = do
--  match "fonts/*"

--------------------------------------------------------------------
--                          Contexts                              --
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
  match "images/*" $ do
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

json :: Rules ()
json = do
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

------------------------------
--- Toggle-display builder ---
------------------------------

buildToggleDisplay :: String -> [Block]
buildToggleDisplay code = do
  let hash = encodeSHA1 code
  let roles = ["PNG","SVG","Code"]
  let headerHead="<div role=\"tabpanel\"><ul class=\"nav nav-tabs\" role=\"tablist\">"
  let headerContent = unwords $ map (buildTab hash) roles
  let headerEnd="</ul></div>"
  let tabsHead = "<div class=\"tab-content\">"
  let tabsContent = unwords $ map (buildPanel code hash) roles
  let tabsEnd = "</div>"
  map (RawBlock "html") [headerHead, headerContent, headerEnd,tabsHead,tabsContent,tabsEnd]

--The tabs
buildTab :: String -> String -> String
buildTab hash role = do
  let id = hash ++ "-" ++ role
  let classname = isActive role
  let tab = "<li role=\"presentation\" class=\"" ++ classname ++ "\"><a href=\"#" ++ id ++ "\" aria-controls=\""++ id ++"\"role=\"tab\" data-toggle=\"tab\">" ++ role ++ "</a></li>"
  tab

isActive :: String -> String
isActive "PNG" = "active"
isActive x = ""

panelContent :: String -> String -> String -> String


panelContent jsonDefinition hash "SVG" = do
  let cleancode = replaceCharacter '\n' ' ' $ replaceCharacter '"' '\'' jsonDefinition
  let clickFunction = "\"bookwormSVG(this," ++ cleancode ++ ")\""
  -- width and height should be pulled from the keyvals
  let id = "SVG-" ++ hash
  let width = "800"
  "<svg style=\"background:grey;width:" ++ width ++ "\" onclick=" ++ clickFunction ++ " id=" ++ id ++ "></svg>"

panelContent code hash "Code" = do
  -- benschmidt.org should *not* be hardcoded in here. Rather, there should be a native version of the web app
  -- bundled with the Hakyll distro.
  let string = writeHtmlString def $ Pandoc nullMeta [(CodeBlock ("Code" ++ "-" ++ hash ,["json"],[]) code)]
  "<a href=http://benschmidt.org/BookwormD3#" ++ (urlEncode code) ++ ">" ++ string ++ "</a>"
  
panelContent code hash "PNG" = do
  writeHtmlString def $ Pandoc nullMeta $ [Plain [Image [Str (urlEncode code)] ("../images/" ++ hash ++ ".png","Static Image")]] 

panelContent code hash format = format

buildPanel :: String -> String -> String -> String
buildPanel code hash role = do
  let id = hash ++ "-" ++ role
  let content = panelContent code hash role
  unwords["<div role=\"tabpanel\" class=\"tab-pane " ++ (isActive role) ++ "\" id=\"" ++ id ++ "\">", content, "</div>"]

--The actual divs

--------------------------------------------------------------------
-- Custom Pandoc Filters
--------------------------------------------------------------------


-- Something else that seems odd to have to code myself:

replaceCharacter :: Char -> Char -> [Char] -> [Char]
replaceCharacter s r value = do
  map (\c -> if c==s then r; else c) value


bookwormSvg :: String -> Inline
bookwormSvg jsonDefinition = do
  let cleancode = replaceCharacter '\n' ' ' $ replaceCharacter '"' '\'' jsonDefinition
  let clickFunction = "\"bookwormSVG(this," ++ cleancode ++ ")\""
  -- width and heigh should be pulled from the keyvals
  let width = "600"
  let id = "SVG-" ++ (encodeSHA1 jsonDefinition)
  RawInline (Format "html") $ "<svg style=\"background:grey;width:" ++ width ++ "\" onclick=" ++ clickFunction ++ " id=" ++ id ++ "></svg>"

--bookwormFormatBlock :: Block -> Block
--bookwormFormatBlock (CodeBlock (codeblock,["bookworm"],keyvals) code) = do
--  let proxy = (CodeBlock (codeblock,["json"],keyvals) code)
--  let svg = bookwormSvg code
--  let attr = ("",["bookworm"],[])
--  Div attr [proxy,Plain [svg]]

-- bookwormFormatBlock x = x

-- extractBlock :: Block -> [String]
-- extractBlock CodeBlock (codeblock,["bookworm"],keyvals) code = [code]
-- extractBlock _ _= []

-- extractBlocks :: Pandoc -> [String]
-- extractBlocks = query extractBlock


--- Format blocks

bookwormFormatBlock2 :: Block -> Block
bookwormFormatBlock2 (CodeBlock (codeblock,["bookworm"],keyvals) code) = do
--  let attr = ("",["bookworm"],keyvals)
  --Div attr [Plain [RawInline (Format "html") (buildToggleDisplay code)]]
--  Div attr $ buildToggleDisplay code
    bookwormFormatBlock2 (CodeBlock (codeblock,["bookworm2"],keyvals) code)

bookwormFormatBlock2 (CodeBlock (codeblock,["bookworm2"],keyvals) code) = do
  -- We also attach a unique identifier to the code.
  let hash = encodeSHA1 code
  let keyvals2 = keyvals ++ [("hashcode",hash),("jsonQuery",(urlEncode code))]
  let attr = (codeblock,["bookworm2"],keyvals2)
  Div attr [CodeBlock (codeblock,["json"],[]) code]
bookwormFormatBlock2 x = x


swapBookwormBlocks :: Pandoc -> Pandoc
swapBookwormBlocks (Pandoc meta blocks) = do
  let newblocks = walk bookwormFormatBlock2 blocks
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
  
