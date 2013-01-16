{-# LANGUAGE OverloadedStrings, Arrows #-}

import Prelude hiding (id)
import Control.Category (id)

import Hakyll
import Hakyll.Web.Feed
import Hakyll.Web.Tags
import Control.Monad
import Control.Applicative ((<$>))       
import Data.Monoid (mempty, mconcat, (<>))

import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

feedConfig = FeedConfiguration { feedTitle = "bgamari.github.com"
                               , feedDescription = "Various ramblings, generally of a technical nature"
                               , feedAuthorName = "Ben Gamari"
                               , feedRoot = "http://bgamari.github.com/"
                               , feedAuthorEmail = "bgamari@gmail.com"
                               }

main :: IO ()
main = hakyll $ do
        match "css/*" $ do
                route   idRoute
                compile compressCssCompiler

        match (fromList [ "publications.mkd", "research-agenda.mkd"
                    , "past-research.mkd"
                    ]) $ do
                route   $ setExtension "html"
                compile $ pandocCompiler
                        >>= loadAndApplyTemplate "templates/default.html" defaultContext
                        >>= relativizeUrls

        match "posts/*.mkd" $ do
                route   $ setExtension "html"
                compile $ pandocCompiler
                        >>= loadAndApplyTemplate "templates/default.html" defaultContext
                        >>= relativizeUrls
     
        create ["rss.xml"] $ do
                route idRoute
                compile $ loadAll "posts/*.mkd" >>= renderRss feedConfig defaultContext
     
        match "media/**" $ do
                route   idRoute
                compile copyFileCompiler

        match "index.html" $ do
                route idRoute
                compile $ getResourceBody
                        >>= loadAndApplyTemplate "templates/default.html" (defaultContext <> field "title" (const $ return "Home"))
                        >>= relativizeUrls

        create ["posts.html"] $ do
                route idRoute
                compile $ loadAll "posts/*.mkd"
                        >>= postList
                        >>= loadAndApplyTemplate "templates/posts.html" defaultContext
                        >>= loadAndApplyTemplate "templates/default.html" (field "title" (const $ return "Posts") <> defaultContext)
                        >>= relativizeUrls

        match "templates/*" $ compile templateCompiler

postList :: [Item String] -> Compiler (Item String)
postList posts =
    return (reverse $ chronological posts)
    >>= mapM (loadAndApplyTemplate "templates/postitem.html" (ctxt<>defaultContext))
    >>= makeItem . mconcat . map itemBody
  where
    ctxt = renderTagList' "tagsList" (const $ fromFilePath "error/404")
  
-- | Render tags as HTML list with links
renderTagList' :: String
               -- ^ Destination key
               -> (String -> Identifier)
               -- ^ Produce a link for a tag
               -> Context String
renderTagList' destination makeUrl =
    field destination $ \item->renderTags <$> getTags (itemIdentifier item)
  where
    renderTags :: [String] -> String
    renderTags = renderHtml . mconcat . map (H.li . toHtml)
