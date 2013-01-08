{-# LANGUAGE OverloadedStrings, Arrows #-}

import Prelude hiding (id)
import Control.Category (id)

import Hakyll
import Hakyll.Web.Feed
import Hakyll.Web.Tags
import Control.Arrow
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

        match (list [ "publications.mkd", "research-agenda.mkd"
                    , "past-research.mkd"
                    ]) $ do
                route   $ setExtension "html"
                compile $ pageCompiler
                        >>> applyTemplateCompiler "templates/default.html"
                        >>> relativizeUrlsCompiler

        match "posts/*.mkd" $ do
                route   $ setExtension "html"
                compile $ pageCompiler
                        >>> applyTemplateCompiler "templates/default.html"
                        >>> relativizeUrlsCompiler
     
        match "rss.xml" $ route idRoute
        create "rss.xml" $
                requireAll_ "posts/*.mkd" >>> renderRss feedConfig
     
        match "media/**" $ do
                route   idRoute
                compile copyFileCompiler

        match "index.html" $ route idRoute
        create "index.html" $ constA mempty
                >>> arr (setField "title" "Home")
                >>> applyTemplateCompiler "templates/index.html"
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

        match "posts.html" $ route idRoute
        create "posts.html" $ constA mempty
                >>> arr (setField "title" "Posts")
                >>> requireAllA "posts/*.mkd" addPostList
                >>> applyTemplateCompiler "templates/posts.html"
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

        match "templates/*" $ compile templateCompiler

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
    >>> requireA "templates/postitem.html" (arr (\(a,b)->(b,a)) ^>> mapCompiler' applyPostItem)
    >>> arr mconcat
    >>> arr pageBody
  where
    applyPostItem :: Compiler (Template, Page String) (Page String)
    applyPostItem = 
        second (renderTagList' "tagsf" (const $ Identifier Nothing "404"))
        >>> arr (\(t,page)->applyTemplate t page)
  
mapCompiler' :: Compiler (a,b) c -> Compiler (a,[b]) [c]
mapCompiler' ar = arr (\(a,bs)->zip (repeat a) bs) >>> mapCompiler ar

-- | Render tags as HTML list with links
--
renderTagList' :: String
               -- ^ Destination key
               -> (String -> Identifier a)
               -- ^ Produce a link for a tag
               -> Compiler (Page a) (Page a)
renderTagList' destination makeUrl =
    id &&& arr getTags >>> setFieldA destination renderTags
  where
    renderTags :: Compiler [String] String
    renderTags =     arr (map (H.li . toHtml))
                 >>> arr (renderHtml . mconcat)
