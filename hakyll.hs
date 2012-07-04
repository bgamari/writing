{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)

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
     
        match "media/**" $ do
                route   idRoute
                compile copyFileCompiler

        match "index.html" $ route idRoute
        create "index.html" $ constA mempty
                >>> arr (setField "title" "Home")
                >>> requireAllA "posts/*.mkd" addPostList
                >>> applyTemplateCompiler "templates/index.html"
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

        match "templates/*" $ compile templateCompiler


addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

