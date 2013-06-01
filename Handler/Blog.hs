module Handler.Blog where

import Import

getBlogR :: BlogId -> Handler RepHtml
getBlogR bid = do
  blog <- runDB $ get404 bid
  defaultLayout $(widgetFile "blog")