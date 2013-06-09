{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Edit where

import Import

data BlogEditModel = BlogEditModel 
    { bemTitle :: Text
    , bemContent :: Textarea } deriving (Show)


data UpsertAuthorModel = UUM
    { uumIdent :: Text
    , uumPassword :: Text
    , uumEmail :: Text
    , uumAuthorName :: Text } deriving Show

upsertAuthor :: UpsertAuthorModel -> Handler (Key Author)
upsertAuthor (UUM ident pass email name) = 
    do 
      maybeAuthor <- runDB $ getBy $ UniqueAuthor name
      case maybeAuthor of 
        Nothing -> do
          let user = User ident $ Just pass
          personId <- runDB $ insert user
          let e = Email email (Just personId) Nothing
          emailId <- runDB $ insert e
          let author = Author name personId emailId
          runDB $ insert author
        Just a -> return $ entityKey a

getCreateR :: Handler RepHtml
getCreateR = do
    (bemWidget, bemEnc) <- generateFormPost $ blogForm Nothing
    defaultLayout $ do
      setTitle "Editing"
      $(widgetFile "create")

postCreateR :: Handler RepHtml
postCreateR = do
    ((bemRes,bemWidget),bemEnc) <- runFormPost $ blogForm Nothing
    case bemRes of
      FormSuccess bem -> do
                  let uum = UUM { uumIdent = "khan"
                                , uumPassword = "monkey"
                                , uumEmail = "khanage@gmail.com"
                                , uumAuthorName = "Khan Thompson" }
                  aId <- upsertAuthor uum                                          
                  now <- liftIO getCurrentTime
                  let title = bemTitle bem
                      content = unTextarea $ bemContent bem
                      blog = Blog aId title now content
                  _ <- runDB $ insert blog
                  setMessage $ toHtml $ "Created " ++ show (blogTitle blog)
                  redirect HomeR
      _ ->
          defaultLayout $ do
            setTitle "Editing"
            $(widgetFile "create")

getEditR :: BlogId -> Handler RepHtml
getEditR bid = do
  blog <- runDB $ get404 bid
  let bem = BlogEditModel { bemTitle = blogTitle blog, bemContent = Textarea $ blogContent blog }
  (bemWidget,bemEnc) <- generateFormPost $ blogForm $ Just bem
  defaultLayout $(widgetFile "edit")

postEditR :: BlogId -> Handler RepHtml
postEditR bid = do
  blog <- runDB $ get404 bid
  -- Todo: authorise
  ((bemResult,bemWidget),bemEnc) <- runFormPost $ blogForm Nothing
  case bemResult of
    FormSuccess bem -> do
      let title = bemTitle bem
          content = unTextarea $ bemContent bem
      runDB $ update bid [BlogTitle =. title, BlogContent =. content]
      setMessage $ toHtml $ "Updated " ++ show title
      redirect HomeR
    _ ->
      defaultLayout $(widgetFile "edit")

postDeleteR :: BlogId -> Handler RepHtml
postDeleteR bid = do
  blog <- runDB $ get404 bid
  runDB $ delete bid
  setMessage $ toHtml $ "Deleted " ++ show (blogTitle blog)
  redirect HomeR

blogForm :: Maybe BlogEditModel -> Form BlogEditModel
blogForm mbem = renderDivs $ BlogEditModel
                <$> areq textField "title" (bemTitle <$> mbem)
                <*> areq textareaField "content" (bemContent <$> mbem)
