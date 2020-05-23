module Parser.Utils where

maybeFail :: (MonadFail m) => String -> Maybe a -> m a
maybeFail e = maybe (fail e) return

guardFail :: (MonadFail m) => Bool -> String -> m ()
guardFail True _  = return ()
guardFail False s = fail s

liftEither :: (MonadFail m) => Either String a -> m a
liftEither (Right a) = return a
liftEither (Left e)  = fail e
