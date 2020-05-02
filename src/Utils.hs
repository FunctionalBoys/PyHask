module Utils where

import qualified Control.Monad.Fail as Fail

maybeFail :: (Fail.MonadFail m) => String -> Maybe a -> m a
maybeFail e = maybe (fail e) return

guardFail :: (Fail.MonadFail m) => Bool -> String -> m ()
guardFail True _  = return ()
guardFail False s = Fail.fail s

liftEither :: (Fail.MonadFail m) => Either String a -> m a
liftEither (Right a) = return a
liftEither (Left e) = fail e
