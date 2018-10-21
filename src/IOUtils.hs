module IOUtils where

bindMaybe :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
bindMaybe a mapper =
  case a of
    Nothing -> return Nothing
    Just a' -> mapper a' >>= \x -> return (Just x)

