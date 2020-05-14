{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WriterImplementation where

instance Functor (Writer l) where
    fmap f (Writer (a, l)) = Writer (f a, l)

instance (Monoid l) => Applicative (Writer l) where
    pure a = Writer (a, mempty)
    (<*>) (Writer (f, l)) writer = fmap f writer

newtype Writer l a = Writer { runWriter :: (a, l) }

instance (Monoid l) => Monad (Writer l) where
    return a = Writer (a, mempty)
    (>>=) (Writer (a, l)) g = Writer (b, mappend l l')
        where
            Writer (b, l') = g a

tell :: Monoid l => l -> Writer l ()
tell s = Writer ((), s)

parseWithLog :: String -> Writer String Int
parseWithLog x = do
  tell ("Parse '" ++ x ++ "' as Int\n")
  return $ read x