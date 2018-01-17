module StateChapterExercises where

import           WriteYourOwnState

-- 1 --
get :: Moi s s
get = Moi $ \s -> (s,s)

-- put --
put :: s -> Moi s ()
put s = Moi $ const ((), s)

-- exec --
exec :: Moi s a -> s -> s
exec (Moi sa) = snd . sa

-- eval --
eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

-- modify --
modify :: (s -> s) -> Moi s ()
modify f = Moi $ (,) () . f
