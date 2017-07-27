{-# LANGUAGE ExistentialQuantification #-}

module Backtest.Config.File where

import           Data.Text (Text)

data Configuration a = Configuration [Option] ([Option] -> a)


-- What I want to be able to do is have parameters that are supplied at the start
-- of the program, that are accessible within the program. That may be required or
-- not and may have defaults or not. A required parameter is not required at any
-- point until the configuration collection is done and the rest of the program has
-- started. Or if configuration can be modified during the program it must not be
-- empty. Is it necessary to allow configuration to be modified while the program is
-- running? I don't think so.

-- | An option may or may not be required. An option may have a default.
-- An option must have a description. An option may have a value.
-- If an option is required, it must have a value at a certain point.

data Option = forall a k. Ord k => Option { required    :: Bool
                                          , def         :: Maybe a
                                          , description :: Text
                                          , val         :: Maybe a
                                          , name        :: k }


-- Is there a way to combine two options to make a new option?
-- If that were the case, then a Configuration would also be an option.
-- So we would have one concept instead of two.
instance Semigroup Option where
  a <> b =
