{-# LANGUAGE DeriveFunctor #-}

module FRP.Dynamic where

import Control.Applicative (Applicative(..))
import FRP.BearRiver

data Dynamic a = Dynamic
    { modified :: Bool
    , current :: a
    } deriving (Show, Functor)

updated :: Dynamic a -> Event a
updated d
    | modified d = Event (current d)
    | otherwise  = NoEvent

holdDyn :: Monad m => a -> SF m (Event a) (Dynamic a)
holdDyn a = Dynamic <$> arr isEvent <*> hold a

constDyn :: Monad m => a -> SF m () (Dynamic a)
constDyn a = Dynamic <$> (True --> constant False) <*> pure a

instance Applicative Dynamic where
    -- NB: 'pure' never fires an update event!
    -- In most cases, 'constDyn' is more appropriate
    pure = Dynamic False

    df <*> dx = Dynamic
        { modified = modified df || modified dx
        , current = (current df) (current dx)
        }

    liftA2 f = \dx dy -> Dynamic
        { modified = modified dx || modified dy
        , current = f (current dx) (current dy)
        }
