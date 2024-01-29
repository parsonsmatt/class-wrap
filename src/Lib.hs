{-# language TypeFamilies #-}

-- | This module contains an example of providing hooks for a type class.
module Lib where

import Control.Monad.Reader

someFunc :: IO ()
someFunc = do
    foo 3
    bar 'a'

    withFooHooks logArguments $ do
        foo 3
        bar 'a'

    pure ()

-- | This is the class for actions 'Foo'. This is the easy way to write the
-- functions that use this behavior.
class Monad m => MonadFoo m where
    foo :: Int -> m Char
    bar :: Char -> m Int

-- | This is a base level instance of 'MonadFoo', defined in 'IO'. It uses
-- it's own behavior without referring to anything else.
instance MonadFoo IO where
    foo i = do
        print ("calling foo in IO with argument: ", i)
        pure (toEnum i)

    bar c = do
        print ("calling bar in IO with argument: ", c)
        pure (fromEnum c)

-- | This is a record that mirrors the 'MonadFoo' class. It is used for the
-- monad transformer that can provide a minimal example of the class.
data FooRec m = FooRec
    { _fooImpl :: Int -> m Char
    , _barImpl :: Char -> m Int
    }

-- | This is our instance for the monad transformer that can provide the
-- implementation for "plucking the constraint" on our MTL-style class.
newtype FooT m a = FooT
    { unFooT :: ReaderT (FooRec m) m a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (FooRec m))

-- | The instance delegates to the functions in our record.
instance Monad m => MonadFoo (FooT m) where
    foo i = FooT $ ReaderT $ \fooRec ->
        _fooImpl fooRec i

    bar c = FooT $ ReaderT $ \fooRec ->
        _barImpl fooRec c

-- | If we are dealing with a concrete 'FooT', then we can provide hook
-- behavior easily.
--
-- However, this signature is a bit awkward to use. Requiring the concrete
-- 'FooT' type means that we need to rewrite our hooks for any other
-- transformer type that has an instance.
--
-- The implementation would work just as well for any @'MonadReader'
-- ('FooRec' m)@, but that means it *wouldn't* work for any other
-- 'MonadReader'. So we can instead make this a class of itself, like:
--
-- @
-- class HasFooRec r where
--     type FooRecM r
--     getFooRec :: r -> FooRec (FooRecM r)
--     setFooRec :: FooRec (FooRecM r) -> r
-- @
--
-- Then, we can define this in the general form:
--
-- @
-- withFooHookFoo :: (HasFooRec r,
-- @
withFooHookFooT :: Monad m => ((Int -> m Char) -> Int -> m Char) -> FooT m r -> FooT m r
withFooHookFooT fooHook (FooT action) =
    FooT $
        local (\fooRec ->
            fooRec
                { _fooImpl =
                    fooHook (_fooImpl fooRec)
                }
            )
            action

-- | This type allows us to define a hook variant for any 'MonadReader'.
class HasFooRec r where
    type FooRecM r :: * -> *
    getFooRec :: r -> FooRec (FooRecM r)
    setFooRec :: FooRec (FooRecM r) -> r -> r

instance HasFooRec (FooRec m) where
    type FooRecM (FooRec m) = m
    getFooRec = id
    setFooRec a _ = a

-- | This implementation now works for any 'MonadReader'.
--
-- However, this only works for 'MonadReader'. If we want to use it for our
-- 'IO' instance, then we are stuck.
--
-- We can wrap our 'IO' calls in 'FooT', but that's only true if we control
-- those calls, and often times, the type is fixed so we can't do it.
withFooHookMonadReader
    ::
        ( MonadReader r m
        , HasFooRec r
        , n ~ FooRecM r
        )
    => ((Int -> n Char) -> Int -> n Char)
    -> m r -> m r
withFooHookMonadReader fooHook =
    local
        (\hasFooRec ->
            let fooRec =
                    getFooRec hasFooRec
                newFooRec =
                    fooRec
                        { _fooImpl = fooHook (_fooImpl fooRec)
                        }
            in
                setFooRec newFooRec hasFooRec
        )

-- | This is a type for our hooks that we may want to apply on our code.
data FooHook m = FooHook
    { _fooHook :: (Int -> m Char) -> Int -> m Char
    , _barHook :: (Char -> m Int) -> Char -> m Int
    }

-- | We must define the empty hook.
emptyHook :: FooHook m
emptyHook =
    FooHook
        { _fooHook = \original i ->
            original i
        , _barHook = \original c ->
            original c
        }

-- | And this is a hook that adds logging of argument and results to
-- a 'FooRec'.
logArguments :: MonadIO m => FooHook m
logArguments =
    FooHook
        { _fooHook = \original i -> do
            liftIO $ print ("Foo called with argument: ", i)
            result <- original i
            liftIO $ print ("Foo returned: ", result)
            pure result
        , _barHook = \original c -> do
            liftIO $ print ("bar called with argument: ", c)
            result <- original c
            liftIO $ print ("bar returned: ", result)
            pure result
        }

-- | And here we are, with another monad transformer. Instead of providing
-- implementations, this just provides hooks.
newtype HookFooT m a = HookFooT { unHookFooT :: ReaderT (FooHook m) m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Provide the *hooks* for a 'HookFooT' type and run it.
withFooHooks :: FooHook m -> HookFooT m a -> m a
withFooHooks fooHook action =
    runReaderT (unHookFooT action) fooHook

-- | And this implementation uses it.
instance (MonadFoo m) => MonadFoo (HookFooT m) where
    foo i = HookFooT $ ReaderT $ \hooks -> do
        _fooHook hooks foo i

    bar c = HookFooT $ ReaderT $ \hooks -> do
        _barHook hooks bar c
