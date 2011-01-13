{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module XMonad.Layout.HyperChoose
    ( HyperChoose
    , NextNoWrap'(..)
    , ChangeLayout'(..)
    , hyperChoose
    ) where
import XMonad
import XMonad.StackSet (Workspace(..))
import Data.Maybe (fromMaybe)


data HyperChoose left right a = HyperChoose LR (left a) (right a) deriving (Show, Read, Eq, Ord)

hyperChoose :: left a -> right a -> HyperChoose left right a
hyperChoose = HyperChoose L
    
data LR = L | R deriving (Read, Show, Eq, Ord)

data NextNoWrap' = NextNoWrap' deriving (Eq, Show, Typeable)
instance Message NextNoWrap'

data ChangeLayout' = FirstLayout' | NextLayout' deriving (Eq, Show, Typeable)
instance Message ChangeLayout'

instance (LayoutClass left a, LayoutClass right a) => LayoutClass (HyperChoose left right) a where
    runLayout (Workspace id a@(HyperChoose L left right) stack) rect = do
        (tiles, left') <- runLayout (Workspace id left stack) rect
        return (tiles, maybeUpdateLeft a left')
    runLayout (Workspace id a@(HyperChoose R left right) stack) rect = do
        (tiles, right') <- runLayout (Workspace id right stack) rect
        return (tiles, maybeUpdateRight a right')

    description (HyperChoose L left _) = description left
    description (HyperChoose R _ right) = description right

    handleMessage a msg | Just NextLayout' == fromMessage msg = do
        a' <- handleMessage a (SomeMessage NextNoWrap')
        maybe (handleMessage a (SomeMessage FirstLayout')) (return . Just) a'

    handleMessage (HyperChoose L left right) msg | Just NextNoWrap' == fromMessage msg = do
         left' <- handleMessage left (SomeMessage NextNoWrap')
         case left' of
              Just left'' -> return $ Just $ HyperChoose L left'' right
              Nothing     -> do
                  left' <- handleMessage' left (SomeMessage Hide)
                  right' <- handleMessage' right (SomeMessage FirstLayout')
                  return $ Just $ HyperChoose R left' right'

    handleMessage a@(HyperChoose R _ right) msg | Just NextNoWrap' == fromMessage msg = do
         right' <- handleMessage right (SomeMessage NextNoWrap')
         return $ maybeUpdateRight a right'

    handleMessage a@(HyperChoose L left _) msg | Just FirstLayout' == fromMessage msg = do
        left' <- handleMessage left (SomeMessage FirstLayout')
        return $ maybeUpdateLeft a left'

    handleMessage (HyperChoose R left right) msg | Just FirstLayout' == fromMessage msg = do
        left'  <- handleMessage' left (SomeMessage FirstLayout')
        right' <- handleMessage' right (SomeMessage Hide)
        return $ Just $ HyperChoose L left' right'

    handleMessage a@(HyperChoose _ left right) msg | Just ReleaseResources == fromMessage msg = do
        left' <- handleMessage left msg
        right' <- handleMessage right msg
        return $ maybeUpdateLeft (fromMaybe a $ maybeUpdateRight a right') left'

    handleMessage a@(HyperChoose L left _) msg =
        maybeUpdateLeft a `fmap` handleMessage left msg
    handleMessage a@(HyperChoose R _ right) msg =
        maybeUpdateRight a `fmap` handleMessage right msg

handleMessage' :: LayoutClass layout a => layout a -> SomeMessage -> X (layout a)
handleMessage' layout msg = fromMaybe layout `fmap` handleMessage layout msg

maybeUpdateLeft :: (LayoutClass left a, LayoutClass right a) => HyperChoose left right a -> Maybe (left a) -> Maybe (HyperChoose left right a)
maybeUpdateLeft (HyperChoose switch left right) (Just left') = Just $ HyperChoose switch left' right
maybeUpdateLeft _ Nothing = Nothing

maybeUpdateRight :: (LayoutClass left a, LayoutClass right a) => HyperChoose left right a -> Maybe (right a) -> Maybe (HyperChoose left right a)
maybeUpdateRight (HyperChoose switch left right) (Just right') = Just $ HyperChoose switch left right' 
maybeUpdateRight _ Nothing = Nothing
