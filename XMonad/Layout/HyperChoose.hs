{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, PatternGuards #-}
module XMonad.Layout.HyperChoose
    ( HyperChoose
    , NextNoWrap'(..)
    , ChangeLayout'(..)
    , hyperChoose
    , (<|||>)
    ) where
import XMonad
import XMonad.StackSet (Workspace(..))
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import XMonad.Layout.HyperChoose.Label


data HyperChoose left right a = HyperChoose LR (left a) (right a) deriving (Show, Read, Eq, Ord)

infixr 3 <|||>
(<|||>), hyperChoose :: left a -> right a -> HyperChoose left right a
hyperChoose = HyperChoose L

(<|||>) = hyperChoose
    
data LR = L | R deriving (Read, Show, Eq, Ord)

data NextNoWrap' = NextNoWrap' deriving (Eq, Show, Typeable)
instance Message NextNoWrap'

data ChangeLayout' = FirstLayout' | NextLayout' deriving (Typeable)
instance Message ChangeLayout'

instance (LayoutClass left a, LayoutClass right a) => LayoutClass (HyperChoose left right) a where
    runLayout (Workspace id a@(HyperChoose L left right) stack) rect = do
        (tiles, left') <- runLayout (Workspace id left stack) rect
        return (tiles, maybeSetLeft a left')
    runLayout (Workspace id a@(HyperChoose R left right) stack) rect = do
        (tiles, right') <- runLayout (Workspace id right stack) rect
        return (tiles, maybeSetRight a right')

    description (HyperChoose L left _) = description left
    description (HyperChoose R _ right) = description right

    handleMessage a msg | Just NextLayout' <- fromMessage msg = do
        a' <- handleMessage a (SomeMessage NextNoWrap')
        maybe (handleMessage a (SomeMessage FirstLayout')) (return . Just) a'

    handleMessage a@(HyperChoose L left right) msg | Just NextNoWrap' <- fromMessage msg = do
         left' <- handleMessage left (SomeMessage NextNoWrap')
         case left' of
              Just left'' -> return $ Just $ HyperChoose L left'' right
              Nothing     ->
                  handleMessage right (SomeMessage FirstLayout') >>= updateToRight a

    handleMessage a@(HyperChoose R _ right) msg | Just NextNoWrap' <- fromMessage msg =
        maybeSetRight a `fmap` handleMessage right (SomeMessage NextNoWrap')

    handleMessage a@(HyperChoose _ left _) msg | Just FirstLayout' <- fromMessage msg =
        handleMessage left (SomeMessage FirstLayout') >>= updateToLeft a

    handleMessage a@(HyperChoose switch left right) msg | Just (JumpToLayout _) <- fromMessage msg = do
        left' <- handleMessage left msg
        right' <- handleMessage right msg
        case (left', right') of
             (Just left'', _)        -> updateToLeft a left'
             (Nothing, Just right'') -> updateToRight a right'
             otherwise               -> return Nothing

    handleMessage a@(HyperChoose _ left right) msg | Just ReleaseResources <- fromMessage msg = do
        left' <- handleMessage left msg
        right' <- handleMessage right msg
        return $ maybeSetLeft (fromMaybe a $ maybeSetRight a right') left'

    handleMessage a@(HyperChoose L left _) msg =
        maybeSetLeft a `fmap` handleMessage left msg
    handleMessage a@(HyperChoose R _ right) msg =
        maybeSetRight a `fmap` handleMessage right msg


handleMessage' :: LayoutClass layout a => layout a -> SomeMessage -> X (layout a)
handleMessage' layout msg = fromMaybe layout `fmap` handleMessage layout msg

maybeSetLeft :: (LayoutClass left a, LayoutClass right a) => HyperChoose left right a -> Maybe (left a) -> Maybe (HyperChoose left right a)
maybeSetLeft (HyperChoose switch left right) (Just left') = Just $ HyperChoose switch left' right
maybeSetLeft _ Nothing = Nothing

maybeSetRight :: (LayoutClass left a, LayoutClass right a) => HyperChoose left right a -> Maybe (right a) -> Maybe (HyperChoose left right a)
maybeSetRight (HyperChoose switch left right) (Just right') = Just $ HyperChoose switch left right' 
maybeSetRight _ Nothing = Nothing

-- | switches to the left side, replaces left with update (if present) and hides right if necessary
updateToLeft :: (LayoutClass left a, LayoutClass right a) => HyperChoose left right a -> Maybe (left a) -> X (Maybe (HyperChoose left right a))
updateToLeft (HyperChoose L _ right) left' = return $ (\left'' -> HyperChoose L left'' right) `fmap` left'
updateToLeft (HyperChoose R left right) left' =
    handleMessage' right (SomeMessage Hide) >>= return . Just . HyperChoose L (fromMaybe left left')

updateToRight :: (LayoutClass left a, LayoutClass right a) => HyperChoose left right a -> Maybe (right a) -> X (Maybe (HyperChoose left right a))
updateToRight (HyperChoose R left _) right' = return $ (HyperChoose R left) `fmap` right'
updateToRight (HyperChoose L left right) right' = do
    left' <- handleMessage' left (SomeMessage Hide)
    return $ Just $ HyperChoose R left'(fromMaybe right right')
