{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, PatternGuards, DeriveDataTypeable #-}
module XMonad.Layout.HyperChoose.Label
    ( Label(..)
    , JumpToLayout(..)
    ) where
import XMonad
import XMonad.StackSet (Workspace(..))


data Label layout a = Label String (layout a)
     deriving (Show, Read, Eq, Ord)

data JumpToLayout = JumpToLayout String deriving Typeable
instance Message JumpToLayout

instance LayoutClass layout a => LayoutClass (Label layout) a where
    runLayout (Workspace id (Label label l) stack) rect = do
        (tiles, l') <- runLayout (Workspace id l stack) rect
        return (tiles, (Label label) `fmap` l')

    description (Label label l) = description l

    handleMessage a@(Label label1 l) msg
        | Just (JumpToLayout label2) <- fromMessage msg
        , label1 == label2 = return $ Just a
        | otherwise = (fmap . fmap) (Label label1) (handleMessage l msg)
