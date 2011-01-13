{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module XMonad.Layout.HyperChoose.Label
    ( Label(..)
    ) where
import XMonad
import XMonad.StackSet (Workspace(..))


data Label layout a = Label String (layout a)
     deriving (Show, Read, Eq, Ord)

instance LayoutClass layout a => LayoutClass (Label layout) a where
    runLayout (Workspace id (Label label l) stack) rect = do
        (tiles, l') <- runLayout (Workspace id l stack) rect
        return (tiles, (Label label) `fmap` l')

    description (Label label l) = label

    handleMessage (Label label l) msg =
        (fmap . fmap) (Label label) (handleMessage l msg)
