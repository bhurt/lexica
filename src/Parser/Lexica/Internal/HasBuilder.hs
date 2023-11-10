{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

module Parser.Lexica.Internal.HasBuilder (
    HasBuilder(..),
    ListBuilder
) where

    import           Data.Kind              (Type)
    import           Data.Proxy
    import qualified Data.Text              as StrictText
    import qualified Data.Text.Lazy         as LazyText
    import qualified Data.Text.Lazy.Builder as LazyBuilder

    class Monoid (Builder f) => HasBuilder f where
        type Elem f :: Type
        type Builder f :: Type
        append :: Proxy f -> Builder f -> Elem f -> Builder f
        runBuilder :: Builder f -> f
        unCons :: f -> Maybe (Elem f, f)

    newtype ListBuilder a = ListBuilder { runListBuilder :: [a] -> [a] }

    instance Semigroup (ListBuilder a) where
        a <> b = ListBuilder $ runListBuilder a . runListBuilder b

    instance Monoid (ListBuilder a) where
        mempty = ListBuilder id
        mappend = (<>)

    instance HasBuilder [a] where
        type Elem [a] = a
        type Builder [a] = ListBuilder a
        append Proxy b a = ListBuilder $ runListBuilder b . (a :)
        runBuilder b = runListBuilder b []
        unCons [] = Nothing
        unCons (x:xs) = Just (x, xs)

    instance HasBuilder LazyText.Text where
        type Elem LazyText.Text = Char
        type Builder LazyText.Text = LazyBuilder.Builder
        append Proxy b a = b <> (LazyBuilder.singleton a)
        runBuilder = LazyBuilder.toLazyText
        unCons = LazyText.uncons

    instance HasBuilder StrictText.Text where
        type Elem StrictText.Text = Char
        type Builder StrictText.Text = LazyBuilder.Builder
        append Proxy b a = b <> (LazyBuilder.singleton a)
        runBuilder = LazyText.toStrict . LazyBuilder.toLazyText
        unCons = StrictText.uncons



