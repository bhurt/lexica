{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Lexica.Internal.TMap (
    TMap,
    singleton,
    range,
    reduce,
    apply,
    merge
) where

    import           GHC.Base                   (liftA2)
    import           Language.Haskell.TH.Syntax (Lift (..))

    data TMap k v =
        Bound v k (TMap k v)
        | Fini v
        deriving (Eq, Ord, Lift)

    instance Functor (TMap k) where
        fmap f (Bound v k r) = Bound (f v) k (fmap f r)
        fmap f (Fini v)      = Fini (f v)

    instance Ord k => Applicative (TMap k) where
        pure = Fini
        liftA2 f (Fini a) (Fini b) = Fini (f a b)
        liftA2 f x@(Fini a) (Bound b k r) = Bound (f a b) k (liftA2 f x r)
        liftA2 f (Bound a k r) y@(Fini b) = Bound (f a b) k (liftA2 f r y)
        liftA2 f x@(Bound a k1 r1) y@(Bound b k2 r2) =
            case compare k1 k2 of
                LT -> Bound (f a b) k1 (liftA2 f r1 y)
                EQ -> Bound (f a b) k1 (liftA2 f r1 r2)
                GT -> Bound (f a b) k2 (liftA2 f x r2)

    instance Foldable (TMap k) where
        foldr f start (Fini v) = f v start
        foldr f start (Bound v _ r) = f v (foldr f start r)

    instance Traversable (TMap k) where
        sequenceA (Fini ma) = Fini <$> ma
        sequenceA (Bound mv k r) =
            liftA2 (\v r'-> Bound v k r') mv (sequenceA r)


    singleton :: forall k v .
                    (Ord k
                    , Enum k
                    , Bounded k) 
                    => (k,  v) 
                    -> v 
                    -> TMap k v
    singleton (k, v) df = 
            if (minBound < k)
            then Bound df (pred k) value
            else value
        where
            value :: TMap k v
            value = if (k == maxBound)
                        then Fini v
                        else Bound v k (Fini df)

    range :: forall k v .
                (Ord k
                , Enum k
                , Bounded k)
                => (k, k, v)
                -> v
                -> TMap k v
    range (k1, k2, v) df =
            case compare k1 k2 of
                LT -> prefix k1 k2
                EQ -> singleton (k1, v) df
                GT -> prefix k2 k1
        where
            prefix :: k -> k -> TMap k v
            prefix lb ub =
                if (minBound < lb)
                then Bound df (pred lb) (value ub)
                else value ub

            value :: k -> TMap k v
            value ub =
                if (ub < maxBound)
                then Bound v ub (Fini df)
                else Fini v

    reduce :: forall k v . Eq v => TMap k v -> TMap k v
    reduce (Fini v) = Fini v
    reduce x@(Bound v1 _ y@(Fini v2))
        | v1 == v2  = y
        | otherwise = x
    reduce (Bound v1 k1 x@(Bound v2 _ _))
        | v1 == v2  = reduce x
        | otherwise = Bound v1 k1 (reduce x)

    apply :: forall k v . Ord k => k -> TMap k v -> v
    apply _ (Fini v)       = v
    apply k (Bound v k2 r)
        | k <= k2           = v
        | otherwise         = apply k r

    merge :: forall k a b c .
                (Eq c
                , Ord k)
                => (a -> b -> c)
                -> TMap k a
                -> TMap k b
                -> TMap k c
    merge f t1 t2 = reduce $ liftA2 f t1 t2
