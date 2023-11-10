{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Lexica.Internal.Lex (
    Lex(..),
    isAccepting,
    simplify,
    derive,
    derivitives
) where

    import           GHC.Base                    (liftA2)
    import           Parser.Lexica.Internal.TMap
 
    data Lex c =
        Accepting
        | Error
        | Token (TMap c Bool)
        | Seq (Lex c) (Lex c)
        | And (Lex c) (Lex c)
        | Or (Lex c) (Lex c)
        | Not (Lex c)
        deriving (Eq, Ord)

    isAccepting :: Lex c -> Bool
    isAccepting Accepting  = True
    isAccepting Error      = False
    isAccepting (Token _) = False
    isAccepting (Seq x y)  = isAccepting x && isAccepting y
    isAccepting (And x y)  = isAccepting x && isAccepting y
    isAccepting (Or x y)   = isAccepting x || isAccepting y
    isAccepting (Not x)    = not (isAccepting x)

    simplify :: forall c . (Ord c, Enum c, Bounded c) => Lex c -> Lex c
    simplify term = maybeOpt (descend simplify term)
        where

            maybeOpt :: Lex c -> Lex c
            maybeOpt x =
                case opt x of
                    Nothing -> x
                    Just y -> maybeOpt (descend maybeOpt y)

            descend :: (Lex c -> Lex c) -> Lex c -> Lex c
            descend _ Accepting  = Accepting
            descend _ Error      = Error
            descend _ (Token s) = Token s
            descend f (Seq x y)  = Seq (f x) (f y)
            descend f (And x y)  = And (f x) (f y)
            descend f (Or x y)   = Or (f x) (f y)
            descend f (Not x)    = Not (f x)

            opt :: Lex c -> Maybe (Lex c)
            opt (Seq (Seq x y) z) = Just $ Seq x (Seq y z)
            opt (Seq (And x y) z) = Just $ And (Seq x z) (Seq y z)
            opt (Seq x (And y z)) = Just $ And (Seq x y) (Seq x z)
            opt (Seq (Or x y) z)  = Just $ Or (Seq x z) (Seq y z)
            opt (Seq x (Or y z))  = Just $ Or (Seq x y) (Seq x z)
            opt (Seq Accepting x) = Just x
            opt (Seq x Accepting) = Just x
            opt (Seq Error _)     = Just Error
            opt (Seq _ Error)     = Just Error

            opt (And Accepting x) = Just x
            opt (And x Accepting) = Just x
            opt (And Error _)     = Just Error
            opt (And _ Error)     = Just Error
            opt (And (Token s) (Token t)) = Just $ Token (merge (&&) s t)
            opt (And (Token s) (And x y)) = 
                Just $ And x (And (Token s) y)
            opt (And (And x y) z) = Just $ And x (And y z)
            opt (And (Or x y) z)  = Just $ Or (And x z) (And y z)
            opt (And x (Or y z))  = Just $ Or (And x y) (And x z)


            -- These two optimizations are invalid.
            -- opt (Or Accepting _)  = Accepting
            -- opt (Or _ Accepting)  = Accepting
            -- Consider the case where it's Or (Token s) Accepting
            -- This is both accepting and can still accept new characters.
            --
            -- Float Accepting values to the head of the list of Ors,
            -- so we can reduce them to a single Accepting.
            opt (Or Accepting (Or Accepting x)) = Just $ Or Accepting x
            opt (Or Accepting Accepting) = Just $ Accepting
            opt (Or x Accepting) = Just $ Or Accepting x
            opt (Or x (Or Accepting y)) = Just $ Or Accepting (Or x y)
            opt (Or Error x)      = Just x
            opt (Or x Error)      = Just x
            -- Float ranges to the end of the list of Ors, so we can
            -- combine them.
            opt (Or (Token s) (Token t)) =
                Just $ Token (merge (||) s t)
            opt (Or (Token s) (Or x y)) =
                Just $ Or x (Or (Token s) y)
            opt (Or (Or x y) z)   = Just $ Or x (Or y z)

            opt (Not Accepting)   = Just Error
            opt (Not Error)       = Just Accepting
            opt (Not (Token s))  = Just $ Token (fmap not s)
            opt (Not (And x y))   = Just $ Or (Not x) (Not y)
            opt (Not (Or x y))    = Just $ And (Not x) (Not y)
            opt (Not (Not x))     = Just x

            opt _                 = Nothing

    derive :: forall c . Ord c => c -> Lex c -> Lex c
    derive _ Accepting         = Error
    derive _ Error             = Error
    derive c (Token s)
        | apply c s            = Accepting
        | otherwise            = Error
    derive c (Seq x y)
        | isAccepting x        = Or (Seq (derive c x) y) (derive c y)
        | otherwise            = Seq (derive c x) y
    derive c (And x y)         = And (derive c x) (derive c y)
    derive c (Or x y)          = Or (derive c x) (derive c y)
    derive c (Not x)           = Not (derive c x)


    derivitives :: forall c . Ord c => Lex c -> TMap c (Lex c)
    derivitives Accepting = pure Error
    derivitives Error     = pure Error
    derivitives (Token s) = fmap go s
        where
            go :: Bool -> Lex c
            go True  = Accepting
            go False = Error
    derivitives (Seq x y)
        | isAccepting x   = liftA2 Or 
                            (fmap (\z -> Seq z y) (derivitives x))
                            (derivitives y)
        | otherwise       = fmap (\z -> Seq z y) (derivitives x)
    derivitives (And x y) = liftA2 And (derivitives x) (derivitives y)
    derivitives (Or x y)  = liftA2 Or (derivitives x) (derivitives y)
    derivitives (Not x)   = Not <$> derivitives x

