-- | Optimization

singleSequences :: Expr -> Maybe Expr
singleSequences (Sequence []) = Just Null
singleSequences (Sequence [n]) = Just n
singleSequences _ = Nothing

defIgnoreNull :: Node -> Maybe Node
defIgnoreNull (Def (Ignore Null) _ rvalue) = Just rvalue
defIgnoreNull _ = Nothing

unusedEscape :: Node -> Maybe Node
unusedEscape (Escape pattern node (Ignore Null) Null)
    | patternUnused pattern node = Just node
unusedEscape _ = Nothing

narrowEscapeLeft :: Node -> Maybe Node
narrowEscapeLeft (Escape pattern (Sequence (n:ns)) catchPattern catchNode)
    | patternUnused pattern n =
        Just $ Sequence [n, Escape pattern (Sequence ns) catchPattern catchNode]
narrowEscapeLeft _ = Nothing

narrowEscapeRight:: Node -> Maybe Node
narrowEscapeRight
    (Escape p@(Final name Null) (Sequence ns) catchPattern catchNode) =
        case break f ns of
            (_, []) -> Nothing
            (before, callNode:_) -> Just $
                Escape p (Sequence (before ++ [callNode])) catchPattern catchNode
    where f (Call (Noun name') (StrNode "run") _) = name == name'
          f _                                     = False
narrowEscapeRight _ = Nothing

singleEscape :: Node -> Maybe Node
singleEscape (Escape (Final n Null) (Call (Noun n') (StrNode "run") (Tuple [v])) cp cn)
    | n == n' && not (nameUsed v n) = Just $ case cn of
        Null -> v
        _    -> Sequence [Def cp Null v, cn]
singleEscape _ = Nothing

unusedFinally :: Node -> Maybe Node
unusedFinally (Finally node Null) = Just node
unusedFinally _ = Nothing

makeList :: Node -> Maybe Node
makeList (Call (Noun "__makeList") (StrNode "run") t@(Tuple _)) = Just t
makeList _ = Nothing

optimizations :: [Node -> Maybe Node]
optimizations =
    [ singleSequences
    , defIgnoreNull
    , unusedEscape
    , narrowEscapeLeft
    , narrowEscapeRight
    , singleEscape
    , unusedFinally
    , makeList
    ]

optimize :: Node -> Node
optimize = rewrite $ \node -> msum $ map ($ node) optimizations

-- | Name analysis

namesBound :: Patt -> [String]
namesBound (BindPatt n) = [n]
namesBound (FinalPatt n _) = [n]
namesBound (IgnorePatt _) = []
namesBound (ListPatt ps) = concatMap namesBound ps
namesBound (VarPatt n _) = [n]
namesBound (ViaPatt _ p) = namesBound p

namesUsed :: Expr -> [String]
namesUsed expr = mapMaybe f $ universe expr
    where f (NounExpr n)     = Just n
          f (AssignExpr n _) = Just n
          f _            = Nothing

-- XXX this is such a horrible hack
nameUsed :: Expr -> String -> Bool
nameUsed _ "_flexList" = True
nameUsed _ "_flexMap" = True
nameUsed _ "_listIterator" = True
nameUsed node name = name `elem` namesUsed node

patternUnused :: Patt -> Expr -> Bool
patternUnused pattern expr =
    all (not . nameUsed expr) (namesBound pattern)

