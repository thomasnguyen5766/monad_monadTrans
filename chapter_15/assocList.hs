import Control.Monad

data MovieReview = MovieReview {
  revTitle :: String
  , revUser :: String
  , revReview :: String
} deriving Show


simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview aList =
  case lookup "title" aList of
    Just (Just title@(_:_)) ->
      case lookup "user" aList of
        Just (Just user@(_:_)) ->
          case lookup "review" aList of
            Just (Just review@('c':_)) ->
              Just (MovieReview title user review)
            _ -> Nothing -- No Review
        _ -> Nothing -- No User
    _ -> Nothing -- No title


{- Usage & Notes/Explanation
  --*Usage*--
  simpleReview [("title", Just "alien"), ("user", Just "Paul John")]
  Nothing

  simpleReview [("title", Just "alien"), ("user", Just "Paul John"), ("review", Just "Great")]
  Just (MovieReview {revTitle = "alien", revUser = "Paul John", revReview = "Great"})

  --*Explanation*--

  - the function returns a MoviewReview If the aLIst contains all of the necessary values, and also values are non-empty Strings

  - Just (Just s@(_:_)) what is this? - 
    case lookup .. .. of return Just value - that value will be mapped to `s`
    @(_:_) - conditional check - the value receive must be alist (_:_) of something that is NOT `empty`

  --*Problem*--
  - Suffering badly from "StairCasing" - case ... of
-}


-- --- *Using Monad* ---
maybeReview :: [(String, Maybe String)] -> Maybe MovieReview
maybeReview aList = do
  title <- lookup1 "title" aList
  user <- lookup1 "user" aList
  review <- lookup1 "review" aList
  return (MovieReview title user review)

lookup1 :: String -> [(String, Maybe String)] -> Maybe String
lookup1 k alist = 
  case lookup k alist of
    Just (Just s@(_:_)) -> Just s
    _ -> Nothing


-- --*Implement review by using Lift*--
liftedReviewi alist =
  liftMi MovieReview (lookup1 "title" alist) (lookup1 "user" alist) (lookup1 "review" alist)
{- Usage & explanation
  --*Usage*--
 

 --*Explanation*--
  - We can see MoviewReview type - just a pure function - receive 3 argument (MoviewReview String String String) and return a value
  - 
 -}

liftMi :: (Monad m) => (a -> b -> c -> r) -> m a -> m b -> m c -> m r
liftMi func ma mb mc = ma >>= (\x -> mb >>= (\y -> mc >>= (\z -> return (func x y z))))