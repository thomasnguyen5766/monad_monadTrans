-- import Control.Monad

data Context = Home | Mobile | Business deriving (Show, Eq)

type Phone = String

albulena = [(Home, "+355-652-55512") , (Mobile, "+355-652-55515"), (Mobile, "+47-922-55-519")]

nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
        (Home, "+47-925-55-121"), (Business, "+47-922-25-551")]

twalumba = [(Business, "+260-02-55-5121")]

-- --*find personal phone number - either Mobile or Home (prefer)*--
onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                        Nothing -> lookup Mobile ps
                        Just n -> Just n

{-
  - Can't handle the case that one person may have more than one personal phone number
-}

-- ---**---
allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd numbers
  where numbers = case filter (contextIs Business) ps of
                    -- [] -> filter (contextIs Mobile) ps
                    ns -> ns ++ filter (contextIs Mobile) ps

contextIs a (b, _) = a == b

{-
  onePersonalPhone && allBusinessPhones - have same patter
  - use  `case ..of` to filter value: return Nothing - Or - return empty list

  - Control.Monad - MonadPlus typeclass - to let us abstract the common pattern out of our `case..of` expression
-}

{-
  class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

    laws:
    mzero >>= f == mzero -- short circuit if mzero on the left of a bind expression
    v >> mzero == mzero -- short circuit if mzero on the right of sequence expression 
-}

class Monad m => MonadPlusi m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlusi [] where
  mzero = []
  mplus = (++)
  -- [] `mplus` ys = ys
  -- xs `mplus` _ = xs

instance MonadPlusi Maybe where
  mzero = Nothing
  Nothing `mplus` ys = ys
  xs `mplus` _ = xs

onePersonalPhoneN :: [(Context, Phone)] -> Maybe Phone
onePersonalPhoneN ps = lookup Home ps `mplus` lookup Mobile ps

allBusinessPhonesN :: [(Context, Phone)] -> [Phone]
allBusinessPhonesN ps = map snd $ filter (contextIs Business) ps `mplus` filter (contextIs Mobile) ps

-- --*generalise the result type of lookup*--
lookupM :: (MonadPlusi m, Eq a) => a -> [(a, b)] -> m b
lookupM _ [] = mzero
lookupM k ((x,y):xs)
  | k == x = return y `mplus` lookupM k xs
  | otherwise = lookupM k xs

allBusinessPhonesNN :: [(Context, Phone)] -> [Phone]
allBusinessPhonesNN ps = lookupM Business ps `mplus` lookupM Mobile ps

{-
  instance Monad Maybe where
    Nothing >> _ = Nothing
    Just _ >> k = k 
-}

-- x `zeroMod` n = guard((x `mod` n) == 0) >> return x