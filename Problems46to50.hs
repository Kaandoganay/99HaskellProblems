module Problems46to50 where

and' :: Bool -> Bool -> Bool
and' True True = True
and' _    _    = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _     _     = True

neg :: Bool -> Bool
neg True = False
neg False = True

nor :: Bool -> Bool -> Bool
nor False False = True
nor _      _    = False


