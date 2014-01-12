{- |
Module      :  Control.Monad.StrictIdentity
Copyright   :  (c) Carter Schonwald 2013
License     :  BSD3, see license file
 
Maintainer  :  libraries@haskell.org
Stability   :  experimental
Portability :  portable
-}
{-# LANGUAGE BangPatterns #-}
 
 
module Control.Monad.StrictIdentity (
    StrictIdentity(..),
    runStrictIdentity)
    where
 

import Control.Monad.Fix 
import Control.Applicative


{- | 'StrictIdentity' is a newtype wrapper for a given type 'a' that 
satisfies the 'Functor', 'Applicative', and 'Monad' laws  when restricted to
terminating strict computations. 

The typical use case is to provide a light weight strict nested 
let notation for code that otherwise must use nested case expressions
as a proxy for a strict let.

the general pattern is to write code of the form

@
foo f h g x y z = runStrictIdentity $! do
    w <- return $! f x y
    j <- return $! h w z
    res <- return $! g w j
    return res
@

An example usage of 'StrictIdentity' that compiles to assembly 
comparable to C is the following:

@
(>>) = unsafeShiftR
(<<) = unsafeShiftL
outerShuffle64A :: Word -> Word 
outerShuffle64A !x =
    runStrictIdentity $! do
        x <- return $! ((x .&. 0x00000000FFFF0000) << 16 )
            .|. ((x>>16) .&. 0x00000000FFFF0000) .|. (x .&. 0xFFFF00000000FFFF)
        x <-  return $! ((x .&. 0x0000FF000000FF00 ) <<  8 )
            .|. (x >> 8) .&. 0x0000FF000000FF00 .|. (x  .&. 0xFF0000FFFF0000FF)
        x<-  return $! (( x .&. 0x00F000F000F000F0 ) << 4 )
            .|. (x >> 4) .&. 0x00F000F000F000F0 .|. (x .&. 0xF00FF00FF00FF00F )
        x<-   return $!((x .&.  0x0C0C0C0C0C0C0C0C )<< 2 )
            .|. (x >> 2) .&. 0x0C0C0C0C0C0C0C0C .|.( x .&. 0xC3C3C3C3C3C3C3C3)
        x<-   return $! ( (x .&. 0x2222222222222222)  << 1 ) 
            .|. (x>> 1) .&. 0x2222222222222222 .|. (x .&. 0x9999999999999999)
        return x
@


-}
 
newtype StrictIdentity a =  StrictIdentity {runStrictIdentity_ :: a }
 
-- | 'runStrictIdentity' unwraps a value of type  @'StrictIdentity' ty@  into a value of type @ty@,  strictly.
runStrictIdentity :: StrictIdentity a -> a 
runStrictIdentity !ma = case runStrictIdentity_ $! ma of 
                            !res -> res  
{-# INLINE  runStrictIdentity #-} 


instance Applicative StrictIdentity where
    {-# INLINE pure #-}
    pure = return 
    {-# INLINE (<*>) #-}
    (<*>) a b = do   f <- a ; v <- b ; return $! (f $! v)
    -- ap a b = liftM2 id a b  =  do  f <- a ; v<- b ; return ((id) )



 
instance Functor StrictIdentity where
    {-# INLINE fmap  #-}
    fmap !f !m = StrictIdentity $! (f $! (runStrictIdentity m))
 
instance Monad StrictIdentity where
    {-# INLINE return  #-}
    return !a = StrictIdentity $! a
    {-# INLINE  (>>=) #-}
    (!m) >>= (!k)  = k $! runStrictIdentity  m    
 --StrictIdentity m >>= k  =  k $! m
instance MonadFix StrictIdentity where
    {-# INLINE mfix  #-}
    mfix !f = StrictIdentity $! (fix  (runStrictIdentity . f))    


{- | 

