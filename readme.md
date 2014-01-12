[![Build Status](https://travis-ci.org/cartazio/strict-identity.png?branch=master)](https://travis-ci.org/cartazio/strict-identity)

#About

strict-identity package is meant to make writing nested strict let 
expression heavy code a bit more pleasant, for all those High Performance 
Haskell lib authors out there.

A simple example of the strict identity monad in action (and working wonderfully)
is the following bit fiddling code, which generates C competitive assembly 
on both major GHC backends, -fasm and -fllvm

```haskell
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
```