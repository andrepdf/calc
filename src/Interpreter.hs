{-# LANGUAGE CApiFFI #-}

module Interpreter
    ( interpret )
where

import Data.Word             ( Word8 )
import Foreign.Ptr           ( Ptr )
import Foreign.C.Types       ( CInt(..), CDouble(..) )
import Foreign.Marshal.Array ( withArray )
import Foreign.Marshal.Utils ( with, toBool )
import Foreign.Storable      ( peek )

foreign import capi "Interpreter.h interpret"
    c_interpret :: Ptr CDouble -> Ptr Word8 -> IO CInt

interpret :: [Word8] -> IO (Maybe Double)
interpret xs =
    withArray xs $ \xptr ->
        with 0 $ \rptr -> do
            err <- c_interpret rptr xptr
            (CDouble res) <- peek rptr
            if not $ toBool err
               then pure $ Nothing
               else pure $ Just res
