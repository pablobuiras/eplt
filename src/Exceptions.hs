{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
module Exceptions where

import Data.Typeable
import Control.Exception

data SomeEPLTException = forall e. (Exception e) => SomeEPLTException e
                       deriving Typeable

instance Exception SomeEPLTException

epltExceptionToException :: Exception e => e -> SomeException
epltExceptionToException = toException . SomeEPLTException

epltExceptionFromException :: Exception e => SomeException -> Maybe e
epltExceptionFromException x = do SomeEPLTException a <- fromException x
                                  cast a

instance Show SomeEPLTException where
    show (SomeEPLTException e) = show e
