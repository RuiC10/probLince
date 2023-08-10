module Exceptions where

data RunException = DeclarationException String
              | AlreadyDeclaredException String
              | DiffNotDeclaredException String
              | ZeroDivisionException


instance Show RunException where
    show (DeclarationException var) = "Variable " ++ var ++ " was used before being declared!"
    show (AlreadyDeclaredException var) = "Variable " ++ var ++ " was already declared!"
    show (DiffNotDeclaredException var) = "Variable " ++ var ++ " was used in a system of differential equations, but was not declared previously!"
    show (ZeroDivisionException) = "Zero division Exception!"
