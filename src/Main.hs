module Main where

import SwapIFC
import qualified Data.Map as M

lowFlowAdding :: Flow Low ()
lowFlowAdding = do
  let f = mkLow 42
  let g = mkLow 10
  lprint $ mkLow "First value to add:"
  lprint f
  lprint $ mkLow "Second value to add:"
  lprint g
  lprint $ mkLow "Adding them together:"
  lprint $ f .+. g

main = runFlow lowFlowAdding

------------------------------------------------------------------------------
-- Side effect example
lowFlowWithSideEffects :: Flow Low ()
lowFlowWithSideEffects = do
  ref <- newFlowRef (10 :: Int)
  lprint $ readFlowRef ref
  lprint $ mkLow "Now adding one to the ref!"
  modifyFlowRef ref (+ 1)
  lprint $ readFlowRef ref

--main = runFlow lowFlowWithSideEffects

------------------------------------------------------------------------------

type Username = String
type Password = String
type Database = M.Map Username Password

database :: Database
database = createDB [("Alan", "Turing"), ("Alonzo", "Church"), ("user", "abc123")] M.empty
  where createDB :: [(Username, Password)] -> M.Map Username Password -> M.Map Username Password
        createDB [] m          = m
        createDB ((u, p):xs) m = createDB xs $ M.insert u p m

-- Validate will, given the username and the password, return a String saying
-- either "Hello " ++ username ++ ", you will be redirected" or
-- "Incorrect username or password"
validate :: Flow Low Username -> Flow High Password ->
            Flow Low String
validate flowUser flowPass = declassify $ do
  user <- upgrade flowUser
  pass <- flowPass
  case M.lookup user database of
    Just pass' -> if pass == pass' then
                    return ("Welcome back, " ++ user ++ "!")
                  else
                    return ("Invalid username/password!")
    Nothing    -> return ("Invalid username/password!")

getUsername :: Flow Low String
getUsername = do
  unlprint "Insert username: "
  flowGetLine

getPassword :: Flow High String
getPassword = do
  upgrade $ unlprint "Insert password"
  flowGetLine
  
--main = runFlow $ lprint $ validate getUsername getPassword

------------------------------------------------------------------------------

highFlowAdding :: Flow High ()
highFlowAdding = do
  let f = mkHigh 42
  let g = mkHigh 10
  lprint $ mkHigh "First value to add:"
  lprint f
  lprint $ mkHigh "Second value to add:"
  lprint g
  lprint $ mkHigh "Adding the together:"
  lprint $ f .+. g
  
--main = runFlow highFlowAdding
