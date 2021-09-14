------------------------------------------------------------------------------
--- This module contains various operations to show a FlatCurry program
--- in human-readable forms, e.g., only the interface or also the
--- complete program translated back into pattern-based rules.
--- These operations are used in the Curry Browser and they are
--- also the basis to implement the `:interface` command
--- of PAKCS or KiCS2.
---
--- The interface description contains the type declarations
--- for all entities defined and exported by this module.
---
--- The human-readable presentation is (almost) Curry source code
--- generated from a FlatCurry program.
---
--- @author Michael Hanus
--- @version April 2021
------------------------------------------------------------------------------

module ShowFlatCurry where

import System.Environment ( getArgs )

import FlatCurry.Types
import FlatCurry.Files
import System.Directory   ( doesFileExist, getModificationTime )
import System.FilePath    ( takeFileName, (</>) )
import System.Process     ( exitWith )
import System.CurryPath   ( lookupModuleSourceInLoadPath, runModuleAction )

import FlatCurry.ShowIntMod

------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [st,prog]        -> showProg st prog
    [st,prog,target] -> saveProg st prog target
    _                -> printUsage args
 where
  showProg st prog 
    | st `elem` ["-i", "-int", "--interface"]
    = runModuleAction printInterface prog
    | st `elem` ["-m", "-mod", "--module"]
    = runModuleAction printCurryMod prog
    | otherwise
    = printUsage [st, prog]

  saveProg st prog target
    | st `elem` ["-i", "-int", "--interface"]
    = runModuleAction (writeInterface target) prog
    | st `elem` ["-m", "-mod", "--module"]
    = runModuleAction (writeCurryMod target) prog
    | otherwise
    = printUsage [st, prog, target]

printUsage :: [String] -> IO ()
printUsage args = do
  putStrLn $ unlines $
    [ "ERROR: Illegal arguments: " ++ unwords args, ""
    , "Usage:", ""
    , "Print/store interface of a Curry module:", ""
    , "    curry-showflat [-i|--interface] modulename [targetfile]", ""
    , "Print/store Curry module reconstructed from the FlatCurry program:", ""
    , "    curry-showflat [-m|--module] modulename [targetfile]"
    ]
  exitWith 1

------------------------------------------------------------------------------
-- Print interface on stdout.
printInterface :: String -> IO ()
printInterface progname = do
  intstring <- genInt False progname
  putStrLn ("Interface of module \"" ++ progname ++ "\":\n")
  putStrLn intstring

-- Write interface into target file.
writeInterface :: String -> String -> IO ()
writeInterface targetfile progname = do
  intstring <- genInt True progname
  writeFile targetfile
            ("--Interface of module \"" ++ progname ++ "\":\n\n" ++
             intstring)
  putStrLn $ "Interface written into file \"" ++ targetfile ++ "\""

-- Generate interface description for a program:
-- If first argument is True, generate stubs (...external) for
-- all functions so that the resulting interface is a valid Curry program.
genInt :: Bool -> String -> IO String
genInt genstub progname =
  getFlatInt progname >>= return . showInterface genstub

------------------------------------------------------------------------------

-- show representation on stdout:
printCurryMod :: String -> IO ()
printCurryMod progname = do
  modstring <- genCurryMod progname
  putStrLn ("-- Program file: " ++ progname)
  putStrLn modstring

-- write representation into file:
writeCurryMod :: String -> String -> IO ()
writeCurryMod targetfile progname = do
  modstring <- genCurryMod progname
  writeFile targetfile
            ("--Program file: " ++ progname ++ "\n\n" ++
             modstring)
  putStrLn $ "Module written into file \"" ++ targetfile ++ "\""

-- generate a human-readable representation of a Curry module:
genCurryMod :: String -> IO String
genCurryMod modname = do
  --prog <- readFlatCurryFile (flatCurryFileName modname)
  prog <- getFlatProg modname
  return $ showCurryModule prog

------------------------------------------------------------------------------
-- Auxiliaries:

-- Get a FlatCurry program (parse only if necessary):
getFlatProg :: String -> IO Prog
getFlatProg modname = do
  mbdirfn <- lookupModuleSourceInLoadPath modname
  let progname    = maybe modname snd mbdirfn
      fcyprogname = flatCurryFileName
                      (maybe modname
                             (\ (d,_) -> d </> takeFileName modname)
                             mbdirfn)
  fcyexists <- doesFileExist fcyprogname
  if not fcyexists
    then readFlatCurry modname
    else do ctime <- getModificationTime progname
            ftime <- getModificationTime fcyprogname
            if ctime > ftime
              then readFlatCurry modname
              else readFlatCurryFile fcyprogname

-- Get a FlatCurry interface (parse only if necessary):
getFlatInt :: String -> IO Prog
getFlatInt modname = do
  mbdirfn <- lookupModuleSourceInLoadPath modname
  let progname     = maybe modname snd mbdirfn
      fintprogname = flatCurryIntName
                       (maybe modname
                              (\ (d,_) -> d </> takeFileName modname)
                              mbdirfn)
  fintexists <- doesFileExist fintprogname
  if not fintexists
    then readFlatCurryInt modname
    else do ctime <- getModificationTime progname
            ftime <- getModificationTime fintprogname
            if ctime > ftime
              then readFlatCurryInt modname
              else readFlatCurryFile fintprogname

-----------------------------------------------------------------------------
