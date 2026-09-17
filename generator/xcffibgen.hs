{-
 - Copyright 2014 Tycho Andersen
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -   http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}
module Main where

import Data.XCB.Python.Parse

import Options.Applicative

import System.Directory
import System.FilePath

import Data.Char (isAlphaNum, isHexDigit)
import Data.List (sort, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Environment (lookupEnv)
import System.Process (readProcess)

keysymSections :: [String]
keysymSections =
  [ "MISCELLANY", "XKB_KEYS", "3270", "LATIN1", "LATIN2", "LATIN3"
  , "LATIN4", "LATIN8", "LATIN9", "KATAKANA", "ARABIC", "CYRILLIC"
  , "GREEK", "TECHNICAL", "SPECIAL", "PUBLISHING", "APL", "HEBREW"
  , "THAI", "KOREAN", "ARMENIAN", "GEORGIAN", "CAUCASUS", "VIETNAMESE"
  , "CURRENCY", "MATHEMATICAL", "BRAILLE", "SINHALA"
  ]

generateKeysym :: FilePath -> IO ()
generateKeysym out = do
  cpp <- fromMaybe "cpp" <$> lookupEnv "CPP"
  contents <- readProcess cpp args ""
  writeFile out $ unlines $ sort $ mapMaybe parseDefine (lines contents)
  where
    args = ["-dM"] ++ map ("-DXK_" ++) keysymSections ++
           ["-include", "X11/keysymdef.h", "-include", "X11/XF86keysym.h", "-"]

    parseDefine line = case words line of
      ("#define" : name@('X' : 'K' : '_' : suffix) : val : _)
        | validName suffix
        , suffix `notElem` keysymSections -> parseValue name val
      ("#define" : name@('X' : 'F' : '8' : '6' : 'X' : 'K' : '_' : suffix) : val : _)
        | validName suffix -> parseValue name val
      _ -> Nothing

    validName suffix = not (null suffix) && all (\c -> isAlphaNum c || c == '_') suffix

    parseValue name val = case val of
      '0' : 'x' : digits
        | not (null digits) && all isHexDigit digits -> Just $ name ++ " = " ++ val
      _ -> case stripPrefix "_EVDEVK(0x" val >>= (fmap reverse . stripPrefix ")" . reverse) of
        Just digits
          | not (null digits) && all isHexDigit digits ->
              Just $ name ++ " = 0x10081000 + 0x" ++ digits
        _ -> error $ "invalid keysym value: " ++ name ++ " " ++ val

data Xcffibgen = Xcffibgen { input :: String
                           , output :: String
                           }

options :: Parser Xcffibgen
options = Xcffibgen
    <$> strOption
        ( long "input"
       <> metavar "DIR"
       <> help "Input directory containing xcb xml files.")
    <*> strOption
        ( long "output"
       <> metavar "DIR"
       <> help "Output directory for generated python.")

run :: Xcffibgen -> IO ()
run (Xcffibgen inp out) = do
  headers <- parseXHeaders inp
  createDirectoryIfMissing True out
  sequence_ $ map processFile $ xform headers
  generateKeysym $ out </> "keysymdef.py"
  where
    processFile (fname, suite) = do
      putStrLn fname
      let fname' = out </> fname ++ ".py"
          contents = renderPy suite
      writeFile fname' contents

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Generate XCB bindings for python."
     <> header "xcffib - the cffi-based XCB generator")
