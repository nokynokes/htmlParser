module OptParse 
  ( Options (..)
  , SingleInput(..)
  , SingleOutput(..)
  , parse
  ) 
  where

import Data.Maybe (fromMaybe)
import Options.Applicative

data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving Show

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

data SingleOutput
  = Stdout
  | OutputFile FilePath
   deriving Show

parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts = 
  info 
    (helper <*> pOptions)
    (fullDesc 
      <> header "hs-blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to html" 
    )
   
pInputFile :: Parser SingleInput
pInputFile = InputFile <$> parser
  where 
    parser = 
      strOption
        ( long "input" 
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
  where 
    parser = 
      strOption
        ( long "output" 
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )

pInputDir :: Parser FilePath
pInputDir = 
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "DIRECTORY"
      <> help "Input Directory"
    )

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "DIRECTORY"
      <> help "Output Directory"
    )
        

pConvertSingle :: Parser Options
pConvertSingle = 
  ConvertSingle <$> pSingleInput <*> pSingleOutput

pConvertDir :: Parser Options
pConvertDir = 
  ConvertDir <$> pInputDir <*> pOutputDir

pSingleInput :: Parser SingleInput
pSingleInput = 
  fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput = 
  fromMaybe Stdout <$> optional pOutputFile 

pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand =
  command "convert" pConvertSingleInfo

pConvertDirCommand :: Mod CommandFields Options
pConvertDirCommand = 
  command "convert-dir" pConvertDirInfo

pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo = 
  info
    (helper <*> pConvertSingle)
    (progDesc "Convert a single markup source to html")

pConvertDirInfo :: ParserInfo Options
pConvertDirInfo = 
  info 
    (helper <*> pConvertDir)
    (progDesc "Convert a directory of markup file to html")

pOptions :: Parser Options
pOptions = 
  subparser $ pConvertSingleCommand <> pConvertDirCommand