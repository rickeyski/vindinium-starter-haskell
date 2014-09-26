{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Options.Applicative

import Vindinium
import Bot

import Data.String (fromString)
import Data.Text (pack, unpack)

data Cmd = Training Settings (Maybe Int) (Maybe Board)
         | Arena Settings
         deriving (Show, Eq)

cmdSettings :: Cmd -> Settings
cmdSettings (Training s _ _) = s
cmdSettings (Arena s) = s

settings :: Parser Settings
settings = Settings <$> (Key <$> argument (Just . pack) (metavar "KEY"))
                    <*> (fromString <$> strOption (long "url"
                                                  <> value "http://vindinium.org"
                                                  <> help "Enter url of Vindinium server"))

trainingCmd :: Parser Cmd
trainingCmd = Training <$> settings
                       -- <*> optional (option (long "turns"
                       --                  <> metavar "N"
                       --                  <> help "Run training for N turns"))
                       --  The above option refused to compile so since it is optional, we ignore it for now
                       <*> pure Nothing
                       <*> pure Nothing

arenaCmd :: Parser Cmd
arenaCmd = Arena <$> settings

cmd :: Parser Cmd
cmd = subparser
    ( command "training" (info trainingCmd
        ( progDesc "Run bot in training mode" ))
   <> command "arena" (info arenaCmd
        (progDesc "Run bot in arena mode" ))
    )

runCmd :: Cmd -> IO ()
runCmd c  = do
    s <- runVindinium (cmdSettings c) $ do
        case c of
            (Training _ t b) -> playTraining t b bot
            (Arena _)        -> playArena bot

    putStrLn $ "Game finished: " ++ unpack (stateViewUrl s)

main :: IO ()
main =
    execParser opts >>= runCmd
  where
    opts = info (cmd <**> helper) idm
