module Main where

import Control.DeepSeq
import Control.Monad (join)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Proxy
import FPS
import Network.BSD (getHostByName, hostAddress)
import Network.Socket (SockAddr(..))
import System.Environment
import Text.Read

import Game.GoreAndAsh
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync
import Game.GoreAndAsh.LambdaCube

import Game
import Game.Core
import Render.Pipeline


gameFPS :: Int
gameFPS = 60

parseArgs :: IO (String, Int)
parseArgs = do
  args <- getArgs
  case args of
    [h, p] -> case readMaybe p of
      Nothing -> fail "Failed to parse port"
      Just pint -> return (h, pint)
    _ -> fail "Misuse of arguments: gore-and-ash-client HOST PORT"


main :: IO ()
main = withModule (Proxy :: Proxy AppMonad) $ do
  gs <- newGameState mainWire
  (host, port) <- liftIO parseArgs
  fps <- makeFPSBounder 60
  firstLoop fps host port gs `catch` errorExit
  where
    -- | Resolve given hostname and port
    getAddr s p = do
      he <- getHostByName s
      return $ SockAddrInet p $ hostAddress he

    errorExit e = do
      liftIO $ case e of
        PipeLineCompileFailed _ _ msg -> putStrLn msg
        PipeLineAlreadyRegistered i -> putStrLn $ "Pipeline already registered: " ++ show i
        PipeLineNotFound i -> putStrLn $ "Pipeline is not found: " ++ show i
        StorageNotFound i -> putStrLn $ "Storage is not found: " ++ show i
        PipeLineIncompatible _ msg -> putStrLn $ "Pipeline incompatible: " ++ msg
      fail "terminate: fatal error"

    firstLoop fps host port gs = do
      (_, gs') <- stepGame gs $ do
        networkSetDetailedLoggingM False
        syncSetLoggingM False
        syncSetRoleM SyncSlave
        networkBind Nothing 1 2 0 0
        addr <- liftIO $ getAddr host (fromIntegral port)
        _ <- networkConnect addr 2 0
        initPipeline
      gameLoop fps gs'

    gameLoop fps gs = do
      waitFPSBound fps
      (mg, gs') <- stepGame gs (return ())
      mg `deepseq` if fromMaybe False $ gameExit <$> join mg
        then cleanupGameState gs'
        else gameLoop fps gs'
