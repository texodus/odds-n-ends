import Network.Socket
import Network.BSD

import Data.Bits
import Data.Time.Clock
import Data.Time.Format

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad

import System.IO
import System.Locale


queryBehavior = defaultHints { addrFlags = [AI_PASSIVE] }
queueLength   = 5
defaultPort   = 23

main = server defaultPort


type LogMessage = String
type Logger     = LogMessage -> IO ()

newtype LogTime = LogTime UTCTime

instance Show LogTime where 
    show (LogTime time) = (formatTime defaultTimeLocale "%F %R" time)

initLogging :: IO Logger 
initLogging =  do msgQueue <- newChan
                  forkIO   $  loop msgQueue
                  writeChan msgQueue "Logging successfully initialized"
                  return (writeChan msgQueue)

    where loop :: Chan LogMessage -> IO ()
          loop    queue           =  do msg  <- readChan queue
                                        time <- getCurrentTime
                                        putStrLn $ (show $ LogTime time) ++ " " ++ msg
                                        loop queue


type Port = Int

server :: Port -> IO () 
server    port =  withSocketsDo $ initServer

    where initServer :: IO ()
          initServer =  do logger <- initLogging
                           addr   <- liftM head $ getAddrInfo (Just queryBehavior) Nothing (Just (show port))
                           sock   <- socket (addrFamily addr) Stream defaultProtocol
                           logger $ "Starting on " ++ (show $ addrAddress addr)
                           bindSocket sock (addrAddress addr)
                           listen sock queueLength
                           procRequests logger sock

          procRequests :: Logger -> Socket -> IO () 
          procRequests    logger    socket =  do (clientSocket, address) <- accept socket
                                                 forkIO $ procMessages (logger . ((show address ++ ": ") ++)) clientSocket 
                                                 procRequests logger socket

          procMessages :: Logger -> Socket -> IO ()
          procMessages    logger    socket =  do logger "client connected" 
                                                 connhdl  <- socketToHandle socket ReadMode
                                                 hSetBuffering connhdl LineBuffering
                                                 messages <- hGetContents connhdl
                                                 mapM_ logger (lines messages)
                                                 hClose connhdl
                                                 logger "client disconnected"


type StockPrice  = Double
type StrikePrice = Double
type Expiration  = Double 
type Interest    = Double
type Volatility  = Double

data Option      = Option StockPrice StrikePrice Expiration Interest Volatility
data OptionType  = Put | Call
    
blackScholes :: OptionType -> Option             -> Double
blackScholes    Call          (Option s x t r v) =  s * normcdf (d1 s x t r v) - x * exp (-r * t) * normcdf (d2 s x t r v)
blackScholes    Put           (Option s x t r v) =  x * exp (-r * t) * normcdf (-(d2 s x t r v)) - s * normcdf (-(d1 s x t r v))

d1 s x t r v          = (log(s / x) + (r + v * v / 2) * t) / (v * sqrt t)
d2 s x t r v          = (d1 s x t r v) - v * sqrt t

horner coeff  base    =  foldr1 (\x y -> y * base + x)  coeff

normcdf x | x < 0     = 1 - w
	  | otherwise = w
    
    where w     = 1.0 - 1.0 / sqrt (2.0 * pi) * exp(-l * l / 2.0) * poly k
	  k     = 1.0 / (1.0 + 0.2316419 * l)
	  l     = abs x
	  poly  = horner coeff 
	  coeff = [0.0, 0.31938153, -0.356563782, 1.781477937, -1.821255978, 1.330274429] 
