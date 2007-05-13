{-# OPTIONS_GHC -fglasgow-exts #-}
module Network.AGI where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Maybe
import Data.Word
import Text.ParserCombinators.Parsec
import System.IO
import System.Posix.Signals

newtype AGI a = AGI { runAGI :: ReaderT [(String, String)] IO a }
    deriving (Monad, MonadIO, Functor)

type EscapeDigits = String
type Command = String
data Timeout =  Timeout Word (Maybe Word) -- ^ timeout, max digits
type ReturnCode = Word

data SoundType = Wav | GSM 

instance Show SoundType where
    show Wav = "wav"
    show GSM  = "gsm"

-- TODO: let user install a custom sipHUP handler (sigHUP is sent when the caller hangs ups)
run :: AGI a -> IO a
run agi =
    do installHandler sigHUP Ignore Nothing
       hSetBuffering stdin LineBuffering
       hSetBuffering stdout LineBuffering
       agiVars <- readAgiVars
       runReaderT (runAGI agi) agiVars

readAgiVars :: IO [(String, String)]
readAgiVars = 
    do mAgiVar <- readAgiVar 
       case mAgiVar of
	    Nothing -> 
		return []
	    Just agiVar ->
		do rest <- readAgiVars
		   return (agiVar:rest)
    where readAgiVar :: IO (Maybe (String, String))
	  readAgiVar =
	      do l <- getLine
		 case l of
		      "" -> return Nothing
		      _ -> let (a,v) = break ((==) ':') l in
				       return (Just (a, dropWhile ((==) ' ') (tail v)))

sendRecv :: Command -> AGI (ReturnCode, String)
sendRecv cmd =
    liftIO $ do putStrLn cmd
                l <- getLine
                let (c,r) = break ((==) ' ') l
	        return (read c, tail r)

parseResult :: CharParser () (Int, Bool)
parseResult =
    do string "result="
       digits <- many1 $ oneOf ('-':['0'..'9'])
       skipMany (char ' ')
       timeout <- parseTimeout
       return (read digits, timeout)

parseTimeout :: CharParser () Bool
parseTimeout =
    do string "(timeout)"
       return True
    <|>
    do return False

answer :: AGI Bool
answer =
    do (code, res) <- sendRecv "ANSWER"
       case res of
            "result=0" -> 
		return $ True
	    _ -> 
		return $ False

hangUp :: Maybe String -> AGI (ReturnCode, String)
hangUp mChannel =
    sendRecv ("HANGUP" ++ (fromMaybe "" mChannel))

-- TODO: does digit include # and * ?
getData :: FilePath -> Maybe Timeout -> AGI (ReturnCode, (Int, Bool))
getData fp mTimeout =
    let cmd = 
	    "GET DATA " ++ fp ++
                        case mTimeout of
			  Nothing -> ""
			  Just (Timeout timeout mMaxDigits) ->
			      " " ++ show timeout ++
				  case mMaxDigits of
				    Nothing -> ""
				    Just maxDigits -> " " ++ show maxDigits
    in
      do (code, res) <- sendRecv cmd
         case parse parseResult res res of
	   Left e -> 
	       return $ (code, ((-1), False))
	   Right (digits,timeout) -> 
	       return $ (code, (digits, timeout))

record :: FilePath -> SoundType -> EscapeDigits -> Word -> Bool -> AGI (ReturnCode, String)
record fp st escapeDigits length beep =
    sendRecv $ "RECORD FILE " ++ fp ++ " " ++ show st ++ " " ++ show escapeDigits ++ " " ++ show length ++ (if beep then " beep" else "")
                
-- TODO: does digit include # and * ?
sayNumber :: Int -> EscapeDigits -> AGI (ReturnCode, String)
sayNumber num escapeDigits =
    sendRecv $ "SAY NUMBER " ++ (show num) ++ " " ++ show escapeDigits

-- TODO: what if the users enters DTMF
stream :: FilePath -> EscapeDigits -> AGI (ReturnCode, String)
stream fp ed = 	
    sendRecv $ "STREAM FILE " ++ fp ++ " " ++ show ed

-- TODO: does digit include # and * ?
-- TODO: positive timeouts only
waitForDigit :: Integer -> AGI (Maybe Int)
waitForDigit timeout =
    do (code, res) <- sendRecv $ "WAIT FOR DIGIT " ++ show timeout
       case parse parseResult res res of
         Left e -> return Nothing -- error ?
         Right (ascii, _) -> return (Just (ascii - 48))
                      