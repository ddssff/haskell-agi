module Main where

-- Standard Haskell Modules

import Control.Monad.Trans
import Data.Maybe
import Data.Word
import System.IO
import System.Random
import System.Posix.Unistd

-- 3rd Party Modules

import Network.AGI

main :: IO ()
main =
       run mainAGI

mainAGI :: AGI ()
mainAGI =
    do answer
       liftIO $ sleep 1
       playGame
       hangUp Nothing
       return ()

playGame :: AGI ()
playGame =
    do secretDigit <- liftIO $ randomRIO (0, 9)
       stream "guessing-game-intro" []
       guess <- waitForDigit (-1)
       play (compare secretDigit) (fromMaybe 0 guess)
       return ()

play oracle guess =
    loop guess
    where
      loop guess =
          case oracle guess of
            LT -> 
                do stream "guessing-game-lower" []
                   sayNumber guess []
                   nextGuess <- waitForDigit (-1)
                   loop (fromMaybe guess nextGuess)
            GT ->
                do stream "guessing-game-higher" []
                   sayNumber guess []
                   nextGuess <- waitForDigit (-1)
                   loop (fromMaybe guess nextGuess)
            EQ ->
                do stream "guessing-game-yay" []
                   sayNumber guess []
                   stream "guessing-game-correct" []

{-

Let's play a game! I am think of a number between 0 and 9. Can you
guess what it is? Enter your guess using the number pad on your phone.

Sorry, the number I am thinking of is higher than

Sorry, the number I am thinking of is less than

Yay! 

is the number I was thinking of! How did you know?

-}