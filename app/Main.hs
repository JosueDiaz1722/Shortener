module Main where
import Network.Wai.Handler.Warp
import Server
import Shortener (shortener)




main :: IO ()
main = run 8082 app