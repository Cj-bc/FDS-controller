module Main where

import qualified Graphics.Vty as Vty
import Brick
import Brick.BChan
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import FaceDataServer.Types (Radian, Percent)
import Network.Multicast (multicastSender)

data Name = NoName
type AppState = (Socket, FaceData)

mkInitialState sock = (sock, defaultFaceData)

hostName = "226.0.0.1"
portNum = 5032

ui s = [vBox [str "Face-Data-Server easy controller"]]



eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'w') [])) = undefined

app :: App AppState e Name
app = App { appDraw         = ui
          , appHandleEvent  = eHandler
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor
          , appAttrMap      = const $ attrMap Vty.defAttr []
          }

main :: IO ()
main = do
    s <- multicastSender hostName portNum
    void $ defaultMain app $ mkInitalState s
