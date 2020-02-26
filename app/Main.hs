module Main where

import qualified Graphics.Vty as Vty
import Brick
import Brick.BChan
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import FaceDataServer.Types (Radian, Percent)
import Network.Multicast (multicastReceiver)

data Name = NoName
type AppState = (Socket, FaceData)

hostName = "224"
mkInitialState sock = (sock, defaultFaceData)


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
    evntChan <- newBChan 10
    sock <- multicastReceiver 
    forkIO $ forever $ do
        writeBChan eventChan 
