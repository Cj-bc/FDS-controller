{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Graphics.Vty as Vty
import Brick
import Brick.BChan
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import FaceDataServer.Types
import FaceDataServer.Connection
import Network.Socket (Socket, SockAddr)
import Network.Multicast (multicastSender)

data Name = NoName deriving (Eq, Ord)
data AppState = AppState { _sock     :: Socket
                         , _addr     :: SockAddr
                         , _faceData :: FaceData
                         }
makeLenses ''AppState

mkInitialState sock addr = AppState sock addr defaultFaceData

hostName = "226.0.0.1"
portNum = 5032

ui s = [vBox [str "Face-Data-Server easy controller"]]

-- Event handler {{{

-- | [Helper function] Update faceData and send it.
update f l s = continue =<< liftIO (do
    let s' = s&(faceData.l)%~f
    sendFaceData (s^.sock) (s^.addr) (s'^.faceData)
    return s')

eHandler s (VtyEvent (Vty.EvKey (Vty.KEsc) []))      = halt s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'a') [])) = update (subtract $ 1/pi) face_y_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 's') [])) = update (subtract $ 1/pi) face_x_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'w') [])) = update (+ 1/pi)          face_x_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'd') [])) = update (+ 1/pi)          face_y_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = update (subtract $ 1/pi) face_z_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'e') [])) = update (+ 1/pi)          face_z_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'd') [])) = undefined
-- }}}

app :: App AppState e Name
app = App { appDraw         = ui
          , appHandleEvent  = eHandler
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor
          , appAttrMap      = const $ attrMap Vty.defAttr []
          }

main :: IO ()
main = do
    (s, adr) <- multicastSender hostName portNum
    void $ defaultMain app $ mkInitialState s adr
