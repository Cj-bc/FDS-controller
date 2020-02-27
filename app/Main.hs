{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Graphics.Vty as Vty
import Brick
import Brick.BChan
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (border)
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

uiFaceRadians :: FaceData -> Widget Name
uiFaceRadians s = border $ vBox [ str "Face Rotations"
                                , hCenter $ str $ "X: " ++ show (s^.face_x_radian)
                                , hCenter $ str $ "Y: " ++ show (s^.face_y_radian)
                                , hCenter $ str $ "Z: " ++ show (s^.face_z_radian)
                                ]

uiMouthPercents s = border $ vBox [ str "Mouth size percentage"
                                  , str $ "height: " ++ show (s^.mouth_height_percent)
                                  , str $ "width: "  ++ show (s^.mouth_width_percent)
                                  ]

ui s = [vBox [ hCenter $ str "Face-Data-Server easy controller"
             , uiFaceRadians fd
             , uiMouthPercents fd
             ]
       ]
    where
        fd = s^.faceData

-- Event handler {{{

-- | [Helper function] Update faceData and send it.
update f l s = continue =<< liftIO (do
    let s' = s&(faceData.l)%~f
    sendFaceData (s^.sock) (s^.addr) (s'^.faceData)
    return s')

eHandler s (VtyEvent (Vty.EvKey (Vty.KEsc) []))      = halt s
-- Face Rotation
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'a') [])) = update (subtract $ 1/pi) face_y_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 's') [])) = update (subtract $ 1/pi) face_x_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'w') [])) = update (+ 1/pi)          face_x_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'd') [])) = update (+ 1/pi)          face_y_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = update (subtract $ 1/pi) face_z_radian s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'e') [])) = update (+ 1/pi)          face_z_radian s
-- Mouth
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'h') [])) = update (subtract 1) mouth_width_percent s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'l') [])) = update (+ 1)        mouth_width_percent s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'j') [])) = update (subtract 1) mouth_height_percent s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'k') [])) = update (+ 1)        mouth_height_percent s
-- Left Eye
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'o') [])) = update (subtract 1)  left_eye_percent s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'p') [])) = update (+ 1)         left_eye_percent s
-- Right Eye
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'u') [])) = update (subtract 1)  right_eye_percent s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'i') [])) = update (+ 1)         right_eye_percent s

eHandler s _ = continue s
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
