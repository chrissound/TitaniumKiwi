{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Lens.Micro ((^.))
import Lens.Micro.TH
import Data.Monoid ((<>))

import qualified Graphics.Vty as V
import Brick
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Monad.IO.Class
import System.Process.Typed
import Data.String
import Data.String.Conversions

data Name = NameField deriving (Eq, Ord, Show)

data MyInput = MyInput deriving (Show, Eq, Ord)

data MyState e = MyState {
    inputPrompt :: (E.Editor T.Text MyInput)
  , myHistory :: [MyCommand]
  } -- deriving (Show)

data MyCommand =
    MyCommand { _cmd' :: T.Text
             , _stdout' :: T.Text
             , _stderr' :: T.Text
             , _exitcode' :: Int
             }
             deriving (Show)

data UserCommand =
    UserCommand { _cmd :: T.Text}
             deriving (Show)

makeLenses ''MyCommand
makeLenses ''UserCommand


theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  ]

htitle t =
  hLimit 20 $
  withAttr "infoTitle" $
  txt t

draw :: MyState e -> [Widget MyInput]
draw (MyState f mcc) = [vBox [
   vBox $ fmap (\x -> hLimit 60 $ hBox [txt $ "." <> (x)]) $ take 15 (repeat "")
  , form
  , vBox $ fmap (\x -> hLimit 60 $ hBox [txt $ ">>> " <> (_cmd' $ x)]) mcc
  ]]
    where
        -- form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        form = E.renderEditor
          (txt . T.concat)
          True
          (E.editor MyInput (Just 1) $ T.concat $ E.getEditContents f)
        -- help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        -- body = str $ "- Name is free-form text\n"

app :: App (MyState e) e MyInput
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter []) -> do
                  let mc'' = T.concat $ E.getEditContents $ inputPrompt s
                  (exitCode, out, err) <- liftIO $ do
                    readProcess $ fromString $ cs mc''
                  let mc = MyCommand (mc'') (cs out) (cs err) (read $ show exitCode)
                  continue $ s {
                      inputPrompt = blankPrompt
                    , myHistory = (mc : myHistory s)
                    }
                VtyEvent vee@(V.EvKey k ms) -> do
                    r <- E.handleEditorEvent vee $ inputPrompt s
                    continue $ s { inputPrompt = r}
                _ -> do
                  continue $ s

        -- , appChooseCursor = focusRingCursor formFocus
        , appChooseCursor = const $ const Nothing
        , appStartEvent = return
        , appAttrMap = const theMap
        }

blankPrompt = E.editor MyInput (Just 1) ""

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        -- initialUserCommand = []
        -- f = mkForm
    let s = MyState blankPrompt []

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app s

    putStrLn "The final form state was:"
    print $ myHistory f'
