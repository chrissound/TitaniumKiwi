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
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Monad.IO.Class
import System.Process.Typed
import Data.String
import Data.String.Conversions
import qualified Data.Vector as Vec
import System.Exit
import qualified Brick.AttrMap as A
import Data.Foldable
import Data.Bool

data Name = NameField deriving (Eq, Ord, Show)

data MyInput = MyInput | MyList deriving (Show, Eq, Ord)

data MyState e = MyState {
    inputPrompt :: (E.Editor T.Text MyInput)
  , myHistory :: [MyCommand]
  , myList :: BL.List MyInput MyCommand
  } -- deriving (Show)

data MyCommand =
    MyCommand { _cmd' :: T.Text
             , _stdout' :: T.Text
             , _stderr' :: T.Text
             , _exitcode' :: ExitCode
             }
             deriving (Show)

-- data UserCommand =
--     UserCommand { _cmd :: T.Text}
--              deriving (Show)

makeLenses ''MyCommand
-- makeLenses ''UserCommand


theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (BL.listAttr,            fg V.white)
  , (BL.listSelectedAttr,    fg V.blue)
  , (customAttr,            fg V.cyan)
  ]

htitle t =
  hLimit 20 $
  withAttr "infoTitle" $
  txt t

customAttr :: A.AttrName
customAttr = BL.listSelectedAttr <> "custom"

miw :: Bool -> MyCommand -> Widget MyInput
-- miw True e = withAttr customAttr $ str $ "(✓) " <> (cs $ _cmd' e)
-- miw False e = str $ "(x)  " <> show e
miw selected e = bool id (withAttr customAttr) selected $ llll
  where llll = txt ((ecti $ _exitcode' e) <> _cmd' e)
  -- undefined

ecti (ExitFailure _) = "(x) $ "
ecti ExitSuccess = "(✓) $ "

takeLeftover :: [a] -> t -> [a]
takeLeftover [] _ = []
takeLeftover (x:xss) _ = xss

lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' takeLeftover xs (drop n xs)

fillRemaining :: a -> [a] -> [a]
fillRemaining x v = v ++ (repeat x)

xxx l = do
  let th = 10
  case (BL.listSelectedElement l) of
    Just (_, x) -> txt . T.unlines . take th . fillRemaining "..." . lastN' th . T.lines $ _stdout' x <> _stderr' x
    Nothing -> vBox $ take 6 $ repeat $ str "."

draw :: MyState e -> [Widget MyInput]
draw (MyState f mcc lll) = [vBox [
   -- vBox $ fmap (\x -> hLimit 60 $ hBox [txt $ "." <> (x)]) $ take 15 (repeat "")
    -- vLimit 5 $ vBox $ take 6 $ repeat $ str "."
    xxx lll
  , form
  , BL.renderList (miw) True lll
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
                VtyEvent (V.EvKey V.KPageUp [])   -> do
                  continue $ s { myList = BL.listMoveBy (-1) $ myList s }
                VtyEvent (V.EvKey V.KPageDown [])   -> do
                  continue $ s { myList = BL.listMoveBy 1 $ myList s }
                  -- continue $ s { myList = BL.listReverse $ myList s }
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter []) -> do
                  let mc'' = T.concat $ E.getEditContents $ inputPrompt s
                  (exitCode, out, err) <- liftIO $ do
                    readProcess $ fromString $ cs mc''
                  let mc = MyCommand (mc'') (cs out) (cs err) (exitCode)
                  continue $ s {
                      inputPrompt = blankPrompt
                    , myHistory = (mc : myHistory s)
                    , myList = BL.listMoveTo 0 $ BL.listInsert 0 mc (myList s)
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
    let bc = MyCommand "" "" ""
    let s = MyState blankPrompt [] (BL.list MyList (Vec.fromList []) 1)

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app s

    putStrLn "The final form state was:"
    print $ myHistory f'
