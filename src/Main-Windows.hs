module Main where

import Control.Exception (catch)
import Control.Monad (unless, void, forever)
import Foreign.Ptr
import Foreign.Marshal hiding (void)
import System.Exit

import Graphics.Win32
import System.Win32.DLL
import Bindings.CEF3

import Common

main :: IO ()
main = do
    mainInstance <- getModuleHandle Nothing
    mainArgs <- new $ C'cef_main_args_t mainInstance

    app <- initialize_app_handler
    exCode <- c'cef_execute_process mainArgs app nullPtr
    unless (exCode >= 0) $ do

        settings <- mkCefSettings
        putStrLn "cef_initialize"
        void $ c'cef_initialize mainArgs settings app nullPtr

        -- register class & create window
        setupWindow mainInstance 200 200

        putStrLn "cef_run_message_loop"
        c'cef_run_message_loop
        putStrLn "cef_shutdown"
        c'cef_shutdown

setupWindow :: HINSTANCE -> Int -> Int -> IO ()
setupWindow mainInstance width height = do
  let winClass = mkClassName "Hello"
  icon    <- loadIcon   Nothing iDI_APPLICATION
  cursor  <- loadCursor Nothing iDC_ARROW
  bgBrush <- createSolidBrush (rgb 0 0 255)
  registerClass
      ( cS_VREDRAW + cS_HREDRAW
      , mainInstance
      , Just icon
      , Just cursor
      , Just bgBrush
      , Nothing
      , winClass
      )
  lpps <- mallocBytes sizeofPAINTSTRUCT
  hwnd <- createWindow
              winClass
              "Test window"
              wS_OVERLAPPEDWINDOW
              Nothing Nothing -- initail window pos: default
              (Just width) (Just height)
              Nothing         -- no parent, i.e, root window is the parent.
              Nothing         -- no menu handle
              mainInstance
              (wndProc lpps)
  showWindow hwnd sW_SHOWNORMAL
  updateWindow hwnd

messagePump :: HWND -> IO ()
messagePump hwnd = allocaMessage $ \ msg -> forever $ do
    getMessage msg (Just hwnd) `catch` \ _ -> exitWith ExitSuccess
    translateMessage msg
    dispatchMessage msg

wndProc :: LPPAINTSTRUCT
    -> HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc lpps hwnd wmsg wParam lParam
 -- | wmsg == wM_PAINT && hwnd /= nullPtr = do
     -- r <- getClientRect hwnd
     -- hdc <- beginPaint hwnd lpps
     -- endPaint hwnd lpps
     -- return 0
 | otherwise =
     defWindowProc (Just hwnd) wmsg wParam lParam
 | wmsg == wM_PAINT && hwnd /= nullPtr = do
    -- (left, top, right, bottom) <- getClientRect hwnd
    startBrowserWindow hwnd
    return 0

startBrowserWindow hwnd = do
    putStrLn "new windowInfo"
    -- windowInfo <- new $ C'cef_window_info_t nullPtr 0 0 nullPtr
    winName <- mkCefString "Test window"
    windowInfo <- new $ C'cef_window_info_t
        wS_EX_CLIENTEDGE
        winName
        -- wS_OVERLAPPEDWINDOW
        wS_CHILD
        (fromIntegral cW_USEDEFAULT)
        (fromIntegral cW_USEDEFAULT)
        400 300
        hwnd    -- Parent_Window
        nullPtr -- Menu
        False   -- window_rendering_disabled
        False   -- transparent_painting
        nullPtr -- Window?

    client <- initialize_client_handler
    cefUrl <- mkCefStringPtr "http://www.google.com"
    browserSettings <- mkBrowserSettings

    putStrLn "cef_browser_host_create_browser"
    void $ c'cef_browser_host_create_browser
        windowInfo client cefUrl browserSettings nullPtr

