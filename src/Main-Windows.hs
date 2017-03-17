module Main where

import Control.Monad (unless, void)
import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Data.Bits ((.|.))

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

        startBrowserWindow

        putStrLn "cef_run_message_loop"
        c'cef_run_message_loop
        putStrLn "cef_shutdown"
        c'cef_shutdown

startBrowserWindow :: IO ()
startBrowserWindow = do
    putStrLn "new windowInfo"
    winName <- mkCefString "Test window"
    windowInfo <- new $ C'cef_window_info_t
        wS_EX_CLIENTEDGE
        winName
        (wS_OVERLAPPEDWINDOW .|. wS_CLIPCHILDREN .|. wS_CLIPSIBLINGS
          .|. wS_VISIBLE)
        (fromIntegral wS_USEDEFAULT)
        (fromIntegral wS_USEDEFAULT)
        (fromIntegral wS_USEDEFAULT)
        (fromIntegral wS_USEDEFAULT)
        nullPtr -- Parent_Window
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

