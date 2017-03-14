module Main where

import Control.Monad (unless, void)
import Foreign.Ptr
import Foreign.Marshal hiding (void)

import Graphics.Win32
import System.Win32.DLL
import Bindings.CEF3

import Common

main :: IO ()
main = do
    hinst <- getModuleHandle Nothing
    mainArgs <- new $ C'cef_main_args_t hinst

    app <- initialize_app_handler
    exCode <- c'cef_execute_process mainArgs app nullPtr
    unless (exCode >= 0) $ do

        settings <- mkCefSettings
        putStrLn "cef_initialize"
        void $ c'cef_initialize mainArgs settings app nullPtr

        putStrLn "new windowInfo"
        -- windowInfo <- new $ C'cef_window_info_t nullPtr 0 0 nullPtr
        winName <- mkCefString "Test window"
        windowInfo <- new $ C'cef_window_info_t
            wS_EX_CLIENTEDGE
            winName
            wS_OVERLAPPEDWINDOW
            (fromIntegral cW_USEDEFAULT)
            (fromIntegral cW_USEDEFAULT)
            400 300
            nullPtr -- Parent_Window
            nullPtr -- Menu
            False   -- window_rendering_disabled
            False   -- transparent_painting
            nullPtr -- Window?

        cefUrl <- mkCefStringPtr "http://www.google.com"

        browserSettings <- mkBrowserSettings
        client <- initialize_client_handler

        putStrLn "cef_browser_host_create_browser"
        void $ c'cef_browser_host_create_browser
            windowInfo client cefUrl browserSettings nullPtr

        putStrLn "cef_run_message_loop"
        c'cef_run_message_loop
        putStrLn "cef_shutdown"
        c'cef_shutdown

