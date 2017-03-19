module Common where

import Control.Monad (void)
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal hiding (void)
import Foreign.Storable
import Bindings.CEF3

mkCefString :: String -> IO C'cef_string_utf16_t
mkCefString str = mkCefStringPtr str >>= peek

-- Build a CEF utf16 string in memory from a Haskell string.
mkCefStringPtr :: String -> IO (Ptr C'cef_string_utf16_t)
mkCefStringPtr str = do
  let sz = fromIntegral . length $ str
  strC <- newCString str

  -- We need to allocate and *initialize* a space for the utf16 string.
  -- CEF will try to call its destructor if it has one, so failure to
  -- initialize results in segfault.
  pUtf16 <- new $ C'cef_string_utf16_t nullPtr 0 nullFunPtr
  void $ c'cef_string_utf8_to_utf16 strC sz pUtf16
  return pUtf16

rtNull1 :: String -> a -> IO (Ptr b)
rtNull1 s _ = putStrLn s >> return nullPtr

rtVoid2 :: String -> a -> b ->  IO ()
rtVoid2 s _ _ = putStrLn s >> return ()

rtVoid3 :: String -> a -> b -> c -> IO ()
rtVoid3 s _ _ _ = putStrLn s >> return ()

rtInt1 :: String -> a -> IO CInt
rtInt1 s _ = putStrLn s >> return 1

rtInt2 :: String -> a -> b -> IO CInt
rtInt2 s _ _ = putStrLn s >> return 1

initialize_cef_base :: IO C'cef_base_t
initialize_cef_base = do
  putStrLn "initialize_cef_base"
  let sz = 0
  C'cef_base_t
    <$> return sz
    <*> mk'cb_cef_base_add_ref    (rtInt1 "cef_base_t.add_ref")
    <*> mk'cb_cef_base_release    (rtInt1 "cef_base_t.release")
    <*> mk'cb_cef_base_get_refct  (rtInt1 "cef_base_t.get_refct")

initialize_app_handler :: IO (Ptr C'cef_app_t)
initialize_app_handler = do
  putStrLn "initialize_app_handler"
  newWithSize (C'cef_app_t
    <$> initialize_cef_base
    <*> mk'cb_cef_app_on_before_command_line_processing
        (rtVoid3 "on_before_command_line_processing")
    <*> mk'cb_cef_app_on_register_custom_schemes
        (rtVoid2 "on_register_custom_schemes")
    <*> mk'cb_cef_app_get_resource_bundle_handler
        (rtNull1 "get_resource_bundle_handler")
    <*> mk'cb_cef_app_get_browser_process_handler
        (rtNull1 "get_browser_process_handler")
    <*> mk'cb_cef_app_get_render_process_handler
        (rtNull1 "get_render_process_handler")
    )

-- Allocate space for a new object, build the object, copy the object to
-- the space, then store the size of the object.  In CEF, size is always at
-- byte offset 0.
newWithSize :: Storable a => IO a -> IO (Ptr a)
newWithSize mkObj = do
  obj <- mkObj
  let sz = sizeOf obj
  putStrLn $ "Size: "++ show sz
  ptr <- new obj
  pokeByteOff ptr 0 sz
  return ptr

-- Settings for configuring CEF process behaviors
mkCefSettings :: IO (Ptr C'cef_settings_t)
mkCefSettings = newWithSize
  (C'cef_settings_t
  <$> return 0 -- dummy size, newWithSize will populate
  <*> return 0 -- single process
  <*> return 1 -- no_sandbox
  <*> mkCefString ""
  <*> return 0 -- multithreaded message loop
  <*> return 0 -- command line args disabled
  <*> mkCefString ""
  <*> return 0 -- persist session cookies
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""-- locale
  <*> mkCefString ""--log file
  <*> return c'LOGSEVERITY_DEFAULT
  <*> return 0 -- release dcheck enabled
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""-- locales dir path
  <*> return 0 -- pack loading disabled
  <*> return 0 -- remote debugging port
  <*> return 5 -- uncaught exception stack size
  <*> return 0
  <*> return 0 -- ignore certificate errors
  <*> return 0 -- background color
  )

-- Settings for configuring the browser behavior
mkBrowserSettings :: IO (Ptr C'cef_browser_settings_t)
mkBrowserSettings = newWithSize
  (C'cef_browser_settings_t
  <$> return 0  -- dummy size, newWithSize will populate
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""
  <*> return 14 -- default font size
  <*> return 10
  <*> return 10
  <*> return 10
  <*> mkCefString ""-- default encoding
  <*> return c'STATE_DEFAULT -- remove_fonts
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return 0 -- background color
  )

-- Callbacks for events within the browser.
initialize_client_handler :: IO (Ptr C'cef_client_t)
initialize_client_handler = do
  putStrLn "initialize_client_handler"
  newWithSize
    (C'cef_client_t
    <$> initialize_cef_base
    <*> mk'cb_cef_client_get_context_menu_handler
        (rtNull1 "get_context_menu_handler")
    <*> mk'cb_cef_client_get_dialog_handler
        (rtNull1 "get_dialog_handler")
    <*> mk'cb_cef_client_get_display_handler
        (rtNull1 "get_display_handler")
    <*> mk'cb_cef_client_get_download_handler
        (rtNull1 "get_download_handler")
    <*> mk'cb_cef_client_get_drag_handler
        (rtNull1 "get_drag_handler")
    <*> mk'cb_cef_client_get_focus_handler
        (rtNull1 "get_focus_handler")
    <*> mk'cb_cef_client_get_geolocation_handler
        (rtNull1 "get_geolocation_handler")
    <*> mk'cb_cef_client_get_jsdialog_handler
        (rtNull1 "get_jsdialog_handler")
    <*> mk'cb_cef_client_get_keyboard_handler
        (rtNull1 "get_keyboard_handler")
    <*> mk'cb_cef_client_get_life_span_handler
        initialize_life_span_handler
        --(rtNull1 "get_life_span_handler")
    <*> mk'cb_cef_client_get_load_handler
        (rtNull1 "get_load_handler")
    <*> mk'cb_cef_client_get_render_handler
        (rtNull1 "get_render_handler")
    <*> mk'cb_cef_client_get_request_handler
        (rtNull1 "get_request_handler")
    <*> mk'cb_cef_client_on_process_message_received
        (\_ _ _ _ -> putStrLn "on_message_process_received" >> return 0)
    )

initialize_life_span_handler :: Ptr C'cef_client_t -> IO (Ptr C'cef_life_span_handler_t)
initialize_life_span_handler pClient = do
  putStrLn "initialize_life_span_handler"
  newWithSize
    (C'cef_life_span_handler_t
    <$> initialize_cef_base
    <*> mk'cb_cef_life_span_handler_on_before_popup
        (\_ _ _ _ _ _ _ _ _ _ -> putStrLn "on_before_popup" >> return 0)
    <*> mk'cb_cef_life_span_handler_on_after_created
        (rtVoid2 "on_after_created")
    <*> mk'cb_cef_life_span_handler_run_modal
        (rtInt2 "run_modal")
    <*> mk'cb_cef_life_span_handler_do_close
        (\_ _ -> do
	    putStrLn "do_close"
	    c'cef_quit_message_loop -- close global cef process
	    return 0)               -- allow browser close
    <*> mk'cb_cef_life_span_handler_on_before_close
        (rtVoid2 "on_before_close")
    )
