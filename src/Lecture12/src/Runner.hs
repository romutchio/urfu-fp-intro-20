module Runner where

import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import System.IO

import App
import Server

{-
  Данный модуль содержит главную функцию, которая запускает web-приложение.
  Она вызывается в `bin/Main.hs`. Здесь происходит настройка веб-сервера wai,
  которому мы передаем настройки, указав номер порта и простой логгер, который
  логгирует информацию в stdout.

  С помощью `appConfig` конфигурируется web-приложение, что позволяет нам
  расширять его новыми параметрами, задавая их с помощью CLI (считывать параметры командной строки).

  Само приложение создается с помощью `mkApplication` из модуля `Server`.
-}
run :: IO ()
run = withStdoutLogger $ \logger -> do
  let
    waiSettings =
      setPort 3000 $
      setBeforeMainLoop (hPutStrLn stderr "listening on port 3000") $
      setLogger logger defaultSettings
    appConfig = Config
      { dbPath = DatabasePath "bookmovie.db"
      }
  -- запускаем приложение с помощью wai
  runSettings waiSettings (mkApplication appConfig)
