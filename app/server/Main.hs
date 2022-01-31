
import Data.Proxy
import Network.Wai.Handler.Warp
import Servant.Server

import Web.Server
import Web.API
import Backend.Commands

main = do
  run 8081 do
    serve (Proxy :: Proxy API) do
      makeServer \token -> reportErrors . runVaultIO' token . runCommand
