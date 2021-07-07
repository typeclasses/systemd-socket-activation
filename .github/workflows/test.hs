import System.Environment
import System.Process

main =
  do
    ghc <- getEnv "ghc"
    callProcess "cabal" ("build" : "all" : constraints ghc)

x .= y =
    "--constraint=" ++ x ++ "==" ++ y

constraints ghc = case ghc of
    "8.10" ->
        [ "base"       .= "4.14.*"
        , "containers" .= "0.6.0.*"
        , "network"    .= "3.1.0.*"
        , "relude"     .= "1.0.0.*"
        , "text"       .= "1.2.3.*"
        , "unix"       .= "2.7.2.*"
        ]
    "9.0" ->
        [ "base"       .= "4.15.*"
        , "containers" .= "0.6.5.*"
        , "network"    .= "3.1.2.*"
        , "text"       .= "1.2.4.*"
        ]
