{s}: rec

{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:kriti-lang' --allow-eval --warnings";
  allScripts = [ghcidScript];
}
