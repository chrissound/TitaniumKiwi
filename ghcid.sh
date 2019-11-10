if which nix; then
    ghcid --command="cabal v2-repl titaniumkiwi" --test=Main.main
    exit 0
fi
if which stack; then

    ghcid '--command=stack ghci --profile' --test='main'
    # ghcid --command "stack ghci app --ghci-options='-fdiagnostics-color=always'" --test=main
    exit 0
fi
