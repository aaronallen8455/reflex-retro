# Reflex Retro

A web app for facilitating team retrospectives using the reflex framework.

start interactive development with `ob run`, then navigate to localhost:8000.
Currently this only works with Chrome.

Use `nix-shell shell.nix` to get into a shell from which you can run `cabal new-repl`.

### Deploying
On my mac, I need to use bash instead of fish: `bash --login`
Then go to the deployment directory and run `ob deploy update` and `ob deploy push`.

3/13/21 - tried updating the obelisk thunk but now deploy command fails, so
currently unable to update it.
