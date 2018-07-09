# nix-miso-template

This repository contains an example of how I structure applications
using `miso`, `nix` and `GHCJS`.

Since the code was developed during live during a workshop, it is
split into 3 standalone projects:

- `server-only` contains a project that contains only the `servant` backend.
- `server-client` adds a `miso` frontend to the backend.
- `final` enables server-side rendering.

The slides can be found in the `slides` subdirectory.
