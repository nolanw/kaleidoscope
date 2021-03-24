# Kaleidoscope

A browser-based [Take It Easy](http://www.burleygames.com/board-games/take-it-easy/) ([BoardGameGeek](https://boardgamegeek.com/boardgame/128/take-it-easy)) board and set of tiles. For playing remotely with people who are unable to share their board and tiles with you in person.

This is intentionally **not** a functional copy of the game. It is a board on which you can place tiles. No single player or multiplayer features exist.

## Development

Kaleidoscope is written in [Elm](https://elm-lang.org), so at a minimum you'll need to [install Elm](https://guide.elm-lang.org/install/elm.html).

Then, to build and run:

```sh
make kaleiscope.js && open index.html
```

### Fancier setup

If you're using Visual Studio Code, [a plugin is available](https://github.com/elm-tooling/elm-language-client-vscode). Note that installing the suggested `elm-format` and `elm-test` packages globally resulted in

```
Error: EACCES: permission denied, mkdir '/opt/local/lib/node_modules/elm-format/unpacked_bin'
```

which went away after adding `--unsafe-perm=true` to the `npm` invocation.

For automatic recompliation on browser refresh whenever relevant files change, install [tychus](https://github.com/devlocker/tychus) and then:

1. Spin up a web server: `python3 -m http.server 8000`
2. Have Tychus start its proxy and watching: `tychus "make" --wait --app-port=8000`
3. `open http://localhost:4000`

### Installation

To build an optimized, single-file version:

```sh
make build/index.html
```

Then simply upload `build/index.html` to your hosting of choice. If that happens to be Nolan's Fastmail account, and if you create a netrc file at `install-netrc`, run:

```sh
make install
```

to build and upload via WebDAV.

### Roadmap

- [x] Draw a hex grid resembling the Take It Easy board.
- [x] Draw tiles with numbers and lines.
- [x] Click a tile to select it.
- [x] Click a hex to place the selected tile.
- [x] Save board state to local storage so it survives a reload.
- [x] Upload it somewhere.
- [ ] Improve aesthetics.
- [x] Save board state in the URL so it can be linked to others.
- [ ] Select a tile by typing its numbers.
- [ ] Place the selected tile by typing a numbered unoccupied hex.
- [ ] Undo/redo.

## References

* [An image of set of player tiles as shipped in the box.](https://boardgamegeek.com/image/296408/take-it-easy)
* [An image of an empty board.](https://boardgamegeek.com/image/296409/take-it-easy)
* [elm-svg documentation.](https://package.elm-lang.org/packages/elm/svg/1.0.1/)
* MDN's [SVG references and guides](https://developer.mozilla.org/en-US/docs/Web/SVG).
* Red Blob Games's [Hexagonal Grids guide](https://www.redblobgames.com/grids/hexagons/) and [implementation notes](https://www.redblobgames.com/grids/hexagons/implementation.html).
