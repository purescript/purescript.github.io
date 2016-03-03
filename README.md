purescript.github.io
====================

The Purescript project homepage.

## Development

You'll need the following installed to proceed:

* [npm](https://www.npmjs.org),
* [Compass](http://compass-style.org/install/),
* [stack](http://haskellstack.org/).

The site is generated using [Hakyll](https://jaspervdj.be/hakyll/). To
regenerate it, run the following:

```
$ grunt
$ stack build
$ stack exec site rebuild
```

The static site will be written to the `_site` directory.

TODO: work out how to get a 'watch' command working.
