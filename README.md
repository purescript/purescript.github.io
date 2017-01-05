purescript.github.io
====================

The PureScript project homepage.

## Development

You'll need the following installed to proceed:

* [npm](https://www.npmjs.org),
* [stack](http://haskellstack.org/).

The site is generated using [Hakyll](https://jaspervdj.be/hakyll/). To
regenerate it, change to the `_generator` directory and run the following:

```
$ npm install
$ npm run -s build
```

The updated static site will be written to the root directory of the
repository.

## Contributing

When contributing changes, please follow the above steps and ensure that both
your changes to the website source and the changes in the generated HTML are
included in your commits. This way, after your PR is merged, GitHub Pages will
automatically pick up the updates without any maintainers having to perform
additional manual steps.
