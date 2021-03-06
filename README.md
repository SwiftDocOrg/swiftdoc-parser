# swiftdoc-parser

> This project is no longer maintained. 
> We're now using [swift-doc](https://github.com/SwiftDocOrg/swift-doc) to generate the SwiftDoc.org website.

---

## Installation

These utilities are built on [Node.js](http://nodejs.org) and use [Graphviz](http://www.graphviz.org) for creating inheritance graphs. To install and run the utilities with these prerequisites:

1. Clone or download the project.
2. Install dependencies with `npm install`.
3. Use the parser to convert a Swift header to JSON with `./index.js --json-only <filename>`.

**Other options:**

```
$ ./index.js --help
Usage: node ./index.js [options] [file names...]

Options:
  --json-only   Output parsed headers as JSON only
  --config      Location of a configuration file  
  --output-dir  Output directory                    [default: "./output"]
  --url-prefix  URL prefix for generated links      [default: "/"]
```

## Contributions

Issues and pull requests should be filed in this repository for problems relating to:

- incorrect or missing declarations
- truncated or missing comments
- errors in hierarchy graphs

For errors in the SwiftDoc.org site itself, see [the site's repository](http://github.com/SwiftDocOrg/SwiftDoc.org) instead.

## Contact

Follow [@SwiftDocOrg](http://twitter.com/SwiftDocOrg) on Twitter.


## License

All code is available under the MIT License. Autogenerated documentation is © 2015 Apple, Inc. All other content is released under the [Creative Commons BY-NC License](http://creativecommons.org/licenses/by-nc/4.0/).
