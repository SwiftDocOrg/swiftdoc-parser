#!/usr/bin/env node

// includes
var fs = require('fs'),
    parser = require('./lib/swiftdoc-parser'),
    builder = require('./lib/swiftdoc-builder'),
    execsyncs = require("execsyncs"),
    path = require('path');

var appRoot = path.join(__dirname, './');
    
// command-line options
var yargs = require('yargs')
        .usage('Usage: $0 [options] [file names...]')
        .help('help')
        .alias('?', 'help')
        .alias('h', 'help')
        .options('json-only', {
            type: 'boolean',
            describe: 'Output parsed headers as JSON only',
        })
        .options('skip-graphs', {
            type: 'boolean',
            describe: 'Suppress graph generation',
        })
        .options('config', {
            type: 'string',
            describe: 'Location of a configuration file',
        })
        .options('output-dir', {
            type: 'string',
            describe: 'Output directory',
            default : './output',
        })
        .options('url-prefix', {
            type: 'string',
            describe: 'URL prefix for generated links',
            default : '/',
        }),
    argv = yargs.argv;

// read config file if one was given
var config = { };
if (argv['config']) {
    config = JSON.parse(fs.readFileSync(argv.config));
}

// find list of source files either in config file or as command line params
var sources = config['source-files'] || argv._ || [];
if (sources.length == 0) {
    console.log('No input files given!\nPlease list input files in a configuration file or on the command line.\n');
    console.log(yargs.help());
    return;
}

// toggle Swift 3 API parsing
if (config['swift-api'] == '3.0') {
    parser.useSwift3APIs();
}

// parse each source file, building up a parsed data object
var parsedData = sources.reduce(function(previous, current) {
    var data = fs.readFileSync(appRoot + current, 'utf8');
    return parser.parse(data, previous);
}, null);

// finalize the parsing; this hoists nested types up to the top level and builds reverse-inheritance
parsedData = parser.finalize(parsedData);

// output JSON blob and stop if that's the request
if (argv['json-only']) {
    console.log(JSON.stringify(parsedData, null, 4));
    return;
}

// set output-related configuration items
builder.setOutputDir(config['output-dir'] || argv['output-dir']);
builder.setURLPrefix(config['url-prefix'] || argv['url-prefix']);
if (argv['skip-graphs']) {
    builder.setOutputGraphs(false);
}

// build output and notify
builder.build(parsedData)
execsyncs('osascript -e \'display notification "Finished build!" with title "swiftdoc-parser"\'');

