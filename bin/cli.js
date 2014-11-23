// Reads a JavaScript file from stdin.

// Writes an array of `[level, start, end]' tokens to stdout.

'use strict';

var scopifier = require('../scopifier-microoptimized'),
    whole = '';

process.stdin.setEncoding('utf8');

process.stdin.on('readable', function () {
    var chunk = process.stdin.read();
    if (chunk !== null) {
        whole += chunk;
    }
});

process.stdin.on('end', function () {
    console.log(JSON.stringify(scopifier(whole)));
});
