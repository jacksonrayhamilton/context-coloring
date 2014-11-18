'use strict';

var JSLINT = require('./jslint'),
    util = require('util'),

    // Accumulated input.
    whole = '';

process.stdin.setEncoding('utf8');

process.stdin.on('readable', function () {
    var chunk = process.stdin.read();
    if (chunk !== null) {
        whole += chunk;
    }
});

process.stdin.on('end', function () {
    var tokens;

    // Generate a syntax tree for the input.
    JSLINT(whole);

    // Minimize an otherwise-circular structure.
    tokens = JSLINT.data().tokens.map(function (token) {
        return {
            from: token.from,
            level: token.function.level,
            line: token.line,
            thru: token.thru
        };
    });

    console.log(JSON.stringify(tokens));
});
