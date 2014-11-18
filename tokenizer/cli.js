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
    var data, tokens;

    // Generate a syntax tree for the input.
    JSLINT(whole);
    data = JSLINT.data();

    // Minimize an otherwise-circular structure.
    tokens = data.tokens.map(function (token) {
        var origin = token,
            level;

        // We always consider the function keyword to be "part" of the scope it
        // creates, even if the name leaks in the case of function statements.
        if (token.kind !== 'function') {
            // Find a variable/parameter's origin.
            while (origin.master) {
                origin = origin.master;
            }
        }

        // Globality is not indicated by origin function.
        if (token.kind !== 'function' &&
                data.global.indexOf(token.string) > -1) {
            level = 0;
        } else {
            level = origin.function.level;
        }

        return {
            from: token.from,
            level: level,
            line: token.line,
            thru: token.thru
        };
    });

    console.log(JSON.stringify(tokens));
});
