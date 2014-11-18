'use strict';

var JSLINT = require('./jslint'),
    util = require('util'),

    // Accumulated input.
    whole = '',

    // Acquires the number of accumulated characters after the end of each line.
    getTotals = function (file) {
        var lines = file.split('\n'),
            total = 0,
            totals = {
                0: total
            },
            i = 0,
            length = lines.length;
        while (i < length) {
            total += lines[i].length + 1;
            totals[i + 1] = total;
            i += 1;
        }
        totals[i + 1] = total + 1;
        return totals;
    };

process.stdin.setEncoding('utf8');

process.stdin.on('readable', function () {
    var chunk = process.stdin.read();
    if (chunk !== null) {
        whole += chunk;
    }
});

process.stdin.on('end', function () {
    var data, totals, out, i, tokens, length, token, origin, level, total;

    // Generate a syntax tree for the input.
    JSLINT(whole);
    data = JSLINT.data();

    totals = getTotals(whole);

    // Minimize an otherwise-circular structure.
    out = [];
    i = 0;
    tokens = data.tokens;
    length = tokens.length;

    while (i < length) {
        token = tokens[i];

        origin = token;

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
                (token.identifier &&
                 data.global.indexOf(token.string) > -1)) {
            level = 0;
        } else {
            level = origin.function.level;
        }
        total = totals[token.line - 1];

        out.push({
            l: level,
            s: total + token.from,
            e: total + token.thru
        });

        i += 1;
    }

    console.log(JSON.stringify(out));
});
