#!/usr/bin/env node

// Copyright (C) 2014-2015  Free Software Foundation, Inc.

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

'use strict';

var padRight = function (value, padding) {
    return value + new Array(Math.max(0, padding - String(value).length) + 1).join(' ');
};

var formatSourceFile = function (sourceFile) {
    var sourceLines = sourceFile.source.split('\n');
    var results = [
        padRight('Hits', 5) + ' | Source',
        new Array(80 + 1).join('-')
    ];
    var linesHit = 0;
    var linesHittable = 0;
    results = results.concat(sourceFile.coverage.map(function (hits, index) {
        var hitsValue = hits === null ? 'N/A' : hits;
        var column = hits === 0 ? '~' : '|';
        if (hits > 0) {
            linesHit += 1;
        }
        if (hits !== null) {
            linesHittable += 1;
        }
        return padRight(hitsValue, 5) + ' ' + column + ' ' + sourceLines[index];
    }));
    results = results.concat([
        '',
        'Lines: ' + linesHit + ' / ' + linesHittable,
        'Coverage: ' + (Math.round(linesHit / linesHittable * 10000) / 100) + '%'
    ]);
    return results.join('\n');
};

var format = function (json) {
    return json.source_files.map(formatSourceFile).join('\n');
};

var read = function () {
    var whole = '';

    process.stdin.setEncoding('utf8');

    process.stdin.on('readable', function () {
        var chunk = process.stdin.read();
        if (chunk !== null) {
            whole += chunk;
        }
    });

    process.stdin.on('end', function () {
        console.log(format(JSON.parse(whole)));
    });
};

read();
