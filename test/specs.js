'use strict';

var assert = require('assert'),
    fs = require('fs'),
    path = require('path'),

    scopifier = require('../scopifier'),
    scopifierMicrooptimized = require('../scopifier-microoptimized'),

    inputPath = path.join(__dirname, 'fixtures', 'vow.js'),
    outputPath = path.join(__dirname, 'fixtures', 'vow.json');

describe('scopifier', function () {

    var input, output;

    before(function (done) {
        fs.readFile(inputPath, 'utf8', function (error, contents) {
            if (error) {
                done(error);
                return;
            }
            input = contents;
            done();
        });
    });

    before(function (done) {
        fs.readFile(outputPath, 'utf8', function (readError, contents) {
            if (readError) {
                done(readError);
                return;
            }
            try {
                output = JSON.parse(contents);
            } catch (parseError) {
                done(parseError);
            }
            done();
        });
    });

    [scopifier, scopifierMicrooptimized].forEach(function (scopifier, index) {
        var message = '';
        if (index === 1) {
            message = ' (microoptimized)';
        }
        it('should work' + message, function () {
            assert.deepEqual(scopifier(input), output);
        });
    });

});
