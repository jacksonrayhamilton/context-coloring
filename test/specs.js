/*jslint stupid: true */

'use strict';

var assert = require('assert');
var fs = require('fs');
var path = require('path');
var scopifier = require('../languages/javascript/scopifier');
var createEmacsBuffer = require('./createEmacsBuffer');
var threes = require('./threes');

describe('emacsBuffer', function () {

    it('should set levels', function () {
        var emacsBuffer = createEmacsBuffer(3);

        emacsBuffer.setLevelForRegion(1, 4, 0);
        assert(emacsBuffer.isLevelForRegion(1, 4, 0));

        emacsBuffer.setLevelForRegion(2, 3, 1);
        assert.strictEqual(emacsBuffer.isLevelForRegion(1, 4, 0), false);
        assert(emacsBuffer.isLevelAtPoint(1, 0));
        assert(emacsBuffer.isLevelAtPoint(2, 1));
        assert(emacsBuffer.isLevelAtPoint(3, 0));
    });

    it('should apply tokens', function () {
        var emacsBuffer = createEmacsBuffer(3);

        emacsBuffer.applyTokens([
            1, 4, 0,
            2, 3, 1
        ]);
        assert(emacsBuffer.isLevelAtPoint(1, 0));
        assert(emacsBuffer.isLevelAtPoint(2, 1));
        assert(emacsBuffer.isLevelAtPoint(3, 0));
    });

});

describe('scopifier', function () {

    it('should recognize scope levels', function () {
        var fixturePath = path.join(__dirname, 'fixtures/function-scopes.js');
        var input = fs.readFileSync(fixturePath, 'utf8');
        var output = scopifier(input);
        var emacsBuffer = createEmacsBuffer();

        emacsBuffer.applyTokens(output);

        assert(emacsBuffer.isLevelForRegion(1, 9, 0));
        assert(emacsBuffer.isLevelForRegion(9, 23, 1));
        assert(emacsBuffer.isLevelForRegion(23, 25, 0));
        assert(emacsBuffer.isLevelForRegion(25, 34, 1));
        assert(emacsBuffer.isLevelForRegion(34, 35, 0));
        assert(emacsBuffer.isLevelForRegion(35, 52, 1));
        assert(emacsBuffer.isLevelForRegion(52, 66, 2));
        assert(emacsBuffer.isLevelForRegion(66, 72, 1));
        assert(emacsBuffer.isLevelForRegion(72, 81, 2));
        assert(emacsBuffer.isLevelForRegion(81, 82, 1));
        assert(emacsBuffer.isLevelForRegion(82, 87, 2));
        assert(emacsBuffer.isLevelForRegion(87, 89, 1));
    });

});
