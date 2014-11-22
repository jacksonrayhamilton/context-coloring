'use strict';

var fs = require('fs'),
    path = require('path'),
    scopifier = require('../scopifier'),
    scopifierMicrooptimized = require('../scopifier-microoptimized'),

    jqueryPath = path.join(__dirname, 'fixtures', 'jquery-2.1.1.js'),
    lodashPath = path.join(__dirname, 'fixtures', 'lodash-2.4.1.js'),
    asyncPath = path.join(__dirname, 'fixtures', 'async-0.9.0.js'),
    mkdirpPath = path.join(__dirname, 'fixtures', 'mkdirp-0.5.0.js');

suite('scopifier', function () {

    var jquery, lodash, async, mkdirp;

    before(function (next) {
        fs.readFile(jqueryPath, 'utf8', function (error, contents) {
            jquery = contents;
            next(error);
        });
    });
    before(function (next) {
        fs.readFile(lodashPath, 'utf8', function (error, contents) {
            lodash = contents;
            next(error);
        });
    });
    before(function (next) {
        fs.readFile(asyncPath, 'utf8', function (error, contents) {
            async = contents;
            next(error);
        });
    });
    before(function (next) {
        fs.readFile(mkdirpPath, 'utf8', function (error, contents) {
            mkdirp = contents;
            next(error);
        });
    });

    [scopifier, scopifierMicrooptimized].forEach(function (scopifier, index) {
        var message = '';
        if (!scopifier) {
            return;
        }
        if (index === 1) {
            message = ' (microoptimized)';
        }
        bench('jquery' + message, function () {
            scopifier(jquery);
        });
        bench('lodash' + message, function () {
            scopifier(lodash);
        });
        bench('async' + message, function () {
            scopifier(async);
        });
        bench('mkdirp' + message, function () {
            scopifier(mkdirp);
        });
    });

});
