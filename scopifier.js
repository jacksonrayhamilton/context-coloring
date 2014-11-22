/*jslint node: true */

'use strict';

var escope = require('escope'),
    esprima = require('esprima'),

    // Given an array of definitions, determines if a definition already exists
    // for a given range. (escope detects variables twice if they are declared
    // and initialized simultaneously; this filters them.)
    isDefined = function (definitions, range) {
        return definitions.some(function (definition) {
            // Check for identical definitions.
            return definition[1] === range[0] &&
                definition[2] === range[1];
        });
    };

// Given code, returns an array of `[level, start, end]' tokens for
// context-coloring.
module.exports = function (code) {
    var ast,
        analyzedScopes,
        scopes = [],
        symbols = [],
        comments,
        emacsified;

    // Gracefully handle parse errors by doing nothing.
    try {
        ast = esprima.parse(code, {
            comment: true,
            range: true
        });
        analyzedScopes = escope.analyze(ast).scopes;
    } catch (error) {
        process.exit(1);
    }

    analyzedScopes.forEach(function (scope) {
        var scopeDefinitions;
        if (scope.level === undefined) {
            if (scope.upper) {
                if (scope.upper.functionExpressionScope) {
                    // Pretend function expression scope doesn't exist.
                    scope.level = scope.upper.level;
                    scope.variables = scope.upper.variables.concat(scope.variables);
                } else {
                    scope.level = scope.upper.level + 1;
                }
            } else {
                scope.level = 0;
            }
            if (scope.functionExpressionScope) {
                // We've only given the scope a level for posterity's sake.
                return;
            }
            scopes.push([
                scope.level,
                scope.block.range[0],
                scope.block.range[1]
            ]);
            scopeDefinitions = [];
            scope.variables.forEach(function (variable) {
                var definitions = [],
                    references = [];
                variable.defs.forEach(function (definition) {
                    var range = definition.name.range;
                    definitions.push([
                        scope.level,
                        range[0],
                        range[1]
                    ]);
                });
                variable.references.forEach(function (reference) {
                    var range = reference.identifier.range;
                    if (isDefined(definitions, range)) {
                        return;
                    }
                    references.push([
                        scope.level,
                        range[0],
                        range[1]
                    ]);
                });
                Array.prototype.push.apply(scopeDefinitions, definitions);
                Array.prototype.push.apply(symbols, definitions);
                Array.prototype.push.apply(symbols, references);
            });
            scope.references.forEach(function (reference) {
                var range = reference.identifier.range;
                if (reference.resolved || isDefined(scopeDefinitions, range)) {
                    return;
                }
                // Handle global references.
                symbols.push([
                    0,
                    range[0],
                    range[1]
                ]);
            });
        }
    });

    comments = ast.comments
        .map(function (comment) {
            var range = comment.range;
            return [
                -1,
                range[0],
                range[1]
            ];
        });

    emacsified = scopes
        .concat(symbols)
        .concat(comments)
        .map(function (token) {
            // Emacs starts counting from 1.
            return [
                token[0],
                token[1] + 1,
                token[2] + 1
            ];
        });

    return emacsified;
};
