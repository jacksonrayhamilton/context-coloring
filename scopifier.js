/*jslint node: true */

'use strict';

var escope = require('escope'),
    esprima = require('esprima'),
    isDefined = function (definitions, range) {
        return definitions.some(function (definition) {
            // Check for identical definitions.
            return definition[1] === range[0] &&
                definition[2] === range[1];
        });
    },
    whole = '';

process.stdin.setEncoding('utf8');

process.stdin.on('readable', function () {
    var chunk = process.stdin.read();
    if (chunk !== null) {
        whole += chunk;
    }
});

process.stdin.on('end', function () {
    var ast,
        analyzedScopes,
        scopes = [],
        symbols = [],
        comments = [],
        continuous,
        emacsified;

    // Gracefully handle parse errors by doing nothing.
    try {
        ast = esprima.parse(whole, {
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

    ast.comments.forEach(function (comment) {
        var range = comment.range;
        comments.push([
            -1,
            range[0],
            range[1]
        ]);
    });

    continuous = symbols.concat(comments).sort(function (a, b) {
        return a[1] - b[1];
    });

    continuous = continuous.slice(1).reduce(function (soFar, token) {
        var previous = soFar[soFar.length - 1];
        // Detect same-color exact tail ends (nothing else is safe to join).
        if (previous[0] === token[0] &&
                previous[2] === token[1] - 1) {
            previous[2] = token[2];
            return soFar;
        }
        soFar.push(token);
        return soFar;
    }, continuous.slice(0, 1));

    emacsified = scopes.concat(continuous);

    emacsified.forEach(function (instruction) {
        // Emacs starts counting from 1.
        instruction[1] += 1;
        instruction[2] += 1;
    });

    console.log(JSON.stringify(emacsified));
});
