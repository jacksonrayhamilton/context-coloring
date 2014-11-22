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
        var scopeDefinitions,
            variables,
            globalReferences;
        if (scope.level !== undefined) {
            // Having its level set implies it was already annotated.
            return;
        }
        if (scope.upper) {
            if (scope.upper.functionExpressionScope) {
                // Pretend function expression scope doesn't exist.
                scope.level = scope.upper.level;
                scope.variables = scope.upper.variables.concat(scope.variables);
            } else {
                scope.level = scope.upper.level + 1;
            }
        } else {
            // Base case.
            scope.level = 0;
        }
        if (scope.functionExpressionScope) {
            // We've only given the scope a level for posterity's sake. We're
            // done now.
            return;
        }
        scopes = scopes.concat([[
            scope.level,
            scope.block.range[0],
            scope.block.range[1]
        ]]);
        scopeDefinitions = [];
        variables = scope.variables.reduce(function (symbols, variable) {
            var definitions,
                references;
            definitions = variable.defs
                .map(function (definition) {
                    var range = definition.name.range;
                    return [
                        scope.level,
                        range[0],
                        range[1]
                    ];
                });
            references = variable.references
                .reduce(function (references, reference) {
                    var range = reference.identifier.range;
                    if (isDefined(definitions, range)) {
                        return references;
                    }
                    // Double array required to concat just the inner array.
                    return references.concat([[
                        scope.level,
                        range[0],
                        range[1]
                    ]]);
                }, []);
            scopeDefinitions = scopeDefinitions.concat(definitions);
            return symbols.concat(definitions).concat(references);
        }, []);
        globalReferences = scope.references.reduce(function (references, reference) {
            var range = reference.identifier.range;
            if (reference.resolved || isDefined(scopeDefinitions, range)) {
                return references;
            }
            // Handle global references.
            return references.concat([[
                0,
                range[0],
                range[1]
            ]]);
        }, []);
        symbols = symbols.concat(variables).concat(globalReferences);
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
