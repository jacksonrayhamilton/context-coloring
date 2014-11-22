'use strict';

var escope = require('escope'),
    esprima = require('esprima'),

    // Given an array of definitions, determines if a definition already exists
    // for a given range. (escope detects variables twice if they are declared
    // and initialized simultaneously; this filters them.)
    isDefined = function (definitions, range) {
        var i, definition;
        for (i = 0; i < definitions.length; i += 1) {
            definition = definitions[i];
            if (definition[1] === range[0] && definition[2] === range[1]) {
                return true;
            }
        }
        return false;
    };

// Given code, returns an array of `[level, start, end]' tokens for
// context-coloring.
module.exports = function (code) {
    var ast,

        analyzedScopes,
        i,
        scope,
        range,

        j,
        k,
        variable,
        mappedDefinitions,
        definition,
        reference,

        definitions,
        references,

        scopes = [],
        symbols = [],

        comments,
        comment;

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

    for (i = 0; i < analyzedScopes.length; i += 1) {
        scope = analyzedScopes[i];
        // Having its level set implies it was already annotated.
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
                // Base case.
                scope.level = 0;
            }
            // We've only given the scope a level for posterity's sake. We're
            // done now.
            if (!scope.functionExpressionScope) {
                range = scope.block.range;
                scopes.push([
                    scope.level,
                    range[0] + 1,
                    range[1] + 1
                ]);
                definitions = [];
                for (j = 0; j < scope.variables.length; j += 1) {
                    variable = scope.variables[j];
                    mappedDefinitions = [];
                    for (k = 0; k < variable.defs.length; k += 1) {
                        definition = variable.defs[k];
                        range = definition.name.range;
                        mappedDefinitions.push([
                            scope.level,
                            range[0] + 1,
                            range[1] + 1
                        ]);
                    }
                    Array.prototype.push.apply(definitions, mappedDefinitions);
                }
                references = [];
                for (j = 0; j < scope.references.length; j += 1) {
                    reference = scope.references[j];
                    range = reference.identifier.range;
                    if (!isDefined(definitions, range)) {
                        references.push([
                            // Handle global references too.
                            reference.resolved ? reference.resolved.scope.level : 0,
                            range[0] + 1,
                            range[1] + 1
                        ]);
                    }
                }
                Array.prototype.push.apply(symbols, definitions);
                Array.prototype.push.apply(symbols, references);
            }
        }
    }

    comments = [];
    for (i = 0; i < ast.comments.length; i += 1) {
        comment = ast.comments[i];
        range = comment.range;
        comments.push([
            -1,
            range[0] + 1,
            range[1] + 1
        ]);
    }

    return scopes.concat(symbols).concat(comments);
};
