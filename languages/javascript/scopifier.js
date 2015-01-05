'use strict';

var escope = require('./lib/escope'),
    esprima = require('./lib/esprima');

// Given code, returns an array of tokens for context-coloring.
function scopifier(code) {

    // Strip BOM.
    code = code.replace(/^\ufeff/g, '');

    var analyzedScopes,
        ast,
        comment,
        definition,
        definitionsCount,
        definitionsIndex,
        i,
        isDefined,
        j,
        k,
        pointer,
        range,
        reference,
        scope,
        scopes,
        tokens,
        variable;

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

    scopes = [];
    tokens = [];
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
                scopes.push(
                    range[0] + 1,
                    range[1] + 1,
                    scope.level
                );
                definitionsIndex = tokens.length;
                definitionsCount = 0;
                for (j = 0; j < scope.variables.length; j += 1) {
                    variable = scope.variables[j];
                    definitionsCount += variable.defs.length;
                    for (k = 0; k < variable.defs.length; k += 1) {
                        definition = variable.defs[k];
                        range = definition.name.range;
                        tokens.push(
                            range[0] + 1,
                            range[1] + 1,
                            scope.level
                        );
                    }
                }
                for (j = 0; j < scope.references.length; j += 1) {
                    reference = scope.references[j];
                    range = reference.identifier.range;
                    isDefined = false;
                    // Determine if a definition already exists for the
                    // range. (escope detects variables twice if they are
                    // declared and initialized simultaneously; this filters
                    // them.)
                    for (k = 0; k < definitionsCount; k += 1) {
                        pointer = definitionsIndex + (k * 3);
                        if (tokens[pointer] === range[0] + 1 &&
                                tokens[pointer + 1] === range[1] + 1) {
                            isDefined = true;
                            break;
                        }
                    }
                    if (!isDefined) {
                        tokens.push(
                            // Handle global references too.
                            range[0] + 1,
                            range[1] + 1,
                            reference.resolved ? reference.resolved.scope.level : 0
                        );
                    }
                }
            }
        }
    }

    for (i = 0; i < ast.comments.length; i += 1) {
        comment = ast.comments[i];
        range = comment.range;
        tokens.push(
            range[0] + 1,
            range[1] + 1,
            -1
        );
    }

    return scopes.concat(tokens);
}

module.exports = scopifier;
