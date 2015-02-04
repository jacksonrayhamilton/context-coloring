// Copyright (C) 2014-2015  Free Software Foundation, Inc.

// This file is part of GNU Emacs.

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

var escope = require('./libraries/escope'),
    esprima = require('./libraries/esprima');

// Given code, returns an array of tokens for context-coloring.
function scopifier(code) {

    var analyzedScopes,
        ast,
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

    // Strip BOM.
    code = code.replace(/^\ufeff/g, '');

    // Gracefully handle parse errors by doing nothing.
    try {
        ast = esprima.parse(code, {
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
            // We've only given the scope a level for posterity's sake.  We're
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
                    // Determine if a definition already exists for the range.
                    // (escope detects variables twice if they are declared and
                    // initialized simultaneously; this filters them.)
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

    return scopes.concat(tokens);
}

module.exports = scopifier;