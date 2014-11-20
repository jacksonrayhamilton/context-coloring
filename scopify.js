/*jslint node: true */

'use strict';

var UglifyJS = require('uglify-js'),
    whole = '';

process.stdin.setEncoding('utf8');

process.stdin.on('readable', function () {
    var chunk = process.stdin.read();
    if (chunk !== null) {
        whole += chunk;
    }
});

process.stdin.on('end', function () {
    var scopes = [],
        symbols = [],
        toplevel = UglifyJS.parse(whole),
        walker = new UglifyJS.TreeWalker(function (node) {
            if (node instanceof UglifyJS.AST_Scope) {
                if (node.level === undefined) {
                    node.level = node.parent_scope ? node.parent_scope.level + 1 : 0;
                    scopes.push([node.level,
                                 node.start.pos + 1,
                                 node.end.endpos + 1]);
                }
            } else if (node instanceof UglifyJS.AST_Symbol) {
                // We don't care about symbols without definitions.
                if (node.thedef === undefined) {
                    return;
                }
                symbols.push([node.thedef.scope.level,
                              node.start.pos + 1,
                              node.end.endpos + 1]);
            }
        });
    toplevel.figure_out_scope();
    toplevel.walk(walker);
    console.log(JSON.stringify(scopes.concat(symbols)));
});
