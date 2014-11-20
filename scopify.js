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
                                 node.start.pos,
                                 node.end.endpos]);
                }
            } else if (node instanceof UglifyJS.AST_Symbol) {
                symbols.push([node.thedef.scope.level,
                              node.start.pos,
                              node.end.endpos]);
            }
        });
    toplevel.figure_out_scope();
    toplevel.walk(walker);
    console.log('scopes', scopes);
    console.log('symbols', symbols);
    // TODO: Flatten a monad.
    // scopes [ [ 0, 0, 206 ], [ 1, 0, 206 ], [ 2, 30, 203 ], [ 3, 115, 174 ] ]
    // symbols [ [ 0, 9, 14 ],
    //   [ 2, 39, 43 ],
    //   [ 2, 44, 49 ],
    //   [ 2, 65, 70 ],
    //   [ 0, 73, 79 ],
    //   [ 2, 102, 107 ],
    //   [ 3, 125, 129 ],
    //   [ 3, 152, 156 ],
    //   [ 2, 157, 162 ],
    //   [ 2, 191, 196 ] ]
});
