// The MONAD function is a macroid that produces monad constructor functions.
// It can take an optional modifier function, which is a function that is
// allowed to modify new monads at the end of the construction processes.

// A monad constructor (sometimes called 'unit' or 'return' in some mythologies)
// comes with three methods, lift, lift_value, and method, all of which can add
// methods and properties to the monad's prototype.

// A monad has a 'bind' method that takes a function that receives a value and
// is usually expected to return a monad.

function MONAD(modifier) {
    'use strict';
    var prototype = Object.create(null);
    prototype.is_monad = true;
    function unit(value) {
        var monad = Object.create(prototype);
        monad.bind = function (func, args) {
            return func.apply(
                undefined,
                [value].concat(Array.prototype.slice.apply(args || []))
            );
        };
        if (typeof modifier === 'function') {
            value = modifier(monad, value);
        }
        return monad;
    }
    unit.method = function (name, func) {
        prototype[name] = func;
        return unit;
    };
    unit.lift_value = function (name, func) {
        prototype[name] = function () {
            return this.bind(func, arguments);
        };
        return unit;
    };
    unit.lift = function (name, func) {
        prototype[name] = function () {
            var result = this.bind(func, arguments);
            return result && result.is_monad === true ? result : unit(result);
        };
        return unit;
    };
    return unit;
}
