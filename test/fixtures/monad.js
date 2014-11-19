(function () {

    'use strict';

    // This is my monad.
    function MONAD() {
        // My monad returns things.
        return function unit(value) {
            var monad = Object.create(null);
            monad.bind = function (func) {
                return func(value);
            };
            monad.go = function () {

            };
            return monad;
        };
    }

    return MONAD;

}());
