(function () {

    'use strict';

    function MONAD() {
        return function unit(value) {
            var monad = Object.create(null);
            monad.bind = function (func) {
                return func(value);
            };
            return monad;
        };
    }

    return MONAD;

}());
