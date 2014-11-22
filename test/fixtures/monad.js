/* A monad. */
function MONAD() {
    abc = 3;
    return function unit(value) {
        // Some details.
        var monad = Object.create(null);
        monad.bind = function (func) {
            return func(value);
        };
        return monad;
    };
}
