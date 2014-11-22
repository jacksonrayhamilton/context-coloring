// vow.js
// Douglas Crockford
// 2013-09-20

// Public Domain

/*global setImmediate */


var VOW = (function () {
    'use strict';

// The VOW object contains a .make function that is used to make vows.
// It may also contain other useful functions.
// In some mythologies, 'VOW' is called 'deferrer'.

    function enlighten(queue, fate) {

// enlighten is a helper function of herald and .when. It schedules the
// processing of all of the resolution functions in either the keepers queue
// or the breakers queue in later turns with the promise's fate.

        queue.forEach(function (func) {
            setImmediate(func, fate);
        });
    }

    return {
        make: function make() {

// The make function makes new vows. A vow contains a promise object and the
// two resolution functions (break and keep) that determine the fate of the
// promise.

            var breakers = [],          // .when's broken queue
                fate,                   // The promise's ultimate value
                keepers = [],           // .when's kept queue
                status = 'pending';     // 'broken', 'kept', or 'pending'

            function enqueue(
                resolution, // 'keep' or 'break'
                func,       // A function that was registered with .when
                vow         // A vow that provides the resolution functions
            ) {

// enqueue is a helper function used by .when. It will append a function to
// either the keepers queue or the breakers queue.

                var queue = resolution === 'keep' ? keepers : breakers;
                queue[queue.length] = typeof func !== 'function'

// If func is not a function, push the resolver so that the value passes to
// the next cascaded .when.

                    ? vow[resolution]

// If the func is a function, push a function that calls func with a value.
// The result can be a promise, or not a promise, or an exception.

                    : function (value) {
                        try {
                            var result = func(value);

// If the result is a promise, then register our resolver with that promise.

                            if (result && result.is_promise === true) {
                                result.when(vow.keep, vow.break);

// But if it is not a promise, then use the result to resolve our promise.

                            } else {
                                vow.keep(result);
                            }

// But if func throws an exception, then break our promise.

                        } catch (e) {
                            vow.break(e);
                        }
                    };
            }

            function herald(state, value, queue) {

// The herald function is a helper function of break and keep.
// It seals the promise's fate, updates its status, enlightens
// one of the queues, and empties both queues.

                if (status !== 'pending') {
                    throw "overpromise";
                }
                fate = value;
                status = state;
                enlighten(queue, fate);
                keepers.length = 0;
                breakers.length = 0;
            }

// Construct and return the vow object.

            return {
                'break': function (value) {

// The break method breaks the promise.

                    herald('broken', value, breakers);
                },
                keep: function keep(value) {

// The keep method keeps the promise.

                    herald('kept', value, keepers);
                },
                promise: {

// The promise is an object with a .when method.

                    is_promise: true,

// The .when method is the promise monad's bind. The .when method can take two
// optional functions. One of those functions may be called, depending on the
// promise's resolution. Both could be called if the the kept function throws.

                    when: function (kept, broken) {

// Make a new vow. Return the new promise.

                        var vow = make();
                        switch (status) {

// If this promise is still pending, then enqueue both kept and broken.

                        case 'pending':
                            enqueue('keep',  kept,   vow);
                            enqueue('break', broken, vow);
                            break;

// If the promise has already been kept, then enqueue only the kept function,
// and enlighten it.

                        case 'kept':
                            enqueue('keep', kept, vow);
                            enlighten(keepers, fate);
                            break;

// If the promise has already been broken, then enqueue only the broken
// function, and enlighten it.

                        case 'broken':
                            enqueue('break', broken, vow);
                            enlighten(breakers, fate);
                            break;
                        }
                        return vow.promise;
                    }
                }
            };
        },
        every: function every(array) {

// The every function takes an array of promises and returns a promise that
// will deliver an array of results only if every promise is kept.

            var remaining = array.length, results = [], vow = VOW.make();

            if (!remaining) {
                vow.break(array);
            } else {
                array.forEach(function (promise, i) {
                    promise.when(function (value) {
                        results[i] = value;
                        remaining -= 1;
                        if (remaining === 0) {
                            vow.keep(results);
                        }
                    }, function (reason) {
                        remaining = NaN;
                        vow.break(reason);
                    });
                });
            }
            return vow.promise;
        },
        first: function first(array) {

// The first function takes an array of promises and returns a promise to
// deliver the first observed kept promise, or a broken promise if all of
// the promises are broken.

            var found = false, remaining = array.length, vow = VOW.make();

            function check() {
                remaining -= 1;
                if (remaining === 0 && !found) {
                    vow.break();
                }
            }

            if (remaining === 0) {
                vow.break(array);
            } else {
                array.forEach(function (promise) {
                    promise.when(function (value) {
                        if (!found) {
                            found = true;
                            vow.keep(value);
                        }
                        check();
                    }, check);
                });
            }
            return vow.promise;
        },
        any: function any(array) {

// The any function takes an array of promises and returns a promise that
// will deliver a possibly sparse array of results of any kept promises.
// The result will contain an undefined element for each broken promise.

            var remaining = array.length, results = [], vow = VOW.make();

            function check() {
                remaining -= 1;
                if (remaining === 0) {
                    vow.keep(results);
                }
            }

            if (!remaining) {
                vow.keep(results);
            } else {
                array.forEach(function (promise, i) {
                    promise.when(function (value) {
                        results[i] = value;
                        check();
                    }, check);
                });
            }
            return vow.promise;
        },
        kept: function (value) {

// Returns a new kept promise.

            var vow = VOW.make();
            vow.keep(value);
            return vow.promise;
        },
        broken: function (reason) {

// Returns a new broken promise.

            var vow = VOW.make();
            vow.break(reason);
            return vow.promise;
        }
    };
}());


// If your system does not have setImmediate, then simulate it with setTimeout.

if (typeof setImmediate !== 'function') {
    setImmediate = function setImmediate(func, param) {
        'use strict';
        return setTimeout(function () {
            func(param);
        }, 0);
    };
}
