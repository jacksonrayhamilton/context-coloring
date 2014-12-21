'use strict';

function threes(array) {
    return array.reduce(function (soFar, number) {
        if (!soFar[soFar.length - 1] || soFar[soFar.length - 1].length === 3) {
            soFar.push([]);
        }
        soFar[soFar.length - 1].push(number);
        return soFar;
    }, []);
}

module.exports = threes;
