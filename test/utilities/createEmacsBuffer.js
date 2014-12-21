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

function createEmacsBuffer() {
    var points = [];

    function noPointInRegion(start, end, fn) {
        var i,
            length,
            index;
        for (i = 0, length = end - start; i < length; i += 1) {
            index = i + start - 1;
            if (fn(points[index], index)) {
                return false;
            }
        }
        return true;
    }

    function setLevelForRegion(start, end, level) {
        noPointInRegion(start, end, function (point, index) {
            /*jslint unparam: true */
            points[index] = level;
        });
    }

    function isLevelForRegion(start, end, level) {
        return noPointInRegion(start, end, function (point) {
            return point !== level;
        });
    }

    function isLevelAtPoint(point, level) {
        return points[point - 1] === level;
    }

    function applyTokens(tokens) {
        threes(tokens).forEach(function (threesome) {
            setLevelForRegion.apply(null, threesome);
        });
    }

    return {
        setLevelForRegion: setLevelForRegion,
        isLevelForRegion: isLevelForRegion,
        isLevelAtPoint: isLevelAtPoint,
        applyTokens: applyTokens
    };
}

module.exports = createEmacsBuffer;
