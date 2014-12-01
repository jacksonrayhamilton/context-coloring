/*global angular */

(function () {

    'use strict';

    function times(n, fn) {
        var i;
        for (i = 0; i < n; i += 1) {
            fn(i);
        }
    }

    function boundHue(hue) {
        return (360 + (hue % 360)) % 360;
    }

    function hue2rgb(p, q, t) {
        if (t < 0) {
            t += 1;
        }
        if (t > 1) {
            t -= 1;
        }
        if (t < 1 / 6) {
            return p + (q - p) * 6 * t;
        }
        if (t < 1 / 2) {
            return q;
        }
        if (t < 2 / 3) {
            return p + (q - p) * (2 / 3 - t) * 6;
        }
        return p;
    }

    function hslToRgb(h, s, l) {
        var r, g, b, q, p;

        h = boundHue(h) / 360;

        if (s === 0) {
            r = g = b = l; // achromatic
        } else {
            q = l < 0.5 ? l * (1 + s) : l + s - l * s;
            p = 2 * l - q;
            r = hue2rgb(p, q, h + 1 / 3);
            g = hue2rgb(p, q, h);
            b = hue2rgb(p, q, h - 1 / 3);
        }

        return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
    }

    function componentToHex(c) {
        var hex = c.toString(16);
        return hex.length === 1 ? '0' + hex : hex;
    }

    function rgbToHexCss(r, g, b) {
        return "#" + componentToHex(r) + componentToHex(g) + componentToHex(b);
    }

    function spreadHsl(hsl) {
        return [hsl.hue, hsl.saturation, hsl.lightness];
    }

    angular.module('color-schemer', [])

        .directive('csCell', [

            function () {
                return {
                    scope: {
                        hsl: '='
                    },
                    template: [
                        '<div class="cs-cell">',
                        '    <div class="cs-cell-color"></div>',
                        '    <pre class="cs-cell-code">{{distance}}&deg;</pre>',
                        '    <pre class="cs-cell-code">{{hexCss}}</pre>',
                        '    <pre class="cs-cell-code cs-cell-code-light"',
                        '         data-ng-style="{color: hexCss}">var a = 0;</pre>',
                        '    <pre class="cs-cell-code cs-cell-code-dark"',
                        '         data-ng-style="{color: hexCss}">var a = 0;</pre>',
                        '</div>'
                    ].join('\n'),
                    link: function (scope, element, attributes) {
                        /*jslint unparam: true */
                        var color = angular.element(element[0].querySelector('.cs-cell-color'));
                        scope.distance = scope.hsl.hue;
                        scope.hexCss = rgbToHexCss.apply(null, hslToRgb.apply(null, spreadHsl(scope.hsl)));
                        color.css('backgroundColor', scope.hexCss);
                    }
                };
            }
        ])

        .controller('CellsController', ['$scope', function ($scope) {

            $scope.reload = function () {
                var colors = [];
                times($scope.numberOfColors, function (index) {
                    var offset,
                        hue;
                    if ($scope.numberOfColors % 2 === 0) {
                        offset = 1;
                    } else {
                        offset = 0;
                    }
                    hue = $scope.center + (
                        (
                            // For evens, never touch center.
                            Math.floor((index + 1 + offset) / 2) *
                                // Alternate between drifting positively and negatively.
                                ((((index + 1 - (offset - 1)) % 2) * 2) - 1)
                        ) *
                            // Space 'em out.
                            $scope.distanceBetweenColors
                    );
                    colors.push({
                        hue: hue,
                        saturation: $scope.saturation,
                        lightness: $scope.lightness
                    });
                });
                colors.sort(function (a, b) {
                    return a.hue - b.hue;
                });
                $scope.colors = colors;
            };

            $scope.numberOfColors = 6;
            $scope.distanceBetweenColors = 30;
            $scope.center = 0;
            $scope.saturation = 1;
            $scope.lightness = 0.5;

            $scope.reload();

        }]);

}());
