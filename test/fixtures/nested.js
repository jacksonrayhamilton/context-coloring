function a() {
    var A = a;
    function b() {
        var B = b;
        function c() {
            var C = c;
            function d() {
                var D = d;
                function e() {
                    var E = e;
                    function f() {
                        var F = f;
                        function g() {
                            var G = g;
                            function h() {
                                var H = [
                                    A,
                                    B,
                                    C,
                                    D,
                                    E,
                                    F,
                                    G
                                ];
                            }
                        }
                    }
                }
            }
        }
    }
}
