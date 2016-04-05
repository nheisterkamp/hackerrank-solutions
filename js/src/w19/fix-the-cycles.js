var _ = require('lodash');

function processData(input) {
    var p = input.split(' ').map(Number),
        a = p[0], b = p[1], c = p[2], d = p[3], e = p[4], f = p[5],
        cycles = [
            [ a, e, d ],
            [ a, b, f ],
            [ a, b, c, d ]
        ];

    var p = -_.min(_.map(cycles, _.sum));
    console.log(p < 0 ? -1 : p);
}

processData('2 -5 0 1 1 1');
