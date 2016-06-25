'use strict';
// https://en.wikipedia.org/wiki/Maximum_subarray_problem
function maxContiguousSubarray(a) {
    let maxEndingHere, maxSoFar;
    maxEndingHere = maxSoFar = a[0];
    for (let x = 1, l = a.length; x < l; x++) {
        maxEndingHere = Math.max(a[x], maxEndingHere + a[x]);
        maxSoFar = Math.max(maxSoFar, maxEndingHere);
    }
    return maxSoFar;
}

function maxNonContiguousSubarray(a) {
    return a.reduce((r, i) => r + (i>0 ? i : 0), 0) ||
        Math.max.apply(Math, a);
}

function processData(input) {
    let lines = input.split('\n'),
        t = lines.shift();

    for (let i = 0; i < t; i++) {
        let a = lines[i*2+1].split(' ').map(parseFloat);
        console.log(maxContiguousSubarray(a),
            maxNonContiguousSubarray(a));
    }
}

processData(`6
1
1
6
-1 -2 -3 -4 -5 -6
2
1 -2
3
1 2 3
1
-10
6
1 -1 -1 -1 -1 5`);
