'use strict';
const _ = require('lodash');

function numberOfVertices(tree, node) {
    return numberOfVerticesTraverse(tree, node) + 1;
}

function numberOfVerticesTraverse(tree, node) {
    var ts = tree[node];
    if (!ts) { return 0; }
    return ts.reduce((r, t) => r + numberOfVerticesTraverse(tree, t), ts.length);
}

function isEvenTree(tree, node) {
    return (numberOfVertices(tree, node) % 2 === 0);
}

function edgesToTree(edges) {
    return edges.reduce((r, [t, f]) => {
        if (r[f]) { r[f].push(t); } 
        else { r[f] = [t]; }
        return r;
    }, {});
}

function getEven(tree) {
    return _.map(_.keys(tree), node => isEvenTree(tree, node));
}

function processData(input) {
    let lines = input.split('\n'),
        [n, m] = lines.shift().split(' ').map(Number),
        edges = lines.map(line => line.split(' ').map(Number)),
        tree = edgesToTree(edges),
        even = getEven(tree);

    console.log(_.compact(even).length - 1);
}
