function tree(n) {
    var i = n + 1, lines = [];
    while (--i) {
        lines.push((new Array(n - i + 1)).join(' ') + (new Array(i * 2)).join('0'));
    }
    return lines.concat((new Array(n).join(' ') + '*')).reverse();
}

function processData(input) {
    console.log(tree(9).join('\n'));
}

processData();
