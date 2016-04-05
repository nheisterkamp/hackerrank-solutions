console.log(processData(`6 6
BGBBGB
GGGGGG
BGBBGB
GGGGGG
BGBBGB
BGBBGB`));

var _ = require('lodash');



function processData(input) {
    var lines = input.split('\n');
    lines.shift();
    var matrix = lines.map(l => l.split('').map(c => c === 'G' ? 1 : 0));



    return matrix;
}
