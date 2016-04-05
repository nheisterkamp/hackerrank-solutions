var _ = require('lodash');

var re = new RegExp('<a .*href="(.*?)".*?>(?:<.*?>)*(.*?)(</.*>)*</a>', 'gmi');
function processData(input) {
    var output = [];
    input.replace(re, function(match, p1, p2) {
        output.push([p1, p2].join(','));
    });
    console.log(output.join('\n'));
    return output.join('\n');
}

require('./boilerplate')('detect-html-links-*', processData);
