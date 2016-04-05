var _ = require('lodash');

function processData(input) {
    input = input.replace(/%3A/g, ':');
    input = input.replace(/%2F/g, '/');
    var urls = input.match(/(https?:\/\/)([^ ]+)/g);
    var domains = _.map(urls, function(url) {
        url = url.substring(url.indexOf(':') + 3);

        var idx1 = url.indexOf('/');
        var idx2 = url.indexOf('?');
        var idx = Math.min(idx1 === -1 ? Infinity : idx1, idx2 === -1 ? Infinity : idx2);

        url = url.substring(0, idx);

        return url.replace(/^ww.\./, '');
    });
    console.log(_.uniq(domains).join(';'));
}

var _input = require('fs').readFileSync(
    require('path').join(__dirname, '..', '..', 'data', 'detect-the-domain-name-1.in'), 'utf-8');

processData(_input);

// process.stdin.resume();
// process.stdin.setEncoding("ascii");
// _input = "";
// process.stdin.on("data", function (input) {
//     _input += input;
// });

// process.stdin.on("end", function () {
//    processData(_input);
// });
