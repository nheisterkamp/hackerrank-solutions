var fs = require('fs');
var path = require('path');
var glob = require('glob');
var diff = require('diff');

var dataPath = path.join(__dirname, '..', '..', 'data');

module.exports = function(pattern, callback) {
    glob(path.join(dataPath, pattern + '.in'), function(err, files) {
        if (err) {
            return new Error(err);
        }

        files.forEach(function(file) {
            var data = fs.readFileSync(file, 'utf-8');
            var output = callback(data, file);

            var testFile = file.substring(0, file.lastIndexOf('.') + 1) + 'out';
            console.log('testFile', '=', testFile);
            try {
                var testOutput = fs.readFileSync(testFile, 'utf-8');
                console.log('testOutput', '=', testOutput);
                if (testOutput) {
                    var diffed = diff.diffTrimmedLines(testOutput, output);
                    console.log('DIFFED');
                    console.log(diffed);
                }
            }
            catch (e) {}
        });
    });
};
