function processData(input) {
    var lines = input.split('\n'),
        T = Number(lines.shift());

    lines.forEach(s => console.log(minPalindrome(s)));
}

function minPalindrome(s) {
    var len = s.length,
        res = 0;

    for (var i = 0; i < Math.floor(len / 2); i++) {
        res += Math.abs(s.charCodeAt(i) - s.charCodeAt(len - i - 1));
    }

    return res;
}


processData(`4
abc
abcba
abcd
cba`);
