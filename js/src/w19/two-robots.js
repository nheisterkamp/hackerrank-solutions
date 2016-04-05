function processData(input) {
    var lines = input.split('\n'),
        t = Number(lines.shift()),
        r = lines.map(line => line.split(' ').map(Number)),
        l = r.length;

    console.log('r', '=', r);

}


processData(`3
5 4
1 5
3 2
4 1
2 4
4 2
1 2
4 3
10 3
2 4
5 4
9 8`);
