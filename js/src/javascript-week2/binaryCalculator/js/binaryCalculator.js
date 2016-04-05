var curr = '';
var operand1 = '';
var operator = '';
var operand2 = '';

function btn0() {
	if (operator) {
		operand2 += '0';
	}
	else {
		operand1 += '0';
	}
	res(curr + '0');
}

function btn1() {
	if (operator) {
		operand2 += '1';
	}
	else {
		operand1 += '1';
	}
	res(curr + '1');
}

function btnClr() {
	res('');
	operand1 = '';
	operator = '';
	operand2 = '';
}

function btnEql() {
	var r = calc(parseInt(operand1, 2), operator, parseInt(operand2, 2));
	btnClr();
	res((r | 0).toString(2));
	curr = '';
}

function calc(lhs, op, rhs) {
	switch (op) {
		case '+':
			return lhs + rhs;
		case '-':
			return lhs - rhs;
		case '*':
			return lhs * rhs;
		case '/':
			return lhs / rhs;
	}
}

function btnSum() {
	operator = '+';
	res(curr + operator);
}

function btnSub() {
	operator = '-';
	res(curr + operator);
}

function btnMul() {
	operator = '*';
	res(curr + operator);
}

function btnDiv() {
	operator = '/';
	res(curr + operator);
}

function res(s) {
	document.getElementById('res').innerHTML = s;
	curr = s;
}

var ids = ['0', '1', 'Clr', 'Eql', 'Sum', 'Sub', 'Mul', 'Div'];
var btns = ids.map(function(id) {
	document.getElementById('btn' + id).onclick = window['btn' + id];
});
res(curr);
btnEql();
