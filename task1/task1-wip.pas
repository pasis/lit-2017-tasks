{ task1-wip.pas
  Evaluates math expression. Unfinished version from lecture 09.03.2017.
  Changes required:
    1. Add support of brackets ()
    2. Handle division by zero

  Author: Dmitry Podgorny <pasis.ua@gmail.com>
}
{ fpc -otask1 task1-wip.pas }

Program Calc;

Var expr : String;
    res : Extended;

{ Removes and returns value of the next element in the `expr' }
function elem(var expr : String) : Extended;
var code : Integer;
    num : Extended;
    tmp : String;
begin
	Val(expr, num, code);
	if code = 0 then
		expr := ''
	else begin
		tmp := Copy(expr, 1, code - 1);
		Delete(expr, 1, code - 1);
		Val(tmp, num, code);
	end;
	elem := num;
end;

{ Returns result of evaluating the expression }
function eval(var expr : String) : Extended;
var num, left, right : Extended;
    op, rop, nop : Char;
begin
	op := '+';
	rop := 'N';
	left := 0;
	right := 0;
	while expr <> '' do begin
		num := elem(expr);

		if rop = '*' then
			right := right * num
		else if rop = '/' then
			right := right / num
		else
			right := num;
		rop := 'N';

		if (expr[1] = '+') or (expr[1] = '-') or (expr = '') then begin
			if op = '+' then
				left := left + right;
			if op = '-' then
				left := left - right;
		end;

		if expr <> '' then begin
			nop := expr[1];
			Delete(expr, 1, 1);
		end else
			nop := 'N';

		if (nop = '+') or (nop = '-') then
			op := nop;
		if (nop = '*') or (nop = '/') then
			rop := nop;
	end;
	eval := left;
end;

Begin
	ReadLn(expr);
	res := eval(expr);
	WriteLn(res);
End.
