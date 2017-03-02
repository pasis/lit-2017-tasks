{ task1.pas
  Evaluates math expression.

  Author: Dmitry Podgorny <pasis.ua@gmail.com>
}
{ fpc -otask1 task1.pas }

Program Calc;

function calc(var expr: String) : Extended; Forward;

Var expr : String;
Var error : Boolean;
Var res : Extended;

function elem(var expr : String) : Extended;
var code : Integer;
var x : Extended;
var tmp : String;
begin
	if expr[1] = '(' then begin
		Delete(expr, 1, 1);
		x := calc(expr);
		if (expr <> '') and (expr[1] = ')') then
			Delete(expr, 1, 1);
	end else begin
		Val(expr, x, code);
		if code = 1 then begin
			WriteLn('Expected number, but got: ', expr[code]);
			Halt(1);
		end;
		if code = 0 then
			expr := ''
		else begin
			tmp := Copy(expr, 1, code - 1);
			Delete(expr, 1, code - 1);
			Val(tmp, x, code);
			if code <> 0 then begin
				WriteLn('Error occured');
				Halt(1);
			end;
		end;
	end;
	elem := x;
end;

function calc(var expr : String) : Extended;
var num, left, right : Extended;
var op, rop, nop : Char;
begin
	op := '+';
	rop := 'N';
	left := 0;
	right := 0;
	while (expr <> '') and (expr[1] <> ')') do begin
		num := elem(expr);
		if error then break;

		if rop = '*' then
			right := right * num;
		if rop = '/' then begin
			if num = 0 then begin
				error := true;
				break;
			end;
			right := right / num;
		end;
		if (rop <> '*') and (rop <> '/') then
			right := num;
		rop := 'N';

		if (expr = '') or (expr[1] = '+') or (expr[1] = '-') or
		   (expr[1] = ')') then begin
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
	calc := left;
end;

Begin
	error := false;
	ReadLn(expr);
	res := calc(expr);
	if error then
		WriteLn('NaN')
	else
		WriteLn(res);
End.
