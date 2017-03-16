{ task2.pas
  Draw a graph of the function.

  Author: Dmitry Podgorny <pasis.ua@gmail.com>
}
{ Use PascalABC.NET to build this program }

Program Graphics;

Uses GraphABC;

{ Adjust these constants or make window size fit these. }
Const w : Integer = 640;
      h : Integer = 480;
      scale : Real = 0.05;

Var expr : String;
    error : Boolean;
    res : Real;
    x, y : Real;
    i, j, i1, j1 : Integer;

function eval(var expr: String; var error : Boolean) : Real; Forward;

function elem(var expr : String; var error : Boolean) : Real;
var code : Integer;
    num : Real;
    tmp : String;
begin
	if expr[1] = 'x' then begin
		num := x;
		Delete(expr, 1, 1);
	end else if expr[1] = '(' then begin
		Delete(expr, 1, 1);
		num := eval(expr, error);
		if (expr <> '') and (expr[1] = ')') then
			Delete(expr, 1, 1);
	end else begin
		Val(expr, num, code);
		if code = 1 then begin
			WriteLn('Expected number, but got: ', expr[code]);
			Halt(1);
		end;
		if code = 0 then
			expr := ''
		else begin
			tmp := Copy(expr, 1, code - 1);
			Delete(expr, 1, code - 1);
			Val(tmp, num, code);
			if code <> 0 then begin
				WriteLn('Error occured');
				Halt(1);
			end;
		end;
	end;
	elem := num;
end;

function eval(var expr : String; var error : Boolean) : Real;
var num, left, right : Real;
    op, rop, nop : Char;
begin
	error := false;
	op := '+';
	rop := 'N';
	left := 0;
	right := 0;
	while (expr <> '') and (expr[1] <> ')') do begin
		num := elem(expr, error);
		if error then break;

		if rop = '*' then
			right := right * num
		else if rop = '/' then begin
			if num = 0 then begin
				error := true;
				break;
			end;
			right := right / num;
		end else
			right := num;
		rop := 'N';

		if (expr = '') or (expr[1] = '+') or (expr[1] = '-') or
		   (expr[1] = ')') then begin
			if op = '+' then
				left := left + right;
			if op = '-' then
				left := left - right;
		end;

		if (expr <> '') and (expr[1] <> ')') then begin
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

{ *************************************
   The above code is taken from task1.
  ************************************* }

procedure draw_axes();
begin
	for i := 0 to w do
		setPixel(i, h div 2, RGB(128, 128, 128));
	for j := 0 to h do
		setPixel(w div 2, j, RGB(128, 128, 128));
end;

procedure draw_graph(expr : String);
var s : String;
begin
	i1 := -1;
	for i := 0 to w do begin
		x := (i - w div 2) * scale;
		s := expr;
		y := eval(s, error);
		if not error then begin
			j := round(h div 2 - y / scale);
			setPixel(i, j, RGB(0, 0, 0));
			if i1 <> -1 then
				Line(i1, j1, i, j);
			i1 := i;
			j1 := j;
		end else
			i1 := -1;
	end;
end;

Begin
	draw_axes();
	ReadLn(expr);
	draw_graph(expr);
	ReadLn; { Wait }
End.
