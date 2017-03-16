{ lists.pas
  Quick and dirty implementation of lists in Pascal.

  Author: Dmitry Podgorny
}

Program Lists;

type TList = record
	value : Integer;
	next : ^TList;
end;

type List = ^TList;

Var a : Array [1..10] of ^TList;
    i : Integer;

function list_new(value : Integer) : List;
var elem : List;
begin
	New(elem);
	elem^.next := NIL;
	elem^.value := value;
	list_new := elem;
end;

procedure list_destroy(elem : List);
begin
	Dispose(elem);
end;

procedure list_add(var head : List; elem : List);
begin
	elem^.next := head;
	head := elem;
end;

procedure list_del(var head : List; elem : List);
var prev, curr : List;
begin
	prev := NIL;
	curr := head;
	while (curr <> NIL) and (curr <> elem) do
	begin
		prev := curr;
		curr := curr^.next;
	end;
	if curr = NIL then
		WriteLn('Error: list doesnt contain the element')
	else if (prev = NIL) then
		head := elem^.next
	else
		prev^.next := elem^.next;
end;

function list_find(head : List; value : Integer) : List;
var curr : List;
begin
	curr := head;
	while (curr <> NIL) and (curr^.value <> value) do
		curr := curr^.next;
	list_find := curr;
end;

procedure list_print(head : List);
var curr : List;
begin
	curr := head;
	if curr = NIL then
		WriteLn('NIL')
	else begin
		while curr^.next <> NIL do
		begin
			Write(curr^.value, ' -> ');
			curr := curr^.next;
		end;
		WriteLn(curr^.value);
	end;
end;

procedure list_add_value(var head : List; value : Integer);
begin
	list_add(head, list_new(value));
end;

procedure list_del_value(var head : List; value : Integer);
var elem : List;
begin
	elem := list_find(head, value);
	list_del(head, elem);
	list_destroy(elem);
end;

Begin
	for i := 1 to 10 do
		a[i] := NIL;
	list_add_value(a[1], 1);
	list_add_value(a[1], 2);
	list_add_value(a[3], 3);
	list_print(a[1]);
	list_print(a[2]);
	list_print(a[3]);
	list_del_value(a[1], 1);
	list_del_value(a[1], 2);
	list_del_value(a[3], 3);
	list_print(a[1]);
End.
