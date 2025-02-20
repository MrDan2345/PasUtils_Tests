program BigInt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

function IsPrime(n: Int64): Int64;
  var i: Int64;
begin
    if n <= 1 then Exit(1);
    if n <= 3 then Exit(0);
    if (n mod 2 = 0) then Exit(2);
    if (n mod 3 = 0) then Exit(3);
    i := Int64(5);
    while i * i <= n do
    begin
      if n mod i = 0 then Exit(i);
      if n mod (i + 2) = 0 then Exit(i + 2);
      Inc(i, 6);
    end;
    Exit(0);
end;

procedure Run;
  var n1, n2, n3, r: TUBigInt;
  //const Value = '-98765432109876543219876543210987654321';
  //const Value = '98765432121';
  //const Value = '987';
  var i: Int64;
begin
  n1 := TUBigInt.Make('91872670824640586998765432109876543219876543210987654322');
  //n1 := TUBigInt.Make('98765432121');
  n2 := TUBigInt.Make('275923482734998765432109876543219876543210987654321');
  //n2 := TUBigInt.Make('987');
  //n3 := TUBigInt.Sub(n1, n2);
  //n3 := TUBigInt.Multiplication(n1, n2);
  n3 := TUBigInt.Division(n1, n2, r);
  //n := TUBigInt.Make(-9876);
  //n := TUBigInt.Make(9876);
  WriteLn(n1.ToString);
  WriteLn(n2.ToString);
  WriteLn(n3.ToString, ' : ', r.ToString);
  Randomize;
  n1 := TUBigInt.MakePrime(16);
  //i := n1.ToInt64;
  //WriteLn(n1.ToString, ' is prime = ', IsPrime(i));
  WriteLn(n1.ToString);
end;

begin
  Run;
end.

