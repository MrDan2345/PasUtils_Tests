program hex_reverse;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
  { you can add units after this };

function SwapHex(const Hex: String): String;
  var i, j: Int32;
begin
  SetLength(Result, Length(Hex));
  for i := 0 to Length(Hex) div 2 - 1 do
  begin
    j := i * 2;
    Result[j + 1] := Hex[Length(Hex) - j - 1];
    Result[j + 2] := Hex[Length(Hex) - j];
  end;
end;

procedure Run;
  var s: String;
begin
  WriteLn('Enter hex:');
  ReadLn(s);
  if (Length(s) = 0) or (Length(s) mod 2 = 1) then
  begin
    WriteLn('Invalid hex number');
    Exit;
  end;
  WriteLn(SwapHex(s));
end;

begin
  Run;
{$if defined(windows)}
  ReadLn;
{$endif}
end.

