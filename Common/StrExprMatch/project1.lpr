program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils;

procedure Run;
  const Str = '[2024.12.03-12.14.15:933][  0]LogNVN: Verbose: Texture NonStreamable (SlateTexture2D: 134131c310) format: 156, mips: 1 (sparse: 0 resident: 1), size: 4194304B (4.00MB), pool: 0:0 (total texture size: 28.11MB, total render target size: 1.06MB)';
  var Match: TUExprMatch;
begin
  //WriteLn(UStrSubStr('Test', 2, 2));
  Match := UStrExprMatch(Str, '[*.*.*-*.*.*:*]');
  //Match := UStrExprMatch(Str, '***');
  if not Match.Match then
  begin
    WriteLn('No Match');
    Exit;
  end;
  WriteLn('Match: ', Match.MatchStr);
end;

begin
  Run;
  ReadLn;
end.

