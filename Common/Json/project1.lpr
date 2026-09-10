program project1;

{$include PasUtilsMode.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  CommonUtils
;
const TestJson = (
`{"name" : "test escape\r \"characters\"\\"}`
);
 var Json: TUJson;
begin
  Json := TUJson.Load(TestJson);
  WriteLn(Json.FormatJson());
  ReadLn;
  Json.Free;
end.

