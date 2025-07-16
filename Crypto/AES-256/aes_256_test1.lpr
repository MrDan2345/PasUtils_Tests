program aes_256_test1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CommonUtils,
  CryptoUtils;

type TAES256Key = array[0..31] of UInt8;

function StringToUInt8Array(const s: String): TUInt8Array;
  var i: Int32;
begin
  Result := nil;
  SetLength(Result, Length(s));
  for i := 0 to High(Result) do
  begin
    Result[i] := Ord(s[i + 1]);
  end;
end;

function KeyToHexString(const Key: TAES256Key): String;
  var i: Int32;
begin
  Result := '';
  for i := 0 to High(Key) do
  begin
    Result += LowerCase(IntToHex(Key[i], 2));
  end;
end;

type TState = array[0..3, 0..3] of UInt8;
type TStateArray = array of TState;

const
  // Rijndael S-box: a predefined substitution table
  SBox: array[0..255] of Byte = (
    $63, $7c, $77, $7b, $f2, $6b, $6f, $c5, $30, $01, $67, $2b, $fe, $d7, $ab, $76,
    $ca, $82, $c9, $7d, $fa, $59, $47, $f0, $ad, $d4, $a2, $af, $9c, $a4, $72, $c0,
    $b7, $fd, $93, $26, $36, $3f, $f7, $cc, $34, $a5, $e5, $f1, $71, $d8, $31, $15,
    $04, $c7, $23, $c3, $18, $96, $05, $9a, $07, $12, $80, $e2, $eb, $27, $b2, $75,
    $09, $83, $2c, $1a, $1b, $6e, $5a, $a0, $52, $3b, $d6, $b3, $29, $e3, $2f, $84,
    $53, $d1, $00, $ed, $20, $fc, $b1, $5b, $6a, $cb, $be, $39, $4a, $4c, $58, $cf,
    $d0, $ef, $aa, $fb, $43, $4d, $33, $85, $45, $f9, $02, $7f, $50, $3c, $9f, $a8,
    $51, $a3, $40, $8f, $92, $9d, $38, $f5, $bc, $b6, $da, $21, $10, $ff, $f3, $d2,
    $cd, $0c, $13, $ec, $5f, $97, $44, $17, $c4, $a7, $7e, $3d, $64, $5d, $19, $73,
    $60, $81, $4f, $dc, $22, $2a, $90, $88, $46, $ee, $b8, $14, $de, $5e, $0b, $db,
    $e0, $32, $3a, $0a, $49, $06, $24, $5c, $c2, $d3, $ac, $62, $91, $95, $e4, $79,
    $e7, $c8, $37, $6d, $8d, $d5, $4e, $a9, $6c, $56, $f4, $ea, $65, $7a, $ae, $08,
    $ba, $78, $25, $2e, $1c, $a6, $b4, $c6, $e8, $dd, $74, $1f, $4b, $bd, $8b, $8a,
    $70, $3e, $b5, $66, $48, $03, $f6, $0e, $61, $35, $57, $b9, $86, $c1, $1d, $9e,
    $e1, $f8, $98, $d9, $8e, $94, $9b, $1e, $87, $e9, $ce, $55, $28, $df, $8c, $a1,
    $89, $0d, $bf, $e6, $42, $68, $41, $99, $2d, $0f, $b0, $54, $bb, $16, $b9, $85
  );

  // Round constants for key expansion
  Rcon: array[1..10] of Byte = (
    $01, $02, $04, $08, $10, $20, $40, $80, $1b, $36
  );

// Galois Field (2^8) multiplication for MixColumns
function GMul(a, b: Byte): Byte;
var
  p: Byte;
  i: Integer;
begin
  p := 0;
  for i := 0 to 7 do
  begin
    if (b and 1) <> 0 then
      p := p xor a;

    if (a and $80) <> 0 then
      a := (a shl 1) xor $1b // x^8 + x^4 + x^3 + x + 1
    else
      a := a shl 1;
    b := b shr 1;
  end;
  Result := p;
end;

// --- Key Expansion ---
procedure KeyExpansion(const Key: TAES256Key; var ExpandedKey: TStateArray);
type
  TWord = array[0..3] of Byte;
var
  temp: TWord;
  i: Integer;
  t: Byte;
begin
  SetLength(ExpandedKey, 15); // 14 rounds + initial key
  // The first 32 bytes (8 words) are the original key
  for i := 0 to 7 do
  begin
    ExpandedKey[i div 4][i mod 4][0] := Key[i*4 + 0];
    ExpandedKey[i div 4][i mod 4][1] := Key[i*4 + 1];
    ExpandedKey[i div 4][i mod 4][2] := Key[i*4 + 2];
    ExpandedKey[i div 4][i mod 4][3] := Key[i*4 + 3];
  end;

  // Generate the rest of the round keys
  i := 8;
  while i < (15 * 4) do
  begin
    // Get the previous word
    temp[0] := ExpandedKey[(i-1) div 4][(i-1) mod 4][0];
    temp[1] := ExpandedKey[(i-1) div 4][(i-1) mod 4][1];
    temp[2] := ExpandedKey[(i-1) div 4][(i-1) mod 4][2];
    temp[3] := ExpandedKey[(i-1) div 4][(i-1) mod 4][3];

    if (i mod 8) = 0 then
    begin
      // Rotate word
      t := temp[0];
      temp[0] := temp[1]; temp[1] := temp[2]; temp[2] := temp[3]; temp[3] := t;
      // SubWord
      temp[0] := SBox[temp[0]];
      temp[1] := SBox[temp[1]];
      temp[2] := SBox[temp[2]];
      temp[3] := SBox[temp[3]];
      // XOR with Rcon
      temp[0] := temp[0] xor Rcon[i div 8];
    end
    else if (i mod 8) = 4 then
    begin
      // SubWord only for AES-256
      temp[0] := SBox[temp[0]];
      temp[1] := SBox[temp[1]];
      temp[2] := SBox[temp[2]];
      temp[3] := SBox[temp[3]];
    end;

    // XOR with word i-8 and store
    ExpandedKey[i div 4][i mod 4][0] := ExpandedKey[(i-8) div 4][(i-8) mod 4][0] xor temp[0];
    ExpandedKey[i div 4][i mod 4][1] := ExpandedKey[(i-8) div 4][(i-8) mod 4][1] xor temp[1];
    ExpandedKey[i div 4][i mod 4][2] := ExpandedKey[(i-8) div 4][(i-8) mod 4][2] xor temp[2];
    ExpandedKey[i div 4][i mod 4][3] := ExpandedKey[(i-8) div 4][(i-8) mod 4][3] xor temp[3];
    Inc(i);
  end;
end;

// --- AES Round Transformations ---
procedure SubBytes(var State: TState);
var r, c: Integer;
begin
  for r := 0 to 3 do
    for c := 0 to 3 do
      State[r, c] := SBox[State[r, c]];
end;

procedure ShiftRows(var State: TState);
var temp: Byte;
begin
  // Row 1
  temp := State[1, 0];
  State[1, 0] := State[1, 1]; State[1, 1] := State[1, 2]; State[1, 2] := State[1, 3]; State[1, 3] := temp;
  // Row 2
  temp := State[2, 0]; State[2, 0] := State[2, 2]; State[2, 2] := temp;
  temp := State[2, 1]; State[2, 1] := State[2, 3]; State[2, 3] := temp;
  // Row 3
  temp := State[3, 3];
  State[3, 3] := State[3, 2]; State[3, 2] := State[3, 1]; State[3, 1] := State[3, 0]; State[3, 0] := temp;
end;

procedure MixColumns(var State: TState);
var c, r: Integer;
  a: array[0..3] of Byte;
begin
  for c := 0 to 3 do
  begin
    for r := 0 to 3 do a[r] := State[r, c];
    State[0, c] := GMul(a[0], 2) xor GMul(a[1], 3) xor GMul(a[2], 1) xor GMul(a[3], 1);
    State[1, c] := GMul(a[0], 1) xor GMul(a[1], 2) xor GMul(a[2], 3) xor GMul(a[3], 1);
    State[2, c] := GMul(a[0], 1) xor GMul(a[1], 1) xor GMul(a[2], 2) xor GMul(a[3], 3);
    State[3, c] := GMul(a[0], 3) xor GMul(a[1], 1) xor GMul(a[2], 1) xor GMul(a[3], 2);
  end;
end;

procedure AddRoundKey(var State: TState; const RoundKey: TState);
var r, c: Integer;
begin
  for r := 0 to 3 do
    for c := 0 to 3 do
      State[c, r] := State[c, r] xor RoundKey[r, c]; // Note the column/row transpose
end;

// --- Padding ---
function PadData_PKCS7(const Data: TUInt8Array; BlockSize: Integer): TUInt8Array;
var
  i, padLen: Integer;
begin
  padLen := BlockSize - (Length(Data) mod BlockSize);
  SetLength(Result, Length(Data) + padLen);
  System.Move(Data[0], Result[0], Length(Data));
  for i := Length(Data) to High(Result) do
    Result[i] := Byte(padLen);
end;

{------------------------------------------------------------------------------}
// Main AES-256 Encryption Function
{------------------------------------------------------------------------------}
function EncryptAES256(const Data: TUInt8Array; const Key: TAES256Key): TUInt8Array;
var
  ExpandedKey: array of TState;
  State: TState;
  PaddedData: TUInt8Array;
  i, r, c: Integer;
begin
  // 1. Expand the key into the full key schedule
  KeyExpansion(Key, ExpandedKey);

  // 2. Pad the data to a multiple of 16 bytes using PKCS#7
  PaddedData := PadData_PKCS7(Data, 16);
  SetLength(Result, Length(PaddedData));

  // 3. Process the data in 16-byte blocks
  for i := 0 to (Length(PaddedData) div 16) - 1 do
  begin
    // Load block into state matrix (column-major order)
    for r := 0 to 3 do
      for c := 0 to 3 do
        State[r, c] := PaddedData[i*16 + r + 4*c];

    // Initial Round
    AddRoundKey(State, ExpandedKey[0]);

    // 13 Main Rounds
    for r := 1 to 13 do
    begin
      SubBytes(State);
      ShiftRows(State);
      MixColumns(State);
      AddRoundKey(State, ExpandedKey[r]);
    end;

    // Final Round (no MixColumns)
    SubBytes(State);
    ShiftRows(State);
    AddRoundKey(State, ExpandedKey[14]);

    // Copy the resulting state matrix to the output array
    for r := 0 to 3 do
      for c := 0 to 3 do
        Result[i*16 + r + 4*c] := State[r, c];
  end;
end;

procedure Run;
  var Data, Cipher: TUInt8Array;
  var Digest: TUSHA256Digest;
  var Key: TAES256Key absolute Digest;
begin
  Key := USHA256('MyPassword');
  WriteLn(KeyToHexString(Key));
  Data := StringToUInt8Array('Hello World!');
  Cipher := EncryptAES256(Data, Key);
  WriteLn(UBytesToHex(Data));
end;

begin
  Run;
end.

