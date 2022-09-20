unit Roller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RegExpr;

type
  TRoller = class
  private
    FNumRolls, FDieSides, FAdditive: integer;
    FRegExParsed: boolean;
    procedure SetNumRolls(NumRolls: integer);
    procedure SetDieSides(DieSides: integer);
    procedure SetAdditive(Additive: integer);
  public
    constructor Create; overload;
    constructor Create(NumRoles, DieSides, Additive: integer); overload;
    constructor Create(DnDRoll: string); overload;
    function Roll: integer;
  published
    property NumRolls: integer read FNumRolls write SetNumRolls;
    property DieSides: integer read FDieSides write SetDieSides;
    property Additive: integer read FAdditive write SetAdditive;
    property IsRegExParsed: boolean read FRegExParsed;
  end;

const
  PATTERN = '^\d+[dD]\d+[\+-]?\d*$';

implementation

constructor TRoller.Create;
begin
  Randomize;
  FNumRolls := 0;
  FDieSides := 0;
  FAdditive := 0;
  FRegExParsed := false;
end;

constructor TRoller.Create(NumRoles, DieSides, Additive: integer);
begin
  FNumRolls := NumRoles;
  FDieSides := DieSides;
  FAdditive := Additive;
  FRegExParsed := false;
end;

constructor TRoller.Create(DnDRoll: string);
var
  Reg: TRegExpr;
  NumRollsFlag, DieSidesFlag, i: integer;
  InvertAdditive: boolean;
  c: char;
begin
  Reg:=TRegExpr.Create(PATTERN);
  NumRollsFlag:=0;
  DieSidesFlag:=0;
  InvertAdditive:=false;
  FRegExParsed:=Reg.Exec(DnDRoll);
  if FRegExParsed then
  begin
    for i:=1 to Length(DnDRoll) do
    begin
      c:=DnDRoll[i];
      if NumRollsFlag = 0 then
      begin
        if (c = 'd') or (c = 'D') then
        begin
          NumRollsFlag:=i;
          FNumRolls:=integer.Parse(DnDRoll.Substring(0, i - 1));
        end;
        continue;
      end;
      if DieSidesFlag = 0 then
      begin
        if (c = '+') or (c = '-') then
        begin
          DieSidesFlag:=i;
          FDieSides:=integer.Parse(DndRoll.SubString(NumRollsFlag, DieSidesFlag - NumRollsFlag - 1));
          if c = '-' then InvertAdditive:=true;
        end;
      end;
    end;
    if (DieSidesFlag = 0) and (FDieSides = 0) then
    begin
      FDieSides:=integer.Parse(DnDRoll.Substring(NumRollsFlag));
    end;
    if DieSidesFlag > 0 then
    begin
      FAdditive:=integer.Parse(DnDRoll.Substring(DieSidesFlag));
      if InvertAdditive then FAdditive*=-1;
    end;
  end;
  FreeAndNil(Reg);
end;

function TRoller.Roll: integer;
var
  rolls, Value: integer;
begin
  Value := 0;
  for rolls := 1 to FNumRolls do
  begin
    Value += Random(FDieSides) + 1;
  end;
  Result := Value + FAdditive;
end;

procedure TRoller.SetNumRolls(NumRolls: integer);
begin
  FNumRolls := NumRolls;
  FRegExParsed := false;
end;

procedure TRoller.SetDieSides(DieSides: integer);
begin
  FDieSides := DieSides;
  FRegExParsed := false;
end;

procedure TRoller.SetAdditive(Additive: integer);
begin
  FAdditive := Additive;
  FRegExParsed := false;
end;

end.
