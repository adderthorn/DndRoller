unit Roller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRoller = class
  private
    FNumRolls, FDieSides, FAdditive: integer;
    procedure SetNumRolls(NumRolls: integer);
    procedure SetDieSides(DieSides: integer);
    procedure SetAdditive(Additive: integer);
  public
    constructor Create; overload;
    constructor Create(NumRoles, DieSides, Additive: integer); overload;
    destructor Destroy; override;
    function Roll: integer;
  published
    property NumRolls: integer read FNumRolls write SetNumRolls;
    property DieSides: integer read FDieSides write SetDieSides;
    property Additive: integer read FAdditive write SetAdditive;
  end;

implementation

constructor TRoller.Create;
begin
  Randomize;
  FNumRolls := 0;
  FDieSides := 0;
  FAdditive := 0;
end;

constructor TRoller.Create(NumRoles, DieSides, Additive: integer);
begin
  FNumRolls := NumRoles;
  FDieSides := DieSides;
  FAdditive := Additive;
end;

destructor TRoller.Destroy;
begin
  FreeAndNil(FNumRolls);
  FreeAndNil(FDieSides);
  FreeAndNil(FAdditive);
end;

function TRoller.Roll: integer;
var
  rolls, Value: integer;
begin
  Value := 0;
  for rolls := 0 to FNumRolls - 1 do
  begin
    Value += Random(FDieSides) + 1;
  end;
  Result := Value + FAdditive;
end;

procedure TRoller.SetNumRolls(NumRolls: integer);
begin
  FNumRolls := NumRolls;
end;

procedure TRoller.SetDieSides(DieSides: integer);
begin
  FDieSides := DieSides;
end;

procedure TRoller.SetAdditive(Additive: integer);
begin
  FAdditive := Additive;
end;

end.
