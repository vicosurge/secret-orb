unit roomunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  Room = class
    Name : string;
    Description : string;
    N, S, E, W, U, D : integer;
    function GetDescription : string;
    constructor Create(aName, aDescription: string; dirN, dirS, dirE, dirW, dirU, dirD: integer);
  end;

implementation

{ Room }

function Room.GetDescription: string;
begin
    Result := Name + ' ' + Description;
end;

constructor Room.Create(aName, aDescription: string; dirN, dirS, dirE, dirW, dirU, dirD: integer);
begin
  inherited Create;
  Name := aName;
  Description := aDescription;
  N := dirN;
  S := dirS;
  E := dirE;
  W := dirW;
  U := dirU;
  D := dirD;
end;

end.