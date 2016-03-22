{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit neopkg;

interface

uses
  neoCircBuf, neofileutils, neoFormUtils, neoGenComboForm, neoGenTextEntry, 
  neopcommcrossplatform, neoTextutils, neotypedef, neoprocess, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('neopkg', @Register);
end.
