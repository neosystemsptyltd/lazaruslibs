unit neoGenComboForm;

interface

uses
  {$ifndef FPC}
  Windows, Messages,
  {$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, neotypedef, neoformutils
  {$ifdef FPC}
  , LResources
  {$endif}
  ;

type
  TGenComboFrm = class(TForm)
    TextLabel: TLabel;
    Combo: TComboBox;
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OnHelpSelected : THelpProc;

    constructor Create(AOWner : TComponent); override;

    function    Execute : Integer;
    procedure   ShowHelpButton;
    procedure   HideHelpButton;
    procedure   DisableHelp;
    procedure   EnableHelp(x: THelpProc);
    procedure   ArrangeButtons;
  end;

var
  GenComboFrm: TGenComboFrm;

implementation

// {$R *.DFM} old delphi

// ***********************************************************
// Create
// ***********************************************************
constructor TGenComboFrm.Create(AOWner : TComponent);
begin
   inherited Create(Aowner);

   OnHelpSelected := nil;
end;

// ***********************************************************
// OK Button
// ***********************************************************
procedure TGenComboFrm.OKButtonClick(Sender: TObject);
begin
   ModalResult := mrOK;
end;

// ***********************************************************
// cancel button
// ***********************************************************
procedure TGenComboFrm.CancelButtonClick(Sender: TObject);
begin
   ModalResult := mrCancel;
end;

// ***********************************************************
// Execute (ie show) the form
// ***********************************************************
function TGenComboFrm.Execute : Integer;
begin
  Execute := ShowModal;
end;

// ***********************************************************
// Show the Help button
// ***********************************************************
procedure TGenComboFrm.ShowHelpButton;
begin
   HelpButton.Visible := true;
   ArrangeButtons;
end;

// ***********************************************************
// hide the Help button
// ***********************************************************
procedure TGenComboFrm.HideHelpButton;
begin
   HelpButton.Visible := False;
   ArrangeButtons;
end;

// ***********************************************************
// Disable help altogether
// ***********************************************************
procedure TGenComboFrm.DisableHelp;
begin
   HideHelpButton;
   OnHelpSelected := nil;
end;

// ***********************************************************
// Enable help altogether
// ***********************************************************
procedure TGenComboFrm.EnableHelp(x: THelpProc);
begin
   OnHelpSelected := x;
   ShowHelpButton;
end;

// ***********************************************************
// Arrange the visible buttons
// ***********************************************************
procedure TGenComboFrm.ArrangeButtons;
var
   ButtonList : TButtonList;
begin
   ButtonList := TButtonList.Create;
   ButtonList.Add(OKButton);
   if HelpButton.Visible then
      ButtonList.Add(HelpButton);

   SpreadButtons(self,ButtonList,20);

   ButtonList.Free;
end;

procedure TGenComboFrm.HelpButtonClick(Sender: TObject);
begin
   if Assigned(OnHelpSelected) then
      OnHelpSelected
   else
      MessageDlg('No help avaliable!',mtWarning,[mbOK],0);
end;

initialization
    {$I neoGenComboForm.lrs}
end.
