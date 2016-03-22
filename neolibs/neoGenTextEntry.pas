unit neoGenTextEntry;

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
  TGenTextEdit = class(TForm)
    EditBox: TEdit;
    LabelText: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure EditBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OnHelpSelected : THelpProc;

    constructor Create(AOwner : TComponent); override;

    function  Execute : integer;
    procedure ShowHelpButton;
    procedure HideHelpButton;
    procedure ArrangeButtons;
    procedure DisableHelp;
    procedure EnableHelp(x : THelpProc);
  end;

var
  GenTextEdit: TGenTextEdit;

implementation

//{$R *.DFM} changed for lazarus:


// **********************************************************************
// Create method
// **********************************************************************
constructor TGenTextEdit.Create(AOwner : TComponent);
begin
   inherited Create(AOwner);

   OnHelpSelected := nil;
end;

// **********************************************************************
// Show the box
// **********************************************************************
function TGenTextEdit.Execute: integer;
begin
  Execute := ShowModal;
end;

// **********************************************************************
// OK Button
// **********************************************************************
procedure TGenTextEdit.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

// **********************************************************************
// Cancel Button
// **********************************************************************
procedure TGenTextEdit.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

// **********************************************************************
// Key Down handler
// **********************************************************************
procedure TGenTextEdit.EditBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if Key = KEY_ENTER then ModalResult := mrOK;
   if Key = KEY_ESC   then ModalResult := mrCancel;
end;

// **********************************************************************
// On show event handler - to set focus to edit box
// **********************************************************************
procedure TGenTextEdit.FormShow(Sender: TObject);
begin
   EditBox.SetFocus;
   EditBox.SelectAll;
end;

// **********************************************************************
// make the help button visible
// **********************************************************************
procedure TGenTextEdit.ShowHelpButton;
begin
   HelpButton.Visible := true;
   ArrangeButtons;
end;

// **********************************************************************
// hide the help button
// **********************************************************************
procedure TGenTextEdit.HideHelpButton;
begin
   HelpButton.Visible := False;
   ArrangeButtons;
end;

// **********************************************************************
// Arrange buttons
// **********************************************************************
procedure TGenTextEdit.ArrangeButtons;
var
   ButtonList : TButtonList;
begin
   ButtonList := TButtonList.Create;
   ButtonList.Add(OKButton);
   ButtonList.Add(CancelButton);
   if HelpButton.Visible then
      ButtonList.Add(HelpButton);

   SpreadButtons(self,ButtonList,20);

   ButtonList.Free;
end;

// **********************************************************************
// On Help button
// **********************************************************************
procedure TGenTextEdit.HelpButtonClick(Sender: TObject);
begin
   // ??????? you will have to fix this help function to be inline with the TApplication help!

   if Assigned(OnHelpSelected) then
      OnHelpSelected
   else
      MessageDlg('No help avaliable!',mtWarning,[mbOK],0);

end;

// **********************************************************************
// Disable help
// **********************************************************************
procedure TGenTextEdit.DisableHelp;
begin
   OnHelpSelected := nil;
   HideHelpButton;
end;

// **********************************************************************
// Enable help
// **********************************************************************
procedure TGenTextEdit.EnableHelp(x : THelpProc);
begin
   OnHelpSelected := x;

   ShowHelpButton;
end;

initialization
    {$I neoGenTextEntry.LRS}
end.
