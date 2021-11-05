//------------------------------------------------------------------------------
//
//  DD_FONT: Font Creator
//  Copyright (C) 2021 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Main Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/dd-font/
//------------------------------------------------------------------------------

unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Buttons, Clipbrd, ExtDlgs, Menus, ImgList, jpeg,
  StdCtrls, pngimage, xTGA, zBitmap, ff_undo, ff_filemenuhistory, ff_engine,
  ff_slider;

const
  MINZOOM = 0;
  MAXZOOM = 10;

const
  MOUSEWHEELTIMEOUT = 100; // Msecs until next mouse wheel even to be proccessed
  
type
  TForm1 = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    OpenSpeedButton1: TSpeedButton;
    SaveSpeedButton1: TSpeedButton;
    CopySpeedButton1: TSpeedButton;
    GridButton1: TSpeedButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    N2: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ToolPanel: TPanel;
    Timer1: TTimer;
    Panel4: TPanel;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    Panel6: TPanel;
    New1: TMenuItem;
    N3: TMenuItem;
    Save2: TMenuItem;
    Cut1: TMenuItem;
    HistoryItem0: TMenuItem;
    HistoryItem1: TMenuItem;
    HistoryItem2: TMenuItem;
    HistoryItem3: TMenuItem;
    HistoryItem4: TMenuItem;
    HistoryItem5: TMenuItem;
    HistoryItem6: TMenuItem;
    HistoryItem7: TMenuItem;
    HistoryItem8: TMenuItem;
    HistoryItem9: TMenuItem;
    N14: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N4: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    NewSpeedButton1: TSpeedButton;
    UndoSpeedButton1: TSpeedButton;
    RedoSpeedButton1: TSpeedButton;
    FontDialog1: TFontDialog;
    BoldSpeedButton: TSpeedButton;
    ItalicSpeedButton: TSpeedButton;
    UnderlineSpeedButton: TSpeedButton;
    StrikeOutSpeedButton: TSpeedButton;
    BackColorSpeedButton: TSpeedButton;
    FrontColorSpeedButton: TSpeedButton;
    SmallerSpeedButton: TSpeedButton;
    BiggerSpeedButton: TSpeedButton;
    SelectFontSpeedButton: TSpeedButton;
    FontNamesComboBox: TComboBox;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    FontSizeLabel: TLabel;
    ZoomInSpeedButton1: TSpeedButton;
    ZoomOutSpeedButton1: TSpeedButton;
    DrawWidthPaintBox: TPaintBox;
    DrawHeightPaintBox: TPaintBox;
    WidthInfoLabel: TLabel;
    SavePictureDialog1: TSavePictureDialog;
    Export1: TMenuItem;
    ExportImage1: TMenuItem;
    N5: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure New1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure BoldSpeedButtonClick(Sender: TObject);
    procedure ItalicSpeedButtonClick(Sender: TObject);
    procedure UnderlineSpeedButtonClick(Sender: TObject);
    procedure StrikeOutSpeedButtonClick(Sender: TObject);
    procedure FontNamesComboBoxClick(Sender: TObject);
    procedure SmallerSpeedButtonClick(Sender: TObject);
    procedure BiggerSpeedButtonClick(Sender: TObject);
    procedure BackColorSpeedButtonClick(Sender: TObject);
    procedure FrontColorSpeedButtonClick(Sender: TObject);
    procedure SelectFontSpeedButtonClick(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ZoomIn1Click(Sender: TObject);
    procedure ZoomOut1Click(Sender: TObject);
    procedure GridButton1Click(Sender: TObject);
    procedure ExportImage1Click(Sender: TObject);
  private
    { Private declarations }
    buffer: TBitmap;
    drawbuffer: TBitmap;
    mousedown: boolean;
    changed: boolean;
    needsupdate: boolean;
    undoManager: TUndoRedoManager;
    filemenuhistory: TFileMenuHistory;
    ffilename: string;
    ff: TFontEngine;
    zoom: integer;
    flastzoomwheel: int64;
    DrawWidthSlider: TSliderHook;
    DrawHeightSlider: TSliderHook;
    procedure Idle(Sender: TObject; var Done: Boolean);
    procedure Hint(Sender: TObject);
    procedure UpdateEnable(const force: Boolean = False);
    procedure InvalidatePaintBox;
    procedure PaintBox1Responer(const X, Y: Integer);
    procedure CreateDrawBuffer;
    procedure DoCreateNew;
    procedure DoLoadFromStream(const s: TStream);
    procedure DoSaveToStream(const s: TStream);
    procedure DoLoadUndo(const s: TStream);
    procedure DoSaveUndo(const s: TStream);
    function DoLoadFromFile(const aname: string): boolean;
    procedure DoSaveToFile(const aname: string);
    procedure OnLoadFileMenuHistory(Sender: TObject; const aname: string);
    function CheckCanClose: boolean;
    procedure SetFileName(const fname: string);
    procedure UpdateControls;
    procedure DrawGrid;
    procedure OnDrawWidthChange(Sender: TObject);
    procedure OnDrawHeightChange(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  ff_utils, ff_defs;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  S := TStrings(Data);
  Temp := LogFont.lfFaceName;
  if (S.Count = 0) or (AnsiCompareText(S[S.Count - 1], Temp) <> 0) then
    S.Add(Temp);
  Result := 1;
end;

procedure CollectFonts(FontList: TStringList);
var
  DC: HDC;
  LFont: TLogFont;
begin
  DC := GetDC(0);
  FillChar(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, LPARAM(FontList), 0);
  ReleaseDC(0, DC);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  doCreate: boolean;
  fList: TStringList;
begin
  DoubleBuffered := True;
  for i := 0 to ComponentCount - 1 do
    if Components[i].InheritsFrom(TWinControl) then
      if not (Components[i] is TListBox) then
        (Components[i] as TWinControl).DoubleBuffered := True;

  buffer := TBitmap.Create;
  drawbuffer := TBitmap.Create;

  flastzoomwheel := GetTickCount;

  ff := TFontEngine.Create;

  DrawWidthSlider := TSliderHook.Create(DrawWidthPaintBox);
  DrawWidthSlider.Min := 5.0;
  DrawWidthSlider.Max := 25.0;
  DrawWidthSlider.Step := 1.0;
  DrawWidthSlider.PageStep := 5.0;
  DrawWidthSlider.OnSliderHookChange := OnDrawWidthChange;

  DrawHeightSlider := TSliderHook.Create(DrawHeightPaintBox);
  DrawHeightSlider.Min := 5.0;
  DrawHeightSlider.Max := 25.0;
  DrawHeightSlider.Step := 1.0;
  DrawHeightSlider.PageStep := 5.0;
  DrawHeightSlider.OnSliderHookChange := OnDrawHeightChange;

  mousedown := False;

  ff_LoadSettingFromFile(ChangeFileExt(ParamStr(0), '.ini'));

  undoManager := TUndoRedoManager.Create;
  undoManager.OnLoadFromStream := DoLoadUndo;
  undoManager.OnSaveToStream := DoSaveUndo;

  filemenuhistory := TFileMenuHistory.Create(self);
  filemenuhistory.MenuItem0 := HistoryItem0;
  filemenuhistory.MenuItem1 := HistoryItem1;
  filemenuhistory.MenuItem2 := HistoryItem2;
  filemenuhistory.MenuItem3 := HistoryItem3;
  filemenuhistory.MenuItem4 := HistoryItem4;
  filemenuhistory.MenuItem5 := HistoryItem5;
  filemenuhistory.MenuItem6 := HistoryItem6;
  filemenuhistory.MenuItem7 := HistoryItem7;
  filemenuhistory.MenuItem8 := HistoryItem8;
  filemenuhistory.MenuItem9 := HistoryItem9;
  filemenuhistory.OnOpen := OnLoadFileMenuHistory;

  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory9));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory8));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory7));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory6));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory5));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory4));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory3));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory2));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory1));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory0));

  ffilename := '';

  GridButton1.Down := opt_showgrid;
  zoom := GetIntInRange(opt_zoom, MINZOOM, MAXZOOM);

  DrawWidthSlider.Position := opt_DrawWidth;
  DrawHeightSlider.Position := opt_DrawHeight;

  fList := TStringList.Create;
  CollectFonts(fList);
  for i := 0 to fList.Count -1 do
    FontNamesComboBox.Items.Add(FList[i]);
  fList.Free;

  doCreate := True;
  if ParamCount > 0 then
    if DoLoadFromFile(ParamStr(1)) then
      doCreate := False;

  if docreate then
  begin
    SetFileName('');
    changed := False;
    needsupdate := True;
    undoManager.Clear;
  end
  else
    DoCreateNew;

  Application.OnIdle := Idle;
  Application.OnHint := Hint;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, drawbuffer);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  undoManager.Free;

  stringtobigstring(filemenuhistory.PathStringIdx(0), @opt_filemenuhistory0);
  stringtobigstring(filemenuhistory.PathStringIdx(1), @opt_filemenuhistory1);
  stringtobigstring(filemenuhistory.PathStringIdx(2), @opt_filemenuhistory2);
  stringtobigstring(filemenuhistory.PathStringIdx(3), @opt_filemenuhistory3);
  stringtobigstring(filemenuhistory.PathStringIdx(4), @opt_filemenuhistory4);
  stringtobigstring(filemenuhistory.PathStringIdx(5), @opt_filemenuhistory5);
  stringtobigstring(filemenuhistory.PathStringIdx(6), @opt_filemenuhistory6);
  stringtobigstring(filemenuhistory.PathStringIdx(7), @opt_filemenuhistory7);
  stringtobigstring(filemenuhistory.PathStringIdx(8), @opt_filemenuhistory8);
  stringtobigstring(filemenuhistory.PathStringIdx(9), @opt_filemenuhistory9);
  opt_showgrid := GridButton1.Down;
  opt_zoom := zoom;
  opt_DrawWidth := Round(DrawWidthSlider.Position);
  opt_DrawHeight := Round(DrawHeightSlider.Position);

  ff_SaveSettingsToFile(ChangeFileExt(ParamStr(0), '.ini'));

  filemenuhistory.Free;

  buffer.Free;
  drawbuffer.Free;

  DrawWidthSlider.Free;
  DrawHeightSlider.Free;

  ff.Free;
end;

procedure TForm1.Idle(Sender: TObject; var Done: Boolean);
begin
  UpdateEnable;
end;

procedure TForm1.UpdateEnable;
begin
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
  UndoSpeedButton1.Enabled := undoManager.CanUndo;
  RedoSpeedButton1.Enabled := undoManager.CanRedo;
  ZoomInSpeedButton1.Enabled := zoom < MAXZOOM;
  ZoomOutSpeedButton1.Enabled := zoom > MINZOOM;
  if needsupdate then
  begin
    UpdateControls;
    InvalidatePaintBox;
    needsupdate := False;
  end;
end;

procedure TForm1.Hint(Sender: TObject);
begin
  StatusBar1.SimpleText := Application.Hint;
end;

resourcestring
  rsTitle = 'Font Creator';

procedure TForm1.About1Click(Sender: TObject);
begin
  MessageBox(
    Handle,
    PChar(Format('%s'#13#10'Version %s'#13#10#13#10'A tool for creating font images.'#13#10'© 2021, jvalavanis@gmail.com', [rsTitle, I_VersionBuilt])),
    PChar(rsTitle),
    MB_OK or MB_ICONINFORMATION or MB_APPLMODAL);
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
  Clipboard.Assign(buffer);
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  if OpenDialog1.Execute then
    DoLoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.InvalidatePaintBox;
begin
  ff.DrawToBitmap(buffer);
  CreateDrawBuffer;
  PaintBox1.Width := drawbuffer.Width;
  PaintBox1.Height := drawbuffer.Height;
  PaintBox1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateEnable;
  InvalidatePaintBox;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mousedown := Button in [mbLeft];
  PaintBox1Responer(X, Y);
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mousedown := False;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  PaintBox1Responer(X, Y);
end;

procedure TForm1.PaintBox1Responer(const X, Y: Integer);
begin
  if mousedown then
  begin
    InvalidatePaintBox;
  end;
end;

procedure TForm1.CreateDrawBuffer;
begin
  drawbuffer.Width := Round(buffer.Width * (1 + 3 * zoom / MAXZOOM));
  drawbuffer.Height := Round(buffer.Height * (1 + 3 * zoom / MAXZOOM));

  drawbuffer.Canvas.StretchDraw(Rect(0, 0, drawbuffer.Width, drawbuffer.Height), buffer);
  DrawGrid;
end;

procedure TForm1.Undo1Click(Sender: TObject);
begin
  if undoManager.CanUndo then
    undoManager.Undo;
end;

procedure TForm1.Redo1Click(Sender: TObject);
begin
  if undoManager.CanRedo then
    undoManager.Redo;
end;

procedure TForm1.File1Click(Sender: TObject);
begin
  filemenuhistory.RefreshMenuItems;
end;

procedure TForm1.DoCreateNew;
begin
  undoManager.Clear;
  SetFileName('');
  ff.Reset;
  needsupdate := True;
  changed := False;
end;

procedure TForm1.DoLoadFromStream(const s: TStream);
begin
  ff.LoadFromStream(s);
  needsupdate := True;
end;

procedure TForm1.DoSaveToStream(const s: TStream);
begin
  ff.SaveToStream(s);
end;

procedure TForm1.DoLoadUndo(const s: TStream);
begin
  DoLoadFromStream(s);
  changed := True;
end;

procedure TForm1.DoSaveUndo(const s: TStream);
begin
  DoSaveToStream(s);
end;

function TForm1.DoLoadFromFile(const aname: string): boolean;
var
  fs: TFileStream;
begin
  if not FileExists(aname) then
  begin
    Result := False;
    Exit;
  end;

  undoManager.Clear;
  Result := True;
  fs := TFileStream.Create(aname, fmOpenRead);
  try
    DoLoadFromStream(fs);
    SetFileName(aname);
    filemenuhistory.AddPath(aname);
    changed := False;
  finally
    fs.Free;
  end;
end;

procedure TForm1.DoSaveToFile(const aname: string);
var
  fs: TFileStream;
begin
  BackupFile(aname);
  fs := TFileStream.Create(aname, fmCreate);
  try
    DoSaveToStream(fs);
    SetFileName(aname);
    filemenuhistory.AddPath(aname);
    changed := False;
  finally
    fs.Free;
  end;
end;

procedure TForm1.OnLoadFileMenuHistory(Sender: TObject; const aname: string);
begin
  if CheckCanClose then
    DoLoadFromFile(aname);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckCanClose;
end;

function TForm1.CheckCanClose: boolean;
var
  ret: integer;
begin
  if changed then
  begin
    ret := MessageBox(Handle, 'Do you want to save changes?', PChar(rsTitle), MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL);
    if ret = IDCANCEL	then
    begin
      Result := False;
      exit;
    end;
    if ret = IDNO	then
    begin
      Result := True;
      exit;
    end;
    if ret = IDYES then
    begin
      Save1Click(self);
      Result := not changed;
      exit;
    end;
  end;
  Result := True;
end;

procedure TForm1.New1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  DoCreateNew;
end;

procedure TForm1.SetFileName(const fname: string);
begin
  ffilename := fname;
  Caption := rsTitle;
  if ffilename <> '' then
    Caption := Caption + ' - ' + MkShortName(ffilename);
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if ffilename = '' then
  begin
    SaveAs1Click(Sender);
    Exit;
  end;

  DoSaveToFile(ffilename);
end;

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    DoSaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.UpdateControls;
begin
  BoldSpeedButton.Down := fsBold in ff.Style;
  ItalicSpeedButton.Down := fsItalic in ff.Style;
  UnderlineSpeedButton.Down := fsUnderline in ff.Style;
  StrikeOutSpeedButton.Down := fsStrikeOut in ff.Style;
  FontNamesComboBox.ItemIndex := FontNamesComboBox.Items.IndexOf(ff.FontName);
  FontSizeLabel.Caption := IntToStr(ff.FontSize);
  WidthInfoLabel.Caption := Format('Image Size: %dx%d   Character Size: %dx%d',
    [
      ff.GridWidth * ff.DrawWidth,
      ff.GridHeight * ff.DrawHeight,
      ff.DrawWidth,
      ff.DrawHeight
      ]
    );
  DrawWidthSlider.Position := ff.DrawWidth;
  DrawHeightSlider.Position := ff.DrawHeight;
end;

procedure TForm1.BoldSpeedButtonClick(Sender: TObject);
var
  stl: TFontStylesBase;
begin
  stl := ff.Style;
  if BoldSpeedButton.Down then
    Include(stl, fsBold)
  else
    Exclude(stl, fsBold);
  if stl <> ff.Style then
  begin
    undoManager.SaveUndo;
    changed := True;
    ff.Style := stl;
    needsupdate := True;
  end;
end;

procedure TForm1.ItalicSpeedButtonClick(Sender: TObject);
var
  stl: TFontStylesBase;
begin
  stl := ff.Style;
  if ItalicSpeedButton.Down then
    Include(stl, fsItalic)
  else
    Exclude(stl, fsItalic);
  if stl <> ff.Style then
  begin
    undoManager.SaveUndo;
    changed := True;
    ff.Style := stl;
    needsupdate := True;
  end;
end;

procedure TForm1.UnderlineSpeedButtonClick(Sender: TObject);
var
  stl: TFontStylesBase;
begin
  stl := ff.Style;
  if UnderlineSpeedButton.Down then
    Include(stl, fsUnderline)
  else
    Exclude(stl, fsUnderline);
  if stl <> ff.Style then
  begin
    undoManager.SaveUndo;
    changed := True;
    ff.Style := stl;
    needsupdate := True;
  end;
end;

procedure TForm1.StrikeOutSpeedButtonClick(Sender: TObject);
var
  stl: TFontStylesBase;
begin
  stl := ff.Style;
  if StrikeOutSpeedButton.Down then
    Include(stl, fsStrikeOut)
  else
    Exclude(stl, fsStrikeOut);
  if stl <> ff.Style then
  begin
    undoManager.SaveUndo;
    changed := True;
    ff.Style := stl;
    needsupdate := True;
  end;
end;

procedure TForm1.FontNamesComboBoxClick(Sender: TObject);
begin
  if FontNamesComboBox.Items.IndexOf(FontNamesComboBox.Text) >= 0 then
  begin
    if FontNamesComboBox.Text <> ff.FontName then
    begin
      undoManager.SaveUndo;
      changed := True;
      ff.FontName := FontNamesComboBox.Text;
      needsupdate := True;
    end;
  end;
end;

const
  MINFONTSIZE = 4;
  MAXFONTSIZE = 96;

procedure TForm1.SmallerSpeedButtonClick(Sender: TObject);
begin
  if ff.FontSize > MINFONTSIZE then
  begin
    undoManager.SaveUndo;
    changed := True;

    if ff.FontSize < 21 then
      ff.FontSize := ff.FontSize - 1
    else if ff.FontSize < 24 then
      ff.FontSize := 20
    else
      ff.FontSize := ff.FontSize - 4;

    needsupdate := True;
  end;
end;

procedure TForm1.BiggerSpeedButtonClick(Sender: TObject);
begin
  if ff.FontSize < MAXFONTSIZE then
  begin
    undoManager.SaveUndo;
    changed := True;

    if ff.FontSize > 19 then
    begin
      ff.FontSize := ff.FontSize + 4;
      if ff.FontSize > MAXFONTSIZE then
        ff.FontSize := MAXFONTSIZE;
    end
    else
      ff.FontSize := ff.FontSize + 1;

    needsupdate := True;
  end;
end;

procedure TForm1.BackColorSpeedButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := ff.BackColor;
  if ColorDialog1.Execute then
    if ColorDialog1.Color <> ff.BackColor then
    begin
      undoManager.SaveUndo;
      changed := True;
      ff.BackColor := ColorDialog1.Color;
      needsupdate := True;
    end;
end;

procedure TForm1.FrontColorSpeedButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := ff.FrontColor;
  if ColorDialog1.Execute then
    if ColorDialog1.Color <> ff.FrontColor then
    begin
      undoManager.SaveUndo;
      changed := True;
      ff.FrontColor := ColorDialog1.Color;
      needsupdate := True;
    end;
end;

procedure TForm1.SelectFontSpeedButtonClick(Sender: TObject);
begin
  ff.ToFont(FontDialog1.Font);
  if FontDialog1.Execute then
  begin
    undoManager.SaveUndo;
    changed := True;
    ff.FromFont(FontDialog1.Font);
    needsupdate := True;
  end;
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
  tick: int64;
begin
  tick := GetTickCount;
  if tick <= flastzoomwheel + MOUSEWHEELTIMEOUT then
    Exit;
  flastzoomwheel := tick;
  pt := PaintBox1.Parent.ScreenToClient(MousePos);
  r := PaintBox1.ClientRect;
  if r.Right > ScrollBox1.Width then
    r.Right := ScrollBox1.Width;
  if r.Bottom > ScrollBox1.Height then
    r.Bottom := ScrollBox1.Height;
  if PtInRect(r, pt) then
    ZoomOut1Click(Sender);
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
  tick: int64;
begin
  tick := GetTickCount;
  if tick <= flastzoomwheel + MOUSEWHEELTIMEOUT then
    Exit;
  flastzoomwheel := tick;
  pt := PaintBox1.Parent.ScreenToClient(MousePos);
  r := PaintBox1.ClientRect;
  if r.Right > ScrollBox1.Width then
    r.Right := ScrollBox1.Width;
  if r.Bottom > ScrollBox1.Height then
    r.Bottom := ScrollBox1.Height;
  if PtInRect(r, pt) then
    ZoomIn1Click(Sender);
end;

procedure TForm1.ZoomIn1Click(Sender: TObject);
begin
  if zoom < MAXZOOM then
  begin
    inc(zoom);
    needsupdate := True;
  end;
end;

procedure TForm1.ZoomOut1Click(Sender: TObject);
begin
  if zoom > MINZOOM then
  begin
    dec(zoom);
    needsupdate := True;
  end;
end;

procedure TForm1.GridButton1Click(Sender: TObject);
begin
  InvalidatePaintBox;
end;

procedure TForm1.DrawGrid;
var
  x, y: integer;
  stepw, steph: double;
begin
  if GridButton1.Down then
  begin
    drawbuffer.Canvas.Pen.Style := psSolid;
    drawbuffer.Canvas.Pen.Color := RGB(160, 160, 128);
    stepw := drawbuffer.Width / ff.GridWidth;

    for x := 1 to ff.GridWidth - 1 do
    begin
      drawbuffer.Canvas.MoveTo(Round(x * stepw) - 1, 0);
      drawbuffer.Canvas.LineTo(Round(x * stepw) - 1, drawbuffer.Height);
    end;

    steph := drawbuffer.Height / ff.GridHeight;
    for y := 1 to ff.GridHeight - 1 do
    begin
      drawbuffer.Canvas.MoveTo(0, Round(y * steph) - 1);
      drawbuffer.Canvas.LineTo(drawbuffer.Width, Round(y * steph) - 1);
    end;
  end;
end;

procedure TForm1.OnDrawWidthChange(Sender: TObject);
var
  w: integer;
begin
  w := Round(DrawWidthSlider.Position);
  if w <> ff.DrawWidth then
  begin
    undoManager.SaveUndo;
    changed := True;
    ff.DrawWidth := w;
    needsupdate := True;
  end;
end;

procedure TForm1.OnDrawHeightChange(Sender: TObject);
var
  h: integer;
begin
  h := Round(DrawHeightSlider.Position);
  if h <> ff.DrawHeight then
  begin
    undoManager.SaveUndo;
    changed := True;
    ff.DrawHeight := h;
    needsupdate := True;
  end;
end;

procedure TForm1.ExportImage1Click(Sender: TObject);
var
  imgfname: string;
begin
  if SavePictureDialog1.Execute then
  begin
    Screen.Cursor := crHourglass;
    try
      imgfname := SavePictureDialog1.FileName;
      BackupFile(imgfname);
      ff.DrawToBitmap(buffer);  // Get fresh copy
      SaveImageToDisk(buffer, imgfname);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

end.

