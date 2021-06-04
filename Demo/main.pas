unit main;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX}

  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.StrUtils, System.Variants, System.Math, System.Character,
  System.NetEncoding, System.DateUtils,

  GerenciaPago, {DBXJSON,} System.JSON,

  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Stan.StorageBin,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMXDelphiZXingQRCode,
  FMX.Objects, FMX.TabControl, FMX.WebBrowser, FMX.ListBox,
  FMX.DateTimeCtrls, FMX.Platform

  {$IFDEF ANDROID}
  ,Androidapi.JNI.Provider //TJSettings_Secure GPS
  ,Androidapi.JNI.OS
  ,Androidapi.JNI.App
  ,Androidapi.JNIBridge
  ,Androidapi.JNI.GraphicsContentViewText // JIntent
  ,Androidapi.JNI.JavaTypes
  ,Androidapi.Helpers
  ,Androidapi.JNI.Location  // Sensor de Localização
  ,Androidapi.JNI.Telephony // Telefone Número
  {$ENDIF}
  ;

type
  TForm2 = class(TForm)
    Layout01: TLayout;
    Layout02: TLayout;
    Layout03: TLayout;
    Layout04: TLayout;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Layout7: TLayout;
    Label5: TLabel;
    Layout8: TLayout;
    Label6: TLabel;
    Layout9: TLayout;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Layout10: TLayout;
    QRCodeBitmap: TImage;
    Layout12: TLayout;
    Memo2: TMemo;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Layout13: TLayout;
    Label11: TLabel;
    Edit5: TEdit;
    Button6: TButton;
    Layout14: TLayout;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    TabItem5: TTabItem;
    Layout16: TLayout;
    Button8: TButton;
    Label15: TLabel;
    TabItem6: TTabItem;
    Layout17: TLayout;
    Button2: TButton;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    GerenciaPago1: TGerenciaPago;
    Layout05: TLayout;
    CheckBox1: TCheckBox;
    Button3: TButton;
    Layout1: TLayout;
    TabItem7: TTabItem;
    Layout2: TLayout;
    Button5: TButton;
    Button9: TButton;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout6: TLayout;
    Layout18: TLayout;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    ListBoxItem13: TListBoxItem;
    ListBoxItem14: TListBoxItem;
    ListBoxItem15: TListBoxItem;
    ListBoxItem16: TListBoxItem;
    EditVenc: TDateEdit;
    EditValor: TEdit;
    EditPedido: TEdit;
    EditDescri: TEdit;
    EditPgto: TComboBox;
    EditEmail: TEdit;
    EditNome: TEdit;
    EditSobre: TEdit;
    EditDCN: TEdit;
    EditCep: TEdit;
    EditEnd: TEdit;
    EditNumero: TEdit;
    EditBairro: TEdit;
    EditCidade: TEdit;
    EditEstado: TEdit;
    EditDoc: TComboBox;
    Layout5: TLayout;
    Button1: TButton;
    Edit6: TEdit;
    TabControl2: TTabControl;
    TabItem4: TTabItem;
    TabItem8: TTabItem;
    Layout11: TLayout;
    BoxTipo: TComboBox;
    procedure GerenciaPago1Status(const Value: string);
    procedure GerenciaPago1Saldo(const Value: Double);
    procedure GerenciaPago1Error(const Value: string);
    procedure GerenciaPago1BoletoLink(const Value: string);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure GerenciaPago1PixCopiar(const Value: string);
    procedure GerenciaPago1PixBase64(const Value: WideString);
  private
    { Private declarations }
    procedure QrCodeMobile(imgQRCode: TImage; texto: string);
    procedure QRCodeWin(imgQRCode: TImage; texto: string);
    procedure m( s: String );
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

{function OpenURL(const URL: string; const DisplayError: Boolean = False): Boolean;
var
  Intent: JIntent;
begin
// There may be an issue with the geo: prefix and URLEncode.
// will need to research
  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW,
    TJnet_Uri.JavaClass.parse(StringToJString(URL)));
  try
    TAndroidHelper.Activity.startActivity(Intent);
    exit(true);
  except
    on e: Exception do
    begin
      if DisplayError then ShowMessage('Error: ' + e.Message);
      exit(false);
    end;
  end;
end;}

function getCamposJsonString(json,value:String): String;
var
   LJSONObject: TJSONObject;
   function TrataObjeto(jObj:TJSONObject):string;
   var i:integer;
       jPar: TJSONPair;
   begin
        result := '';
        for i := 0 to jObj.Size - 1 do
        begin
             jPar := jObj.Get(i);
             if jPar.JsonValue Is TJSONObject then
                result := TrataObjeto((jPar.JsonValue As TJSONObject)) else
             if sametext(trim(jPar.JsonString.Value),value) then
             begin
                  Result := jPar.JsonValue.Value;
                  break;
             end;
             if result <> '' then
                break;
        end;
   end;
begin
   try
      LJSONObject := nil;
      LJSONObject := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(json),0) as TJSONObject;
      result := TrataObjeto(LJSONObject);
   finally
      LJSONObject.Free;
   end;
end;

function StripNonJson(s: string): string;
var
  ch: char;
  inString: boolean;
begin
  Result := '';
  inString := false;
  for ch in s do
  begin
    if ch = '"' then
      inString := not inString;
    if TCharacter.IsWhiteSpace(ch) and not inString then
      continue;
    Result := Result + ch;
  end;
end;

function JsonSinComillas(s: String): String;
begin
  result := StringReplace( s, '"','', [rfReplaceAll] );
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  GerenciaPago1.CopiaColaPix;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  with GerenciaPago1.LancarDados do begin
    Vencimento      := Editvenc.Text;
    Valor           := EditValor.Text;
    PedidoNumero    := EditPedido.Text;
    Descricao       := EditDescri.Text;

    case EditPgto.ItemIndex of
     0: MeioPagar   := TPgto.Boleto;
     1: MeioPagar   := TPgto.Loterica;
     2: MeioPagar   := TPgto.Pix;
    end;

    Email           := EditEmail.Text;
    Nome            := EditNome.Text;
    SobreNome       := EditSobre.Text;
    Doc             := EditDoc.Selected.Text;
    DocNumero       := EditDCN.Text;
    Cep             := EditCep.Text;
    Endereco        := EditEnd.Text;
    Numero          := EditNumero.Text;
    Bairro          := EditBairro.Text;
    Cidade          := EditCidade.Text;
    Estado          := EditEstado.Text;
  end;

  GerenciaPago1.GerarBoleto;

  if GerenciaPago1.LancarDados.MeioPagar = TPgto.Pix then
  Button1.Enabled := True else
  Button1.Enabled := False;

  //m(GerenciaPago1.MemoRetono);
  m(GerenciaPago1.RetornaID);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  if not GerenciaPago1.Ativo then
  begin
    with GerenciaPago1 do begin
      Ativo := True;
      ConfigKey.PublicKey      := Edit1.Text;
      ConfigKey.AccessToken    := Edit2.Text;
      ConfigKey.Cliente_id     := Edit3.Text;
      ConfigKey.Cliente_Secret := Edit4.Text;
      ConfigKey.SandBox        := CheckBox1.IsChecked;
    end;

    {lbStatus.Text        := MP.lbStatus;
    MemoCore.Lines.Text   := MP.MemoRetono;
    MemoError.Lines.Text  := MP.MemoError;}
    Button3.Text := 'Stop';
  end
  else
  begin
    GerenciaPago1.Ativo := False;
    Button3.Text  := 'INICIAR';
  end;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  GerenciaPago1.RetornaConta;

  Label16.Text := 'UserID: '   + GerenciaPago1.DadosConta.ID;
  Label17.Text := 'Usuário: '  + GerenciaPago1.DadosConta.nickname;
  Label18.Text := 'Name: '     + GerenciaPago1.DadosConta.first_name+' '+GerenciaPago1.DadosConta.last_name;
  Label19.Text := 'Email: '    + GerenciaPago1.DadosConta.email;
end;

function getData2(JsonString: String; User: String; Field: String): String;
var
  JSonValue: TJSonValue;
  JsonArray: TJSONArray;
  ArrayElement: TJSonValue;
  FoundValue: TJSonValue;
begin
  Result :='';

  // create TJSonObject from string
  JsonValue := TJSonObject.ParseJSONValue(JsonString);

  // get the array
  JsonArray := JsonValue as TJSONArray;

  // iterate the array
  for ArrayElement in JsonArray do begin
      FoundValue := ArrayElement.FindValue(User);
      if FoundValue <> nil then begin
        Result := ArrayElement.GetValue<string>(User + '.' + Field);
        break;
      end;
  end;
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  case BoxTipo.ItemIndex of
    0: begin
        GerenciaPago1.BuscarPedido(Edit5.Text);
        Label12.Text := 'Status: '+ GerenciaPago1.RetConsulta.RetStatus;
        Label13.Text := 'Creditado: '+ GerenciaPago1.RetConsulta.RetDataCredt;
       end;
    1: begin
        GerenciaPago1.CancelarPedido(Edit5.Text);
       end;
  end;

  m( GerenciaPago1.MemoRetono );
end;

procedure TForm2.Button8Click(Sender: TObject);
begin
  GerenciaPago1.ConfigKey.PublicKey      := Edit1.Text;
  GerenciaPago1.ConfigKey.AccessToken    := Edit2.Text;
  GerenciaPago1.ConfigKey.Cliente_id     := Edit3.Text;
  GerenciaPago1.ConfigKey.Cliente_Secret := Edit4.Text;

  GerenciaPago1.SaldoConta;
end;

procedure TForm2.Button9Click(Sender: TObject);
var
  Base64 : WideString;
  Json   : TJSONObject;
begin
  // PEGANDO SO O RESULTS E REMOVENDO []
  {Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(Memo1.Text)),0));
  Base64 := ( TJSONObject(Json.Get('results').JsonValue).ToString );
  Memo1.Lines.Clear;
  Memo1.Lines.Add(Base64);

  Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(Memo1.Text)),0));
  Base64 := ( TJSONObject(Json.Get('point_of_interaction').JsonValue).ToString );
  Base64 :=  getCamposJsonString(Base64, 'qr_code');
  Memo2.Lines.Add(Base64);
  Base64 := ( TJSONObject(Json.Get('point_of_interaction').JsonValue).ToString );
  Base64 :=  getCamposJsonString(Base64, 'qr_code_base64');
  Memo2.Lines.Add(Base64);
  Base64 :=  getCamposJsonString(Memo1.Text, 'status');
  Memo2.Lines.Add(Base64);}
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  TabControl1.TabIndex    := 0;
  TabControl1.OnChange(Sender);
end;

procedure TForm2.GerenciaPago1BoletoLink(const Value: string);
{$IF Defined(ANDROID)}
var
  Intent: JIntent;
{$ENDIF}
begin
  {$IF Defined(ANDROID)}
    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
    Intent.setData(StrToJURI(Value));
    tandroidhelper.Activity.startActivity(Intent);
    // SharedActivity.startActivity(Intent);
  {$ELSEIF Defined(MSWINDOWS)}
    ShellExecute(0, 'OPEN', PChar(Value), '', '', SW_SHOWNORMAL);
  {$ELSEIF Defined(IOS)}
    SharedApplication.OpenURL(StrToNSUrl(Value));
  {$ELSEIF Defined(MACOS)}
    _system(PAnsiChar('open ' + AnsiString(Value)));
  {$ENDIF}
end;

procedure TForm2.GerenciaPago1Error(const Value: string);
begin
  Label7.Text := Value;
end;

procedure TForm2.GerenciaPago1PixBase64(const Value: WideString);
begin
  m(Value);
end;

procedure TForm2.GerenciaPago1PixCopiar(const Value: string);
begin
  Edit6.Text  := Value;

  {$IFDEF MSWINDOWS}
    QRCodeWin(QRCodeBitmap, Value);
  {$ELSE}
    QRCodeMobile(QRCodeBitmap, Value);
  {$ENDIF}
end;

procedure TForm2.GerenciaPago1Saldo(const Value: Double);
begin
  Label6.Text := 'Saldo em conta é de: '+Value.ToString;
end;

procedure TForm2.GerenciaPago1Status(const Value: string);
begin
  Label5.Text := Value;
end;

procedure TForm2.m(s: String);
begin
  //Memo2.Lines.Clear;
  Memo2.Lines.Add(s);
end;

procedure TForm2.QrCodeMobile(imgQRCode: TImage; texto: string);
const
    downsizeQuality: Integer = 2; // bigger value, better quality, slower rendering
var
    QRCode: TDelphiZXingQRCode;
    Row, Column: Integer;
    pixelColor : TAlphaColor;
    vBitMapData : TBitmapData;
    pixelCount, y, x: Integer;
    columnPixel, rowPixel: Integer;

    function GetPixelCount(AWidth, AHeight: Single): Integer;
    begin
        if QRCode.Rows > 0 then
          Result := Trunc(Min(AWidth, AHeight)) div QRCode.Rows
        else
          Result := 0;
    end;
begin
    // Not a good idea to stretch the QR Code...
    if imgQRCode.WrapMode = TImageWrapMode.iwStretch then
        imgQRCode.WrapMode := TImageWrapMode.Fit;


    QRCode := TDelphiZXingQRCode.Create;

    try
        QRCode.Data := '  ' + texto;
        QRCode.Encoding := TQRCodeEncoding.qrAuto;
        QRCode.QuietZone := 4;
        pixelCount := GetPixelCount(imgQRCode.Width, imgQRCode.Height);

        case imgQRCode.WrapMode of
            TImageWrapMode.iwOriginal,
            TImageWrapMode.iwTile,
            TImageWrapMode.iwCenter:
            begin
                if pixelCount > 0 then
                    imgQRCode.Bitmap.SetSize(QRCode.Columns * pixelCount,
                    QRCode.Rows * pixelCount);
            end;

            TImageWrapMode.iwFit:
            begin
                if pixelCount > 0 then
                begin
                    imgQRCode.Bitmap.SetSize(QRCode.Columns * pixelCount * downsizeQuality,
                        QRCode.Rows * pixelCount * downsizeQuality);
                    pixelCount := pixelCount * downsizeQuality;
                end;
            end;

            //TImageWrapMode.iwStretch:
            //    raise Exception.Create('Not a good idea to stretch the QR Code');
        end;
        if imgQRCode.Bitmap.Canvas.BeginScene then
        begin
            try
                imgQRCode.Bitmap.Canvas.Clear(TAlphaColors.White);
                if pixelCount > 0 then
                begin
                      if imgQRCode.Bitmap.Map(TMapAccess.maWrite, vBitMapData)  then
                      begin
                            try
                                 For Row := 0 to QRCode.Rows - 1 do
                                 begin
                                    for Column := 0 to QRCode.Columns - 1 do
                                    begin
                                        if (QRCode.IsBlack[Row, Column]) then
                                            pixelColor := TAlphaColors.Black
                                        else
                                            pixelColor := TAlphaColors.White;

                                        columnPixel := Column * pixelCount;
                                        rowPixel := Row * pixelCount;

                                        for x := 0 to pixelCount - 1 do
                                            for y := 0 to pixelCount - 1 do
                                                vBitMapData.SetPixel(columnPixel + x,
                                                    rowPixel + y, pixelColor);
                                    end;
                                 end;
                            finally
                              imgQRCode.Bitmap.Unmap(vBitMapData);
                            end;
                      end;
                end;
            finally
                imgQRCode.Bitmap.Canvas.EndScene;
          end;
        end;
    finally
        QRCode.Free;
    end;
end;

procedure TForm2.QRCodeWin(imgQRCode: TImage; texto: string);
var
  QRCode: TDelphiZXingQRCode;
  Row, Column: Integer;
  pixelColor : TAlphaColor;
  vBitMapData : TBitmapData;
begin
    imgQRCode.DisableInterpolation := true;
    imgQRCode.WrapMode := TImageWrapMode.iwStretch;

    QRCode := TDelphiZXingQRCode.Create;
    try
        QRCode.Data := texto;
        QRCode.Encoding := TQRCodeEncoding.qrAuto;
        QRCode.QuietZone := 4;
        imgQRCode.Bitmap.SetSize(QRCode.Rows, QRCode.Columns);

        for Row := 0 to QRCode.Rows - 1 do
        begin
            for Column := 0 to QRCode.Columns - 1 do
            begin
                if (QRCode.IsBlack[Row, Column]) then
                    pixelColor := TAlphaColors.Black
                else
                    pixelColor := TAlphaColors.White;

                if imgQRCode.Bitmap.Map(TMapAccess.maWrite, vBitMapData)  then
                try
                    vBitMapData.SetPixel(Column, Row, pixelColor);
                finally
                    imgQRCode.Bitmap.Unmap(vBitMapData);
                end;
            end;
        end;

    finally
        QRCode.Free;
    end;
end;

procedure TForm2.TabControl1Change(Sender: TObject);
begin
  Button2.Text := 'EMITIR '+EditPgto.Selected.Text;
end;

end.
