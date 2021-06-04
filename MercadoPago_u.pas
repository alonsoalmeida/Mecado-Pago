unit MercadoPago_u;

interface

uses
  DBXJSON, JSON, System.SysUtils, System.StrUtils, System.Classes,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,

  RestClient_u;

type

  TMPRestClient = Class ( TRestClient )
  private
  protected
    constructor Create ; override ;
  End;

  TRetBusca = record
    RetStatus, RetDataCredt: String;
  end;

  TBoleto = record
    BoletoRetorno : Boolean;
    BoletoLink, BoletoBarra, BoletoID, BoletoVencimento : String;
  end;

  // INFORMAÇÕES DA CONTA DO USUÁRRIO
  TMPUser = record
    ID,
    nickname,
    first_name,
    last_name,
    email :String;
  end;

  TMPCobro = record
    ID : String;
    date_created,
    status,
    external_reference,
    merchant_order_id,
    reason,
    currency_id,
    payment_type,
    json                :String;

    transaction_amount,
    total_paid_amount,
    shipping_cost       :Real;

  end;

  TProcedureMPCobro = procedure ( x : TMPCobro ) of object ;

  TMercadoPago = Class( TComponent )
  private
    sResposta       : String ;
    FMemoRetorno    : WideString;
    FMemoError      : String;

    FClient_ID      : String;
    FClient_Secret  : String;
    FSandBox        : Boolean;
    FUser           : TMPUser;
    FStatus         : String;
    FBoleto         : TBoleto;
    FContaSaldo     : Double;

    req             : TRequest ;
    Json            : TJSONObject;
    TimerApagaStatus: TTimer ;

    FPublicKey      : String;
    FAccessToken    : String;

    FCollector_ID   : String;

    FStatusCon      : TRetBusca;

    // DADOS DO BOLETO
    Vdate_of_expiration,
    Vtransaction_amount,
    Vexternal_reference,
    Vdescription,
    Vpayment_method_id,
    // DOADOS DO COMPRADOS
    Vemail,
    Vfirst_name,
    Vlast_name,
    Vtype,    //TIPO DE DOCUMENTO, CPF OU CNPJ CASO BRASIL
    Vnumber,  //NUMERO DO CPF OU CNPJ
    //ENDEREÇO DO COMPRADOR
    Vzip_code,
    Vstreet_name,
    Vstreet_number,
    Vneighborhood,
    Vcity,
    Vfederal_unit : String;

    vError : String;

    // ESTORNO
    Rid,
    Rpayments,
    Rcurrency,
    Ramount,
    Rcoverage_applied,
    Rcoverage_elegible,
    Rdocumentation_required,
    Rdocumentation_status,
    Rdocumentation,
    Rdate_documentation_deadline,
    Rdate_created,
    Rdate_last_updated,
    Rlive_mode: String;

    procedure TimerApagaStatusOnTimer(Sender: TObject);
    procedure Status( s : String );
    procedure m( s : WideString );
    procedure xError( s : String );
    procedure SetClient_ID(const Value: String);
    procedure SetClient_Secret(const Value: String);
    procedure SetMemo(const Value: WideString);

    function sandboxurl( s : String ) : String ;
    function JSONDateTimeToDelphiDateTime( s : String ) : TDateTime ;
    procedure SetStatus(const Value: String);

    procedure SetPublicKey(const Value: String);
    procedure SetAccessToken(const Value: String);
    function UriSaveName( s : String ) : String ;
  public
    constructor Create( sPublicKey, sAccessToken, sClientID, sClientSecret : String );
    destructor Destroy ;
    procedure get_user_me ;

    function SaldoConta(s: String):String;
    function CadBoleto(): String;
    function GerarBoleto() : String;
    function BuscarPedido(s: String): String;
    function CancelarPedido(s: String): String;

    function CadCliente(): String;
    function GerarCliente(): String;
  published
    property lbStatus : String read FStatus write SetStatus ;

    property MemoRetono : WideString read FMemoRetorno write SetMemo ;
    property MemoError : String read FMemoError write FMemoError ;

    property PublicKey : String read FPublicKey write SetPublicKey ;
    property AccessToken : String read FAccessToken write SetAccessToken ;
    property Client_ID : String read FClient_ID write SetClient_ID ;
    property Collector_ID : String read FCollector_ID write FCollector_ID ;
    property Client_Secret : String read FClient_Secret write SetClient_Secret ;

    property User : TMPUser read FUser write FUser ;

    property RetConsulta : TRetBusca read FStatusCon write FStatusCon ;
    property Boleto : TBoleto read FBoleto write FBoleto ;
    property ContaSaldo : Double read FContaSaldo write FContaSaldo ;
    property SandBox : Boolean read FSandBox write FSandBox ;

    // LANÇAMENTO DO BOLETO
    property Bdate_of_expiration  : String read Vdate_of_expiration write Vdate_of_expiration ;
    property Btransaction_amount  : String read Vtransaction_amount write Vtransaction_amount ;
    property Bexternal_reference  : String read Vexternal_reference write Vexternal_reference ;
    property Bdescription         : String read Vdescription        write Vdescription ;
    property Bpayment_method_id   : String read Vpayment_method_id  write Vpayment_method_id ;
    property Bemail               : String read Vemail              write Vemail ;
    property Bfirst_name          : String read Vfirst_name         write Vfirst_name ;
    property Blast_name           : String read Vlast_name          write Vlast_name ;
    property Btype                : String read Vtype               write Vtype ;
    property Bnumber              : String read Vnumber             write Vnumber ;
    property Bzip_code            : String read Vzip_code           write Vzip_code ;
    property Bstreet_name         : String read Vstreet_name        write Vstreet_name ;
    property Bstreet_number       : String read Vstreet_number      write Vstreet_number ;
    property Bneighborhood        : String read Vneighborhood       write Vneighborhood ;
    property Bcity                : String read Vcity               write Vcity ;
    property Bfederal_unit        : String read Vfederal_unit       write Vfederal_unit ;

    // ESTORNO
    property id                          : String read Rid write Rid ;
    property payments                    : String read Rpayments write Rpayments ;
    property currency                    : String read Rcurrency write Rcurrency ;
    property amount                      : String read Ramount write Ramount ;
    property coverage_applied            : String read Rcoverage_applied write Rcoverage_applied ;
    property coverage_elegible           : String read Rcoverage_elegible write Rcoverage_elegible ;
    property documentation_required      : String read Rdocumentation_required write Rdocumentation_required ;
    property documentation_status        : String read Rdocumentation_status write Rdocumentation_status ;
    property documentation               : String read Rdocumentation write Rdocumentation ;
    property date_documentation_deadline : String read Rdate_documentation_deadline write Rdate_documentation_deadline ;
    property date_created                : String read Rdate_created write Rdate_created ;
    property date_last_updated           : String read Rdate_last_updated write Rdate_last_updated ;
    property live_mode                   : String read Rlive_mode write Rlive_mode ;
  end;

implementation

uses DateUtils, Character ;

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

procedure SaveToFile( sFileName: String ; sTexto : String );
var
  f : textfile;
begin
  try
    AssignFile( f, sFileName );
    Rewrite( f );
    Write(f, sTexto );
    Flush(f);
    CloseFile(f);
  except
  end;
end;

{ TMercadoPago }

constructor TMercadoPago.Create(
        sPublicKey,
        sAccessToken,
        sClientID,
        sClientSecret : String );
begin
  req := TRequest.Create ;
  TimerApagaStatus := TTimer.Create( Self );
  TimerApagaStatus.Interval := 2*1000 ;
  TimerApagaStatus.Enabled := False ;
  TimerApagaStatus.OnTimer := TimerApagaStatusOnTimer ;
  PublicKey := sPublicKey ;
  AccessToken := sAccessToken ;
  Client_ID := sClientID ;
  Client_Secret := sClientSecret ;
end;

destructor TMercadoPago.Destroy;
begin
  TimerApagaStatus.Free ;
  req.Free ;
end;

function TMercadoPago.CancelarPedido(s: String): String;
begin
  req.uri := '/v1/payments/'+s;
  req.params.Add('?access_token='+FAccessToken);

  //Os cancelamentos podem ser realizados somente com status pending e in process
  req.body.Add('{"status" : "cancelled"}');

  try
    sResposta := TMPRestClient.Put(req);
  except
    on E: Exception do
      xError( 'Error: '+E.ClassName + ': ' +E.Message );
  end;

  m( 'Resposta: '+sResposta );

  Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sResposta)),0));
  vError := getCamposJsonString(sResposta, 'error');
  if vError <> '' then
  begin
    xError( 'Error: '+( TJSONObject(Json.Get('message').JsonValue).ToString ) );
    exit;
  end;
end;

function TMercadoPago.JSONDateTimeToDelphiDateTime(s: String): TDateTime;
var ano, mes, dia, hora, min, seg : Word;
begin
  // 12345678901234567890
  // 2018-08-02T14:02:33.000-04:00
  s := JsonSinComillas( s );
  ano  := StrToInt( Copy( s, 1, 4 ));
  mes  := StrToInt( Copy( s, 6, 2 ));
  dia  := StrToInt( Copy( s, 9, 2 ));
  hora := StrToInt( Copy( s,12, 2 ));
  min  := StrToInt( Copy( s,15, 2 ));
  seg  := StrToInt( Copy( s,18, 2 ));
  Result := EncodeDateTime( ano, mes, dia, hora, min, seg, 0);
end;

procedure TMercadoPago.m(s: WideString);
begin
  FMemoRetorno := s;
end;

procedure TMercadoPago.xError(s: String);
begin
  FMemoError := s;
end;

function TMercadoPago.BuscarPedido(s: String): String;
var
 vStatus : String;
begin
  Status( 'Buscando Pagamento' );

  req.Clear;
  if s = '' then
  begin
    req.uri := '/v1/payments/search';
    req.params.Add('?access_token='+FAccessToken);
  end
  else
  begin
    req.uri := '/v1/payments/search?id='+s; //external_reference
    req.params.Add('&access_token='+FAccessToken);
  end;

  try
    sResposta := TMPRestClient.Get(req);
  except
    on E: Exception do
      xError( 'Error: '+E.ClassName + ': ' +E.Message );
  end;

  m( 'Resposta: '+sResposta );
//  raise Exception.Create(getCamposJsonString(sResposta, 'status'));

  Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sResposta)),0));
  vError := getCamposJsonString(sResposta, 'error');
  if vError <> '' then
  begin
    xError( 'Error: '+( TJSONObject(Json.Get('message').JsonValue).ToString ) );
    exit;
  end;

  case AnsiIndexStr(getCamposJsonString(sResposta, 'status'),
      ['pending', 'approved', 'in_process', 'rejected', 'cancelled', 'refunded', 'charged_back']) of
        0 : FStatusCon.RetStatus := 'Pendente';
        1 : FStatusCon.RetStatus := 'Aprovado';
        2 : FStatusCon.RetStatus := 'Em Processo';
        3 : FStatusCon.RetStatus := 'Rejeitado';
        4 : FStatusCon.RetStatus := 'Cancelado';
        5 : FStatusCon.RetStatus := 'Reembolso';
        6 : FStatusCon.RetStatus := 'Estorno Cartão';
  end;

  Status( 'Buscando Pagamento '+FStatusCon.RetStatus );
end;

function TMercadoPago.CadCliente(): String;
var
 Enviar : TStringList;
begin
  Enviar := TStringList.Create;
  Enviar.Add(
  '{'+#10+
  '      "email": "bruno@gmail.com",'+#10+
  '      "first_name": "Bruce",'+#10+
  '      "last_name": "Wayne",'+#10+
  '      "phone": {'+#10+
  '          "area_code": "023",'+#10+
  '          "number": "12345678"'+#10+
  '      },'+#10+
  '      "identification": {'+#10+
  '          "type": "DNI",'+#10+
  '          "number": "12345678"'+#10+
  '      },'+#10+
  '      "address": {'+#10+
  '          "zip_code": "SG1 2AX",'+#10+
  '          "street_name": "Old Knebworth Ln"'+#10+
  '      },'+#10+
  '      "description": "Lorem Ipsum"'+#10+
  '  }');
  Result := Enviar.Text;
end;

function TMercadoPago.GerarCliente(): String;
var
 vError : String;
begin
  Status( 'Adicionar Cliente' );

  req.Clear ;
  req.uri := '/v1/customers';
  req.params.Add('?access_token='+FAccessToken);
  req.body.Add( CadCliente );

  try
    sResposta := TMPRestClient.Post(req);
  except
    on E: Exception do
      xError( 'Error: '+E.ClassName + ': ' +E.Message );
  end;

  m( 'Resposta: '+sResposta );

  Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sResposta)),0));

  vError := getCamposJsonString(sResposta, 'error');
  if vError <> '' then
  begin
    xError( 'Error: '+( TJSONObject(Json.Get('message').JsonValue).ToString ));
    exit;
  end;

  //FBoleto.BoletoLink        := ( TJSONObject(Json.Get('transaction_details').JsonValue).ToString );

  Status( 'Cliente Adicionado com sucesso!' );
end;

function TMercadoPago.CadBoleto: String;
var
 Enviar : TStringList;
begin
  Enviar := TStringList.Create;
  Enviar.Add(
  '{'+#10+
  '"installments": 1,'+#10+ // NUMERO DE PACELAS
  '"date_of_expiration": "'+  DateToISO8601(StrToDate(Bdate_of_expiration), False)+'",'+#10+
  '"transaction_amount": "'+  Btransaction_amount+'",'+#10+
  '"external_reference": "'+  Bexternal_reference+'",'+#10+ //NUMERO DO PEDIDO DE SEU SITE PARA FUTURA CONCILIAÇÃO FINANCEIRA
  '"description": "'+         Bdescription+'",'+#10+
  '"payment_method_id": "'+   Bpayment_method_id+'",'+#10+ // MEIO DE PAGAMENTO

  // DODOS DO COMPRADOS
  '"payer": {'+#10+
  '"email": "'+               Bemail+'",'+#10+
  '"first_name": "'+          Bfirst_name+'",'+#10+
  '"last_name": "'+           Blast_name+'",'+#10+

  '"identification": {'+#10+
  '"type": "'+                Btype+'",'+#10+ //TIPO DE DOCUMENTO, CPF OU CNPJ CASO BRASIL
  '"number": "'+              Bnumber+'"'+#10+
  '},'+#10+

  //ENDEREÇO DO COMPRADOR
  '"address": {'+#10+
  '"zip_code": "'+            Bzip_code+'",'+#10+
  '"street_name": "'+         Bstreet_name+'",'+#10+
  '"street_number": "'+       Bstreet_number+'",'+#10+
  '"neighborhood": "'+        Bneighborhood+'",'+#10+
  '"city": "'+                Bcity+'",'+#10+
  '"federal_unit": "'+        Bfederal_unit+'"'+#10+
  '}'+#10+
  '}'+#10+
  '}');

  Result := Enviar.Text;
end;

function TMercadoPago.GerarBoleto: String;
var
 ano, mes, dia : word;
 data : TDateTime;
 vError : String;
begin
  Status( 'Adicionando Pagamento Por Boleto' );

  req.Clear ;
  req.uri := '/v1/payments'; //advanced_payments
  req.params.Add('?access_token='+FAccessToken);
  req.body.Add( CadBoleto );

  try
    sResposta := TMPRestClient.Post(req);
  except
    on E: Exception do
      xError( 'Error: '+E.ClassName + ': ' +E.Message );
  end;

  m( 'Resposta: '+sResposta );

  Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sResposta)),0));

  vError := getCamposJsonString(sResposta, 'error');
  if vError <> '' then
  begin
    xError( 'Error: '+( TJSONObject(Json.Get('message').JsonValue).ToString ) );
    exit;
  end;

  FBoleto.BoletoLink        := ( TJSONObject(Json.Get('transaction_details').JsonValue).ToString );
  FBoleto.BoletoLink        :=  getCamposJsonString(Boleto.BoletoLink, 'external_resource_url');

  FBoleto.BoletoBarra       := ( TJSONObject(Json.Get('barcode').JsonValue).ToString );
  FBoleto.BoletoBarra       := getCamposJsonString(Boleto.BoletoBarra, 'content');

  FBoleto.BoletoRetorno     := StrToBool( TJSONObject(Json.Get('captured').JsonValue).ToString );
  FBoleto.BoletoID          := ( TJSONObject(Json.Get('id').JsonValue).ToString );

  ano := StrToInt(Copy(TJSONObject(Json.Get('date_of_expiration').JsonValue).ToString,2,4));
  mes := StrToInt(Copy(TJSONObject(Json.Get('date_of_expiration').JsonValue).ToString,7,2));
  dia := StrToInt(Copy(TJSONObject(Json.Get('date_of_expiration').JsonValue).ToString,10,2));
  data := EncodeDate(ano, mes, dia);
  FBoleto.BoletoVencimento  := ( FormatdateTime('yyyy/mm/dd', data));

  Status( 'Pagamento gerado e incluído com sucesso!' );
end;

procedure TMercadoPago.get_user_me;
var
 vError : String;
begin
  Status( 'Buscando dados da conta' );
  req.Clear ;
  req.uri := '/users/me';
  req.params.Add('?access_token='+FAccessToken);
  try
    sResposta := TMPRestClient.Get(req);
  except
    on E: Exception do
      xError( 'Error: '+E.ClassName + ': ' +E.Message );
  end;

  m( 'Resposta: '+sResposta );

  Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sResposta)),0));
  FUser.ID         := JsonSinComillas( TJSONObject(Json.Get('id').JsonValue).ToString );
  FUser.nickname   := JsonSinComillas( TJSONObject(Json.Get('nickname').JsonValue).ToString );
  FUser.first_name := JsonSinComillas( TJSONObject(Json.Get('first_name').JsonValue).ToString );
  FUser.last_name  := JsonSinComillas( TJSONObject(Json.Get('last_name').JsonValue).ToString );
  FUser.email      := JsonSinComillas( TJSONObject(Json.Get('email').JsonValue).ToString );
  Status( 'Datos da conta retornados corretamente!' );
end;

function TMercadoPago.SaldoConta(s: String): String;
var
  vError : String;
 const
 vID    = '166957358';
 vToken = 'APP_USR-3067742096444572-042712-95042da85e0714597f6b707e6234d23c-166957358';
begin
  Status( 'Buscando saldo da conta!' );
  req.Clear ;
  req.uri := '/users/'+vID+'/mercadopago_account/balance';
  req.params.Add('?access_token='+vToken);
  //req.params.Add('&access_token='+FAccessToken);
  try
    sResposta := TMPRestClient.Get(req);
  except
    on E: Exception do
      xError( 'Error: '+E.ClassName + ': ' +E.Message );
  end;
  m( 'Resposta: '+sResposta );

  Json        := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sResposta)),0));

  vError := getCamposJsonString(sResposta, 'error');
  if vError <> '' then
  begin
    xError( 'Error: '+( TJSONObject(Json.Get('message').JsonValue).ToString ) );
    exit;
  end;

  ContaSaldo  := StrToCurr(StringReplace(JsonSinComillas( TJSONObject(Json.Get('total_amount').JsonValue).ToString), '.', ',', [] ));
  Status( 'Buscando saldo da conta!' );
end;

function TMercadoPago.sandboxurl(s: String): String;
begin
  if FSandBox then
  result := result+'/sandbox';
  result := result + s;
end;

procedure TMercadoPago.SetAccessToken(const Value: String);
begin
  FAccessToken := Value;
  m( 'Access Token: '+Value );
end;

procedure TMercadoPago.SetClient_ID(const Value: String);
begin
  FClient_ID := Value;
  m( 'ClientID: '+Value );
end;

procedure TMercadoPago.SetMemo(const Value: WideString);
begin
  FMemoRetorno  := Value;
  PublicKey     := FPublicKey ;
  AccessToken   := FAccessToken ;
  Client_ID     := FClient_ID ; // Para ser visto no memorando
  Client_Secret := FClient_Secret ;
end;

procedure TMercadoPago.SetPublicKey(const Value: String);
begin
  FPublicKey := Value;
  m( 'PublicKey: '+Value);
end;

procedure TMercadoPago.SetStatus(const Value: String);
begin
  FStatus := Value;
  //FStatus.Visible := False ;
end;

procedure TMercadoPago.Status(s: String);
begin
  FStatus := s ;
end;

procedure TMercadoPago.TimerApagaStatusOnTimer(Sender: TObject);
begin
  {if Assigned( FStatus ) then
  FStatus.Visible := False ;}
end;

function TMercadoPago.UriSaveName(s: String): String;
begin
  s := RightStr( s, Length(s)-1 );
  result := StringReplace( s, '\','',[rfReplaceAll]);
end;

procedure TMercadoPago.SetClient_Secret(const Value: String);
begin
  FClient_Secret := Value;
  m( 'ClientSecret: '+Value );
end;

{ TMPRestClient }

constructor TMPRestClient.Create;
begin
  inherited;
  sURLDominio := 'https://api.mercadopago.com';
end;

end.

