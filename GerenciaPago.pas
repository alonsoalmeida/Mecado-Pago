unit GerenciaPago;

interface

uses
  DBXJSON, JSON, System.SysUtils, System.StrUtils, System.Classes,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Platform,

  RestClient_u, DateUtils, Character;

type
  TOnStatus     = procedure(const Value: string) of object;
  TOnSaldo      = procedure(const Value: Double) of object;
  TOnError      = procedure(const Value: String) of object;
  TOnBoletoLink = procedure(const Value: String) of object;

  TOnPixBase64  = procedure(const Value: WideString) of object;
  TOnPixCopiar  = procedure(const Value: String) of object;
  //TIPOS DE PAGAMENTO
  TPgto         = (Boleto = 0, Loterica = 1, Pix = 2);

  TRetBusca = record
    RetStatus, RetDataCredt: String;
  end;

  // INFORMAÇÕES DA CONTA DO USUÁRRIO
  TConta = record
    ID,
    nickname,
    first_name,
    last_name,
    email :String;
  end;

  // ENDEREÇO DA API
  TMPRestClient = Class ( TRestClient )
  private
  protected
    constructor Create ; override ;
  end;

  // LANÇAMENTO DE DADOS
  TDados = class(tComponent)
  private
    // DADOS DO BOLETO
    Vinstallments,
    Vdate_of_expiration,
    Vtransaction_amount,
    Vexternal_reference,
    Vdescription,
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
    Vpayment_method_id: TPgto;

    published
    // LANÇAMENTO DO BOLETO
    property Parcelas       : String read Vinstallments       write Vinstallments ;       // Número de poarcelas
    property Vencimento     : String read Vdate_of_expiration write Vdate_of_expiration ; // Vencimento
    property Valor          : String read Vtransaction_amount write Vtransaction_amount ; // Valor
    property PedidoNumero   : String read Vexternal_reference write Vexternal_reference ; // Numero do Pedido
    property Descricao      : String read Vdescription        write Vdescription ;        // Descricao do produto ou serviço
    // Dados do Pagador
    property Email          : String read Vemail              write Vemail ;
    property Nome           : String read Vfirst_name         write Vfirst_name ;
    property SobreNome      : String read Vlast_name          write Vlast_name ;
    property Doc            : String read Vtype               write Vtype ;
    property DocNumero      : String read Vnumber             write Vnumber ;

    // Enderço do Pagador
    property Cep            : String read Vzip_code           write Vzip_code ;
    property Endereco       : String read Vstreet_name        write Vstreet_name ;
    property Numero         : String read Vstreet_number      write Vstreet_number ;
    property Bairro         : String read Vneighborhood       write Vneighborhood ;
    property Cidade         : String read Vcity               write Vcity ;
    property Estado         : String read Vfederal_unit       write Vfederal_unit ;

    property MeioPagar      : TPgto  read Vpayment_method_id  write Vpayment_method_id; // Meio de pagamento [Boleto, Pix, Cartao, Loterica]
  end;

  // MENU DE CHAVES KEYS
  TConfig = class(TComponent)
  private
    FSandBox        : Boolean;
    FAccessToken    : String;
    FPublicKey      : String;
    FCliente_id     : String;
    FCliente_Secret : String;
    procedure SetAccessToken(const Value: String);
    procedure SetCliente_id(const Value: String);
    procedure SetCliente_Secret(const Value: String);
    procedure SetPublicKey(const Value: String);
    procedure SetSandBox(const Value: Boolean);
  published
   property PublicKey : String read FPublicKey write SetPublicKey;
   property AccessToken : String read FAccessToken write SetAccessToken;
   property Cliente_id : String read FCliente_id write SetCliente_id;
   property Cliente_Secret : String read FCliente_Secret write SetCliente_Secret;
   property SandBox: Boolean read FSandBox write FSandBox;
  end;

  TGerenciaPago = class(TComponent)
  private
    // Verifica se o componente está ativo
    FAtivo            : Boolean;

    req               : TRequest;
    Json              : TJSONObject;
    // Pega o status
    FStatus           : TOnStatus;
    // Pega o saldo da conta
    FSaldo            : TOnSaldo;
    // Infomações da Conta
    FUser             : TConta;
    // Informações de error
    FError            : TOnError;
    // Link do Boleto
    FBoletoLink       : TOnBoletoLink;
    // Imagem do pix
    FPixBase64        : TOnPixBase64;
    // Copiar e colar pix
    FPixCopiar        : TOnPixCopiar;
    // Retorna o status do pedido de cobrança
    FRetBusca         : TRetBusca;

    TimerApagaStatus  : TTimer;
    sResposta         : String;
    FMemoRetorno      : WideString;

    vError            : String;
    CopiaCola         : String;
    FRetornaID        : String;

    procedure SetError(const Value: String);
    procedure SetStatus(const Value: String);
    procedure SetSaldo(const Value: Double);
    procedure SetBoletoLink(const Value: String);
    procedure SetMemo(const Value: WideString);

    procedure SetPixBase64(const Value: WideString);
    procedure SetPixCopiar(const Value: String);

  protected
    { Protected declarations }
    FSubConfig        : TConfig;
    FSubDados         : TDados;
  public
    { Public declarations }
    constructor Create (AOwner: TComponent); override;
    destructor Destroy ;

    // SandBoxURL
    function SandBoxURL( s : String ) : String ;
    // Informações da conta
    procedure RetornaConta;
    // Informações do saldo da conta
    procedure SaldoConta;
    // Retorna o status do pedido de cobrança
    procedure BuscarPedido(s: String);
    // Faz a emissão do boleto
    procedure GerarBoleto();
    // Retorna erro se ouver
    procedure xError( s : String );
    // Retorna a resposta
    procedure m( s : WideString );
    // Copia e Cola Pix
    procedure CopiaColaPix;
    // Verifica se esta ativo ou nao
    function StatusAtivo:Boolean;
    // Cancelar Pedido
    function CancelarPedido(s: String): String;

  published
    { Published declarations }
    // Informações da Conta
    property Ativo        : Boolean         read FAtivo       write FAtivo;
    property ConfigKey    : TConfig         read FSubConfig;
    property LancarDados  : TDados          read FSubDados;
    property DadosConta   : TConta          read FUser          write FUser;
    property RetConsulta  : TRetBusca       read FRetBusca      write FRetBusca;

    property RetornaID    : String          read FRetornaID     write FRetornaID;

    property OnError      : TOnError        read FError         write FError;
    property OnStatus     : TOnStatus       read FStatus        write FStatus;
    property OnSaldo      : TOnSaldo        read FSaldo         write FSaldo;
    property OnBoletoLink : TOnBoletoLink   read FBoletoLink    write FBoletoLink;

    property MemoRetono : WideString read FMemoRetorno write SetMemo ;
    property OnPixBase64  : TOnPixBase64    read FPixBase64     write FPixBase64;
    property OnPixCopiar  : TOnPixCopiar    read FPixCopiar     write FPixCopiar;

  end;

procedure Register;

implementation

{$R ./SGerenciaPago.dcr}

procedure Register;
begin
  RegisterComponents('pagOnline', [TGerenciaPago]);
end;

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


{ TGerenciaPago }

procedure TConfig.SetAccessToken(const Value: String);
begin
  FAccessToken := Value;
end;

procedure TConfig.SetCliente_id(const Value: String);
begin
  FCliente_id := Value;
end;

procedure TConfig.SetCliente_Secret(const Value: String);
begin
  FCliente_Secret := Value;
end;

procedure TConfig.SetPublicKey(const Value: String);
begin
  FPublicKey := Value;
end;

procedure TConfig.SetSandBox(const Value: Boolean);
begin
  FSandBox := Value;
end;

{ TGerenciaPago }

procedure TGerenciaPago.BuscarPedido(s: String);
var
  vStatus : String;
  i : Integer;
  JSObj,jsonObject  : TJSONObject;
  JSArray           : TJSONArray;
  JSValue           : TJSONValue;
begin
  if not StatusAtivo then exit;

  SetStatus( 'Buscando Pagamento' );

  req.Clear;
  if s = '' then
  begin
    req.uri := '/v1/payments/search/?status=approved';
    req.params.Add('?access_token='+FSubConfig.FAccessToken);
  end
  else
  begin
    req.uri := '/v1/payments/search?id='+s; //external_reference
    req.params.Add('&access_token='+FSubConfig.FAccessToken);
  end;

  try
    sResposta := TMPRestClient.Get(req);
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

  // Pega o status do pedido
  JSObj := TJSONObject.ParseJSONValue(sResposta) as TJSONObject;
  if(JSObj <> nil)then
  begin
      jsArray := JSObj.GetValue<TJSONArray>('results') as TJSONArray;
      for i := 0 to jsArray .Count-1 do
      begin
        jsonObject := jsArray.Items[i] as TJSONObject;

        case AnsiIndexStr(jsonObject.GetValue<string>('status'),
            ['pending', 'approved', 'in_process', 'rejected', 'cancelled', 'refunded', 'charged_back']) of
              0 : FRetBusca.RetStatus := 'Pendente';
              1 : FRetBusca.RetStatus := 'Aprovado';
              2 : FRetBusca.RetStatus := 'Em Processo';
              3 : FRetBusca.RetStatus := 'Rejeitado';
              4 : FRetBusca.RetStatus := 'Cancelado';
              5 : FRetBusca.RetStatus := 'Reembolso';
              6 : FRetBusca.RetStatus := 'Estorno Cartão';
        end;

        // DATA DA APROVAÇÃO
        FRetBusca.RetDataCredt := DateToStr(ISO8601ToDate(jsonObject.GetValue<string>('date_created')));
      end;
  end;

  SetStatus( 'Buscando Pagamento '+FRetBusca.RetStatus+' '+FRetBusca.RetDataCredt );
end;

function TGerenciaPago.CancelarPedido(s: String): String;
begin
  req.Clear;
  req.uri := '/v1/payments/'+s;
  req.params.Add('?access_token='+FSubConfig.FAccessToken);

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

procedure TGerenciaPago.CopiaColaPix;
var
 Svc: IFMXClipboardService;
 Image: TBitmap;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Svc) then
  Svc.SetClipboard(CopiaCola);
end;

constructor TGerenciaPago.Create(AOwner: TComponent);
begin
  inherited;
    req := TRequest.Create;

    {TimerApagaStatus := TTimer.Create( Self );
    TimerApagaStatus.Interval := 2*1000 ;
    TimerApagaStatus.Enabled := False ;
    TimerApagaStatus.OnTimer := TimerApagaStatusOnTimer;}

    // CRIA OS MENUS NO OBJETO SPECTOR
    FSubConfig := TConfig.Create(Self);
    FSubConfig.Name := 'ConfigKey';
    FSubConfig.SetSubComponent(True);

    FSubDados := TDados.Create(Self);
    FSubDados.Name := 'LancarDados';
    FSubDados.SetSubComponent(True);
end;

destructor TGerenciaPago.Destroy;
begin
  //TimerApagaStatus.Free;
  req.Free;
end;

procedure TGerenciaPago.GerarBoleto;
var
 Enviar : TStringList;
 ano, mes, dia : word;
 data : TDateTime;
 vError, Link, Base64, Copiar   : WideString;
begin
 try
   if not StatusAtivo then exit;

   with FSubDados do begin
    Enviar := TStringList.Create;
    Enviar.Add(
    '{'+#10+
    '"installments": 1,'+#10+ // NUMERO DE PACELAS
    '"date_of_expiration": "'+  DateToISO8601(StrToDate(Vencimento), False)+'",'+#10+
    '"transaction_amount": '+   StringReplace(Valor, ',', '.', [])+','+#10+ //
    '"external_reference": "'+  PedidoNumero+'",'+#10+ //NUMERO DO PEDIDO DE SEU SITE PARA FUTURA CONCILIAÇÃO FINANCEIRA
    '"description": "'+         Descricao+'",');

    if Vpayment_method_id = Boleto then
    Enviar.Add('"payment_method_id": "bolbradesco",'); // MEIO DE PAGAMENTO
    if Vpayment_method_id = Loterica then
    Enviar.Add('"payment_method_id": "pec",'); // MEIO DE PAGAMENTO
    if Vpayment_method_id = Pix then
    Enviar.Add('"payment_method_id": "pix",'); // MEIO DE PAGAMENTO

    // DODOS DO COMPRADOS
    Enviar.Add(
    '"payer": {'+#10+
    '"email": "'+               Email+'",'+#10+
    '"first_name": "'+          Nome+'",'+#10+
    '"last_name": "'+           SobreNome+'",'+#10+

    '"identification": {'+#10+
    '"type": "'+                Doc+'",'+#10+ //TIPO DE DOCUMENTO, CPF OU CNPJ CASO BRASIL
    '"number": "'+              DocNumero+'"'+#10+
    '},'+#10+

    //ENDEREÇO DO COMPRADOR
    '"address": {'+#10+
    '"zip_code": "'+            Cep+'",'+#10+
    '"street_name": "'+         Endereco+'",'+#10+
    '"street_number": "'+       Numero+'",'+#10+
    '"neighborhood": "'+        Bairro+'",'+#10+
    '"city": "'+                Cidade+'",'+#10+
    '"federal_unit": "'+        Estado+'"'+#10+
    '}'+#10+
    '}'+#10+
    '}');

      {if Vpayment_method_id in [Boleto, Loterica] then
      begin
        if Valor.tointeger > 5 then
        xError( 'Error: O valor minimo para boleto e letrica não pode ser menor que 5.00' );
        exit;
      end;}

      SetStatus( 'Adicionando Pagamento Por Boleto' );

      req.Clear ;
      req.uri := '/v1/payments'; //advanced_payments
      req.params.Add('?access_token='+FSubConfig.FAccessToken);
      req.body.Add( Enviar.Text );

      try
        sResposta := TMPRestClient.Post(req);
      except
        on E: Exception do
        xError( 'Error: '+E.ClassName + ': ' +E.Message );
      end;

      m( 'Resposta: '+sResposta );

      Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sResposta)),0));

      FRetornaID := ( TJSONObject(Json.Get('id').JsonValue).ToString );

      if Vpayment_method_id in [Boleto, Loterica] then
      begin
        Link        := ( TJSONObject(Json.Get('transaction_details').JsonValue).ToString );
        Link        :=  getCamposJsonString(Link, 'external_resource_url');
        FBoletoLink( Link );
      end;

      if Vpayment_method_id = Pix then
      begin
        Copiar := ( TJSONObject(Json.Get('point_of_interaction').JsonValue).ToString );
        Copiar :=  getCamposJsonString(Copiar, 'qr_code');
        FPixCopiar( Copiar );
        CopiaCola := Copiar;

        Base64 := ( TJSONObject(Json.Get('point_of_interaction').JsonValue).ToString );
        Base64 :=  getCamposJsonString(Base64, 'qr_code_base64');
        FPixBase64( Base64 );
      end;

      SetStatus( 'Pagamento gerado e incluído com sucesso!' );
   end;
  Except
   On E: Exception do
   begin
    xError( 'Error: '+E.ClassName + ': ' +E.Message );
   end;
 end;
end;

procedure TGerenciaPago.m(s: WideString);
begin
  FMemoRetorno := s;
end;

procedure TGerenciaPago.RetornaConta;
var
 vError : String;
begin
  if not StatusAtivo then exit;
  SetStatus( 'Buscando dados da conta' );

  req.Clear ;
  req.uri := '/users/me';
  req.params.Add('?access_token='+ FSubConfig.FAccessToken);
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

  SetStatus( 'Dados da conta retornados corretamente!' );
end;

procedure TGerenciaPago.SaldoConta;
var
  vError : String;
 const
 vID    = '166957358';
begin
  if not StatusAtivo then exit;

  SetStatus( 'Buscando saldo da conta!' );
  req.Clear ;
  req.uri := '/users/'+vID+'/mercadopago_account/balance';
  req.params.Add('?access_token='+FSubConfig.FAccessToken);
  try
    sResposta := TMPRestClient.Get(req);
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

  SetSaldo(StrToCurr(StringReplace(JsonSinComillas( TJSONObject(Json.Get('total_amount').JsonValue).ToString), '.', ',', [] )));
  SetStatus( 'Buscando saldo da conta!' );
end;

function TGerenciaPago.SandBoxURL(s: String): String;
begin
  if FSubConfig.FSandBox then
  result := result+'/sandbox';
  result := result + s;
end;

procedure TGerenciaPago.SetBoletoLink(const Value: String);
begin
  FBoletoLink(Value);
end;

procedure TGerenciaPago.SetError(const Value: String);
begin
  FError(Value);
end;

procedure TGerenciaPago.SetMemo(const Value: WideString);
begin
  FMemoRetorno := Value;
end;

procedure TGerenciaPago.SetPixBase64(const Value: WideString);
begin
  FPixBase64(Value);
end;

procedure TGerenciaPago.SetPixCopiar(const Value: String);
begin
  FPixCopiar(Value);
end;

procedure TGerenciaPago.SetSaldo(const Value: Double);
begin
  FSaldo(Value);
end;

procedure TGerenciaPago.SetStatus(const Value: String);
begin
  FStatus(Value);
end;

function TGerenciaPago.StatusAtivo: Boolean;
begin
   if not FAtivo then
   begin
     xError( 'Error: Componente não ativo!' );
     Result := False;
   end
   else
   begin
     xError( '' );
     Result := True;
   end;
end;

procedure TGerenciaPago.xError(s: String);
begin
  SetError(s);
end;

{ TMPRestClient }

constructor TMPRestClient.Create;
begin
  inherited;
  sURLDominio := 'https://api.mercadopago.com';
end;

end.
