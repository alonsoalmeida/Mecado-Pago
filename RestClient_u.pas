unit RestClient_u;

interface

uses
  DBXJSON, JSON, System.Classes, System.SysUtils,

  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent;

type

  TRequest = class
    uri     : String;
    vToken  : String;
    body    : TStringList;
    headers : TStringList;
    params  : TStringList;

  public
    procedure Clear ;
    constructor Create ;
    destructor Destroy ;
  end;

  TRestClient = Class ( TComponent )
  private
    NetHTTP : TNetHTTPClient;
    function armarParametros( sl : TStringList ) : String ;
  protected
    constructor Create ; virtual ;
    destructor Destroy ;
  public
    sURLDominio : String ;
    class function Get( req : TRequest ) : String ;
    class function Post( req : TRequest ) : String ;
    class function Put( req : TRequest ) : String ;
  End;

implementation

uses StrUtils ;

{ TRestClient }

function TRestClient.armarParametros(sl: TStringList): String;
var i : integer ;
begin
  result := '';
  for I := 0 to sl.Count - 1 do begin
    result := result+sl[i];
    if i<sl.Count-1 then
      result := result + '&';
  end;
end;

constructor TRestClient.Create;
var
 req : TRequest;
begin
    NetHTTP := TNetHTTPClient.Create(nil);

   try
    with NetHTTP do begin
      Accept            := 'application/json';
      ContentType       := 'application/json';
      Asynchronous      := False;
      ConnectionTimeout := 60000;
      ResponseTimeout   := 60000;
      HandleRedirects   := True;
      AllowCookies      := False;
      UserAgent         := 'MercadoPago PHP SDK v0.5.2';
    end;
    except
      on E: Exception do
        Raise Exception.Create( 'Error: '+E.ClassName + ': ' +E.Message );
   end;
end;

destructor TRestClient.Destroy;
begin
  NetHTTP.Free ;
end;

class function TRestClient.Get(req: TRequest ): String;
var s : String ;
begin
  With Self.Create do

  try
    try
      s := sURLDominio + req.uri + armarParametros( req.params ) ;
      result := NetHTTP.Get(s).ContentAsString(tencoding.UTF8);
    except
      on E: Exception do
        Raise Exception.Create( 'Error: '+E.ClassName + ': ' +E.Message );
    end;
  finally
    Free;
  end;

end;

class function TRestClient.Post(req: TRequest): String;
var
  sUrl : String ;
  JsonToSend: TStringStream;
begin
  With Self.Create do
  try
    try
      sUrl := sURLDominio + req.uri + armarParametros( req.params ) ;
      JsonToSend := TStringStream.Create(Utf8Encode(req.body.Text));

      result := NetHTTP.Post(sUrl, JsonToSend ).ContentAsString(tencoding.UTF8);
    except
      on E: Exception do
        Raise Exception.Create( 'Error: '+E.ClassName + ': ' +E.Message );
    end;
  finally
    Free;
  end;
end;

class function TRestClient.Put(req: TRequest): String;
var
  sUrl : String ;
  JsonToSend: TStringStream;
begin
  With Self.Create do
  try
    try
      sUrl := sURLDominio + req.uri + armarParametros( req.params ) ;
      JsonToSend := TStringStream.Create(Utf8Encode(req.body.Text));

      result := NetHTTP.Put(sUrl, JsonToSend ).ContentAsString(tencoding.UTF8);
    except
      on E: Exception do
        Raise Exception.Create( 'Error: '+E.ClassName + ': ' +E.Message );
    end;
  finally
    Free;
  end;
end;

{ TRequest }

procedure TRequest.Clear;
begin
  body.Clear ;
  headers.Clear ;
  params.Clear ;
end;

constructor TRequest.Create;
begin
  body := TStringList.Create ;
  headers := TStringList.Create ;
  params := TStringList.Create ;
end;

destructor TRequest.Destroy;
begin
  body.Free ;
  headers.Free ;
  params.Free ;
end;

end.
