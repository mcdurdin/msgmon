unit MsgMon.System.Data.Search;

interface

uses
  System.Generics.Collections,
  System.UIConsts,
  System.UITypes;

type
  TMMSearch = class
  public
    Text: string;
    Color: TColor;
  end;

  TMMSearches = class(TObjectList<TMMSearch>)
  public
    procedure LoadDefault;
    function LoadFromJSON(definition: string): Boolean;
    procedure SaveToJSON(var definition: string);
  end;

implementation

uses
  System.JSON,
  System.SysUtils;

{ TMMSearches }

procedure TMMSearches.LoadDefault;
begin
  Clear;
end;

function TMMSearches.LoadFromJSON(definition: string): Boolean;
var
  o: TJSONObject;
  v: TJSONValue;
  i: Integer;
  c: TJSONArray;
  s: TMMSearch;
begin
  Result := False;
  Clear;

  v := TJSONObject.ParseJSONValue(definition);
  try
    if not Assigned(v) or not (v is TJSONObject) then Exit;

    o := v as TJSONObject;
    v := o.Values['searches'];
    if not Assigned(v) or not (v is TJSONArray) then Exit;
    c := v as TJSONArray;
    for i := 0 to c.Count - 1 do
    begin
      if not (c.Items[i] is TJSONObject) then Exit;

      o := c.Items[i] as TJSONObject;

      s := TMMSearch.Create;
      s.Text := o.Values['text'].Value;
      try
        s.Color := StringToColor(o.Values['color'].Value);
      except
        s.Color := TColors.Black;
      end;

      Add(s);
    end;

    Result := True;
  finally
    v.Free;

    if not Result then
      // If it failed to load, then reset the searches
      LoadDefault;
  end;
end;

procedure TMMSearches.SaveToJSON(var definition: string);
var
  o: TJSONObject;
  js: TJSONObject;
  c: TJSONArray;
  s: TMMSearch;
begin
  // Export searches to a json object
  o := TJSONObject.Create;
  try
    c := TJSONArray.Create;
    o.AddPair('searches', c);
    for s in Self do
    begin
      js := TJSONObject.Create;
      js.AddPair('text', s.Text);
      js.AddPair('color', ColorToString(s.Color));
      c.Add(js);
    end;

    definition := o.ToString;
  finally
    o.Free;
  end;
end;

end.
