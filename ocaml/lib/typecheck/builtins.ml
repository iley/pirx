module Type = Pirx_types.Type

let protos : Proto.t list =
  [
    {
      name = "PirxString";
      external_name = None;
      args = [ ("len", Type.Int); ("value", Type.Pointer Type.Int8) ];
      ret_type = Type.String;
      variadic = false;
      external_ = true;
    };
    {
      name = "printf";
      external_name = Some "PirxPrintf";
      args = [ ("fmt", Type.String) ];
      ret_type = Type.Void;
      variadic = true;
      external_ = true;
    };
    {
      name = "putchar";
      external_name = None;
      args = [ ("ch", Type.Int) ];
      ret_type = Type.Void;
      variadic = false;
      external_ = true;
    };
  ]
