package body Concorde.UI.Models.Data_Source is

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out Root_Data_Source_Model;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class
   is
      pragma Unreferenced (State, Client, Request);
      M : Root_Data_Source_Model'Class renames
        Root_Data_Source_Model'Class (Model);
      Response : Concorde.Json.Json_Object;
      Table    : Concorde.Json.Json_Object;
      Headings : Concorde.Json.Json_Array;
      Rows     : Concorde.Json.Json_Array;
   begin
      for I in 1 .. M.Column_Count loop
         Headings.Append (Concorde.Json.String_Value (M.Column_Heading (I)));
      end loop;
      Table.Set_Property ("headings", Headings);

      for Row_Index in 1 .. M.Row_Count loop
         declare
            Row : Concorde.Json.Json_Array;
         begin
            for Col_Index in 1 .. M.Column_Count loop
               Row.Append
                 (Concorde.Json.String_Value
                    (M.Cell_Value (Row_Index, Col_Index)));
            end loop;
            Rows.Append (Row);
         end;
      end loop;
      Table.Set_Property ("data", Rows);

      Response.Set_Property ("table", Table);
      return Response;
   end Handle;

end Concorde.UI.Models.Data_Source;
