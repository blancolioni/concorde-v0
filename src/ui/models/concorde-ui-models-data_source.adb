with Ada.Containers.Vectors;
with Ada.Exceptions;

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
      M : Root_Data_Source_Model'Class renames
        Root_Data_Source_Model'Class (Model);
      Response  : Concorde.Json.Json_Object;
      Table     : Concorde.Json.Json_Object;
      Headings  : Concorde.Json.Json_Array;
      Rows      : Concorde.Json.Json_Array;
      Json_Sort : constant Concorde.Json.Json_Value'Class :=
        Request.Get_Property ("sort");
      Sort_Column : constant Natural :=
        (if Json_Sort.Is_Null then 0
         else Integer'Value (Json_Sort.Image));
      Json_Asc    : constant Concorde.Json.Json_Value'Class :=
        Request.Get_Property ("ascending");
      Ascending   : constant Boolean :=
        (Json_Asc.Is_Null or else Boolean'Value (Json_Asc.Image));
      package Sort_Maps is new Ada.Containers.Vectors (Positive, Positive);
      Sorted_Map  : Sort_Maps.Vector;
   begin

      for I in 1 .. M.Row_Count loop
         Sorted_Map.Append (I);
      end loop;

      if Sort_Column > 0 then

         declare
            use type Values.Model_Value_Type;
            function "<" (Left, Right : Positive) return Boolean
            is (if Ascending
                then M.Cell_Value (Left, Sort_Column)
                < M.Cell_Value (Right, Sort_Column)
                else M.Cell_Value (Left, Sort_Column)
                > M.Cell_Value (Right, Sort_Column));

            package Sorting is
              new Sort_Maps.Generic_Sorting ("<");
         begin
            Sorting.Sort (Sorted_Map);
         end;
      end if;

      for I in 1 .. M.Column_Count loop
         declare
            Heading : Concorde.Json.Json_Object;
         begin
            Heading.Set_Property ("id", M.Column_Heading_Id (I));
            Heading.Set_Property ("label", M.Column_Heading_Label (I));
            Headings.Append (Heading);
         end;
      end loop;

      Table.Set_Property ("headings", Headings);

      for Sorted_Row_Index in 1 .. M.Row_Count loop
         declare
            Source_Row_Index : constant Positive :=
              Sorted_Map.Element (Sorted_Row_Index);
            Row : Concorde.Json.Json_Object;
         begin
            for Col_Index in 1 .. M.Column_Count loop
               declare
                  Prop_Value : constant Values.Model_Value_Type :=
                    M.Cell_Value (Source_Row_Index, Col_Index);
                  Json_Value : constant Concorde.Json.Json_Value'Class :=
                    M.Column_Renderer (Col_Index).To_Json
                    (Prop_Value);
               begin
                  Row.Set_Property
                    (Name  => M.Column_Heading_Id (Col_Index),
                     Value => Json_Value);
               end;
            end loop;
            Rows.Append (Row);
         end;
      end loop;
      Table.Set_Property ("data", Rows);

      Response.Set_Property ("table", Table);
      return Response;

   exception
      when E : others =>
         return M.Error
           (State   => State,
            Client  => Client,
            Request => Request,
            Message => Ada.Exceptions.Exception_Message (E));

   end Handle;

end Concorde.UI.Models.Data_Source;
