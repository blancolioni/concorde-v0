with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Concorde.Elementary_Functions;
with Concorde.Logging;
with Concorde.Real_Images;

with Concorde.Db.Effect;
with Concorde.Db.Node;
with Concorde.Db.Network_Value;

package body Concorde.Network is

   Log_Updates : constant Boolean := True;

   procedure Log
     (Message : String);

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   ----------------------
   -- Commit_New_Value --
   ----------------------

   procedure Commit_New_Value
     (Network    : Concorde.Db.Network_Reference;
      Tag        : String)
   is
      Definition    : constant Concorde.Db.Node.Node_Type :=
                        Concorde.Db.Node.Get_By_Tag (Tag);
      Node_Value    : constant Concorde.Db.Network_Value.Network_Value_Type :=
                        Concorde.Db.Network_Value.Get_By_Network_Value
                          (Network, Definition.Get_Node_Reference);
   begin
      Concorde.Db.Network_Value.Update_Network_Value
        (Node_Value.Get_Network_Value_Reference)
        .Set_Real_Value (Node_Value.New_Value)
        .Done;
   end Commit_New_Value;

   ----------------------------
   -- Create_Initial_Network --
   ----------------------------

   procedure Create_Initial_Network
     (Network       : Concorde.Db.Network_Reference;
      Initial_Value : not null access function (Tag : String) return Real)
   is
   begin
      for Node of Concorde.Db.Node.Scan_By_Tag loop
         declare
            Value : constant Real := Initial_Value (Node.Tag);
         begin
            Concorde.Db.Network_Value.Create
              (Network      => Network,
               Node         => Node.Get_Node_Reference,
               Active       => True,
               Changed      => False,
               Real_Value   => Value,
               New_Value    => Value,
               Old_Value    => Value,
               Changed_At   => Concorde.Calendar.Clock);
         end;
      end loop;
   end Create_Initial_Network;

   -------------------
   -- Current_Value --
   -------------------

   function Current_Value
     (Network : Concorde.Db.Network_Reference;
      Tag     : String)
      return Real
   is
      Definition    : constant Concorde.Db.Node.Node_Type :=
                        Concorde.Db.Node.Get_By_Tag (Tag);
      Node_Value    : constant Concorde.Db.Network_Value.Network_Value_Type :=
                        Concorde.Db.Network_Value.Get_By_Network_Value
                          (Network, Definition.Get_Node_Reference);
   begin
      if not Definition.Has_Element then
         raise Constraint_Error with
           "no such node: " & Tag;
      end if;

      return Node_Value.Real_Value;
   end Current_Value;

   --------------------
   -- Inertial_Value --
   --------------------

   function Inertial_Value
     (Network : Concorde.Db.Network_Reference;
      Tag     : String;
      Inertia : Non_Negative_Real)
      return Real
   is
      pragma Unreferenced (Inertia);
   begin
      return Current_Value (Network, Tag);
   end Inertial_Value;

   -----------------
   -- Last_Change --
   -----------------

   function Last_Change
     (Network : Concorde.Db.Network_Reference;
      Tag     : String)
      return Concorde.Calendar.Time
   is
      Definition    : constant Concorde.Db.Node.Node_Type :=
                        Concorde.Db.Node.Get_By_Tag (Tag);
      Node_Value    : constant Concorde.Db.Network_Value.Network_Value_Type :=
                        Concorde.Db.Network_Value.Get_By_Network_Value
                          (Network, Definition.Get_Node_Reference);
   begin
      return Node_Value.Changed_At;
   end Last_Change;

   ---------
   -- Log --
   ---------

   procedure Log
     (Message  : String)
   is
   begin
      Concorde.Logging.Log
        (Actor    => "economy",
         Location => "",
         Category => "",
         Message  => Message);
   end Log;

   --------------------
   -- Previous_Value --
   --------------------

   function Previous_Value
     (Network : Concorde.Db.Network_Reference;
      Tag     : String)
      return Real
   is
      Definition    : constant Concorde.Db.Node.Node_Type :=
                        Concorde.Db.Node.Get_By_Tag (Tag);
      Node_Value    : constant Concorde.Db.Network_Value.Network_Value_Type :=
                        Concorde.Db.Network_Value.Get_By_Network_Value
                          (Network, Definition.Get_Node_Reference);
   begin
      return Node_Value.Old_Value;
   end Previous_Value;

   -------------------
   -- Set_New_Value --
   -------------------

   procedure Set_New_Value
     (Network : Concorde.Db.Network_Reference;
      Tag     : String;
      Value   : Real)
   is
      Definition    : constant Concorde.Db.Node.Node_Type :=
                        Concorde.Db.Node.Get_By_Tag (Tag);
      Node_Value    : constant Concorde.Db.Network_Value.Network_Value_Type :=
                        Concorde.Db.Network_Value.Get_By_Network_Value
                          (Network, Definition.Get_Node_Reference);
   begin
      Concorde.Db.Network_Value.Update_Network_Value
        (Node_Value.Get_Network_Value_Reference)
        .Set_New_Value (Value)
        .Set_Changed_At (Concorde.Calendar.Clock)
        .Set_Old_Value (Node_Value.Real_Value)
        .Done;
   end Set_New_Value;

   ------------
   -- Update --
   ------------

   procedure Update
     (Network : Concorde.Db.Network_Reference)
   is
      use Concorde.Db;

      package Node_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Node_Reference);

      package Node_Maps is
        new WL.String_Maps (Real);

      Changed_Ids : Node_Lists.List;
      Node_Value  : Node_Maps.Map;

      function Current_Value (Tag : String) return Real
      is (if Node_Value.Contains (Tag)
          then Node_Value.Element (Tag)
          else Current_Value (Network, Tag));

      function Scaled_Value
        (Tag     : String;
         Current : Real;
         Scale   : Unit_Real)
         return Real
      is (Scale * Current + (1.0 - Scale) * Previous_Value (Network, Tag));

      procedure Update_Value
        (Tag       : String;
         New_Value : Real);

      ------------------
      -- Update_Value --
      ------------------

      procedure Update_Value
        (Tag       : String;
         New_Value : Real)
      is
      begin
         if not Node_Value.Contains (Tag) then
            Node_Value.Insert (Tag, New_Value);
         else
            Node_Value.Replace (Tag, Node_Value.Element (Tag) + New_Value);
         end if;
      end Update_Value;

   begin

      for Node of Concorde.Db.Node.Scan_By_Tag loop
         if Concorde.Db.Effect.First_Reference_By_To (Node.Get_Node_Reference)
           = Null_Effect_Reference
         then
            Changed_Ids.Append (Node.Get_Node_Reference);
         end if;
      end loop;

      while not Changed_Ids.Is_Empty loop
         declare
            use Ada.Strings.Unbounded;
            New_Changed_Ids : Node_Lists.List;
         begin

            for Id of Changed_Ids loop
               declare
                  use Concorde.Calendar;
                  From_Node : constant Concorde.Db.Node.Node_Type :=
                                Concorde.Db.Node.Get (Id);
                  Changed   : constant Time :=
                                Last_Change (Network, From_Node.Tag);
--                               Concorde.Db.Network_Value.Get_By_Network_Value
--                                    (Network, From_Node.Get_Node_Reference)
--                                    .Changed_At;
                  Days_Changed : constant Real :=
                                   Real (Concorde.Calendar.Clock - Changed)
                                   / Real (Days (1));
                  Updated      : Boolean := False;
                  X            : constant Real :=
                                Current_Value (From_Node.Tag);
                  Log_Line     : Unbounded_String :=
                                     To_Unbounded_String
                                       ("[" & From_Node.Tag
                                        & "="
                                        & Image (X)
                                        & "]");
               begin

                  for Effect of
                    Concorde.Db.Effect.Select_By_Node (Id)
                  loop
                     declare
                        use Concorde.Elementary_Functions;
                        To_Id : constant Node_Reference :=
                                  Effect.To;
                        To_Node : constant Concorde.Db.Node.Node_Type :=
                                    Concorde.Db.Node.Get (To_Id);
                        D       : constant Real :=
                                    (if Days_Changed >= Effect.Inertia
                                     then 1.0
                                     else Days_Changed
                                     / Effect.Inertia);
                        Effective_Value : constant Real :=
                                            (if D = 1.0
                                             then X
                                             else Scaled_Value
                                               (From_Node.Tag, X, D));
                        Y       : constant Real :=
                                    Effect.Add
                                      + Effect.Multiply
                                    * (Effective_Value ** Effect.Exponent);
                     begin
                        Updated := True;
                        Update_Value (To_Node.Tag, Y);

                        Log_Line := Log_Line
                          & " (" & To_Node.Tag & " "
                          & Image (Y)
                          & ")";
                     end;
                  end loop;

                  if Updated then
                     Log (To_String (Log_Line));
                  end if;

               end;
            end loop;

            if not Node_Value.Is_Empty then
               for Position in Node_Value.Iterate loop
                  declare
                     Tag : constant String := Node_Maps.Key (Position);
                     Value : constant Real := Node_Maps.Element (Position);
                     Id : constant Concorde.Db.Node_Reference :=
                             Concorde.Db.Node.Get_Reference_By_Tag (Tag);
                  begin
                     New_Changed_Ids.Append (Id);
                     Set_New_Value (Network, Tag, Value);
                     Commit_New_Value (Network, Tag);

                     if Log_Updates then
                        Log (Tag
                             & "="
                             & Image (Value));
                     end if;
                  end;
               end loop;
               Node_Value.Clear;
            end if;
            Changed_Ids := New_Changed_Ids;
         end;
      end loop;

   end Update;

end Concorde.Network;
