with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Concorde.Calendar;
with Concorde.Identifiers;
with Concorde.Logging;
with Concorde.Real_Images;

with Concorde.Expressions;
with Concorde.Parser;
with Concorde.Symbols;
with Concorde.Values;

with Concorde.Db.Calculation;
with Concorde.Db.Derived_Metric;
with Concorde.Db.Effect;
with Concorde.Db.Node;
with Concorde.Db.Network_Value;
with Concorde.Db.Historical_Value;

package body Concorde.Network is

   Log_Updates : constant Boolean := True;

   type Cached_Expression_Record is
      record
         Expression : Concorde.Expressions.Expression_Type;
      end record;

   package Cached_Expression_Maps is
     new WL.String_Maps (Cached_Expression_Record);

   Expression_Cache : Cached_Expression_Maps.Map;

   procedure Log
     (Message : String);

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   type Network_Environment is
     new Concorde.Values.Environment_Interface with
      record
         Network : Concorde.Db.Network_Reference;
         X       : Concorde.Db.Node_Reference :=
                     Concorde.Db.Null_Node_Reference;
      end record;

   overriding function Get
     (Environment : Network_Environment;
      Symbol      : Concorde.Symbols.Symbol_Id;
      With_Delay  : Real := 0.0)
      return Concorde.Values.Value_Interface'Class;

   function Save_History (Node : Concorde.Db.Node.Node_Type) return Boolean
   is (Node.Content in Concorde.Db.Rating | Concorde.Db.Setting);

   ----------------------
   -- Commit_New_Value --
   ----------------------

   procedure Commit_New_Value
     (Network    : Concorde.Db.Network_Reference;
      Tag        : String)
   is
      Definition    : constant Concorde.Db.Node.Node_Type :=
                        Concorde.Db.Node.Get_By_Tag (Tag);
      Node : constant Concorde.Db.Node_Reference :=
               Definition.Get_Node_Reference;
      Node_Value    : constant Concorde.Db.Network_Value.Network_Value_Type :=
                        Concorde.Db.Network_Value.Get_By_Network_Value
                          (Network, Node);
   begin
      if Node_Value.Current_Value /= Node_Value.New_Value then
         if Save_History (Definition) then
            declare
               use Concorde.Db.Historical_Value;
               Previous : constant Historical_Value_Type :=
                            First_By_Previous_Historical_Value
                              (Network, Node, True);
            begin
               if Previous.Has_Element then
                  Update_Historical_Value
                    (Previous.Get_Historical_Value_Reference)
                    .Set_To (Concorde.Calendar.Clock)
                    .Set_Is_Previous (False)
                    .Set_Next (Node_Value.New_Value)
                    .Done;
               end if;
            end;

            Concorde.Db.Historical_Value.Create
              (Network     => Network,
               Node        => Node,
               Is_Previous => True,
               From        => Concorde.Calendar.Clock,
               To          => Concorde.Calendar.Clock,
               Next        => 0.0,
               Value       => Node_Value.New_Value);
         end if;

         Concorde.Db.Network_Value.Update_Network_Value
           (Node_Value.Get_Network_Value_Reference)
           .Set_Current_Value (Node_Value.New_Value)
           .Done;

      end if;
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
              (Network       => Network,
               Node          => Node.Get_Node_Reference,
               Active        => True,
               Current_Value => Value,
               New_Value     => Value);

            if Save_History (Node) then
               Concorde.Db.Historical_Value.Create
                 (Network     => Network,
                  Node        => Node.Get_Node_Reference,
                  Is_Previous => True,
                  From        => Concorde.Calendar.Zero_Time,
                  To          => Concorde.Calendar.Clock,
                  Next        => Value,
                  Value       => Value);
            end if;
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
   begin
      if not Definition.Has_Element then
         raise Constraint_Error with
           "no such node: " & Tag;
      end if;

      return Current_Value (Network, Definition.Get_Node_Reference);

   end Current_Value;

   -------------------
   -- Current_Value --
   -------------------

   function Current_Value
     (Network : Concorde.Db.Network_Reference;
      Node    : Concorde.Db.Node_Reference)
      return Real
   is
      Definition    : constant Concorde.Db.Node.Node_Type :=
                        Concorde.Db.Node.Get (Node);
      Node_Value    : constant Concorde.Db.Network_Value.Network_Value_Type :=
                        Concorde.Db.Network_Value.Get_By_Network_Value
                          (Network, Node);
   begin
      case Definition.Content is
         when Concorde.Db.Rating =>
            return Signed_Unit_Clamp (Node_Value.Current_Value);
         when Concorde.Db.Setting =>
            return Unit_Clamp (Node_Value.Current_Value);
         when Concorde.Db.Quantity | Concorde.Db.Money =>
            return Node_Value.Current_Value;
      end case;
   end Current_Value;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Network     : Concorde.Db.Network_Reference;
      Calculation : Concorde.Db.Calculation_Reference)
      return Real
   is
      Rec : constant Concorde.Db.Calculation.Calculation_Type :=
              Concorde.Db.Calculation.Get (Calculation);
      Env : constant Network_Environment :=
              Network_Environment'
                (Network => Network,
                 X       => Rec.Node);
      Id  : constant Concorde.Identifiers.Object_Identifier :=
              Rec.Identifier;
      Expr : constant Concorde.Expressions.Expression_Type :=
               (if Expression_Cache.Contains (Id)
                then Expression_Cache.Element (Id).Expression
                else raise Constraint_Error with
                  "calculation not loaded: [" & Id & "]");
   begin
      return Expr.Evaluate (Env).To_Real;
   exception
      when E : others =>
         raise Constraint_Error with
           "unable to evaluate: " & Expr.To_String
           & ": " & Ada.Exceptions.Exception_Message (E);
   end Evaluate;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Environment : Network_Environment;
      Symbol      : Concorde.Symbols.Symbol_Id;
      With_Delay  : Real := 0.0)
      return Concorde.Values.Value_Interface'Class
   is
      use type Concorde.Db.Node_Reference;
      Name : constant String :=
               Ada.Characters.Handling.To_Lower
                 (Concorde.Symbols.Get_Name (Symbol));
      Node : constant Concorde.Db.Node_Reference :=
               (if Name = "x"
                then Environment.X
                else Concorde.Db.Node.Get_Reference_By_Tag (Name));
      X    : constant Real :=
               (if Node = Concorde.Db.Null_Node_Reference
                then (raise Constraint_Error with
                    "get: no such node: " & Name)
                else Inertial_Value
                  (Environment.Network, Node, With_Delay));
   begin
      return Concorde.Values.Constant_Value (X);
   end Get;

   --------------------
   -- Inertial_Value --
   --------------------

   function Inertial_Value
     (Network : Concorde.Db.Network_Reference;
      Tag     : String;
      Inertia : Non_Negative_Real)
      return Real
   is
   begin
      return Inertial_Value
        (Network => Network,
         Node    => Concorde.Db.Node.Get_Reference_By_Tag (Tag),
         Inertia => Inertia);
   end Inertial_Value;

   --------------------
   -- Inertial_Value --
   --------------------

   function Inertial_Value
     (Network : Concorde.Db.Network_Reference;
      Node    : Concorde.Db.Node_Reference;
      Inertia : Non_Negative_Real)
      return Real
   is
   begin
      if Inertia = 0.0 or else
        not Save_History (Concorde.Db.Node.Get (Node))
      then
         return Current_Value (Network, Node);
      end if;

      declare
         use Concorde.Calendar;
         DT : constant Duration := Days (Inertia);
         Now   : constant Time := Clock;
         Start : constant Time := Now - DT;
      begin
         Concorde.Logging.Log
           (Actor    => "network",
            Location => Concorde.Db.Node.Get (Node).Tag,
            Category => "inertial value",
            Message  =>
              "inertia=" & Image (Inertia)
            & "; start=" & Image (Start, True));

         for History of
           Concorde.Db.Historical_Value
             .Select_Historical_Value_From_Bounded_By_From
               (Network, Node, Start, Clock)
         loop
            declare
               Full_Duration : constant Duration :=
                                 History.To - History.From;
               Partial_Duration : constant Duration :=
                                    Start - History.From;
               Scale            : constant Real :=
                                    Real (Partial_Duration)
                                    / Real (Full_Duration);
               Result           : constant Real :=
                                    History.Value
                                      + (History.Next - History.Value)
                                    * Scale;
            begin
               Concorde.Logging.Log
                 (Actor    => "network",
                  Location => Concorde.Db.Node.Get (Node).Tag,
                  Category => "inertial value",
                  Message  =>
                    "inertia=" & Image (Inertia)
                  & "; date=" & Image (Start, True)
                  & "; interval="
                  & Image (History.From, True)
                  & " .. "
                  & Image (History.To, True)
                  & "; scale="
                  & Image (Scale)
                  & "; range="
                  & Image (History.Value)
                  & " .. "
                  & Image (History.Next)
                  & "; current="
                  & Image (Current_Value (Network, Node))
                  & "; inertial value="
                  & Image (Result));
               return Result;
            end;
         end loop;

         return Current_Value (Network, Node);

      end;
   end Inertial_Value;

   ------------------
   -- Load_Network --
   ------------------

   procedure Load_Network is
   begin
      for Calculation of Concorde.Db.Calculation.Scan_By_Identifier loop
         if Calculation.Expression /= "" then
            Expression_Cache.Insert
              (Key      => Calculation.Identifier,
               New_Item => (Expression =>
                                Concorde.Parser.Parse_Expression
                              (Calculation.Expression)));
         end if;
      end loop;
   end Load_Network;

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

      for Metric of
        Concorde.Db.Derived_Metric.Scan_By_Tag
      loop
         declare
            Tag   : constant String := Metric.Tag;
         begin
            Set_New_Value
              (Network, Tag,
               Evaluate
                 (Network     => Network,
                  Calculation => Metric.Calculation));

            Commit_New_Value (Network, Tag);

         exception

            when E : others =>
               raise Constraint_Error with
                 "error updating "
                 & Tag
                 & ": " & Ada.Exceptions.Exception_Message (E);
         end;
      end loop;

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
                  From_Node : constant Concorde.Db.Node.Node_Type :=
                                Concorde.Db.Node.Get (Id);
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
                        To_Id : constant Node_Reference :=
                                  Effect.To;
                        To_Node : constant Concorde.Db.Node.Node_Type :=
                                    Concorde.Db.Node.Get (To_Id);
                        New_Value : constant Real :=
                                      Evaluate
                                        (Network,
                                         Effect.Get_Calculation_Reference);
                     begin
                        Updated := True;
                        Update_Value (To_Node.Tag, New_Value);

                        Log_Line := Log_Line
                          & " (" & To_Node.Tag & " "
                          & Image (New_Value)
                          & ")";
                     end;
                  end loop;

                  if Updated then
                     Log (To_String (Log_Line));
                  end if;

               exception

                  when E : others =>
                     raise Constraint_Error with
                       "error updating "
                       & From_Node.Tag
                       & ": " & Ada.Exceptions.Exception_Message (E);

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
