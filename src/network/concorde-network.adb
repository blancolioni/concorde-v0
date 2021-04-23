with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;

with WL.String_Maps;

with Concorde.Calendar;
with Concorde.Logging;
with Concorde.Real_Images;

with Concorde.Expressions;
with Concorde.Evaluator;
with Concorde.Parser;
with Concorde.Primitives.Loader;

with Concorde.Handles.Derived_Metric;
with Concorde.Handles.Historical_Value;
with Concorde.Handles.Network_Value;
with Concorde.Handles.Node;

package body Concorde.Network is

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   package Network_Node_Maps is
     new WL.String_Maps (Network_Node_Type);

   Network_Node_Map : Network_Node_Maps.Map;

   package Network_Value_Maps is
     new WL.String_Maps (Network_Value_Type);

   type Observer_Access is access all Node_Observer'Class;

   package Observer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Observer_Access);

   package Value_History_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Concorde.Calendar.Time,
        Element_Type => Real,
        "<"          => Concorde.Calendar."<");

   type Network_Value_Record is
      record
         Original_Start : Concorde.Calendar.Time;
         Current_Start  : Concorde.Calendar.Time;
         Current_Value  : Real    := 0.0;
         New_Value      : Real    := 0.0;
         Updated        : Boolean := False;
         Handle         : Concorde.Handles.Network_Value.Network_Value_Handle;
         Observers      : Observer_Lists.List;
         History        : Value_History_Maps.Map;
         New_History    : Value_History_Maps.Map;
      end record;

   package Network_Value_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Network_Value_Type);

   Network_Value_List : Network_Value_Lists.List;

   procedure Save_Value (Value : Network_Value_Type);
   procedure Save_History (Value : Network_Value_Type);

   type Network_Node_Record is
      record
         Handle : Concorde.Handles.Node.Node_Handle;
      end record;

   type Network_Record is
      record
         Value_Map : Network_Value_Maps.Map;
      end record;

   package Network_Maps is
     new WL.String_Maps (Network_Type);

   Network_Map : Network_Maps.Map;

   package Calculation_Maps is
     new WL.String_Maps (Concorde.Evaluator.Evaluation_Handle,
                         Concorde.Evaluator."=");

   Calculation_Map : Calculation_Maps.Map;

   type Calculation_Item is
      record
         Node  : Network_Node_Type;
         Eval  : Concorde.Evaluator.Evaluation_Handle;
      end record;

   package Calculation_Item_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Calculation_Item);

   Metric_Nodes  : Calculation_Item_Lists.List;

   procedure Set_Value
     (Value     : Network_Value_Type;
      New_Value : Real);

   procedure Commit_Value
     (Value     : Network_Value_Type);

   procedure Create_Network
     (Network       : Concorde.Handles.Network.Network_Class);

   ------------------
   -- Add_Observer --
   ------------------

   procedure Add_Observer
     (Value    : Network_Value_Type;
      Observer : not null access Node_Observer'Class)
   is
   begin
      Value.Observers.Append (Observer_Access (Observer));
   end Add_Observer;

   ------------------
   -- Commit_Value --
   ------------------

   procedure Commit_Value
     (Value     : Network_Value_Type)
   is
   begin
      if Value.Current_Value /= Value.New_Value then
         Value.History.Insert
           (Concorde.Calendar.Clock, Value.Current_Value);
         Value.New_History.Insert
           (Concorde.Calendar.Clock, Value.Current_Value);
         Value.Current_Value := Value.New_Value;
         Value.Updated := True;
      end if;
   end Commit_Value;

   ----------------------------
   -- Create_Initial_Network --
   ----------------------------

   procedure Create_Initial_Network
     (Network       : Concorde.Handles.Network.Network_Class;
      Initial_Value : not null access function (Tag : String) return Real)
   is
   begin
      for Node of Concorde.Handles.Node.Scan_By_Tag loop
         declare
            Value : constant Real := Initial_Value (Node.Tag);
         begin
            Concorde.Handles.Network_Value.Create
              (Network       => Network,
               Node          => Node,
               Active        => True,
               Current_Start => Concorde.Calendar.Zero_Time,
               Current_Value => Value);
         end;
      end loop;
   end Create_Initial_Network;

   --------------------
   -- Create_Network --
   --------------------

   procedure Create_Network
     (Network       : Concorde.Handles.Network.Network_Class)
   is
      New_Net : constant Network_Type :=
                  new Network_Record'
                    (Value_Map => <>);

      function Load_History
        (Node : Concorde.Handles.Node.Node_Class)
         return Value_History_Maps.Map;

      ------------------
      -- Load_History --
      ------------------

      function Load_History
        (Node : Concorde.Handles.Node.Node_Class)
         return Value_History_Maps.Map
      is
      begin
         return Map : Value_History_Maps.Map do
            for Historical_Value of
              Concorde.Handles.Historical_Value
                .Select_By_Historical_Value (Network, Node)
            loop
               Map.Insert (Historical_Value.To, Historical_Value.Value);
            end loop;
         end return;
      end Load_History;

   begin
      for Value_Handle of
        Concorde.Handles.Network_Value.Select_By_Network (Network)
      loop
         declare
            Value : constant Network_Value_Type :=
                      new Network_Value_Record'
                        (Current_Start  => Value_Handle.Current_Start,
                         Current_Value  => Value_Handle.Current_Value,
                         Original_Start => Value_Handle.Current_Start,
                         New_Value      => Value_Handle.Current_Value,
                         Handle         =>
                           Value_Handle.To_Network_Value_Handle,
                         Updated        => False,
                         Observers      => <>,
                         History        => Load_History (Value_Handle.Node),
                         New_History    => <>);
         begin
            New_Net.Value_Map.Insert
              (Value_Handle.Node.Tag, Value);
            Network_Value_List.Append (Value);
         end;
      end loop;
      Network_Map.Insert (Network.Identifier, New_Net);
   end Create_Network;

   -------------------
   -- Current_Value --
   -------------------

   function Current_Value (Value : Network_Value_Type) return Real is
   begin
      return Value.Current_Value;
   end Current_Value;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Network     : Network_Type;
      Calculation : Concorde.Handles.Calculation.Calculation_Class)
      return Real
   is
      use Calculation_Maps;
      Id       : constant String := Calculation.Identifier;
      Position : Cursor := Calculation_Map.Find (Id);
   begin
      if not Has_Element (Position) then
         declare
            Handle : constant Concorde.Evaluator.Evaluation_Handle :=
                       Concorde.Evaluator.Compile
                         (Concorde.Parser.Parse_Expression
                            (Calculation.Expression),
                          null);
            Inserted : Boolean;
         begin
            Calculation_Map.Insert
              (Id, Handle, Position, Inserted);
            pragma Assert (Inserted);
         end;
      end if;

      pragma Assert (Calculation_Map.Contains (Id));

      return Concorde.Evaluator.Evaluate
        (Element (Position), Network);
   end Evaluate;

   -----------------
   -- Get_Network --
   -----------------

   function Get_Network
     (From : Concorde.Handles.Network.Network_Class) return Network_Type
   is
   begin
      if not Network_Map.Contains (From.Identifier) then
         Create_Network (From);
      end if;
      return Network_Map (From.Identifier);
   end Get_Network;

   -----------------------
   -- Get_Network_Value --
   -----------------------

   function Get_Network_Value
     (Network : Network_Type;
      Node    : Network_Node_Type)
      return Network_Value_Type
   is
   begin
      if Network.Value_Map.Contains (Node.Handle.Tag) then
         return Network.Value_Map.Element (Node.Handle.Tag);
      else
         raise Constraint_Error with
           "no such network node [" & Node.Handle.Tag & "]";
      end if;
   end Get_Network_Value;

   --------------
   -- Get_Node --
   --------------

   function Get_Node (Tag : String) return Network_Node_Type is
   begin
      if Network_Node_Map.Contains (Tag) then
         return Network_Node_Map.Element (Tag);
      else
         raise Constraint_Error with
           "no such network node: " & Tag;
      end if;
   end Get_Node;

   -------------
   -- Get_Tag --
   -------------

   function Get_Tag (Node : Network_Node_Type) return String is
   begin
      return Node.Handle.Tag;
   end Get_Tag;

   --------------------
   -- Inertial_Value --
   --------------------

   function Inertial_Value
     (Value     : Network_Value_Type;
      Inertia   : Non_Negative_Real;
      Smoothing : Non_Negative_Real)
      return Real
   is
   begin
      Concorde.Logging.Log
        (Value.Handle.Node.Tag,
         "inertia=" & Image (Inertia)
         & "; smoothing=" & Image (Smoothing));

      if Inertia = 0.0 and then Smoothing = 0.0 then
         return Current_Value (Value);
      end if;

      declare
         use Concorde.Calendar;
         use Value_History_Maps;
         DT          : constant Concorde_Duration := Days (Inertia);
         DS          : constant Concorde_Duration := Days (Smoothing);
         DH          : Concorde_Duration := 0.0;
         Now         : constant Time := Clock;
         Start_Time  : constant Time := Now - DT - DS;
         Finish_Time : constant Time := Now - DT;
         Start       : constant Value_History_Maps.Cursor :=
                         Value.History.Ceiling (Start_Time);
         Finish      : constant Value_History_Maps.Cursor :=
                         Value.History.Floor (Finish_Time);
         Position    : Value_History_Maps.Cursor :=
                         (if Has_Element (Start)
                          then Start
                          else Value.History.First);
         Total       : Real := 0.0;
      begin
         Concorde.Logging.Log
           (Value.Handle.Node.Tag,
            "scanning history from "
            & Image (Start_Time)
            & " to "
            & Image (Finish_Time));

         while Has_Element (Position) loop
            declare
               To : constant Time :=
                      (if Position = Finish
                       then Finish_Time
                       else Key (Position));
               From : constant Time :=
                        (if Position = Value.History.First
                         then Start_Time
                         else Key (Previous (Position)));
               Current_V : constant Real := Element (Position);
               Next_Value    : constant Real :=
                                 (if Position = Value.History.Last
                                  then Value.Current_Value
                                  else Element (Next (Position)));
               Min           : constant Time :=
                                 (if From > Start_Time
                                  then From
                                  else Start_Time);
               Max           : constant Time :=
                                 (if To < Finish_Time
                                  then To
                                  else Finish_Time);
               Min_Scale     : constant Real :=
                                 Real (Min - From)
                                 / Real (To - From);
               Max_Scale     : constant Real :=
                                 Real (Max - From)
                                 / Real (To - From);
               Change        : constant Real :=
                                 Next_Value - Current_V;
               Min_Value     : constant Real :=
                                 Current_V
                                   + Min_Scale * Change;
               Max_Value     : constant Real :=
                                 Current_V
                                   + Max_Scale * Change;
               V             : constant Real :=
                                 (if DS = 0.0
                                  then Min_Value
                                  else Real (Max - Min) * Min_Value
                                  + 0.5 * Real (Max - Min)
                                  * (Max_Value - Min_Value));
            begin
               if DS = 0.0 then
                  Concorde.Logging.Log
                       (Actor    => "network",
                        Location => Value.Handle.Node.Tag,
                        Category => "inertial value",
                        Message  =>
                          "inertial value="
                        & Image (V)
                        & "; inertia=" & Image (Inertia)
                        & "; date=" & Image (Start_Time)
                        & "; interval="
                        & Image (From)
                        & " .. "
                        & Image (To)
                        & "; scale="
                        & Image (Min_Scale)
                        & "; range="
                        & Image (Current_V)
                        & " .. "
                        & Image (Next_Value)
                        & "; current="
                        & Image (Current_Value (Value)));
                     return V;
               end if;

               DH := DH + (Max - Min);
               Total := Total + V;

               Concorde.Logging.Log
                 (Actor    => "network",
                  Location => Value.Handle.Node.Tag,
                  Category => "smoothed value",
                  Message  =>
                    "interval="
                  & "; interval="
                  & Image (From)
                  & " .. "
                  & Image (To)
                  & " "
                  & Image (Real (Max - Min))
                  & "; range=["
                  & Image (Min_Value)
                  & ","
                  & Image (Max_Value)
                  & "]; area="
                  & Image (V)
                  & "; new total="
                  & Image (Total));

            end;

            exit when Position = Finish;
            Next (Position);
         end loop;

         if Smoothing = 0.0 then
            return Current_Value (Value);
         else
            Total := Total + Real (DS - DH) * Current_Value (Value);
            Concorde.Logging.Log
              (Actor    => "network",
               Location => Value.Handle.Node.Tag,
               Category => "smoothed value",
               Message  =>
                 "total="
               & Image (Total)
               & "; interval="
               & Image (Real (DS))
               & "; result="
               & Image (Total / Real (DS)));

            return Total / Real (DS);
         end if;
      end;

   end Inertial_Value;

   ------------------
   -- Load_Network --
   ------------------

   procedure Load_Network is
   begin
      Concorde.Primitives.Loader.Load_Primitives;

      for Node of
        Concorde.Handles.Node.Scan_By_Identifier
      loop
         if Network_Node_Map.Contains (Node.Tag) then
            raise Constraint_Error with
              "node tag is not unique: " & Node.Tag;
         end if;

         Network_Node_Map.Insert
           (Node.Tag,
            new Network_Node_Record'
              (Handle => Node.To_Node_Handle));
      end loop;

      for Calculation of Concorde.Handles.Calculation.Scan_By_Identifier loop
         if Calculation.Expression /= "" then
            declare
               Expr : constant Concorde.Expressions.Expression_Type :=
                        Concorde.Parser.Parse_Expression
                          (Calculation.Expression);
               Eval : constant Concorde.Evaluator.Evaluation_Handle :=
                        Concorde.Evaluator.Compile
                          (Expression => Expr,
                           Current    => Get_Node (Calculation.Node.Tag));
            begin
               Calculation_Map.Insert (Calculation.Identifier, Eval);
            end;
         end if;
      end loop;

      for Metric of
        Concorde.Handles.Derived_Metric.Scan_By_Tag
      loop
         declare
            Id    : constant String := Metric.Calculation.Identifier;
         begin
            Metric_Nodes.Append
              (Calculation_Item'
                 (Node => Get_Node (Metric.Tag),
                  Eval => Calculation_Map.Element (Id)));
         end;
      end loop;

   end Load_Network;

   ---------------------
   -- Remove_Observer --
   ---------------------

   procedure Remove_Observer
     (Value    : Network_Value_Type;
      Observer : not null access Node_Observer'Class)
   is
      use Observer_Lists;
      Position : Cursor :=
                   Value.Observers.Find (Observer_Access (Observer));
   begin
      pragma Assert (Has_Element (Position), "observer not found");
      Value.Observers.Delete (Position);
   end Remove_Observer;

   ------------------
   -- Save_History --
   ------------------

   procedure Save_History (Value : Network_Value_Type) is
      use Concorde.Calendar;
      use Value_History_Maps;
      From : Time := Value.Original_Start;
   begin
      for Position in Value.New_History.Iterate loop
         Concorde.Handles.Historical_Value.Create
           (Network     => Value.Handle.Network,
            Node        => Value.Handle.Node,
            From        => From,
            To          => Key (Position),
            Value       => Element (Position));
         From := Key (Position);
      end loop;
   end Save_History;

   ------------------
   -- Save_Network --
   ------------------

   procedure Save_Network is
   begin
      for Value of Network_Value_List loop
         if Value.Updated then
            Save_Value (Value);
         end if;
      end loop;
   end Save_Network;

   ----------------
   -- Save_Value --
   ----------------

   procedure Save_Value (Value : Network_Value_Type) is
   begin
      Value.Handle.Update_Network_Value
        .Set_Current_Value (Value.Current_Value)
        .Set_Current_Start (Value.Current_Start)
        .Done;
      Save_History (Value);
   end Save_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Value     : Network_Value_Type;
      New_Value : Real)
   is
   begin
      Value.New_Value := New_Value;
   end Set_Value;

   --------------------
   -- Update_Network --
   --------------------

   procedure Update_Network (Network : Network_Type) is
   begin
      for Metric of Metric_Nodes loop
         declare
            Node      : constant Network_Node_Type := Metric.Node;
            Eval      : constant Concorde.Evaluator.Evaluation_Handle :=
                          Metric.Eval;
            New_Value : constant Real :=
                          Concorde.Evaluator.Evaluate (Eval, Network);
         begin
            Concorde.Logging.Log
              ("update",
               Get_Tag (Node)
               & " <- "
               & Concorde.Real_Images.Approximate_Image (New_Value));
            Set_Value
              (Get_Network_Value (Network, Node), New_Value);
         end;
      end loop;

      for Metric of Metric_Nodes loop
         Commit_Value (Get_Network_Value (Network, Metric.Node));
      end loop;

   end Update_Network;

   ------------------
   -- Update_Value --
   ------------------

   procedure Update_Value
     (Network : Network_Type; Tag : String; New_Value : Real)
   is
   begin
      Update_Value (Get_Network_Value (Network, Tag), New_Value);
   end Update_Value;

   ------------------
   -- Update_Value --
   ------------------

   procedure Update_Value (Value : Network_Value_Type; New_Value : Real) is
   begin
      Set_Value (Value, New_Value);
      Commit_Value (Value);
   end Update_Value;

end Concorde.Network;
