with Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with WL.Processes;
with WL.String_Maps;
with WL.String_Sets;

with Concorde.Calendar;
with Concorde.Evaluator;
with Concorde.Expressions;
with Concorde.Logging;
with Concorde.Parser;
with Concorde.Primitives.Loader;
with Concorde.Real_Images;
with Concorde.Symbols;

with Concorde.Handles.Derived_Metric;
with Concorde.Handles.Effect;
with Concorde.Handles.Historical_Value;
with Concorde.Handles.Node;
with Concorde.Handles.Network_Value;

package body Concorde.Network is

   package Calculation_Maps is
     new WL.String_Maps (Concorde.Evaluator.Evaluation_Handle,
                         Concorde.Evaluator."=");

   Calculation_Map : Calculation_Maps.Map;

   type Calculation_Item is
      record
         Node  : Concorde.Nodes.Node_Handle;
         Eval  : Concorde.Evaluator.Evaluation_Handle;
      end record;

   package Calculation_Item_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Calculation_Item);

   package Calculation_Layer_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Calculation_Item_Lists.List, Calculation_Item_Lists."=");

   package Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Concorde.Nodes.Node_Handle, Concorde.Nodes."=");

   Metric_Nodes  : Calculation_Item_Lists.List;
   Initial_Nodes : Node_Lists.List;
   All_Nodes     : Node_Lists.List;
   Layer_Nodes   : Calculation_Layer_Lists.List;

   type Network_Record is
      record
         Network   : Concorde.Handles.Network.Network_Handle;
         Container : Concorde.Nodes.Value_Container;
      end record;

   package Network_Vectors is
     new Ada.Containers.Vectors (Network_Handle, Network_Record);

   Vector : Network_Vectors.Vector;

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
               Current_Value => Value,
               New_Value     => Value);

            Concorde.Handles.Historical_Value.Create
              (Network     => Network,
               Node        => Node,
               Is_Previous => True,
               From        => Concorde.Calendar.Zero_Time,
               To          => Concorde.Calendar.Clock,
               Next        => Value,
               Value       => Value);
         end;
      end loop;
   end Create_Initial_Network;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Network     : Concorde.Handles.Network.Network_Class;
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
                            (Calculation.Expression));
            Inserted : Boolean;
         begin
            Calculation_Map.Insert
              (Id, Handle, Position, Inserted);
            pragma Assert (Inserted);
         end;
      end if;

      pragma Assert (Calculation_Map.Contains (Id));

      return Concorde.Evaluator.Evaluate
        (Element (Position),
         Get_Value_Container (Get_Network_Handle (Network)));
   end Evaluate;

   ------------------------
   -- Get_Network_Handle --
   ------------------------

   function Get_Network_Handle
     (Network : Concorde.Handles.Network.Network_Class)
      return Network_Handle
   is
   begin
      for Handle in 1 .. Vector.Last_Index loop
         if Vector.Element (Handle).Network.Identifier
           = Network.Identifier
         then
            return Handle;
         end if;
      end loop;
      Vector.Append
        (Network_Record'
           (Network   => Network.To_Network_Handle,
            Container => Concorde.Nodes.New_Container (Network)));
      return Vector.Last_Index;
   end Get_Network_Handle;

   -------------------------
   -- Get_Value_Container --
   -------------------------

   function Get_Value_Container
     (Handle : Network_Handle)
      return Concorde.Nodes.Value_Container
   is
   begin
      return Vector.Element (Handle).Container;
   end Get_Value_Container;

   ------------------
   -- Load_Network --
   ------------------

   procedure Load_Network is

      package String_Lists is
        new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

      package Dependency_Maps is
        new WL.String_Maps (String_Lists.List, String_Lists."=");

      Calculated : WL.String_Sets.Set;
      Remaining  : WL.String_Sets.Set;
      Initial    : WL.String_Sets.Set;

      Dependencies : Dependency_Maps.Map;

      procedure Update_Dependencies
        (Node : Concorde.Handles.Node.Node_Class;
         Ids  : Concorde.Symbols.Symbol_Id_Array);

      -------------------------
      -- Update_Dependencies --
      -------------------------

      procedure Update_Dependencies
        (Node : Concorde.Handles.Node.Node_Class;
         Ids  : Concorde.Symbols.Symbol_Id_Array)
      is
         Tag : constant String := Node.Tag;
      begin
         if not Dependencies.Contains (Tag) then
            Dependencies.Insert (Tag, String_Lists.Empty_List);
         end if;

         declare
            List : String_Lists.List renames
                     Dependencies (Tag);
         begin
            for Id of Ids loop
               declare
                  Name : constant String :=
                           Concorde.Symbols.Get_Name (Id);
               begin
                  if Name /= "x"
                    and then not Concorde.Primitives.Is_Primitive (Name)
                    and then not List.Contains (Name)
                  then
                     List.Append (Name);
                  end if;
               end;
            end loop;
         end;
      end Update_Dependencies;

   begin

      Ada.Text_IO.Put_Line ("Loading primitives ...");

      Concorde.Primitives.Loader.Load_Primitives;

      Ada.Text_IO.Put_Line ("Loading calculations ...");

      for Calculation of Concorde.Handles.Calculation.Scan_By_Identifier loop
         if Calculation.Expression /= "" then
            declare
               Expr : constant Concorde.Expressions.Expression_Type :=
                        Concorde.Parser.Parse_Expression
                          (Calculation.Expression);
               Eval : constant Concorde.Evaluator.Evaluation_Handle :=
                        Concorde.Evaluator.Compile (Expr);
               Ids  : constant Concorde.Symbols.Symbol_Id_Array :=
                        Expr.Free_Variables;

               function Show_Ids (Index : Positive) return String
               is (if Index = Ids'First
                   then Concorde.Symbols.Get_Name (Ids (Index))
                   & Show_Ids (Index + 1)
                   elsif Index <= Ids'Last
                   then ", " & Concorde.Symbols.Get_Name (Ids (Index))
                   & Show_Ids (Index + 1)
                   else "");

            begin

               if False then
                  Ada.Text_IO.Put_Line
                    (Calculation.Node.Tag
                     & ": "
                     & Show_Ids (Ids'First));
               end if;

               Update_Dependencies (Calculation.Node, Ids);
               Calculation_Map.Insert (Calculation.Identifier, Eval);
            end;
         end if;
      end loop;

      Ada.Text_IO.Put_Line ("Finding all nodes ...");

      for Node of Concorde.Handles.Node.Scan_By_Tag loop
         Remaining.Include (Node.Tag);
         Initial.Include (Node.Tag);
         All_Nodes.Append (Concorde.Nodes.Get_Handle (Node));
      end loop;

      Ada.Text_IO.Put_Line ("Loading metrics ...");

      for Metric of
        Concorde.Handles.Derived_Metric.Scan_By_Tag
      loop
         declare
            Id    : constant String := Metric.Calculation.Identifier;
         begin
            Metric_Nodes.Append
              (Calculation_Item'
                 (Node =>
                      Concorde.Nodes.Get_Handle (Metric),
                  Eval => Calculation_Map.Element (Id)));
            Initial.Delete (Metric.Tag);
         end;
      end loop;

      Ada.Text_IO.Put_Line ("Finding initial nodes");

      for Node of Concorde.Handles.Node.Scan_By_Tag loop
         if Initial.Contains (Node.Tag) then
            if not Concorde.Handles.Effect.First_By_To (Node).Has_Element then
               Remaining.Delete (Node.Tag);
               Calculated.Include (Node.Tag);
               Initial_Nodes.Append
                 (Concorde.Nodes.Get_Handle (Node));
            end if;
         end if;
      end loop;

      if False then
         while not Remaining.Is_Empty loop

            Ada.Text_IO.Put_Line ("Finding next layer");

            declare
               Finished : String_Lists.List;
               Layer    : Calculation_Item_Lists.List;

               procedure Check (Tag : String);

               -----------
               -- Check --
               -----------

               procedure Check (Tag : String) is
                  Node      : constant Concorde.Handles.Node.Node_Handle :=
                                Concorde.Handles.Node.Get_By_Tag (Tag);
                  Deps      : constant String_Lists.List :=
                                (if Dependencies.Contains (Tag)
                                 then Dependencies (Tag)
                                 else String_Lists.Empty_List);
                  Available : Boolean := True;
               begin

                  Ada.Text_IO.Put ("checking: " & Tag);
                  Ada.Text_IO.Flush;

                  for Dep of Deps loop
                     if not Calculated.Contains (Dep) then
                        Ada.Text_IO.Put_Line
                          (": dependency " & Dep & " unavailable");
                        Available := False;
                        exit;
                     end if;
                  end loop;

                  if Available then
                     declare
                        use Concorde.Handles.Calculation;
                        Calculation : constant Calculation_Class :=
                                        First_By_Node (Node);
                     begin
                        Finished.Append (Tag);
                        Layer.Append
                          (Calculation_Item'
                             (Node =>
                                  Concorde.Nodes.Get_Handle (Node),
                              Eval => Calculation_Map.Element
                                (Calculation.Identifier)));
                        Ada.Text_IO.Put_Line (": ready");
                     end;
                  end if;
               end Check;

            begin

               if False then
                  Remaining.Iterate (Check'Access);

                  if Layer.Is_Empty then
                     raise Constraint_Error with
                       "cannot construct layer";
                  end if;

                  for Tag of Finished loop
                     Remaining.Delete (Tag);
                     Calculated.Include (Tag);
                  end loop;

                  Layer_Nodes.Append (Layer);
               end if;

            end;
         end loop;
      end if;

   end Load_Network;

   ------------------
   -- Save_Network --
   ------------------

   procedure Save_Network is
      Process : WL.Processes.Process_Type;
   begin
      Process.Start_Bar ("saving network",
                         Natural (Vector.Last_Index)
                         * Concorde.Nodes.Node_Count,
                         With_Percentage => True,
                         Bar_Length      => 60);
      for E of Vector loop

         declare

            procedure Save_Value
              (Node    : Concorde.Handles.Node.Node_Handle;
               Value   : Real);

            ----------------
            -- Save_Value --
            ----------------

            procedure Save_Value
              (Node    : Concorde.Handles.Node.Node_Handle;
               Value   : Real)
            is
               Network_Value         : constant Concorde.Handles.Network_Value
                 .Network_Value_Handle :=
                   Concorde.Handles.Network_Value
                     .Get_By_Network_Value (E.Network, Node);
            begin
               if Network_Value.Has_Element then
                  Network_Value.Update_Network_Value
                    .Set_Current_Value (Value)
                    .Done;
               else
                  Concorde.Logging.Log
                    ("warning", Node.Tag & " has no value in network "
                     & E.Network.Identifier);
               end if;

               Process.Tick;

            end Save_Value;

         begin
            Concorde.Nodes.Iterate
              (E.Container, Save_Value'Access);
         end;

      end loop;

      Process.Finish;

   end Save_Network;

   --------------------
   -- Update_Network --
   --------------------

   procedure Update_Network (Network : Network_Handle) is
      Container : constant Concorde.Nodes.Value_Container :=
                    Get_Value_Container (Network);
   begin
      for Metric of Metric_Nodes loop
         declare
            Node      : constant Concorde.Nodes.Node_Handle :=
                          Metric.Node;
            Eval      : constant Concorde.Evaluator.Evaluation_Handle :=
                          Metric.Eval;
            New_Value : constant Real :=
                          Concorde.Evaluator.Evaluate
                            (Handle  => Eval,
                             Network => Container);
         begin
            Concorde.Logging.Log
              ("update",
               Concorde.Nodes.Get_Tag (Node)
               & " <- "
               & Concorde.Real_Images.Approximate_Image (New_Value));
            Concorde.Nodes.Set_Value
              (Container, Node, New_Value);
         end;
      end loop;

      for Metric of Metric_Nodes loop
         Concorde.Nodes.Commit_Value (Container, Metric.Node);
      end loop;

   end Update_Network;

   ------------------
   -- Update_Value --
   ------------------

   procedure Update_Value
     (Network   : Network_Handle;
      Tag       : String;
      New_Value : Real)
   is
   begin
      Concorde.Nodes.Update_Value
        (Get_Value_Container (Network), Tag, New_Value);
   end Update_Value;

end Concorde.Network;
