with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with WL.String_Maps;

with Concorde.Handles.Network_Value;

package body Concorde.Nodes is

   package Node_Handle_Maps is
     new WL.String_Maps (Node_Handle);

   Node_Handle_Map : Node_Handle_Maps.Map;

   package Node_Handle_Vectors is
     new Ada.Containers.Vectors
       (Node_Handle, Concorde.Handles.Node.Node_Handle,
        Concorde.Handles.Node."=");

   Node_Handle_Vector : Node_Handle_Vectors.Vector;

   package Value_Handle_Maps is
     new WL.String_Maps (Value_Handle);

   type Observer_Access is access all Node_Observer'Class;

   package Observer_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Observer_Access);

   type Value_Record is
      record
         Loaded        : Boolean := False;
         Current_Value : Real    := 0.0;
         New_Value     : Real    := 0.0;
         Observers : Observer_Lists.List;
      end record;

   package Value_Handle_Vectors is
     new Ada.Containers.Vectors (Value_Handle, Value_Record);

   package Node_Value_Handle_Vectors is
     new Ada.Containers.Vectors (Node_Handle, Value_Handle);

   type Value_Container_Element is
      record
         Map       : Value_Handle_Maps.Map;
         Nodes     : Node_Value_Handle_Vectors.Vector;
      end record;

   package Value_Container_Vectors is
     new Ada.Containers.Vectors (Value_Container, Value_Container_Element);

   Value_Handle_Vector    : Value_Handle_Vectors.Vector;
   Value_Container_Vector : Value_Container_Vectors.Vector;

   --  procedure Check_Value
   --    (Container : Value_Container;
   --     Tag       : String);

   ------------------
   -- Add_Observer --
   ------------------

   procedure Add_Observer
     (Handle   : Value_Handle;
      Observer : not null access Node_Observer'Class)
   is
   begin
      Value_Handle_Vector (Handle).Observers.Append
        (Observer_Access (Observer));
   end Add_Observer;

   ------------------
   -- Commit_Value --
   ------------------

   procedure Commit_Value (Node : Value_Handle) is
      Rec : Value_Record renames Value_Handle_Vector (Node);
   begin
      Rec.Current_Value := Rec.New_Value;
   end Commit_Value;

   ------------------
   -- Commit_Value --
   ------------------

   procedure Commit_Value
     (Container : Value_Container;
      Node      : Node_Handle)
   is
   begin
      Commit_Value (Value_Container_Vector (Container).Nodes (Node));
   end Commit_Value;

   -------------------
   -- Current_Value --
   -------------------

   function Current_Value (Node : Value_Handle) return Real is
   begin
      return Value_Handle_Vector.Element (Node).Current_Value;
   end Current_Value;

   -------------------
   -- Current_Value --
   -------------------

   function Current_Value
     (Container : Value_Container;
      Tag       : String)
      return Real
   is
      Map    : Value_Handle_Maps.Map renames
                 Value_Container_Vector (Container).Map;
      pragma Assert (not Map.Is_Empty, "value map is empty");
      Handle : constant Value_Handle :=
                 (if Map.Contains (Tag)
                  then Map.Element (Tag)
                  else raise Constraint_Error with
                  Tag & " not found in value map");
   begin
      return Current_Value (Handle);
   end Current_Value;

   -------------------
   -- Current_Value --
   -------------------

   function Current_Value
     (Container : Value_Container;
      Node      : Node_Handle)
      return Real
   is
   begin
      return Current_Value
        (Get_Handle (Container, Node));
   end Current_Value;

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle
     (Node    : Concorde.Handles.Node.Node_Class)
      return Node_Handle
   is
      Key : constant String := Node.Tag;
   begin
      if not Node_Handle_Map.Contains (Key) then
         Node_Handle_Vector.Append (Node.To_Node_Handle);
         Node_Handle_Map.Insert (Key, Node_Handle_Vector.Last_Index);
      end if;

      return Node_Handle_Map (Key);
   end Get_Handle;

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle
     (Tag : String)
      return Node_Handle
   is
   begin
      return Get_Handle
        (Concorde.Handles.Node.Get_By_Tag (Tag));
   end Get_Handle;

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle
     (Container : Value_Container;
      Node      : Node_Handle)
      return Value_Handle
   is
   begin
      return Value_Container_Vector (Container).Nodes (Node);
   end Get_Handle;

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle
     (Container : Value_Container;
      Tag       : String)
      return Value_Handle
   is
   begin
      return Get_Handle (Container, Node_Handle_Map (Tag));
   end Get_Handle;

   -------------
   -- Get_Tag --
   -------------

   function Get_Tag
     (Handle : Node_Handle)
      return String
   is
   begin
      return Node_Handle_Vector (Handle).Tag;
   end Get_Tag;

   --------------------
   -- Inertial_Value --
   --------------------

   function Inertial_Value
     (Node      : Value_Handle;
      Inertia   : Non_Negative_Real;
      Smoothing : Non_Negative_Real)
      return Real
   is
      pragma Unreferenced (Inertia, Smoothing);
   begin
      return Current_Value (Node);
   end Inertial_Value;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Value_Container;
      Process   : not null access procedure
        (Node : Concorde.Handles.Node.Node_Handle; Value : Real))
   is
   begin
      for Handle in 1 .. Node_Handle_Vector.Last_Index loop
         Process (Node_Handle_Vector.Element (Handle),
                  Current_Value (Get_Handle (Container, Handle)));
      end loop;
   end Iterate;

   -------------------
   -- New_Container --
   -------------------

   function New_Container
     (Network : Concorde.Handles.Network.Network_Class)
      return Value_Container
   is
      Handles : Node_Value_Handle_Vectors.Vector;
      Map     : Value_Handle_Maps.Map;
   begin
      for Handle in 1 .. Node_Handle_Vector.Last_Index loop
         declare
            Node  : constant Concorde.Handles.Node.Node_Handle :=
                      Node_Handle_Vector.Element (Handle);
            Tag   : constant String := Node.Tag;
            Value : constant Real :=
                      Concorde.Handles.Network_Value
                        .Get_By_Network_Value (Network, Node)
                        .Current_Value;
         begin
            Value_Handle_Vector.Append
              (Value_Record'
                 (Loaded        => True,
                  Current_Value => Value,
                  New_Value     => Value,
                  Observers     => <>));
            Map.Insert (Tag, Value_Handle_Vector.Last_Index);
            Handles.Append (Value_Handle_Vector.Last_Index);
         end;
      end loop;

      Value_Container_Vector.Append
        (Value_Container_Element'
           (Map => Map,
            Nodes => Handles));

      return Value_Container_Vector.Last_Index;
   end New_Container;

   ----------------
   -- Node_Count --
   ----------------

   function Node_Count return Natural is
   begin
      return Natural (Node_Handle_Vector.Last_Index);
   end Node_Count;

   ---------------------
   -- Remove_Observer --
   ---------------------

   procedure Remove_Observer
     (Handle : Value_Handle; Observer : not null access Node_Observer'Class)
   is
      use Observer_Lists;
      Position : Cursor :=
                   Value_Handle_Vector (Handle)
                   .Observers.Find (Observer_Access (Observer));
   begin
      pragma Assert (Has_Element (Position), "observer not found");
      Value_Handle_Vector (Handle).Observers.Delete (Position);
   end Remove_Observer;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Node      : Value_Handle;
      New_Value : Real)
   is
      Rec : Value_Record renames Value_Handle_Vector (Node);
   begin
      Rec.New_Value := New_Value;
   end Set_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Container : Value_Container;
      Node      : Node_Handle;
      New_Value : Real)
   is
      Handle : constant Value_Handle :=
                 Value_Container_Vector (Container).Nodes (Node);
   begin
      Set_Value (Handle, New_Value);
   end Set_Value;

   ------------------
   -- Update_Value --
   ------------------

   procedure Update_Value
     (Node      : Value_Handle;
      New_Value : Real)
   is
   begin
      Set_Value (Node, New_Value);
      Commit_Value (Node);
   end Update_Value;

   ------------------
   -- Update_Value --
   ------------------

   procedure Update_Value
     (Container : Value_Container;
      Tag       : String;
      New_Value : Real)
   is
      Map : Value_Handle_Maps.Map renames
              Value_Container_Vector (Container).Map;
      Handle : constant Value_Handle := Map (Tag);
   begin
      Update_Value (Handle, New_Value);
   end Update_Value;

   ------------------
   -- Update_Value --
   ------------------

   procedure Update_Value
     (Container : Value_Container;
      Node      : Node_Handle;
      New_Value : Real)
   is
   begin
      Update_Value (Value_Container_Vector (Container).Nodes (Node),
                    New_Value);
   end Update_Value;

end Concorde.Nodes;
