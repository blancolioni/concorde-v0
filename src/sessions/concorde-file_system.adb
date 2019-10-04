with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Sets;

package body Concorde.File_System is

   package Node_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Real_Node_Id, Node_Interface'Class);

   package Node_Sets is
     new Ada.Containers.Ordered_Sets (Real_Node_Id);

   Node_Vector : Node_Vectors.Vector;
   Free_Set    : Node_Sets.Set;

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node : in out Leaf_Node;
      Name  : String;
      Child : Node_Id)
   is
      pragma Unreferenced (Node, Child);
   begin
      raise Constraint_Error with
        "Bind_Child called on leaf node";
   end Bind_Child;

   ------------
   -- Create --
   ------------

   function Create
     (Node : Node_Interface'Class)
      return Node_Id
   is
      Id : Node_Id;
   begin
      if Free_Set.Is_Empty then
         Id := Node_Vector.Last_Index + 1;
         Node_Vector.Append (Node);
      else
         Id := Free_Set.First_Element;
         Free_Set.Delete_First;
         Node_Vector.Replace_Element (Id, Node);
      end if;
      return Id;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Node : Node_Id)
   is
   begin
      Free_Set.Insert (Node);
   end Delete;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child (Node : in out Leaf_Node; Name : String)
   is
      pragma Unreferenced (Node, Name);
   begin
      raise Constraint_Error with
        "Delete_Child called on leaf node";
   end Delete_Child;

   ---------
   -- Get --
   ---------

   function Get
     (Id : Node_Id)
      return Node_Interface'Class
   is
   begin
      if Free_Set.Contains (Id)
        or else Id > Node_Vector.Last_Index
      then
         raise Constraint_Error with
           "Context.File_System.Get: bad node id";
      end if;
      return Node_Vector.Element (Id);
   end Get;

   ---------------
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node  : Leaf_Node;
      Child : String)
      return Node_Id
   is
      pragma Unreferenced (Node, Child);
   begin
      return (raise Constraint_Error with
                "Get_Child called on leaf node");
   end Get_Child;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Leaf_Node; Name : String) return Boolean
   is
      pragma Unreferenced (Node, Name);
   begin
      return False;
   end Has_Child;

   -------------
   -- Is_Leaf --
   -------------

   overriding function Is_Leaf (Node : Leaf_Node) return Boolean is
      pragma Unreferenced (Node);
   begin
      return True;
   end Is_Leaf;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Leaf_Node;
      Process : not null access procedure
        (Name : String; Child : Node_Interface'Class))
   is
      pragma Unreferenced (Node, Process);
   begin
      raise Constraint_Error with
        "Iterate_Children called on leaf node";
   end Iterate_Children;

   ------------
   -- Update --
   ------------

   function Update
     (Node : Node_Id)
      return access Node_Interface'Class
   is
   begin
      return Node_Vector.Reference (Node).Element;
   end Update;

end Concorde.File_System;
