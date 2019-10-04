with Ada.Strings.Unbounded;

with Concorde.Db.Faction;
with Concorde.Db.World;

package body Concorde.File_System.Home is

   use type Concorde.Db.Faction_Reference;

   type Faction_Node_Id is
     new Node_Id_Interface with
      record
         Ref : Concorde.Db.Faction_Reference;
      end record;

   overriding function Is_Empty
     (Id : Faction_Node_Id)
      return Boolean;

   overriding function Get
     (Id : Faction_Node_Id)
      return Node_Interface'Class;

   overriding function Update
     (Node : Faction_Node_Id)
      return access Node_Interface'Class;

   type Faction_Node_Record is
     new Leaf_Node with
      record
         Ref : Concorde.Db.Faction_Reference;
      end record;

   overriding function Contents
     (Node : Faction_Node_Record)
      return String;

   type Home_Record is
     new Branch_Node with null record;

   overriding function Has_Child
     (Node : Home_Record;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node  : Home_Record;
      Child : String)
      return Node_Id;

   overriding procedure Bind_Child
     (Node  : in out Home_Record;
      Name  : String;
      Child : Node_Id);

   overriding procedure Delete_Child
     (Node   : in out Home_Record;
      Name   : String);

   overriding procedure Iterate_Children
     (Node    : Home_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id));

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Home_Record;
      Name  : String;
      Child : Node_Id)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Bind_Child;

   --------------
   -- Contents --
   --------------

   overriding function Contents
     (Node : Faction_Node_Record)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for World of
        Concorde.Db.World.Select_By_Owner
          (Concorde.Db.Faction.Get (Node.Ref).Get_Owner_Reference)
      loop
         Result := Result & World.Name & Character'Val (10);
      end loop;
      return To_String (Result);
   end Contents;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Home_Record;
      Name   : String)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Delete_Child;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Id : Faction_Node_Id)
      return Node_Interface'Class
   is
   begin
      return Faction_Node_Record'
        (Ref => Id.Ref);
   end Get;

   ---------------
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node  : Home_Record;
      Child : String)
      return Node_Id
   is
      pragma Unreferenced (Node);
   begin
      return Faction_Node_Id'
        (Ref =>
           Concorde.Db.Faction.First_Reference_By_Name (Child));
   end Get_Child;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Home_Record;
      Name : String)
      return Boolean
   is
      pragma Unreferenced (Node);
   begin
      return Concorde.Db.Faction.First_Reference_By_Name (Name)
        /= Concorde.Db.Null_Faction_Reference;
   end Has_Child;

   ---------------
   -- Home_Node --
   ---------------

   function Home_Node return Node_Interface'Class is
   begin
      return Home : Home_Record;
   end Home_Node;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty
     (Id : Faction_Node_Id)
      return Boolean
   is
   begin
      return Id.Ref = Concorde.Db.Null_Faction_Reference;
   end Is_Empty;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Home_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id))
   is
      pragma Unreferenced (Node);
   begin
      for Faction of Concorde.Db.Faction.Scan_By_Name loop
         Process (Faction.Name,
                  Faction_Node_Id'
                    (Ref => Faction.Get_Faction_Reference));
      end loop;
   end Iterate_Children;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Node : Faction_Node_Id)
      return access Node_Interface'Class
   is
      pragma Unreferenced (Node);
   begin
      return (raise Constraint_Error with
                "read-only filesystem");
   end Update;

end Concorde.File_System.Home;
