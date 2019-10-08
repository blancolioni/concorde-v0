with Concorde.File_System.Db_FS;

with Concorde.Db.Faction;
with Concorde.Db.World;

package body Concorde.File_System.Home is

   function Get_Reference
     (World : Concorde.Db.World.World_Type)
      return Concorde.Db.World_Reference
   is (World.Get_World_Reference);

   function Get_Owned_World
     (Faction : Concorde.Db.Faction_Reference;
      Name    : String)
      return Concorde.Db.World_Reference;

   procedure Iterate_Owned_Worlds
     (Faction : Concorde.Db.Faction_Reference;
      Process : not null access
        procedure (World : Concorde.Db.World.World_Interface'Class));

   package World_Directory is
     new Concorde.File_System.Db_FS
       (Container_Handle      => Concorde.Db.Faction_Reference,
        Record_Reference      => Concorde.Db.World_Reference,
        Get_Record            => Concorde.Db.World.Get,
        Get_Reference         => Get_Reference,
        Get_Reference_By_Name => Get_Owned_World,
        Null_Record_Reference => Concorde.Db.Null_World_Reference,
        Record_Interface      => Concorde.Db.World.World_Interface,
        Iterate               => Iterate_Owned_Worlds,
        "="                   => Concorde.Db."=");

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
     new Branch_Node with
      record
         Ref : Concorde.Db.Faction_Reference;
      end record;

   overriding function Has_Child
     (Node : Faction_Node_Record;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node  : Faction_Node_Record;
      Child : String)
      return Node_Id;

   overriding procedure Bind_Child
     (Node  : in out Faction_Node_Record;
      Name  : String;
      Child : Node_Id);

   overriding procedure Delete_Child
     (Node   : in out Faction_Node_Record;
      Name   : String);

   overriding procedure Iterate_Children
     (Node    : Faction_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id));

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

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Faction_Node_Record;
      Name  : String;
      Child : Node_Id)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Bind_Child;

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

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Faction_Node_Record;
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
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node  : Faction_Node_Record;
      Child : String)
      return Node_Id
   is
      pragma Unreferenced (Child);
   begin
      return World_Directory.Get_Container_Node_Id (Node.Ref);
   end Get_Child;

   ---------------------
   -- Get_Owned_World --
   ---------------------

   function Get_Owned_World
     (Faction : Concorde.Db.Faction_Reference;
      Name    : String)
      return Concorde.Db.World_Reference
   is
      use Concorde.Db;
      Owner   : constant Owner_Reference :=
        Concorde.Db.Faction.Get (Faction).Get_Owner_Reference;
      World   : constant Concorde.Db.World.World_Type :=
        Concorde.Db.World.First_By_Name (Name);
   begin
      if World.Owner = Owner then
         return World.Get_World_Reference;
      else
         return Null_World_Reference;
      end if;
   end Get_Owned_World;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Home_Record;
      Name : String)
      return Boolean
   is
      pragma Unreferenced (Node);
      use type Concorde.Db.Faction_Reference;
   begin
      return Concorde.Db.Faction.First_Reference_By_Name (Name)
        /= Concorde.Db.Null_Faction_Reference;
   end Has_Child;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Faction_Node_Record;
      Name : String)
      return Boolean
   is
      pragma Unreferenced (Node);
   begin
      return Name = "worlds";
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
      use type Concorde.Db.Faction_Reference;
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

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Faction_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Node_Id))
   is
   begin
      Process ("worlds", World_Directory.Get_Container_Node_Id (Node.Ref));
   end Iterate_Children;

   --------------------------
   -- Iterate_Owned_Worlds --
   --------------------------

   procedure Iterate_Owned_Worlds
     (Faction : Concorde.Db.Faction_Reference;
      Process : not null access
        procedure (World : Concorde.Db.World.World_Interface'Class))
   is
   begin
      for World of Concorde.Db.World.Select_By_Owner
        (Concorde.Db.Faction.Get (Faction).Get_Owner_Reference)
      loop
         Process (World);
      end loop;
   end Iterate_Owned_Worlds;

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
