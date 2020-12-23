with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with Concorde.File_System.Db_FS;

with Concorde.Handles.Faction;
with Concorde.Handles.World;

package body Concorde.File_System.Home is

   function Get_Reference
     (World : Concorde.Handles.World.World_Type)
      return Concorde.Handles.World_Reference
   is (World.Get_World_Reference);

   function Get_Owned_World
     (Faction : Concorde.Handles.Faction.Faction_Handle;
      Name    : String)
      return Concorde.Handles.World_Reference;

   procedure Iterate_Owned_Worlds
     (Faction : Concorde.Handles.Faction.Faction_Handle;
      Process : not null access
        procedure (World : Concorde.Handles.World.World_Interface'Class));

   function World_Contents
     (World : Concorde.Handles.World.World_Type)
      return String;

   package World_Directory is
     new Concorde.File_System.Db_FS
       (Container_Handle      => Concorde.Handles.Faction.Faction_Handle,
        Record_Reference      => Concorde.Handles.World_Reference,
        Get_Record            => Concorde.Handles.World.Get,
        Get_Reference         => Get_Reference,
        Get_Reference_By_Name => Get_Owned_World,
        Null_Record_Reference => Concorde.Handles.Null_World_Reference,
        Record_Interface      => Concorde.Handles.World.World_Interface,
        Iterate               => Iterate_Owned_Worlds,
        Contents              => World_Contents,
        "="                   => Concorde.Handles."=");

   type Faction_Node_Id is
     new Node_Id_Interface with
      record
         Ref : Concorde.Handles.Faction.Faction_Handle;
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
         Ref : Concorde.Handles.Faction.Faction_Handle;
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
           Concorde.Handles.Faction.First_By_Name (Child));
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
     (Faction : Concorde.Handles.Faction.Faction_Handle;
      Name    : String)
      return Concorde.Handles.World_Reference
   is
      use Concorde.Db;
      Owner   : constant Owner_Reference :=
        Concorde.Handles.Faction.Get (Faction).Get_Owner_Reference;
      World   : constant Concorde.Handles.World.World_Type :=
        Concorde.Handles.World.First_By_Name (Name);
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
      use type Concorde.Handles.Faction.Faction_Handle;
   begin
      return Concorde.Handles.Faction.First_By_Name (Name)
        /= Concorde.Handles.Null_Faction_Reference;
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
      use type Concorde.Handles.Faction.Faction_Handle;
   begin
      return Id.Ref = Concorde.Handles.Null_Faction_Reference;
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
      for Faction of Concorde.Handles.Faction.Scan_By_Name loop
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
     (Faction : Concorde.Handles.Faction.Faction_Handle;
      Process : not null access
        procedure (World : Concorde.Handles.World.World_Interface'Class))
   is
   begin
      for World of Concorde.Handles.World.Select_By_Owner
        (Concorde.Handles.Faction.Get (Faction).Get_Owner_Reference)
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

   --------------------
   -- World_Contents --
   --------------------

   function World_Contents
     (World : Concorde.Handles.World.World_Type)
      return String
   is
      package String_Vectors is
        new Ada.Containers.Indefinite_Vectors (Positive, String);

      Headings : String_Vectors.Vector;
      Values   : String_Vectors.Vector;

      procedure Add (Field_Name : String);

      function Collate return String;

      ---------
      -- Add --
      ---------

      procedure Add (Field_Name : String) is
      begin
         Headings.Append (Field_Name);
         Values.Append (World.Get (Field_Name));
      end Add;

      -------------
      -- Collate --
      -------------

      function Collate return String is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;
      begin
         for I in 1 .. Headings.Last_Index loop
            Result := Result & Headings.Element (I) & ": "
              & Values.Element (I) & Character'Val (10);
         end loop;
         return To_String (Result);
      end Collate;

   begin
      Add ("name");
      Add ("category");
      Add ("climate");
      Add ("habitability");

      return Collate;
   end World_Contents;

end Concorde.File_System.Home;
