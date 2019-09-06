with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Concorde.Db;

package Concorde.Contexts is

   type Root_Context_Type is abstract tagged private;
   subtype Context_Type is Root_Context_Type'Class;

   package Child_Name_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Context_List is tagged private;

   function Class
     (Context : Root_Context_Type)
      return String
      is abstract;

   function Name
     (Context : Root_Context_Type)
      return String
      is abstract;

   procedure Get_Child_Contexts
     (Context  : Root_Context_Type;
      Children : in out Context_List'Class)
   is abstract;

   function Get_Content
     (Context : Root_Context_Type'Class)
      return String;

   procedure Iterate_Contexts
     (Context : Root_Context_Type'Class;
      Process : not null access
        procedure (Context : Context_Type));

   procedure Iterate_Content_Lines
     (Context : Root_Context_Type;
      Process : not null access
        procedure (Line : String));

   function Is_Root (Context : Root_Context_Type) return Boolean;
   function Is_Valid (Context : Root_Context_Type) return Boolean;

   function Has_Child_Context
     (Context : Root_Context_Type;
      Name    : String)
      return Boolean;

   function Child_Context
     (Context : Root_Context_Type;
      Name    : String)
      return Context_Type
     with Pre'Class =>
       Root_Context_Type'Class (Context).Has_Child_Context (Name);

   procedure Get_Child_Names
     (Context : Root_Context_Type'Class;
      Names   : out Child_Name_Lists.List);

   procedure Iterate_Child_Names
     (Context : Root_Context_Type'Class;
      Process : not null access
        procedure (Name : String));

   procedure Match_Children
     (Context : Root_Context_Type'Class;
      Pattern : String;
      Matches : out Child_Name_Lists.List);

   type Context_Path is
     new Root_Context_Type with private;

   function Context
     (Path : Context_Path)
      return Context_Type;

   procedure To_Parent
     (Path : in out Context_Path);

   procedure To_Child
     (Path          : in out Context_Path;
      Child_Context : Context_Type);

   function Go
     (Start : Context_Path;
      Scope : String)
      return Context_Path;

   function Initial_Context_Path
     (Faction : Concorde.Db.Faction_Reference)
      return Context_Path;

private

   type Root_Context_Type is abstract tagged
      record
         null;
      end record;

   function Is_Root (Context : Root_Context_Type) return Boolean
   is (False);

   function Is_Valid (Context : Root_Context_Type) return Boolean
   is (True);

   package Context_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Context_Type);

   type Context_List is new Context_Lists.List with null record;

   type Context_Path is new Root_Context_Type with
      record
         List : Context_Lists.List;
      end record;

   overriding function Class
     (Context : Context_Path)
      return String;

   overriding function Name
     (Context : Context_Path)
      return String;

   overriding function Is_Valid (Context : Context_Path) return Boolean
   is (not Context.List.Is_Empty and then Context.Context.Is_Valid);

   overriding function Has_Child_Context
     (Context : Context_Path;
      Name    : String)
      return Boolean
   is (not Context.List.Is_Empty
       and then Context.List.First_Element.Has_Child_Context (Name));

   overriding function Child_Context
     (Context : Context_Path;
      Name    : String)
      return Context_Type;

   overriding procedure Get_Child_Contexts
     (Context  : Context_Path;
      Children : in out Context_List'Class);

   overriding procedure Iterate_Content_Lines
     (Context : Context_Path;
      Process : not null access
        procedure (Line : String));

end Concorde.Contexts;
