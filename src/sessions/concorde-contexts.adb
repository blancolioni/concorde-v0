with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Unbounded;

with Concorde.Version;

package body Concorde.Contexts is

   package Node_Holders is
     new Ada.Containers.Indefinite_Holders (Context_Node_Interface'Class);

   type Child_Node_Record is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Node : Node_Holders.Holder;
      end record;

   package Child_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Child_Node_Record);

   package Child_Node_Maps is
     new WL.String_Maps (Child_Node_Lists.Cursor, Child_Node_Lists."=");

   type Simple_Node_Record is
     new Context_Node_Interface with
      record
         Child_List : Child_Node_Lists.List;
         Child_Map  : Child_Node_Maps.Map;
      end record;

   overriding function Has_Children
     (Node : Simple_Node_Record)
      return Boolean
   is (True);

   overriding function Has_Child
     (Node : Simple_Node_Record;
      Name : String)
      return Boolean
   is (Node.Child_Map.Contains (Name));

   overriding function Get_Child
     (Node  : Simple_Node_Record;
      Child : String)
      return Context_Node_Interface'Class
   is (Child_Node_Lists.Element (Node.Child_Map.Element (Child)).Node.Element);

   overriding procedure Iterate_Children
     (Node    : Simple_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Context_Node_Interface'Class));

   type Root_Node_Record is new Simple_Node_Record with null record;

   Root : Root_Node_Record := (others => <>);

   type File_Node_Record is
     new Context_Node_Interface with
      record
         Contents : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Has_Children
     (Node : File_Node_Record)
      return Boolean
   is (False);

   overriding function Has_Child
     (Node : File_Node_Record;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node  : File_Node_Record;
      Child : String)
      return Context_Node_Interface'Class;

   overriding procedure Iterate_Children
     (Node    : File_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Context_Node_Interface'Class))
   is null;

   function Split_Path
     (Path : String)
      return String_Vectors.Vector;

   function Follow_Path
     (Path : String_Vectors.Vector)
      return Context_Node_Interface'Class;

   --------------------
   -- Append_History --
   --------------------

   procedure Append_History
     (Context : in out Context_Type;
      Item    : String) is
   begin
      Context.History.Append (Item);
   end Append_History;

   ------------------
   -- Change_Scope --
   ------------------

   function Change_Scope
     (Context : in out Context_Type;
      Path    : String)
      return Boolean
   is
      Old_Path : constant String_Vectors.Vector := Context.Current_Path;
      New_Path : constant String_Vectors.Vector := Split_Path (Path);
   begin
      for Item of New_Path loop
         if Item = "." then
            null;
         elsif Item = ".." then
            Context.Set_Parent_Scope;
         elsif Context.Current_Node.Has_Child (Item) then
            Context.Set_Child_Scope (Item);
         else
            Context.Current_Path := Old_Path;
            return False;
         end if;
      end loop;
      return True;
   end Change_Scope;

   --------------------
   -- Create_Context --
   --------------------

   procedure Create_Context
     (Context       : in out Context_Type;
      Default_Scope : String)
   is
   begin
      Context.Home_Path := Split_Path (Default_Scope);
      Context.Current_Path := Context.Home_Path;
      Context.History.Clear;
   end Create_Context;

   ------------------
   -- Current_Node --
   ------------------

   function Current_Node
     (Context : Context_Type)
      return Context_Node_Interface'Class
   is
   begin
      return Follow_Path (Context.Current_Path);
   end Current_Node;

   -----------------
   -- Follow_Path --
   -----------------

   function Follow_Path
     (Path : String_Vectors.Vector)
      return Context_Node_Interface'Class
   is

      function Go
        (Current : Context_Node_Interface'Class;
         Index   : Positive)
         return Context_Node_Interface'Class;

      --------
      -- Go --
      --------

      function Go
        (Current : Context_Node_Interface'Class;
         Index   : Positive)
         return Context_Node_Interface'Class
      is
      begin
         if Index > Path.Last_Index then
            return Current;
         else
            return Go (Current.Get_Child (Path.Element (Index)), Index + 1);
         end if;
      end Go;

   begin
      return Go (Root, 1);
   end Follow_Path;

   ---------------
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node  : File_Node_Record;
      Child : String)
      return Context_Node_Interface'Class
   is
      pragma Unreferenced (Node);
   begin
      return (raise Constraint_Error with
                "not a directory: " & Child);
   end Get_Child;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Context : Context_Type; Offset : Integer) return String
   is
   begin
      if Offset < 0 then
         return Context.History.Element
           (Context.History.Last_Index + 1 + Offset);
      else
         return Context.History.Element (Offset);
      end if;
   end Get_History;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : File_Node_Record;
      Name : String)
      return Boolean
   is
      pragma Unreferenced (Node, Name);
   begin
      return False;
   end Has_Child;

   --------------------
   -- History_Length --
   --------------------

   function History_Length (Context : Context_Type) return Natural is
   begin
      return Context.History.Last_Index;
   end History_Length;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Simple_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Context_Node_Interface'Class))
   is
   begin
      for Child of Node.Child_List loop
         Process (Ada.Strings.Unbounded.To_String (Child.Name),
                  Child.Node.Element);
      end loop;
   end Iterate_Children;

   ---------------
   -- Pop_Scope --
   ---------------

   procedure Pop_Scope
     (Context : in out Context_Type)
   is
   begin
      Context.Current_Path :=
        Context.Scope_Stack.Last_Element;
      Context.Scope_Stack.Delete_Last;
   end Pop_Scope;

   ----------------
   -- Push_Scope --
   ----------------

   procedure Push_Scope
     (Context : in out Context_Type)
   is
   begin
      Context.Scope_Stack.Append (Context.Current_Path);
   end Push_Scope;

   ---------------
   -- Root_Node --
   ---------------

   function Root_Node return Context_Node_Interface'Class is
   begin
      return Root;
   end Root_Node;

   ---------------------
   -- Set_Child_Scope --
   ---------------------

   procedure Set_Child_Scope
     (Context    : in out Context_Type;
      Child_Name : String)
   is
      pragma Assert (Context.Current_Node.Has_Child (Child_Name));
   begin
      Context.Current_Path.Append (Child_Name);
   end Set_Child_Scope;

   -----------------------
   -- Set_Default_Scope --
   -----------------------

   procedure Set_Default_Scope (Context : in out Context_Type) is
   begin
      Context.Current_Path := Context.Home_Path;
   end Set_Default_Scope;

   ----------------------
   -- Set_Parent_Scope --
   ----------------------

   procedure Set_Parent_Scope (Context : in out Context_Type) is
   begin
      if not Context.Current_Path.Is_Empty then
         Context.Current_Path.Delete_Last;
      end if;
   end Set_Parent_Scope;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Context : in out Context_Type;
      Name    : String;
      Value   : String)
   is
   begin
      if Context.Environment.Contains (Name) then
         Context.Environment.Replace (Name, Value);
      else
         Context.Environment.Insert (Name, Value);
      end if;
   end Set_Value;

   ----------------
   -- Split_Path --
   ----------------

   function Split_Path
     (Path : String)
      return String_Vectors.Vector
   is
      P : constant String :=
        (if Path (Path'Last) = '/' then Path else Path & '/');
      Start : Positive := P'First;
      Index : Positive := Start;
   begin
      return Vector : String_Vectors.Vector do
         for Ch of P loop
            if Ch = '/' then
               if Index > Start then
                  Vector.Append (P (Start .. Index - 1));
               end if;

               Start := Index + 1;
            end if;
            Index := Index + 1;
         end loop;
      end return;
   end Split_Path;

   -----------
   -- Value --
   -----------

   function Value
     (Context : Context_Type;
      Name    : String;
      Default : String := "")
      return String
   is
   begin
      return (if Context.Environment.Contains (Name)
              then Context.Environment.Element (Name)
              else Default);
   end Value;

begin
   Root.Child_List.Append
     (Child_Node_Record'
        (Name => Ada.Strings.Unbounded.To_Unbounded_String ("version.txt"),
         Node => Node_Holders.To_Holder
           (File_Node_Record'
                (Contents =>
                     Ada.Strings.Unbounded.To_Unbounded_String
                   (Concorde.Version.Version_String)))));
end Concorde.Contexts;
