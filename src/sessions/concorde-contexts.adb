package body Concorde.Contexts is

   function Split_Path
     (Path : String)
      return String_Vectors.Vector;

   function Follow_Path
     (Root : Concorde.File_System.Node_Interface'Class;
      Path : String_Vectors.Vector)
      return Concorde.File_System.Node_Interface'Class;

   --------------------
   -- Append_History --
   --------------------

   procedure Append_History
     (Context : in out Context_Type;
      Item    : String) is
   begin
      Context.History.Append (Item);
   end Append_History;

   procedure Bind
     (Context : in out Context_Type;
      Scope   : String;
      Node    : Concorde.File_System.Node_Interface'Class)
   is null;

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
      Root          : Concorde.File_System.Node_Id;
      Default_Scope : String)
   is
   begin
      Context.Root := Root;
      Context.Home_Path := Split_Path (Default_Scope);
      Context.Current_Path := Context.Home_Path;
      Context.History.Clear;
   end Create_Context;

   ------------------
   -- Current_Node --
   ------------------

   function Current_Node
     (Context : Context_Type)
      return Concorde.File_System.Node_Interface'Class
   is
   begin
      return Follow_Path
        (Concorde.File_System.Get (Context.Root), Context.Current_Path);
   end Current_Node;

   ---------------
   -- Find_Node --
   ---------------

   function Find_Node
     (Context : Context_Type;
      Path    : String)
      return Concorde.File_System.Node_Id
   is
      Start : constant String_Vectors.Vector :=
        (if Path'Length > 0 and then Path (Path'First) = '/'
         then String_Vectors.Empty_Vector
         else Context.Current_Path);
      Rest  : constant String_Vectors.Vector :=
        Split_Path (Path);
      Full_Path : String_Vectors.Vector := Start;

      It        : Concorde.File_System.Node_Id := Context.Root;

   begin

      Full_Path.Append (Rest);

      if Full_Path.Is_Empty then
         return Context.Root;
      end if;

      for Name of Full_Path loop
         declare
            Node : constant Concorde.File_System.Node_Interface'Class :=
              Concorde.File_System.Get (It);
         begin
            if Node.Is_Leaf then
               raise Context_Error with
                 "not a directory: " & Path;
            elsif not Node.Has_Child (Name) then
               raise Context_Error with
                 "no such file or directory: " & Path;
            else
               It := Node.Get_Child (Name);
            end if;
         end;
      end loop;

      return It;
   end Find_Node;

   -----------------
   -- Follow_Path --
   -----------------

   function Follow_Path
     (Root : Concorde.File_System.Node_Interface'Class;
      Path : String_Vectors.Vector)
      return Concorde.File_System.Node_Interface'Class
   is

      function Go
        (Current : Concorde.File_System.Node_Interface'Class;
         Index   : Positive)
         return Concorde.File_System.Node_Interface'Class;

      --------
      -- Go --
      --------

      function Go
        (Current : Concorde.File_System.Node_Interface'Class;
         Index   : Positive)
         return Concorde.File_System.Node_Interface'Class
      is
      begin
         if Index > Path.Last_Index then
            return Current;
         else
            return Go
              (Concorde.File_System.Get
                 (Current.Get_Child (Path.Element (Index))), Index + 1);
         end if;
      end Go;

   begin
      return Go (Root, 1);
   end Follow_Path;

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

   --------------------
   -- History_Length --
   --------------------

   function History_Length (Context : Context_Type) return Natural is
   begin
      return Context.History.Last_Index;
   end History_Length;

   procedure New_Scope
     (Context : in out Context_Type;
      Scope   : String)
   is null;

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

--  begin
--
--     System_Root_Node.Bind_Child
--       ("version.txt",
--        (File_Node_Record'
--             (Contents =>
--                Ada.Strings.Unbounded.To_Unbounded_String
--                  (Concorde.Version.Version_String))));
--
--     System_Root_Node.Bind_Child
--       ("home",
--        Simple_Node_Record'
--          (Child_List => <>,
--           Child_Map  => <>));

end Concorde.Contexts;
