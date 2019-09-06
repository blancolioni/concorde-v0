with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Concorde.Contexts.Errors;
with Concorde.Contexts.Root;

with Concorde.Contexts.Faction_Container;
with Concorde.Contexts.Factions;

package body Concorde.Contexts is

   -------------------
   -- Child_Context --
   -------------------

   function Child_Context
     (Context : Root_Context_Type;
      Name    : String)
      return Context_Type
   is
      Children : Context_List;
   begin
      Root_Context_Type'Class (Context).Get_Child_Contexts (Children);
      for Child of Children loop
         if Child.Name = Name then
            return Child;
         end if;
      end loop;
      return Concorde.Contexts.Errors.Error_Context
        (Name & ": not found in " & Root_Context_Type'Class (Context).Name);
   end Child_Context;

   -------------------
   -- Child_Context --
   -------------------

   overriding function Child_Context
     (Context : Context_Path;
      Name    : String)
      return Context_Type
   is
   begin
      if Context.List.Is_Empty then
         return Concorde.Contexts.Root.Root_Context.Child_Context (Name);
      else
         return Context.List.First_Element.Child_Context (Name);
      end if;
   end Child_Context;

   -----------
   -- Class --
   -----------

   overriding function Class
     (Context : Context_Path)
      return String
   is
      pragma Unreferenced (Context);
   begin
      return "path";
   end Class;

   -------------
   -- Context --
   -------------

   function Context
     (Path : Context_Path)
      return Context_Type
   is
   begin
      if Path.List.Is_Empty then
         return Concorde.Contexts.Root.Root_Context;
      else
         return Path.List.First_Element;
      end if;
   end Context;

   ------------------------
   -- Get_Child_Contexts --
   ------------------------

   overriding procedure Get_Child_Contexts
     (Context  : Context_Path;
      Children : in out Context_List'Class)
   is
   begin
      if Context.List.Is_Empty then
         Concorde.Contexts.Root.Root_Context.Get_Child_Contexts (Children);
      else
         Context.List.First_Element.Get_Child_Contexts (Children);
      end if;
   end Get_Child_Contexts;

   ---------------------
   -- Get_Child_Names --
   ---------------------

   procedure Get_Child_Names
     (Context : Root_Context_Type'Class;
      Names   : out Child_Name_Lists.List)
   is
      procedure Add_Name (Context : Context_Type);

      --------------
      -- Add_Name --
      --------------

      procedure Add_Name (Context : Context_Type) is
      begin
         Names.Append (Context.Name);
      end Add_Name;

   begin
      Names.Clear;
      Context.Iterate_Contexts (Add_Name'Access);
   end Get_Child_Names;

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content
     (Context : Root_Context_Type'Class)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;

      procedure Add_Line (Line : String);

      --------------
      -- Add_Line --
      --------------

      procedure Add_Line (Line : String) is
      begin
         if Result /= "" then
            Result := Result & Character'Val (10);
         end if;
         Result := Result & Line;
      end Add_Line;

   begin
      Context.Iterate_Content_Lines (Add_Line'Access);
      return To_String (Result);
   end Get_Content;

   --------
   -- Go --
   --------

   function Go
     (Start : Context_Path;
      Scope : String)
      return Context_Path
   is
      Path    : constant String :=
                  (if Scope /= "" and then Scope (Scope'Last) = '/'
                   then Scope
                   else Scope & "/");
      Current : Positive := Path'First;
      Result  : Context_Path;

   begin

      if Path /= "" and then Path (Path'First) = '/' then
         Result.List.Append (Concorde.Contexts.Root.Root_Context);
         Current := Current + 1;
      else
         Result := Start;
      end if;

      while Current <= Path'Last loop
         declare
            Index : constant Positive :=
                      Ada.Strings.Fixed.Index (Path, "/", Current);
            Next  : Positive := Index + 1;
            Name  : constant String := Path (Current .. Index - 1);
         begin
            if Name = "." then
               null;
            elsif Name = ".." then
               Result.To_Parent;
            elsif Result.Has_Child_Context (Name) then
               Result.List.Insert
                 (Result.List.First,
                  Result.Child_Context (Name));
            else
               Result.List.Insert
                 (Result.List.First,
                  Concorde.Contexts.Errors.Error_Context
                    ("scope not found"));
               exit;
            end if;

            while Next <= Path'Last
              and then Path (Next) = '/'
            loop
               Next := Next + 1;
            end loop;

            Current := Next;
         end;
      end loop;

      return Result;

   end Go;

   ---------------
   -- Has_Child --
   ---------------

   function Has_Child_Context
     (Context : Root_Context_Type;
      Name    : String)
      return Boolean
   is
      Children : Child_Name_Lists.List;
   begin
      Root_Context_Type'Class (Context).Get_Child_Names (Children);
      for Child_Name of Children loop
         if Child_Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Has_Child_Context;

   --------------------------
   -- Initial_Context_Path --
   --------------------------

   function Initial_Context_Path
     (Faction : Concorde.Db.Faction_Reference)
      return Context_Path
   is
      Path : Context_Path;
   begin
      Path.List.Append
        (Concorde.Contexts.Factions.Faction_Context (Faction));
      Path.List.Append
        (Concorde.Contexts.Faction_Container.Faction_Container_Context);
      Path.List.Append (Concorde.Contexts.Root.Root_Context);
      return Path;
   end Initial_Context_Path;

   -------------------------
   -- Iterate_Child_Names --
   -------------------------

   procedure Iterate_Child_Names
     (Context : Root_Context_Type'Class;
      Process : not null access
        procedure (Name : String))
   is
      Children : Child_Name_Lists.List;
   begin
      Context.Get_Child_Names (Children);
      for Name of Children loop
         Process (Name);
      end loop;
   end Iterate_Child_Names;

   ---------------------------
   -- Iterate_Content_Lines --
   ---------------------------

   procedure Iterate_Content_Lines
     (Context : Root_Context_Type;
      Process : not null access
        procedure (Line : String))
   is
      List : Child_Name_Lists.List;
   begin
      Root_Context_Type'Class (Context).Get_Child_Names (List);
      for Name of List loop
         Process (Name);
      end loop;
   end Iterate_Content_Lines;

   ---------------------------
   -- Iterate_Content_Lines --
   ---------------------------

   overriding procedure Iterate_Content_Lines
     (Context : Context_Path;
      Process : not null access
        procedure (Line : String))
   is
   begin
      if Context.List.Is_Empty then
         Concorde.Contexts.Root.Root_Context.Iterate_Content_Lines
           (Process);
      else
         Context.List.First_Element.Iterate_Content_Lines (Process);
      end if;
   end Iterate_Content_Lines;

   ----------------------
   -- Iterate_Contexts --
   ----------------------

   procedure Iterate_Contexts
     (Context : Root_Context_Type'Class;
      Process : not null access
        procedure (Context : Context_Type))
   is
      Children : Context_List;
   begin
      Context.Get_Child_Contexts (Children);
      for Child of Children loop
         Process (Child);
      end loop;
   end Iterate_Contexts;

   --------------------
   -- Match_Children --
   --------------------

   procedure Match_Children
     (Context : Root_Context_Type'Class;
      Pattern : String;
      Matches : out Child_Name_Lists.List)
   is
      Children : Child_Name_Lists.List;
   begin
      Matches.Clear;
      Context.Get_Child_Names (Children);
      for Name of Children loop
         if Name'Length >= Pattern'Length
           and then Name (Name'First .. Name'First + Pattern'Length - 1)
           = Pattern
         then
            Matches.Append (Name);
         end if;
      end loop;
   end Match_Children;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Context : Context_Path)
      return String
   is
      function N (Position : Context_Lists.Cursor) return String;

      -------
      -- N --
      -------

      function N (Position : Context_Lists.Cursor) return String is
      begin
         if not Context_Lists.Has_Element (Position) then
            return "";
         elsif not Context_Lists.Has_Element
           (Context_Lists.Previous (Position))
         then
            return Context_Lists.Element (Position).Name;
         else
            return Context_Lists.Element (Position).Name
              & "/" & N (Context_Lists.Previous (Position));
         end if;
      end N;

   begin
      if Context.List.Is_Empty
        or else Context.List.First_Element.Is_Root
      then
         return "/";
      elsif not Context.List.First_Element.Is_Valid then
         return Context.List.First_Element.Name;
      else
         return N (Context.List.Last);
      end if;
   end Name;

   --------------
   -- To_Child --
   --------------

   procedure To_Child
     (Path          : in out Context_Path;
      Child_Context : Context_Type)
   is
   begin
      Path.List.Insert (Path.List.First, Child_Context);
   end To_Child;

   ---------------
   -- To_Parent --
   ---------------

   procedure To_Parent
     (Path : in out Context_Path)
   is
   begin
      if not Path.List.Is_Empty then
         Path.List.Delete_First;
      end if;
      if Path.List.Is_Empty then
         Path.List.Append (Concorde.Contexts.Root.Root_Context);
      end if;
   end To_Parent;

end Concorde.Contexts;
