with Concorde.Star_Systems;

with Concorde.UI.Models.Galaxy;
with Concorde.UI.Models.Market;
with Concorde.UI.Models.Orbits;
with Concorde.UI.Models.Star_System;
with Concorde.UI.Models.World;

with Concorde.UI.Views.Console;
with Concorde.UI.Views.Galaxy;
with Concorde.UI.Views.Star_System;
with Concorde.UI.Views.World;

with Concorde.UI.Views.Tables;

with Concorde.Db;
with Concorde.Db.Market;
with Concorde.Db.World;

package body Concorde.Commands.Views is

   type Load_Galaxy_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_Galaxy_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type;

   type Load_Star_System_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_Star_System_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type;

   type Load_World_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_World_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type;

   type Load_Market_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Load_Market_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type;

   type Show_Orbiting_Ships_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Show_Orbiting_Ships_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type;

   type Console_View_Command is
     new Load_View_Command with null record;

   overriding function Create_View
     (Command   : Console_View_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_Galaxy_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Session);
   begin
      if Contains (Arguments, "table") then
         return Concorde.UI.Views.Tables.Create_Table_View
           (Concorde.UI.Models.Galaxy.Create_Galaxy_Model);
      else
         return Concorde.UI.Views.Galaxy.Galaxy_View
           (Concorde.UI.Models.Galaxy.Create_Galaxy_Model);
      end if;
   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_Star_System_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Session);
   begin
      if not Contains (Arguments, "name") then
         return null;
      end if;

      declare
         use Concorde.Db;
         Star_System : constant Star_System_Reference :=
                         Concorde.Star_Systems.Find_Exact
                           (Argument (Arguments, "name"));
      begin
         if Star_System = Null_Star_System_Reference then
            return null;
         end if;

         if Contains (Arguments, "table") then
            return Concorde.UI.Views.Tables.Create_Table_View
              (Concorde.UI.Models.Star_System.Create (Star_System));
         else
            return Concorde.UI.Views.Star_System.Star_System_View
              (Concorde.UI.Models.Star_System.Create (Star_System));
         end if;
      end;
   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_World_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Session);
   begin
      if (not Contains (Arguments, "star-system-name")
          or else not Contains (Arguments, "world-number"))
        and then not Contains (Arguments, "world-name")
      then
         return null;
      end if;

      declare
         use Concorde.Db;
         Star_System  : constant Star_System_Reference :=
                          (if Contains (Arguments, "star-system-name")
                           then Concorde.Star_Systems.Find_Exact
                             (Argument (Arguments, "star-system-name"))
                           else Null_Star_System_Reference);
         World_Number : constant Natural :=
                          Positive'Value
                            (Argument (Arguments, "world-number", "0"));
         World_Name   : constant String :=
                          Argument (Arguments, "world-name", "");
         Reference    : World_Reference := Null_World_Reference;
         Index        : Natural := 0;
      begin

         if World_Name /= "" then
            Reference :=
              Concorde.Db.World.First_Reference_By_Name (World_Name);
         else
            if Star_System = Null_Star_System_Reference then
               return null;
            end if;

            for World of
              Concorde.Db.World.Select_By_Star_System (Star_System)
            loop
               Index := Index + 1;
               if Index = World_Number then
                  Reference := World.Get_World_Reference;
                  exit;
               end if;
            end loop;
         end if;

         if Reference = Null_World_Reference then
            return null;
         end if;

         if Contains (Arguments, "table") then
            return Concorde.UI.Views.Tables.Create_Table_View
              (Concorde.UI.Models.World.Create (Reference),
               Headings_Down => True);
         else
            return Concorde.UI.Views.World.World_View
              (Concorde.UI.Models.World.Create (Reference));
         end if;
      end;

   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Load_Market_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Session);
   begin
      if Argument_Count (Arguments) = 1 then
         declare
            use Concorde.Db;
            World : constant Concorde.Db.World_Reference :=
                      Concorde.Db.World.First_Reference_By_Name
                        (Argument (Arguments, 1));
            Market : constant Concorde.Db.Market_Reference :=
                       (if World = Null_World_Reference
                        then Null_Market_Reference
                        else Concorde.Db.Market.Get_Reference_By_World
                          (World));

         begin
            if World = Null_World_Reference
              or else Market = Null_Market_Reference
            then
               return null;
            end if;

            declare
               Model : constant Concorde.UI.Models.Market.Market_Model :=
                         Concorde.UI.Models.Market.Create (Market);
            begin
               return Concorde.UI.Views.Tables.Create_Table_View (Model);
            end;
         end;
      else
         return null;
      end if;
   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Show_Orbiting_Ships_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Session);
   begin
      if Argument_Count (Arguments) = 1 then
         declare
            use Concorde.Db;
            World  : constant Concorde.Db.World_Reference :=
                       Concorde.Db.World.First_Reference_By_Name
                         (Argument (Arguments, 1));
         begin
            if World = Null_World_Reference then
               return null;
            end if;

            declare
               Model : constant Concorde.UI.Models.Orbits.Orbiting_Ship_Model
                 := Concorde.UI.Models.Orbits.Create (World);
            begin
               return Concorde.UI.Views.Tables.Create_Table_View (Model);
            end;
         end;
      else
         return null;
      end if;
   end Create_View;

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Command   : Console_View_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Arguments : Argument_List)
      return Concorde.UI.Views.View_Type
   is
      pragma Unreferenced (Command, Arguments, Session);
   begin
      return Concorde.UI.Views.Console.Console_View;
   end Create_View;

   ------------------------
   -- Load_View_Commands --
   ------------------------

   procedure Load_View_Commands is
      Load_Galaxy      : Load_Galaxy_Command;
      Load_Star_System : Load_Star_System_Command;
      Load_World       : Load_World_Command;
      Load_Market      : Load_Market_Command;
      Show_Orbit       : Show_Orbiting_Ships_Command;
      Console          : Console_View_Command;
   begin
      Register ("load-galaxy-view", Load_Galaxy);
      Register ("load-star-system-view", Load_Star_System);
      Register ("load-world-view", Load_World);
      Register ("show-market", Load_Market);
      Register ("show-orbit", Show_Orbit);
      Register ("console", Console);
   end Load_View_Commands;

   -------------
   -- Perform --
   -------------

   overriding procedure Perform
     (Command   : Load_View_Command;
      Session   : Concorde.Sessions.Concorde_Session;
      Writer    : Writer_Interface'Class;
      Arguments : Argument_List)
   is
      use Concorde.UI.Views;
      View : constant View_Type :=
               Load_View_Command'Class (Command)
               .Create_View (Session, Arguments);
   begin
      if View = null then
         Writer.Put_Error
           ("unable to create view");
      end if;

      Session.Main_View.Add_Child (View);
   end Perform;

end Concorde.Commands.Views;
