with Ada.Text_IO;

with WL.Guids.Maps;

with Gnoga.Gui.Base;

with Concorde.UI.Models.Dashboard;
with Concorde.UI.Views.Dashboard;

with Concorde.UI.Models.Login;
with Concorde.UI.Views.Login;

with Concorde.Commands;

with Concorde.Factions;
with Concorde.Star_Systems;
with Concorde.Worlds;

with Concorde.Db.Faction;
with Concorde.Db.Script;
with Concorde.Db.Script_Line;
with Concorde.Db.User;

package body Concorde.Sessions is

   package Session_Maps is
     new WL.Guids.Maps (Concorde_Session);

   protected Session_Map is
      function Find_Session
        (Gnoga_View : Gnoga.Gui.View.Pointer_To_View_Base_Class)
         return Concorde_Session;
      procedure End_Session (Session : in out Concorde_Session);
      procedure End_All_Sessions;
      procedure Broadcast (Signal : Concorde.Signals.Signal_Type);
      procedure Add_Session (Session : Concorde_Session);
   private
      Map : Session_Maps.Map;
   end Session_Map;

   procedure Show_Login_View
     (Session : not null access Root_Concorde_Session'Class);

   procedure On_Main_View_Destroyed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Main_Window_Key_Press
     (Object         : in out Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event : in     Gnoga.Gui.Base.Keyboard_Event_Record);

   -------------------
   -- Activate_View --
   -------------------

   procedure Activate_View
     (Session : in out Root_Concorde_Session'Class;
      View    : access Concorde.UI.Views.Root_View_Type'Class)
   is
      Acc : constant View_Access := View_Access (View);
   begin
      if not Session.Views.Contains (Acc) then
         Session.Views.Insert (Session.Views.First, Acc);
      end if;

      Session.Active_View := Acc;
   end Activate_View;

   -----------------
   -- Add_Handler --
   -----------------

   overriding function Add_Handler
     (Session : in out Root_Concorde_Session;
      Signal  : Concorde.Signals.Signal_Type;
      Handler : Concorde.Signals.Handler_Type;
      Data    : Concorde.Signals.Signal_Data_Interface'Class)
      return Concorde.Signals.Handler_Id
   is
   begin
      return Session.Dispatcher.Add_Handler (Signal, Handler, Data);
   end Add_Handler;

   --------------------
   -- Add_To_History --
   --------------------

   procedure Add_To_History
     (Session      : in out Root_Concorde_Session'Class;
      Command_Line : String)
   is
   begin
      Session.History.Append (Command_Line);
   end Add_To_History;

   ---------------
   -- Broadcast --
   ---------------

   procedure Broadcast (Signal : Concorde.Signals.Signal_Type) is
   begin
      Session_Map.Broadcast (Signal);
   end Broadcast;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Session     : not null access Root_Concorde_Session'Class;
      Main_Window : Gnoga.Gui.Window.Pointer_To_Window_Class)
   is
   begin
      Session.Main_Window := Main_Window;
      Main_Window.On_Key_Press_Handler (On_Main_Window_Key_Press'Access);
      Session.Show_Login_View;
   end Connect;

   ----------------------
   -- End_All_Sessions --
   ----------------------

   procedure End_All_Sessions is
   begin
      Session_Map.End_All_Sessions;
   end End_All_Sessions;

   -----------------
   -- End_Session --
   -----------------

   procedure End_Session (Session : in out Concorde_Session) is
   begin
      Ada.Text_IO.Put_Line
        ("ending session: "
         & WL.Guids.To_String (Session.Id));
      Session_Map.End_Session (Session);

   end End_Session;

   ------------------
   -- Find_Session --
   ------------------

   function Find_Session
     (Gnoga_View : Gnoga.Gui.View.Pointer_To_View_Base_Class)
      return Concorde_Session
   is
   begin
      return Session_Map.Find_Session (Gnoga_View);
   end Find_Session;

   -----------
   -- Login --
   -----------

   procedure Login
     (Session : not null access Root_Concorde_Session'Class;
      User    : Concorde.Db.User_Reference)
   is
   begin
      Session.User := User;
      Session.Administrator :=
        Concorde.Db.User.Get (User).Administrator;

      Session.Faction :=
        Concorde.Db.Faction.First_Reference_By_User (User);

      Session.Context :=
        Concorde.Contexts.Initial_Context_Path (Session.Faction);

      Ada.Text_IO.Put_Line
        ("session started for user " & Session.User_Name
         & "; initial context "
         & Session.Context.Name);

      Session.Set_Environment_Value
        ("FACTION",
         Concorde.Factions.Name (Session.Faction));

      Session.Set_Environment_Value
        ("CAPITAL_SYSTEM",
         Concorde.Star_Systems.Name
           (Concorde.Factions.Capital_System (Session.Faction)));

      Session.Set_Environment_Value
        ("CAPITAL_WORLD",
         Concorde.Worlds.Name
           (Concorde.Factions.Capital_World (Session.Faction)));

      if Session.Is_Gnoga then
         declare
            use Concorde.Db;
            use Concorde.UI.Models.Dashboard;
            Main_Model : constant Dashboard_Model :=
                           Create_Dashboard_Model (Session);
            Main_View  : constant Concorde.UI.Views.View_Type :=
                           Concorde.UI.Views.Dashboard.Dashboard_View
                             (Main_Model);
            Rc_Script  : constant Concorde.Db.Script_Reference :=
                           Concorde.Db.Script.Get_Reference_By_User_Name
                             (User, "rc");
         begin
            Session.Current_View.Gnoga_View.Visible (False);
            Concorde.UI.Views.Destroy
              (Concorde.UI.Views.View_Type (Session.Current_View));
            Main_View.Create (Session, Session.Main_Window.all, "dashboard");
            Session.Main_Window.Set_View (Main_View.Gnoga_View.all);
            Session.Current_View := View_Access (Main_View);
            Session.Views.Append (View_Access (Main_View));
            Main_View.Gnoga_View.On_Destroy_Handler
              (On_Main_View_Destroyed'Access);
            Main_View.Gnoga_View.Focus;

            if Rc_Script /= Null_Script_Reference then
               for Line of
                 Concorde.Db.Script_Line.Select_Script_Line_Bounded_By_Index
                   (Rc_Script, 1, Positive'Last)
               loop
                  Concorde.Commands.Execute_Command_Line
                    (Line    => Line.Line,
                     Session => Concorde.Sessions.Concorde_Session (Session),
                     Writer  => Concorde.Commands.Null_Writer);
               end loop;
            end if;
         end;
      end if;

   end Login;

   ------------
   -- Logout --
   ------------

   procedure Logout (Session : not null access Root_Concorde_Session'Class) is
   begin
      Session.User := Concorde.Db.Null_User_Reference;
      Session.Show_Login_View;
   end Logout;

   -----------------------
   -- New_Gnoga_Session --
   -----------------------

   function New_Gnoga_Session return Concorde_Session is
   begin
      return Session : constant Concorde_Session :=
        new Root_Concorde_Session'
          (Id => WL.Guids.New_Guid,
           others => <>)
      do
         Session.Is_Gnoga := True;
         Session_Map.Add_Session (Session);
         Ada.Text_IO.Put_Line
           ("new session: "
            & WL.Guids.To_String (Session.Id));
      end return;
   end New_Gnoga_Session;

   ----------------------
   -- New_Repl_Session --
   ----------------------

   function New_Repl_Session
     (User : Concorde.Db.User_Reference)
      return Concorde_Session
   is
   begin
      return Session : constant Concorde_Session :=
        new Root_Concorde_Session'
          (Id     => WL.Guids.New_Guid,
           others => <>)
      do
         Session.Login (User);
         Session_Map.Add_Session (Session);
         Ada.Text_IO.Put_Line
           ("new session: "
            & WL.Guids.To_String (Session.Id));
      end return;
   end New_Repl_Session;

   ----------------------------
   -- On_Main_View_Destroyed --
   ----------------------------

   procedure On_Main_View_Destroyed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      View : Gnoga.Gui.View.View_Type'Class renames
               Gnoga.Gui.View.View_Type'Class (Object);
      Session : Concorde_Session :=
                  Find_Session (View'Unchecked_Access);
   begin
      if Session /= null then
         End_Session (Session);
      end if;
   end On_Main_View_Destroyed;

   ------------------------------
   -- On_Main_Window_Key_Press --
   ------------------------------

   procedure On_Main_Window_Key_Press
     (Object         : in out Gnoga.Gui.Base.Base_Type'Class;
      Keyboard_Event : in     Gnoga.Gui.Base.Keyboard_Event_Record)
   is
      Session : constant Concorde_Session :=
                  Concorde_Session (Object.Connection_Data);
   begin
      if not Session.Current_View.Accepts_Text_Entry then
         Session.Main_View.Handle_Key_Press
           (Meta    => Keyboard_Event.Meta,
            Alt     => Keyboard_Event.Alt,
            Control => Keyboard_Event.Control,
            Shift   => Keyboard_Event.Shift,
            Key     =>
              (if Keyboard_Event.Key_Code = 0
               then Wide_Character'Pos (Keyboard_Event.Key_Char)
               else Keyboard_Event.Key_Code));
      end if;
   end On_Main_Window_Key_Press;

   --------------------
   -- Remove_Handler --
   --------------------

   overriding procedure Remove_Handler
     (Session : in out Root_Concorde_Session;
      Signal  : Concorde.Signals.Signal_Type;
      Id      : Concorde.Signals.Handler_Id)
   is
   begin
      Session.Dispatcher.Remove_Handler (Signal, Id);
   end Remove_Handler;

   -----------------
   -- Send_Signal --
   -----------------

   overriding procedure Send_Signal
     (Session : in out Root_Concorde_Session;
      Signal  : Concorde.Signals.Signal_Type)
   is
   begin
      Session.Dispatcher.Call_Handlers (Session, Signal);
   end Send_Signal;

   -----------------
   -- Session_Map --
   -----------------

   protected body Session_Map is

      -----------------
      -- Add_Session --
      -----------------

      procedure Add_Session (Session : Concorde_Session) is
      begin
         Map.Insert (Session.Id, Session);
      end Add_Session;

      ---------------
      -- Broadcast --
      ---------------

      procedure Broadcast (Signal : Concorde.Signals.Signal_Type) is
      begin
         for Session of Map loop
            Session.Send_Signal (Signal);
         end loop;
      end Broadcast;

      ----------------------
      -- End_All_Sessions --
      ----------------------

      procedure End_All_Sessions is
      begin
         while not Map.Is_Empty loop
            declare
               Session : constant Concorde_Session :=
                           Session_Maps.Element (Map.First);
            begin
               Map.Delete (Session.Id);
               while not Session.Views.Is_Empty loop
                  declare
                     V : Concorde.UI.Views.View_Type :=
                           Concorde.UI.Views.View_Type
                             (Session.Views.First_Element);
                  begin
                     Concorde.UI.Views.Destroy (V);
                     Session.Views.Delete_First;
                  end;
               end loop;
            end;
         end loop;
      end End_All_Sessions;

      -----------------
      -- End_Session --
      -----------------

      procedure End_Session (Session : in out Concorde_Session) is
      begin
         Map.Delete (Session.Id);
         while not Session.Views.Is_Empty loop
            declare
               V : Concorde.UI.Views.View_Type :=
                     Concorde.UI.Views.View_Type (Session.Views.First_Element);
            begin
               Concorde.UI.Views.Destroy (V);
               Session.Views.Delete_First;
            end;
         end loop;

         Session := null;
      end End_Session;

      ------------------
      -- Find_Session --
      ------------------

      function Find_Session
        (Gnoga_View : Gnoga.Gui.View.Pointer_To_View_Base_Class)
      return Concorde_Session
      is
         use type Gnoga.Gui.View.Pointer_To_View_Base_Class;
      begin
         for Session of Map loop
            if Session.Main_View.Gnoga_View = Gnoga_View then
               return Session;
            end if;
         end loop;
         return null;
      end Find_Session;

   end Session_Map;

   ---------------------------
   -- Set_Environment_Value --
   ---------------------------

   procedure Set_Environment_Value
     (Session : in out Root_Concorde_Session'Class;
      Name    : String;
      Value   : String)
   is
   begin
      if Session.Environment.Contains (Name) then
         Session.Environment.Replace (Name, Value);
      else
         Session.Environment.Insert (Name, Value);
      end if;
   end Set_Environment_Value;

   ---------------------
   -- Show_Login_View --
   ---------------------

   procedure Show_Login_View
     (Session : not null access Root_Concorde_Session'Class)
   is
      Login_Model : constant Concorde.UI.Models.Login.Login_Model :=
                      Concorde.UI.Models.Login.Create_Login_Model
                        (Session);
      Login_View  : constant Concorde.UI.Views.View_Type :=
                      Concorde.UI.Views.Login.Login_View
                        (Login_Model);
   begin
      if Session.Current_View /= null then
         Session.Current_View.Gnoga_View.Visible (False);
      end if;
      Login_View.Create (Session, Session.Main_Window.all, "");
      Session.Main_Window.Set_View (Login_View.Gnoga_View.all);
      Session.Current_View := View_Access (Login_View);
   end Show_Login_View;

   --------------------
   -- Update_Context --
   --------------------

   procedure Update_Context
     (Session : in out Root_Concorde_Session'Class;
      Context : Concorde.Contexts.Context_Path)
   is
   begin
      Session.Context := Context;
   end Update_Context;

   ---------------
   -- User_Name --
   ---------------

   function User_Name
     (Session : Root_Concorde_Session'Class)
      return String
   is
      use type Concorde.Db.User_Reference;
   begin
      if Session.User = Concorde.Db.Null_User_Reference then
         return "not logged in";
      else
         return Concorde.Db.User.Get (Session.User).Login;
      end if;
   end User_Name;

end Concorde.Sessions;
