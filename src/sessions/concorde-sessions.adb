with Ada.Text_IO;
with Ada.Exceptions;

with Tropos.Reader;
with Concorde.Configure;

with Concorde.Commands.Writers;

with Concorde.File_System.Root;

with Concorde.UI.Models.Loader;

with Concorde.Handles.Faction;
with Concorde.Handles.User;

package body Concorde.Sessions is

   function Default_Dashboard return Nazar.Json.Json_Value'Class;

   protected body Session_Data is

      ------------------
      -- Close_Client --
      ------------------

      procedure Close_Client
        (Client_Id      : Concorde.UI.Client_Id)
      is
      begin
         Client_Map.Delete (Client_Id);
      end Close_Client;

      -------------------
      -- Create_Client --
      -------------------

      procedure Create_Client
        (User           : Concorde.Handles.User_Reference;
         Context        : Concorde.Contexts.Context_Type;
         Model_Name     : String;
         Model_Argument : String;
         Client_Id      : out Concorde.UI.Client_Id)
      is
         use type Concorde.UI.Client_Id;
      begin
         Client_Id := 0;

         if not Concorde.UI.Models.Loader.Exists (Model_Name) then
            return;
         end if;

         Last_Client := Last_Client + 1;

         declare
            Model : Concorde.UI.Models.Root_Concorde_Model'Class :=
              Concorde.UI.Models.Loader.Get (Model_Name);
         begin
            Model.Start (User, Model_Argument);
            Client_Map.Insert
              (Last_Client,
               Client_Type'
                 (Model   =>
                      Model_Holders.To_Holder (Model),
                  Context => Context));
         end;

         Client_Id := Last_Client;
      end Create_Client;

      ---------------------
      -- Execute_Command --
      ---------------------

      procedure Execute_Command
        (Client_Id : Concorde.UI.Client_Id;
         Writer    : in out Concorde.Writers.Writer_Interface'Class;
         Command   : String)
      is
      begin
         Concorde.Commands.Execute_Command_Line
           (Line    => Command,
            Context => Client_Map (Client_Id).Context,
            Writer  => Writer);
      end Execute_Command;

      ---------------------------
      -- Get_Environment_Value --
      ---------------------------

      function Get_Environment_Value
        (Name : String)
         return Json.Json_Value'Class
      is
      begin
         if Environment.Contains (Name) then
            return Environment.Element (Name);
         else
            return Nazar.Json.Null_Value;
         end if;
      end Get_Environment_Value;

      ---------------
      -- Get_Model --
      ---------------

      function Get_Model
        (Client_Id : Concorde.UI.Client_Id)
         return Concorde.UI.Models.Root_Concorde_Model'Class
      is
      begin
         return Client_Map.Element (Client_Id).Model.Element;
      end Get_Model;

      ---------------------------
      -- Set_Environment_Value --
      ---------------------------

      procedure Set_Environment_Value
        (Name  : String;
         Value : Json.Json_Value'Class)
      is
      begin
         if Environment.Contains (Name) then
            Environment.Replace (Name, Value);
         else
            Environment.Insert (Name, Value);
         end if;
      end Set_Environment_Value;

      ---------------
      -- Set_Model --
      ---------------

      procedure Set_Model
        (Client_Id : Concorde.UI.Client_Id;
         Model     : Concorde.UI.Models.Root_Concorde_Model'Class)
      is
      begin
         Client_Map (Client_Id).Model :=
           Model_Holders.To_Holder (Model);
      end Set_Model;

   end Session_Data;

   ------------------
   -- Close_Client --
   ------------------

   overriding procedure Close_Client
     (Session   : in out Root_Concorde_Session;
      Client    : Concorde.UI.Client_Id)
   is
   begin
      Session.Data.Close_Client (Client);
   end Close_Client;

   -----------------------
   -- Default_Dashboard --
   -----------------------

   function Default_Dashboard return Nazar.Json.Json_Value'Class is

      Boxes : Nazar.Json.Json_Array;

      procedure Add_Box
        (Id : Natural;
         Left, Top : Positive;
         Right, Bottom : Positive;
         Child_1       : Natural := 0;
         Child_2       : Natural := 0);

      -------------
      -- Add_Box --
      -------------

      procedure Add_Box
        (Id            : Natural;
         Left, Top     : Positive;
         Right, Bottom : Positive;
         Child_1       : Natural := 0;
         Child_2       : Natural := 0)
      is
         Box    : Nazar.Json.Json_Object;
      begin

         Box.Set_Property ("id", Id);

         declare
            Anchor : Nazar.Json.Json_Object;
         begin
            Anchor.Set_Property ("left", Left);
            Anchor.Set_Property ("top", Top);
            Anchor.Set_Property ("right", Right);
            Anchor.Set_Property ("bottom", Bottom);
            Box.Set_Property ("anchor", Anchor);
         end;

         if Child_1 > 0 then
            declare
               Child_Boxes : Nazar.Json.Json_Array;
            begin
               Child_Boxes.Append (Json.Integer_Value (Child_1));
               Child_Boxes.Append (Json.Integer_Value (Child_2));
               Box.Set_Property ("childBoxes", Child_Boxes);
            end;
         end if;

         Boxes.Append (Box);
      end Add_Box;

      Next_Id : Natural := 0;

   begin
      for Config of
        Tropos.Reader.Read_Config
          (Concorde.Configure.Scenario_File
             ("default", "factions", "dashboard.config"))
      loop
         declare
            function Anchor (Name : String) return Positive
            is (Config.Child ("anchor").Get (Name));

            function Child (Index : Positive) return Natural
            is (if Config.Contains ("childBoxes")
                then Config.Child ("childBoxes").Get (Index)
                else 0);
         begin
            Add_Box
              (Id      => Next_Id,
               Left    => Anchor ("left"),
               Top     => Anchor ("top"),
               Right   => Anchor ("right"),
               Bottom  => Anchor ("bottom"),
               Child_1 => Child (1),
               Child_2 => Child (2));
         end;
         Next_Id := Next_Id + 1;
      end loop;

      return Dashboard : Nazar.Json.Json_Object do
         Dashboard.Set_Property ("nextId", Next_Id);
         Dashboard.Set_Property ("boxes", Boxes);
      end return;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Unable to load default dashboard: "
            & Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "config path: "
            & Concorde.Configure.Scenario_File
              ("default", "factions", "dashboard.config"));
         raise;
   end Default_Dashboard;

   ---------------------
   -- Execute_Command --
   ---------------------

   overriding function Execute_Command
     (Session : in out Root_Concorde_Session;
      Client  : Concorde.UI.Client_Id;
      Command : String)
      return Nazar.Json.Json_Value'Class
   is
      Writer : Concorde.Commands.Writers.Json_Writer;

   begin

      Session.Data.Execute_Command
        (Client, Writer, Command);
      return Writer.To_Json;

   end Execute_Command;

   ---------------------------
   -- Handle_Client_Request --
   ---------------------------

   overriding function Handle_Client_Request
     (Session : in out Root_Concorde_Session;
      Client  : Concorde.UI.Client_Id;
      Request : Nazar.Json.Json_Value'Class)
      return Nazar.Json.Json_Value'Class
   is
      Model : Concorde.UI.Models.Root_Concorde_Model'Class :=
        Session.Data.Get_Model (Client);
   begin
      return Model.Handle (Session, Client, Request);
   end Handle_Client_Request;

   --------------------
   -- Handle_Message --
   --------------------

   overriding function Handle_Message
     (Session : in out Root_Concorde_Session;
      Message : Nazar.Json.Json_Value'Class)
      return Nazar.Json.Json_Value'Class
   is
      pragma Unreferenced (Session);
   begin
      return Message;
   end Handle_Message;

   ----------------------
   -- Is_Administrator --
   ----------------------

   overriding function Is_Administrator
     (Session   : Root_Concorde_Session)
      return Boolean
   is
   begin
      return Concorde.Handles.User.Get (Session.User).Administrator;
   end Is_Administrator;

   -------------------------------
   -- New_Administrator_Session --
   -------------------------------

   function New_Administrator_Session
     return Concorde.UI.State_Interface'Class
   is
      User : constant Concorde.Handles.User.User_Type :=
        Concorde.Handles.User.Get_By_Login ("root");
   begin
      return New_Session (User.Login, User.Password);
   end New_Administrator_Session;

   ----------------
   -- New_Client --
   ----------------

   overriding function New_Client
     (Session        : in out Root_Concorde_Session;
      Model_Name     : String;
      Model_Argument : String)
      return Concorde.UI.Client_Id
   is
   begin
      return Id : Concorde.UI.Client_Id do
         Session.Data.Create_Client
           (Session.User, Session.Default_Context,
            Model_Name, Model_Argument,
            Id);
      end return;
   end New_Client;

   -----------------
   -- New_Session --
   -----------------

   function New_Session
     (User_Name : String;
      Password  : String)
      return Concorde.UI.State_Interface'Class
   is
      User : constant Concorde.Handles.User.User_Type :=
        Concorde.Handles.User.Get_By_Login (User_Name);
   begin
      return Session : Root_Concorde_Session do
         if User.Has_Element
           and then User.Password = Password
         then
            Session.User := User.Get_User_Reference;

            declare
               Faction : constant Concorde.Handles.Faction.Faction_Type :=
                 Concorde.Handles.Faction.First_By_User
                   (User.Get_User_Reference);
               Home    : constant String :=
                 (if Faction.Has_Element
                  then "/home/" & Faction.Name
                  else "/");
            begin
               if Faction.Has_Element
                 or else User.Administrator
               then
                  Session.Default_Context.Create_Context
                    (Root          =>
                       Concorde.File_System.Root.System_Root_Node_Id,
                     Default_Scope => Home);
                  Session.Data := new Session_Data;
                  Session.Data.Set_Environment_Value
                    ("HOME", Nazar.Json.String_Value (Home));
                  Session.Data.Set_Environment_Value
                    ("DASHBOARD", Default_Dashboard);
               else
                  Session.User := Concorde.Handles.Null_User_Reference;
               end if;
            end;
         end if;
      end return;
   end New_Session;

   -------------------
   -- Replace_Model --
   -------------------

   overriding procedure Replace_Model
     (Session        : in out Root_Concorde_Session;
      Client         : Concorde.UI.Client_Id;
      Model_Name     : String;
      Model_Argument : String)
   is
      Model : Concorde.UI.Models.Root_Concorde_Model'Class :=
        Concorde.UI.Models.Loader.Get (Model_Name);
   begin
      Model.Start (Session.User, Model_Argument);
      Session.Data.Set_Model (Client, Model);
   end Replace_Model;

   ---------------
   -- User_Name --
   ---------------

   overriding function User_Name
     (Session   : Root_Concorde_Session)
      return String
   is
   begin
      return Concorde.Handles.User.Get (Session.User).Login;
   end User_Name;

   -----------
   -- Valid --
   -----------

   overriding function Valid
     (Session   : Root_Concorde_Session)
      return Boolean
   is
      use type Concorde.Handles.User_Reference;
   begin
      return Session.User /= Concorde.Handles.Null_User_Reference;
   end Valid;

end Concorde.Sessions;
