with Concorde.Commands.Writers;

with Concorde.File_System.Root;

with Concorde.UI.Models.Loader;

with Concorde.Db.Faction;
with Concorde.Db.User;

package body Concorde.Sessions is

   function Default_Dashboard return Concorde.Json.Json_Value'Class;

   ------------------
   -- Close_Client --
   ------------------

   overriding procedure Close_Client
     (Session   : in out Root_Concorde_Session;
      Client    : Concorde.UI.Client_Id)
   is
   begin
      Session.Client_Map.Delete (Client);
   end Close_Client;

   -----------------------
   -- Default_Dashboard --
   -----------------------

   function Default_Dashboard return Concorde.Json.Json_Value'Class is

      Boxes : Concorde.Json.Json_Array;

      procedure Add_Box
        (Id : Natural;
         Left, Top : Positive;
         Right, Bottom : Positive;
         Child_1       : Natural := 0;
         Child_2       : Natural := 0);

      procedure Add_Box
        (Id            : Natural;
         Left, Top     : Positive;
         Right, Bottom : Positive;
         Child_1       : Natural := 0;
         Child_2       : Natural := 0)
      is
         Box    : Concorde.Json.Json_Object;
      begin

         Box.Set_Property ("id", Id);

         declare
            Anchor : Concorde.Json.Json_Object;
         begin
            Anchor.Set_Property ("left", Left);
            Anchor.Set_Property ("top", Top);
            Anchor.Set_Property ("right", Right);
            Anchor.Set_Property ("bottom", Bottom);
            Box.Set_Property ("anchor", Anchor);
         end;

         if Child_1 > 0 then
            declare
               Child_Boxes : Concorde.Json.Json_Array;
            begin
               Child_Boxes.Append (Json.Integer_Value (Child_1));
               Child_Boxes.Append (Json.Integer_Value (Child_2));
               Box.Set_Property ("childBoxes", Child_Boxes);
            end;
         end if;

         Boxes.Append (Box);
      end Add_Box;

   begin
      Add_Box (0, 1, 1, 13, 13, 1, 2);
      Add_Box (1, 1, 1, 13, 11);
      Add_Box (2, 1, 11, 13, 13);

      return Dashboard : Concorde.Json.Json_Object do
         Dashboard.Set_Property ("nextId", 3);
         Dashboard.Set_Property ("boxes", Boxes);
      end return;
   end Default_Dashboard;

   ---------------------
   -- Execute_Command --
   ---------------------

   overriding function Execute_Command
     (Session : in out Root_Concorde_Session;
      Client  : Concorde.UI.Client_Id;
      Command : String)
      return Concorde.Json.Json_Value'Class
   is
      Writer : Concorde.Commands.Writers.Json_Writer;
      Position : constant Client_Maps.Cursor :=
        Session.Client_Map.Find (Client);
   begin

      if Client_Maps.Has_Element (Position) then
         declare
            Context : Concorde.Contexts.Context_Type renames
              Session.Client_Map (Position).Context;
         begin
            Concorde.Commands.Execute_Command_Line
              (Command, Session, Context, Writer);
         end;
      else
         Writer.Put_Error ("bad client");
      end if;

      return Writer.To_Json;

   end Execute_Command;

   ---------------------------
   -- Handle_Client_Request --
   ---------------------------

   overriding function Handle_Client_Request
     (Session : in out Root_Concorde_Session;
      Client  : Concorde.UI.Client_Id;
      Request : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class
   is
      Model : Concorde.UI.Models.Root_Concorde_Model'Class renames
        Session.Client_Map (Client).Model.Reference;
   begin
      return Model.Handle (Session, Client, Request);
   end Handle_Client_Request;

   --------------------
   -- Handle_Message --
   --------------------

   overriding function Handle_Message
     (Session : in out Root_Concorde_Session;
      Message : Concorde.Json.Json_Value'Class)
      return Concorde.Json.Json_Value'Class
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
      return Concorde.Db.User.Get (Session.User).Administrator;
   end Is_Administrator;

   -------------------------------
   -- New_Administrator_Session --
   -------------------------------

   function New_Administrator_Session
     return Concorde.UI.State_Interface'Class
   is
      User : constant Concorde.Db.User.User_Type :=
        Concorde.Db.User.Get_By_Login ("root");
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
      use type Concorde.UI.Client_Id;
   begin
      if not Concorde.UI.Models.Loader.Exists (Model_Name) then
         return 0;
      end if;

      Session.Last_Client := Session.Last_Client + 1;

      declare
         Model : Concorde.UI.Models.Root_Concorde_Model'Class :=
           Concorde.UI.Models.Loader.Get (Model_Name);
      begin
         Model.Start (Session.User, Model_Argument);
         Session.Client_Map.Insert
           (Session.Last_Client,
            Client_Type'
              (Model =>
                Model_Holders.To_Holder (Model),
               Context => Session.Default_Context));
      end;

      return Session.Last_Client;
   end New_Client;

   -----------------
   -- New_Session --
   -----------------

   function New_Session
     (User_Name : String;
      Password  : String)
      return Concorde.UI.State_Interface'Class
   is
      User : constant Concorde.Db.User.User_Type :=
        Concorde.Db.User.Get_By_Login (User_Name);
   begin
      return Session : Root_Concorde_Session do
         if User.Has_Element
           and then User.Password = Password
         then
            Session.User := User.Get_User_Reference;

            declare
               Faction : constant Concorde.Db.Faction.Faction_Type :=
                 Concorde.Db.Faction.First_By_User
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
                  Session.Environment.Insert
                    ("HOME", Concorde.Json.String_Value (Home));
                  Session.Environment.Insert
                    ("DASHBOARD", Default_Dashboard);
               else
                  Session.User := Concorde.Db.Null_User_Reference;
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
      Session.Client_Map (Client).Model :=
        Model_Holders.To_Holder (Model);
   end Replace_Model;

   ---------------
   -- User_Name --
   ---------------

   overriding function User_Name
     (Session   : Root_Concorde_Session)
      return String
   is
   begin
      return Concorde.Db.User.Get (Session.User).Login;
   end User_Name;

   -----------
   -- Valid --
   -----------

   overriding function Valid
     (Session   : Root_Concorde_Session)
      return Boolean
   is
      use type Concorde.Db.User_Reference;
   begin
      return Session.User /= Concorde.Db.Null_User_Reference;
   end Valid;

end Concorde.Sessions;
