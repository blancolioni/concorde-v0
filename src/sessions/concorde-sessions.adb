with Concorde.Commands.Writers;

with Concorde.UI.Models.Loader;

with Concorde.Db.User;

package body Concorde.Sessions is

   --------------------
   -- Add_To_History --
   --------------------

   procedure Add_To_History
     (Session : in out Root_Concorde_Session'Class;
      Item    : String)
   is
   begin
      Session.Commands.Append (Item);
   end Add_To_History;

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

   ---------------------
   -- Execute_Command --
   ---------------------

   overriding function Execute_Command
     (Session : in out Root_Concorde_Session;
      Command : String)
      return String
   is
      Writer : Concorde.Commands.Writers.String_Writer;
   begin

      Concorde.Commands.Execute_Command_Line
        (Command, Session, Writer);

      return Writer.To_String;

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
      return Model.Handle (Session, Request);
   end Handle_Client_Request;

   -------------
   -- History --
   -------------

   function History
     (Session : Root_Concorde_Session'Class;
      Offset  : Integer)
      return String
   is
   begin
      if Offset = 0 then
         return "";
      elsif Offset > 0 then
         return Session.Commands.Element (Offset);
      else
         return Session.Commands.Element
           (Session.Commands.Last_Index + 1 + Offset);
      end if;
   end History;

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

   ----------------
   -- New_Client --
   ----------------

   overriding function New_Client
     (Session    : in out Root_Concorde_Session;
      Model_Name : String)
      return Concorde.UI.Client_Id
   is
      use type Concorde.UI.Client_Id;
   begin
      if not Concorde.UI.Models.Loader.Exists (Model_Name) then
         return 0;
      end if;

      Session.Last_Client := Session.Last_Client + 1;
      Session.Client_Map.Insert
        (Session.Last_Client,
         Client_Type'
           (Model =>
                Client_Holders.To_Holder
                  (Concorde.UI.Models.Loader.Get (Model_Name))));
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
         end if;
      end return;
   end New_Session;

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
