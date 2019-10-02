with Concorde.Db.User;

package body Concorde.Sessions is

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
     (Session   : in out Root_Concorde_Session)
      return Concorde.UI.Client_Id
   is
      use type Concorde.UI.Client_Id;
      Client : Client_Type;
   begin
      Session.Last_Client := Session.Last_Client + 1;
      Session.Client_Map.Insert (Session.Last_Client, Client);
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
