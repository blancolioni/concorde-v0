with Concorde.Db.User;

package body Concorde.UI.Models.Login is

   ------------------------
   -- Create_Login_Model --
   ------------------------

   function Create_Login_Model
     (Session : not null access Concorde.Sessions.Root_Concorde_Session'Class)
      return Login_Model
   is
   begin
      return new Root_Login_Model'
        (Watchers => <>,
         Session  => Concorde.Sessions.Concorde_Session (Session));
   end Create_Login_Model;

   -----------
   -- Login --
   -----------

   function Login
     (Model    : Root_Login_Model'Class;
      Name     : String;
      Password : String)
      return Boolean
   is
      User : constant Concorde.Db.User.User_Type :=
                  Concorde.Db.User.Get_By_Login (Name);
   begin
      if User.Has_Element
        and then User.Password = Password
      then
         Model.Session.Login (User.Get_User_Reference);
         return True;
      else
         return False;
      end if;
   end Login;

end Concorde.UI.Models.Login;
