with Concorde.Sessions;

package Concorde.UI.Models.Login is

   type Root_Login_Model is
     new Root_Concorde_Model with private;

   function Login
     (Model    : Root_Login_Model'Class;
      Name     : String;
      Password : String)
      return Boolean;

   type Login_Model is access all Root_Login_Model'Class;

   function Create_Login_Model
     (Session : not null access Concorde.Sessions.Root_Concorde_Session'Class)
      return Login_Model;

private

   type Root_Login_Model is
     new Root_Concorde_Model with
      record
         Session : Concorde.Sessions.Concorde_Session;
      end record;

   overriding function Title
     (Model : Root_Login_Model)
      return String
   is ("Login");

end Concorde.UI.Models.Login;
