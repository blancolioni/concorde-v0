with Ada.Text_IO;

with Nazar.Controllers.Console;
with Nazar.Web_UI.Main;
with Nazar.Web_UI.Views.Web_Console;

with Concorde.UI.Models.Console;

package body Concorde.UI.Web_UI is

   type Web_UI_Type is
     new UI_Interface with
      record
         null;
      end record;

   overriding procedure Start
     (Web_UI  : in out Web_UI_Type);

   overriding procedure Stop
     (Item    : in out Web_UI_Type;
      Message : String);

   Local_Web_UI : aliased Web_UI_Type;

   ----------------
   -- Get_Web_UI --
   ----------------

   function Get_Web_UI return UI_Type is
   begin
      return Local_Web_UI'Access;
   end Get_Web_UI;

   -----------
   -- Start --
   -----------

   overriding procedure Start (Web_UI  : in out Web_UI_Type) is
      pragma Unreferenced (Web_UI);

      Model : constant Concorde.UI.Models.Console.Concorde_Console_Model :=
                Concorde.UI.Models.Console.Console_Model
                  ("/home");
      View  : constant Nazar.Web_UI.Views.Web_Console.Nazar_Web_Console_View :=
        Nazar.Web_UI.Views.Web_Console.Web_Console_View (Model);
      Controller : Nazar.Controllers.Console.Nazar_Console_Controller_Record;
   begin

      Logging.On_Starting;

      Create_Routes;
      Create_Socket;

      AWS.Server.Start
        (Web_Server => Server,
         Name       => "Concorde",
         Callback   => Routes.Handle_Http_Request'Access,
         Port       => 8080);

      Model : constant Concorde.UI.Models.Console.Concorde_Console_Model :=
                Concorde.UI.Models.Console.Console_Model
                  ("/home");
      View  : constant Nazar.Web_UI.Views.Web_Console.Nazar_Web_Console_View :=
        Nazar.Web_UI.Views.Web_Console.Web_Console_View (Model);
      Controller : Nazar.Controllers.Console.Root_Console_Controller;
   begin
      Controller.Start_Console (Model, View);
      Nazar.Web_UI.Main.Start
        (Application_Name => "Concorde",
         Port             => 8080,
         Top_View         => View);
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (Item    : in out Web_UI_Type;
      Message : String)
   is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (Message);
   end Stop;

end Concorde.UI.Web_UI;
