with Ada.Text_IO;

with AWS.Server;
with AWS.Status;

with Concorde.UI.Web_UI.Handlers;
with Concorde.UI.Web_UI.Routes;

package body Concorde.UI.Web_UI is

   type Web_UI_Type is
     new UI_Interface with
      record
         null;
      end record;

   overriding procedure Start
     (Web_UI  : Web_UI_Type);

   overriding procedure Stop
     (Item    : Web_UI_Type;
      Message : String);

   WS : AWS.Server.HTTP;

   procedure Create_Routes;

   -------------------
   -- Create_Routes --
   -------------------

   procedure Create_Routes is
   begin
      Routes.Add_Route
        (Method  => AWS.Status.POST,
         Path    => "/login",
         Handler => Handlers.Handle_Login);
      Routes.Add_Route
        (Method  => AWS.Status.POST,
         Path    => "/new-client",
         Handler => Handlers.Handle_New_Client);
      Routes.Add_Route
        (Method  => AWS.Status.POST,
         Path    => "/client/:client",
         Handler => Handlers.Handle_Client_Request);
   end Create_Routes;

   ----------------
   -- Get_Web_UI --
   ----------------

   function Get_Web_UI return UI_Interface'Class is
   begin
      return Web_UI : Web_UI_Type;
   end Get_Web_UI;

   -----------
   -- Start --
   -----------

   overriding procedure Start (Web_UI : Web_UI_Type) is
   begin

      On_UI_Started (Web_UI);

      Create_Routes;

      AWS.Server.Start
        (Web_Server => WS,
         Name       => "Concorde",
         Callback   => Routes.Handle'Access,
         Port       => 8080);
      AWS.Server.Wait;
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (Item    : Web_UI_Type;
      Message : String)
   is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line ("stopping: " & Message);
      AWS.Server.Shutdown (WS);
   end Stop;

end Concorde.UI.Web_UI;
